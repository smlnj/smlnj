(* located-arg-fn.sml
 *
 * Create located arguments.
 *)

functor LocatedArgFn (
    structure SA : STAGED_ALLOCATION
	where type reg_id = int
        where type loc_kind = CLocKind.loc_kind
  ) :> sig

    type located_arg = { 
	       k : CLocKind.loc_kind, 
	       width : int,                (* bit width of the argument *)
	       narrowing : int option,     (* bit width to convert the argument to *)
	       loc : int,                  (* register id or stack offset *)
	       offset : int,               (* offset if the argument is split between two locations *)
	       arg : Vararg.arg            (* the argument *)
             }

  (* converts a sequence of arguments and staged allocation locations to located arguments *)
    val mkLocatedArgs : (Vararg.arg list * SA.loc list) -> located_arg list

    val toString : located_arg -> string

  end = struct

    structure V = Vararg
    structure CK = CLocKind
    structure SA = SA

    type located_arg = { 
	       k : CK.loc_kind, 
	       width : int,                (* bit width of the argument *)
	       narrowing : int option,     (* bit width to convert the argument to *)
	       loc : int,                  (* register id or stack offset *)
	       offset : int,               (* offset if the argument is split between two locations *)
	       arg : Vararg.arg            (* the argument *)
             }

    fun narrowLocdArg narrowing {arg, k, width, loc, offset, narrowing=n} =
	    {arg=arg, k=k, width=width, loc=loc, offset=offset, narrowing=SOME narrowing}

    fun narrowLocdArgs (narrowing, locdArgs) =
	    List.map (narrowLocdArg narrowing) locdArgs

    fun combineLocdArg ({arg, k, width, loc, offset, narrowing} : located_arg, (i, locdArgs)) = let
	    val offset = offset + i
	    val locdArg = {arg=arg, k=k, width=width, loc=loc, offset=offset, narrowing=narrowing}
            in
	       (i + 1, locdArg :: locdArgs)
	    end

    val combineLocdArgs = List.rev o #2 o List.foldl combineLocdArg (0, []) 

  (* take an argument and a location and return a located argument *)
    fun mkLocatedArg (arg:V.arg, SA.REG (w, k, r)) = 
	    [{arg=arg, k=k, width=w, narrowing=NONE, loc=r, offset=0}]
      | mkLocatedArg (arg, SA.BLOCK_OFFSET (w, (CK.GPR | CK.STK), off)) = 
	    [{arg=arg, k=CK.STK, width=w, narrowing=NONE, loc=off, offset=0}]
      | mkLocatedArg (arg, SA.BLOCK_OFFSET (w, (CK.FPR | CK.FSTK), off)) = 
	    [{arg=arg, k=CK.FSTK, width=w, narrowing=NONE, loc=off, offset=0}]
      | mkLocatedArg (arg, SA.NARROW (loc, narrowing, k)) = 
	    narrowLocdArgs (narrowing, mkLocatedArg (arg, loc))
      | mkLocatedArg (arg, SA.COMBINE (l1, l2)) = let
	    val locdArgs1 = mkLocatedArg (arg, l1)
	    val locdArgs2 = mkLocatedArg (arg, l2)
	    in
	        combineLocdArgs (locdArgs1 @ locdArgs2)
	    end

  (* converts a sequence of arguments and staged allocation locations to located arguments *)
    val mkLocatedArgs = List.concat o ListPair.mapEq mkLocatedArg

    fun k2s CK.GPR = "GPR"
      | k2s CK.FPR = "FPR"
      | k2s CK.STK = "STK"
      | k2s CK.FSTK = "FSTK"
    val i2s = Int.toString

    fun toString {k, width, narrowing, offset, arg, loc} =
	"{k="^k2s k^",width="^i2s width^",narrowing="^i2s (Option.getOpt(narrowing, width))^",offset="^i2s offset^",loc="^i2s loc^"}"

  end

(* conrep.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature CONREP =
  sig

    (* `infer isRec cons` determines the representation of a datatype and its
     * constructors.  The constructors are represented by triples `(sym, isConst, ty)`,
     * where `sym` is the constructor name, `isConst` is a boolean that is true for
     * nullary constructors, and `ty` is the type of the constructor.
     *)
    val infer : bool -> (Symbol.symbol * bool * Types.ty) list
                     -> (Access.conrep list * Access.consig)

  end (* signature CONREP *)


structure ConRep : CONREP =
  struct

    open Access Types

    fun notconst(_,b,_) = not b

    fun count l = foldl (fn (c,acc) => if notconst c then acc+1 else acc) 0 l

    (* the first argument indicates whether this is a recursive datatype *)
    fun infer false ([(_, false, CONty(_,[_,_]))]) =
	([UNTAGGED], CSIG(1,0)) (* [TRANSPARENT] *)
	  (* The TRANSPARENT conrep is temporarily turned off;
	     it should be working very soon. Ask zsh. *)

      | infer _ cons = let
	  val multiple = (count cons) > 1
	  fun decide (ctag,vtag, (_,true,_)::rest, reps) =
		if multiple andalso !ElabDataControl.boxedconstconreps
		  then decide(ctag, vtag+1, rest, (TAGGED vtag) :: reps)
		  else decide(ctag+1, vtag, rest, (CONSTANT ctag) :: reps)
	    | decide (ctag,vtag, (_,false,CONty(_,[_,_]))::rest, reps) =
		if multiple
		  then decide(ctag, vtag+1, rest, (TAGGED vtag) :: reps)
		  else decide(ctag, vtag+1, rest, (UNTAGGED :: reps))
	    | decide (_, _, _::_, _) =
		ErrorMsg.impossible "Conrep: unexpected conrep-decide"
	    | decide (ctag, vtag, [], reps) = (rev reps, CSIG(vtag,ctag))
	  in
	    decide(0, 0, cons, [])
	  end

  end (* structure ConRep *)

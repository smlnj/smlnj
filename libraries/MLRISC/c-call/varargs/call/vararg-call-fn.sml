(* vararg-call-fn.sml
 * 
 * COPYRIGHT (c) 2008 Michael Rainey (http://cs.uchicago.edu/~mrainey)
 * All rights reserved.
 *
 * Glues together the ML side of the variadic call. The call setup is
 * as follows:
 *  - convert the arguments to requests
 *  - convernt the requests to locations
 *  - marshal the locations for the interpreter
 *  - transfer control to the interpreter by using the compiler's 
 *    standard C calling facility
 *)

functor VarargCallFn (

  (* machine-specific data for staged allocation *)
    structure SA : STAGED_ALLOCATION
	where type reg_id = int
        where type loc_kind = CLocKind.loc_kind
  (* parameter convention *)
    val params : SA.stage list
  (* return convention *)
    val returns : SA.stage list
  (* initial store *)
    val store0 : SA.store

    val bitWidthOfPointer : int
  (* byte alignment of a pointer *)
    val alignBOfPointer : int
  (* byte alignment of an integer *)
    val alignBOfInt : int
  (* byte alignment of a double-precision float *)
    val alignBOfDouble : int
  (* register kind of ints *)
    val kindOfInt : CLocKind.loc_kind
  (* register kind of pointers *)
    val kindOfPointer : CLocKind.loc_kind
  (* register kind of doubles *)
    val kindOfDouble : CLocKind.loc_kind

  ) = struct

    structure LocatedArgs = LocatedArgFn (structure SA = SA)
    structure V = Vararg

    fun widthOfArg (V.SINT_ARG i) = 32
      | widthOfArg (V.DOUBLE_ARG r) = 64
      | widthOfArg (V.PTR_ARG s) = bitWidthOfPointer
      | widthOfArg (V.STRING_ARG _) = bitWidthOfPointer

    fun kindOfArg (V.SINT_ARG i) = kindOfInt
      | kindOfArg (V.DOUBLE_ARG r) = kindOfDouble
      | kindOfArg (V.PTR_ARG s) = kindOfPointer
      | kindOfArg (V.STRING_ARG _) = kindOfPointer

    fun alignOfArg (V.SINT_ARG i) = alignBOfInt
      | alignOfArg (V.DOUBLE_ARG r) = alignBOfDouble
      | alignOfArg (V.PTR_ARG s) = alignBOfPointer
      | alignOfArg (V.STRING_ARG _) = alignBOfPointer

    fun argToReq a = (widthOfArg a, kindOfArg a, alignOfArg a)

  (* apply the variadic C function to args *)
    fun dispatchLowlevelCall (cFun, args) = let
	    val reqs = List.map argToReq args
	    val (locs, store) = SA.allocateSeq params (reqs, store0)
	    val locdArgs = LocatedArgs.mkLocatedArgs (args, locs)
(*val _ = print ((String.concatWith " " (List.map LocatedArgs.toString locdArgs))^"\n")*)
	    val nLocdArgs = List.length locdArgs
	    val {startLocdArgs, endLocdArgs} = Marshal.marshalLocdArgs locdArgs
            in
	     (* call the interpreter *)
	       SMLNJPrimCCall.applyInterp(
		     cFun, 
		     startLocdArgs,
		     endLocdArgs)
	    end

  end

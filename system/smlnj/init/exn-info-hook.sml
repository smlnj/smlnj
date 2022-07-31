(* exn-info-hook.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ExnInfoHook =
  struct

  (* an exception value is represented as a triple (ID, ARG, TRACE), where
   *
   *	ID	-- is a string ref; the ref cell serves as the unique id for
   *		   the exception and the string is the exception's name.
   *	ARG	-- is the argument value for the exception (or unit)
   *	TRACE	-- is a string list representing the traceback of the
   *		   exception.
   *)
    val exnName : PrimTypes.exn -> PrimTypes.string =
	  InlineT.cast (fn (PrimTypes.ref s, _, _) => s)

    local
      fun dummy (e: PrimTypes.exn) =
	    PreString.concat2
		(exnName e,
		 " (more info unavailable: ExnInfoHook not initialized)")
    in

    val exnMessageHook = PrimTypes.ref dummy

    fun exnMessage e = InlineT.! exnMessageHook e

    end (* local *)

  end

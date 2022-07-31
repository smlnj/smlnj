(* use-hook.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure UseHook : sig

  (* The type of the "use" hook.  The string is the file to use. *)
    type hook = PrimTypes.string -> PrimTypes.unit

    val useHook : hook PrimTypes.ref

    val use : PrimTypes.string -> PrimTypes.unit

  end = struct

    type hook = PrimTypes.string -> PrimTypes.unit

    val useHook = PrimTypes.ref (fn (_ : PrimTypes.string) => ())

    fun use s = InlineT.! useHook s

  end

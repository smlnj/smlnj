(* print-hook.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Hook for the top-level `print` function.
 *)

structure PrintHook =
  struct

    local
    (* to have something to initialize prHook with... *)
      fun discard (s: PrimTypes.string) = ()
    in
    val prHook = PrimTypes.ref discard (* very crude *)
    fun print s = InlineT.! prHook s
    end (* local *)

  end

(* print-hook.sml
 *
 * COPYRIGHT (c) 1997 AT&T Labs Research.
 *
 * This is a hook for the top-level print function, which allows
 * it to be rebound.
 *)

(* Imported from Init/print-hook.sml via the "print-hook" primitive. *)
(* In its original state it is uninitialized because it is defined at a
 * time when the IO stack is not yet available.  That's why we do an
 * assignment here.  (The bootstrap mechanism must make sure that this
 * code actually gets executed...) *)
local
    val _ = PrintHook.prHook := TextIO.print
in
    structure PrintHook = PrintHook
end


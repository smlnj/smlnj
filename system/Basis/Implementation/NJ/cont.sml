(* cont.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Continuation operations
 *)

structure Cont : CONT =
  struct

    type 'a cont = 'a PrimTypes.cont

    val callcc : ('a cont -> 'a) -> 'a = InlineT.callcc
    val throw : 'a cont -> 'a -> 'b = InlineT.throw

  (* a function for creating an isolated continuation from a function *)
    val isolate = InlineT.isolate

  (* versions of the continuation operations that do not capture/restore the
   * exception handler context.
   *)
    type 'a control_cont = 'a InlineT.control_cont
    val capture = InlineT.capture
    val escape = InlineT.escape

  end;



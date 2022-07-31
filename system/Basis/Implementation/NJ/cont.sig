(* cont.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Continuation operations
 *)

signature CONT =
  sig

    type 'a cont
    val callcc : ('a cont -> 'a) -> 'a
    val throw : 'a cont -> 'a -> 'b

  (* a function for creating an isolated continuation from a function *)
    val isolate : ('a -> unit) -> 'a cont

  (* versions of the continuation operations that do not capture/restore the
   * exception handler context.
   *)
    type 'a control_cont
    val capture : ('a control_cont -> 'a) -> 'a
    val escape : 'a control_cont -> 'a -> 'b

  end;



(* general.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure General : PRE_GENERAL =
  struct

    type unit = unit
    type exn = exn

    exception Bind = Bind
    exception Match = Match
    exception Subscript = Subscript
    exception Size = Size
    exception Overflow = Overflow
    exception Chr = Chr
    exception Div = Div
    exception Domain = Domain
    exception Span = Span

    exception Fail = Fail

    datatype order = datatype order

    val ! = !
    val op := = op :=

(*
    fun f o g = fn x => f(g x)
    fun a before b = a
*)
    val op o = op o
    val op before  = op before
    val ignore = ignore

  end (* structure General *)


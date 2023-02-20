(* Copyright 1996 by AT&T Bell Laboratories *)
(* fixity.sml *)

signature FIXITY =
sig
  datatype fixity = NONfix | INfix of (int*int)
  val infixleft : int -> fixity
  val infixright : int -> fixity
  val fixityToString : fixity -> string

end (* signature FIXITY *)


structure Fixity : FIXITY =
struct

  datatype fixity = NONfix | INfix of (int*int)

  (* building fixities *)
  fun infixleft n = INfix (n+n, n+n+1)
  fun infixright n = INfix (n+n+1, n+n)

  fun fixityToString NONfix = "nonfix "
    | fixityToString (INfix (i,_)) =
         (if i mod 2 = 0 then "infix " else "infixr ")^
         (if i div 2 > 0 then Int.toString (i div 2)^" " else "")

end (* structure Fixity *)


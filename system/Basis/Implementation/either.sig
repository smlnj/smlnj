(* either.sig
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The EITHER signature is a propsed SML Basis Library extension (proposal 2015-002).
 *)

signature EITHER =
  sig

    datatype ('left, 'right) either = INL of 'left | INR of 'right

    val isLeft : ('left, 'right) either -> bool
    val isRight : ('left, 'right) either -> bool

    val asLeft : ('left, 'right) either -> 'left option
    val asRight : ('left, 'right) either -> 'right option

    val map : ('ldom -> 'lrng) * ('rdom -> 'rrng)
	      -> ('ldom, 'rdom) either
		-> ('lrng, 'rrng) either

    val mapLeft  : ('ldom -> 'lrng) -> ('ldom, 'rdom) either -> ('lrng, 'rdom) either
    val mapRight : ('rdom -> 'rrng) -> ('ldom, 'rdom) either -> ('ldom, 'rrng) either

    val app : ('left -> unit) * ('right -> unit)
	      -> ('left, 'right) either
		-> unit

    val appLeft  : ('left -> unit) -> ('left, 'right) either -> unit
    val appRight : ('right -> unit) -> ('left, 'right) either -> unit

    val fold : ('left * 'b -> 'b) * ('right * 'b -> 'b)
               -> 'b -> ('left, 'right) either -> 'b

    val proj : ('a, 'a) either -> 'a

    val partition : (('left, 'right) either) list -> ('left list * 'right list)

  end

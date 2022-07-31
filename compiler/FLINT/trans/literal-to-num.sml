(* literal-to-num.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Support for intinf literals.
 *)

signature LITERAL_TO_NUM =
  sig

    val isNegative : IntInf.int -> bool

    val repDigits : IntInf.int -> word list  (* expose representation *)

    val lowVal : IntInf.int -> int option

  end

structure LiteralToNum : LITERAL_TO_NUM =
  struct

    fun isNegative (i : IntInf.int) = (i < 0)

    local
      fun unBI (CoreIntInf.BI x) = x
    in
    val repDigits = #digits o unBI o CoreIntInf.concrete
    fun lowVal i = let
	  val l = CoreIntInf.lowValue i
	  in
	    if l = CoreIntInf.neg_base_as_int then NONE else SOME l
	  end
    end (* local *)

  end

(* int32.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Int32Imp : INTEGER =
  struct
    structure I32 = InlineT.Int32

    type int = Int32.int

    val precision = SOME 32

    val minIntVal : int = ~2147483648
    val minInt : int option = SOME minIntVal
    val maxInt : int option = SOME 2147483647

    val ~       : int -> int = I32.~
    val op +    : int * int -> int  = I32.+
    val op -    : int * int -> int  = I32.-
    val op *    : int * int -> int  = I32.*
    val op div  : int * int -> int  = I32.div
    val op mod  : int * int -> int  = I32.mod
    val quot    : int * int -> int  = I32.quot
    val rem     : int * int -> int  = I32.rem
    val op <    : int * int -> bool = I32.<
    val op <=   : int * int -> bool = I32.<=
    val op >    : int * int -> bool = I32.>
    val op >=   : int * int -> bool = I32.>=
    val min     : int * int -> int = I32.min
    val max     : int * int -> int = I32.max
    val abs     : int -> int = I32.abs

    fun sign 0 = 0
      | sign i = if I32.<(i, 0) then ~1 else 1

    fun sameSign (i,j) = (sign i = sign j)

    fun compare (i, j) =
	  if (I32.<(i, j)) then LESS
	  else if (I32.>(i, j)) then GREATER
	  else EQUAL

    val scan = NumScan32.scanInt
    val fmt = NumFormat32.fmtInt
    val toString = fmt StringCvt.DEC
    val fromString = PreBasis.scanString (scan StringCvt.DEC)

    val toInt : int -> Int.int = I32.toInt
    val fromInt : Int.int -> int = I32.fromInt
    val toLarge : int -> LargeInt.int = I32.toLarge
    val fromLarge : LargeInt.int -> int = I32.fromLarge

  end

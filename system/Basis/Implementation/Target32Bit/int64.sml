(* int64.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Implementation of Word64 for 32-bit targets.
 *)

structure Int64Imp : sig

    include INTEGER

    val extern : int -> Word32.word * Word32.word
    val intern : Word32.word * Word32.word -> int

  end = struct

    structure I64 = InlineT.Int64

    type int = Int64.int	(* from Basis/TypesOnly *)

    val extern = I64.extern
    val intern = I64.intern

    val precision = SOME 64
    val minInt = SOME(~0x8000000000000000 : int)
    val maxInt = SOME( 0x7fffffffffffffff : int)

    val toInt = I64.toInt
    val fromInt = I64.fromInt

    val toLarge = I64.toLarge
    val fromLarge = I64.fromLarge

    val op * : int * int -> int = I64.*
    val op + : int * int -> int = I64.+
    val op - : int * int -> int = I64.-
    val op div : int * int -> int = I64.div
    val op mod : int * int -> int = I64.mod
    val quot : int * int -> int = I64.quot
    val rem : int * int -> int = I64.rem

    val op > : int * int -> bool = I64.>
    val op >= : int * int -> bool = I64.>=
    val op < : int * int -> bool = I64.<
    val op <= : int * int -> bool = I64.<=

    val ~   : int -> int = ~
    val min : int * int -> int = I64.min
    val max : int * int -> int = I64.max
    val abs : int -> int = I64.abs

    fun sign 0 = 0
      | sign i = if I64.<(i, 0) then ~1 else 1

    fun sameSign (i, j) = (sign i = sign j)

    fun compare (i, j) =
	  if (I64.<(i, j)) then LESS
	  else if (I64.>(i, j)) then GREATER
	  else EQUAL

    val scan = NumScan64.scanInt
    val fmt = NumFormat64.fmtInt
    val toString = fmt StringCvt.DEC
    val fromString = PreBasis.scanString (scan StringCvt.DEC)

  end

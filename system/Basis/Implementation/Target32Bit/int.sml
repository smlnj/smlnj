(* int.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Default int structure (31 bits) for 32-bit targets.
 *)

structure IntImp : INTEGER =
  struct
    structure Int = InlineT.Int

    exception Div = Assembly.Div
    exception Overflow = Assembly.Overflow

    type int = int

    val precision = SOME 31
    val minIntVal = ~1073741824
    val minInt = SOME minIntVal
    val maxInt = SOME 1073741823

    val toLarge : int -> LargeInt.int = Int.toLarge
    val fromLarge : LargeInt.int -> int = Int.fromLarge
    val toInt = Int.toInt
    val fromInt = Int.fromInt

    val ~ 	: int -> int = Int.~
    val op * 	: int * int -> int  = Int.*
    val op + 	: int * int -> int  = Int.+
    val op - 	: int * int -> int  = Int.-
    val op div 	: int * int -> int  = Int.div
    val op mod 	: int * int -> int  = Int.mod
    val op quot : int * int -> int  = Int.quot
    val op rem 	: int * int -> int  = Int.rem
    val min 	: int * int -> int  = Int.min
    val max 	: int * int -> int  = Int.max
    val abs 	: int -> int = Int.abs

    fun sign 0 = 0
      | sign i = if Int.<(i, 0) then ~1 else 1

    fun sameSign (i,j) = (sign i = sign j)

    fun compare (i, j) =
	  if (Int.<(i, j)) then General.LESS
	  else if (Int.>(i, j)) then General.GREATER
	  else General.EQUAL
    val op > 	: int * int -> bool = Int.>
    val op >= 	: int * int -> bool = Int.>=
    val op < 	: int * int -> bool = Int.<
    val op <= 	: int * int -> bool = Int.<=

    fun fmt radix = (NumFormat32.fmtInt radix) o InlineT.Int32.fromInt

    fun scan radix = let
	  val scanInt32 = NumScan32.scanInt radix
	  fun f getc cs = (case scanInt32 getc cs
		   of NONE => NONE
		    | SOME(i, cs') => SOME(Int32Imp.toInt i, cs')
		  (* end case *))
	  in
	    f
	  end

    val toString = fmt StringCvt.DEC

(*
    val fromString = PreBasis.scanString (scan StringCvt.DEC)
*)
    local
      structure Word = InlineT.Word
      structure CV = InlineT.CharVector
    in
  (* optimized version of fromString; it is about 2x as fast as
   * using scanString:
   *)
    fun fromString s = let
	  val n = size s
	  val z = ord #"0"
	  val sub = CV.sub
	  infix ++
	  fun x ++ y = InlineT.Int.fast_add(x, y)
	  fun num (i, a) = if i >= n
		then a
		else let
		  val c = ord (sub (s, i)) - z
		  in
		    if c < 0 orelse c > 9
		      then a
		      else num (i ++ 1, 10 * a - c)
		  end
	(* Do the arithmetic using the negated absolute to avoid
	 * premature overflow on minInt.
	 *)
	  fun negabs i = if i >= n
		then NONE
		else let
		  val c = z - ord (sub (s, i))
		  in
		    if c > 0 orelse c < ~9
		      then NONE
		      else SOME (num (i ++ 1, c))
		  end
	  fun skipwhite i = if i >= n
		then NONE
	        else let
		  val c = sub (s, i)
		  in
		    if Char.isSpace c
		      then skipwhite (i ++ 1)
		    else if c = #"-" orelse c = #"~"
		      then negabs (i ++ 1)
		    else if c = #"+"
		      then Option.map ~ (negabs (i ++ 1))
		      else Option.map ~ (negabs i)
		  end
	  in
	    skipwhite 0
	  end
    end (* local *)

  end  (* structure IntImp *)

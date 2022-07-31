(* Copyright 1989 by AT&T Bell Laboratories 
 *
 *)
(* Support for IEEE floating-point constants
 * Double precision format (for normalized numbers):
 *   Bias = 1023.
 *   Exponent = 11 bits.
 *   Range of exponent = [1..2046]
 *   Mantissa = 52 (+1) bits.
 *   Value = (-1)^s * 2^(e-1023) * 1.f
 *)
structure IEEERealConst = RealConst(
struct
    val significant = 53 (* 52 + redundant 1 bit *)
    val minexp = ~1021 and  maxexp = 1024
    val itow = Word.fromInt
    val wtoi = Word.toIntX

    fun transreal (sign, frac, exp) =
	 if frac(0,1)=0 
           then if sign=0 then "\000\000\000\000\000\000\000\000"
                          else "\128\000\000\000\000\000\000\000"
          else implode
	        [Char.chr(wtoi 
		   (Word.orb(Word.<<(itow sign, 0w7), 
			     Word.>>(itow(exp+1022), 0w4)))),
		 Char.chr(wtoi 
		   (Word.andb(0w255, 
			      Word.orb(Word.<<(itow(exp+1022), 0w4),
				       itow (frac(1,4)))))),
		 Char.chr(frac(5,8)),
		 Char.chr(frac(13,8)),
		 Char.chr(frac(21,8)),
		 Char.chr(frac(29,8)),
		 Char.chr(frac(37,8)),
		 Char.chr(frac(45,8))]

end)


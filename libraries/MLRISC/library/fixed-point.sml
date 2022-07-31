(* 
 * A simple fixed point datatype
 * 
 * -- Allen
 *)

functor FixedPoint (val decimal_bits : int) : FIXED_POINT =
struct

   structure W = Word31

   infix << >> 
   infix & ||

   val decimal_places = (decimal_bits + 2) div 3
   val realToString   = Real.fmt (StringCvt.FIX(SOME decimal_places))

   val op<< = W.<<
   val op>> = W.>>
   val op&  = W.andb
   val op|| = W.orb
   val op div = W.div
   val word = W.fromInt
   val int  = W.toInt

   type fixed_point = W.word

   val bits  = word decimal_bits
   val bits2 = bits >> 0w1
   val bits4 = bits >> 0w2

   val zero     = 0w0
   val one      = 0w1 << bits
   val realOne  = Real.fromInt(int one)

   val compare  = W.compare

   val op*      = fn(i:fixed_point,j:fixed_point) => W.*(i >> bits2,j >> bits2)
   val op/      = fn(i:fixed_point,j:fixed_point) => (i << bits) div j

   val mask     = one - 0w1

   fun fromInt i    =  (word i) << bits
   fun fixed_point(a,b) = let val b' = word b
                          in (((word a) << bits) + (b' >> 0w1)) div b' end
   fun fromReal r    = word(Real.round(Real.*(r, realOne)))
   fun toReal p      = Real./(Real.fromInt(int p), realOne)
   fun toWord f      = f
   fun toString p    = realToString(toReal p)
        handle Overflow => "inf"

   fun scale(i,j)   = W.*(i, word j)
   fun i div j      = W.div(i,word j)

   val min = W.min 
   val max = W.max 

end


(*
 * 64-bit word datatype.
 * Word64.word is implemented as Word32.word * Word32.word
 * A constant of this type can be specified as a pair of 32-bit words.
 * Also pattern matching can also be applied in the same manner. 
 *
 * -- Allen
 *)

structure Word64 : WORD =
struct
   structure W = Word32

   type word = W.word * W.word (* high, low *)

   val wordSize = 64

   fun isNeg w = W.>>(w,0w31) = 0w1 (* test the sign bit *)

   fun toLargeWord(x,y) = y (* strip high order bits *)
   fun toLargeWordX(x,y) = y (* strip high order bits *)
   fun fromLargeWord w = (0w0 : W.word,w)

   fun toLargeInt(x:W.word,y) = 
        if x <> 0w0 orelse isNeg y then raise Overflow
        else W.toLargeInt y

   fun toLargeIntX(x,y) = 
         if x = 0w0 then 
           if isNeg y then raise Overflow else W.toLargeInt y
         else if (W.notb x) = 0w0 then
           if isNeg y then W.toLargeIntX y else raise Overflow
         else raise Overflow    

   fun fromLargeInt i = (if i >= 0 then 0w0 else W.notb 0w0,W.fromLargeInt i)

   fun toInt(x:W.word,y) = 
        if x <> 0w0 orelse isNeg y then raise Overflow else W.toInt y

   fun toIntX(x,y) = 
         if x = 0w0 then
            if isNeg y then raise Overflow else W.toInt y
         else if (W.notb x) = 0w0 then
            if isNeg y then W.toIntX y else raise Overflow
         else raise Overflow

   fun fromInt i = if i >= 0 then (0w0:W.word,W.fromInt i)
                   else (W.notb 0w0,W.fromInt i)

   fun orb((a,b),(c,d)) = (W.orb(a,c),W.orb(b,d))

   fun xorb((a,b),(c,d)) = (W.xorb(a,c),W.xorb(b,d))

   fun andb((a,b),(c,d)) = (W.andb(a,c),W.andb(b,d))

   fun notb(a,b) = (W.notb a,W.notb b)

   fun plus((a,b),(c,d)) = 
   let val y = W.+(b,d)
       val x = W.+(a,c)
       val x = if y < b then W.+(x,0w1) else x (* carry *)
   in  (x,y) end

   fun minus((a,b),(c,d)) =
   let val x = W.-(a,c)
       val y = W.-(b,d)
       val x = if b < d then W.-(x,0w1) else x (* borrow *)
   in  (x,y) end

   fun mult((a,b),(c,d)) =
   let (* multiply 32x32 -> 64.
        * Split them into two pairs of 16 bit words in order to deal
        * with carries in a portable manner.  This is really annoying.
        *)
       fun multiply(u,v) = 
       let val a = W.>>(u,0w16)
           val b = W.andb(u,0wxffff)
           val c = W.>>(v,0w16)
           val d = W.andb(v,0wxffff)
           val ac = a*c
           val bc = b*c
           val ad = a*d
           val bd = b*d
           val bc_hi = W.>>(bc,0w16)
           val bc_lo = W.<<(bc,0w16)
           val ad_hi = W.>>(ad,0w16)
           val ad_lo = W.<<(ad,0w16)
           val AC    = (ac,0w0:W.word)
           val BC    = (bc_hi,bc_lo)
           val AD    = (ad_hi,ad_lo)
           val BD    = (0w0:W.word,bd)
       in  plus(AC,plus(BC,plus(AD,BD))) end
       fun shift32(a,b)  = (b,0w0)
       val ad = multiply(a,d)
       val bc = multiply(b,c)
       val bd = multiply(b,d)
   in  plus(plus(shift32(ad),shift32(bc)),bd) end

   fun gt((a,b):word,(c,d):word) = a > c orelse a=c andalso b > d
   fun ge((a,b):word,(c,d):word) = a > c orelse a=c andalso b >= d
   fun lt((a,b):word,(c,d):word) = a < b orelse a=c andalso b < d
   fun le((a,b):word,(c,d):word) = a < b orelse a=c andalso b <= d

   fun compare ((a,b):word, (c,d):word) =
       if a < c then LESS
       else if a > c then GREATER
       else if b < d then LESS
       else if b > d then GREATER
       else EQUAL

   fun sll((a,b),c) =
       if c >= 0w32 then  
            let val x = W.<<(b,c-0w32)
            in  (x,0w0) end
       else let val x = W.<<(a,c)
                val y = W.<<(b,c)
                val z = W.>>(b,0w32-c)
            in  (W.orb(x,z),y) end

   fun srl((a,b),c) = 
       if c >= 0w32 then
            let val y = W.>>(a,c-0w32)
            in  (0w0,y) end
       else let val x = W.>>(a,c)
                val y = W.>>(b,c)
                val z = W.<<(W.andb(a,W.<<(0w1,c)-0w1),0w32-c)
            in  (x,W.orb(y,z)) end

   fun sra((a,b),c) = 
       if c >= 0w32 then
            let val y = W.~>>(a,c-0w32)
                val x = if isNeg a then W.notb 0w0 else 0w0
            in  (x,y) end
       else let val x = W.~>>(a,c)
                val y = W.>>(b,c)
                val z = W.<<(W.andb(a,W.<<(0w1,c)-0w1),0w32-c)
            in  (x,W.orb(y,z)) end

   fun min (w1, w2) = if lt(w1,w2) then w1 else w2

   fun max (w1, w2) = if gt(w1,w2) then w1 else w2

   fun divide((a,b):word,(0w0,0w0):word) = raise Div 
     | divide((0w0,b),(0w0,d)) = (0w0:W.word,b div d)
     | divide((a,b),(c,d)) = raise Match
      (* okay, not yet supported, I'm lazy *)

   fun padZero(b,0) = b
     | padZero(b,n) = padZero("0"^b,n-1)

   fun hex(0w0,y) = W.toString y
     | hex(x,y) =
       let val a = W.toString x
           val b = W.toString y
       in  a^padZero(b,8-size b) end

   fun bin(0w0,y) = W.fmt StringCvt.BIN y
     | bin(x,y) = 
       let val a = W.fmt StringCvt.BIN x
           val b = W.fmt StringCvt.BIN y
       in  a^padZero(b,32-size b) end

   fun fmt StringCvt.BIN = bin
     | fmt StringCvt.DEC = raise Match
     | fmt StringCvt.HEX = hex
     | fmt StringCvt.OCT = raise Match

   val toString = hex

   val scan = fn _ => raise Match
   fun fromString s = 
       case W.fromString s of
         SOME w => SOME(0w0:W.word,w)
       | NONE => NONE

   val op <   = lt
   val op <=  = le
   val op >   = gt
   val op >=  = ge
   val op *   = mult
   val op +   = plus
   val op -   = minus
   val op <<  = sll
   val op >>  = srl
   val op ~>> = sra
   val op div = divide
   fun op mod(a:word,b:word):word = a-(a div b)*b

end

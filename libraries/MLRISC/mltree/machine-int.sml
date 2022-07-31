(*
 * How to evaluate constants for various widths.
 *
 * Internally, we represent machine_int as a signed integer.
 * So when we do bit or unsigned operations we have to convert to
 * the unsigned representation first.
 *
 * Note: this implementation requires andb, orb, xorb etc in IntInf.
 * You have to upgrade to the latest version of smlnj-lib if this
 * fails to compile.
 *)
local

   val maxSz = 65

in

structure MachineInt : MACHINE_INT =
struct

   structure I = IntInf
   structure S = String
   type machine_int = I.int
   type sz = int

   datatype div_rounding_mode = DIV_TO_ZERO | DIV_TO_NEGINF

   val itow = Word.fromInt

   (* Parse hex or binary, but not octal, that's for wussies *)
   val hexToInt = StringCvt.scanString (I.scan StringCvt.HEX)
   val binToInt = StringCvt.scanString (I.scan StringCvt.BIN)

   (* Precompute some tables for faster arithmetic *)
   local
   val pow2table = Array.tabulate(maxSz,fn n => I.<<(1,itow n))  (* 2^n *)
   val masktable = Array.tabulate(maxSz,
                       fn n => I.-(I.<<(1,itow n),1))      (* 2^n-1 *)
   val maxtable  = Array.tabulate(maxSz+1,
                    fn 0 => 0
                     | n => I.-(I.<<(1,itow(n-1)),1))  (* 2^{n-1}-1 *)
   val mintable  = Array.tabulate(maxSz+1,
                    fn 0 => 0
                     | n => I.~(I.<<(1,itow(n-1))))   (* -2^{n-1} *)
   in

   fun pow2 i       = if i < maxSz then Array.sub(pow2table, i)
                      else I.<<(1,itow i)
   fun maskOf sz    = if sz < maxSz then Array.sub(masktable, sz)
                      else I.-(I.<<(1,itow sz),1)
   fun maxOfSize sz = if sz < maxSz then Array.sub(maxtable, sz)
                      else I.-(I.<<(1,itow(sz-1)),1)
   fun minOfSize sz = if sz < maxSz then Array.sub(mintable, sz)
                      else I.~(I.<<(1,itow(sz-1)))
   end

   (* queries *)
   fun isNeg(i)    = I.sign i < 0
   fun isPos(i)    = I.sign i > 0
   fun isZero(i)   = I.sign i = 0
   fun isNonNeg(i) = I.sign i >= 0
   fun isNonPos(i) = I.sign i <= 0
   fun isEven(i)   = isZero(I.rem(i,2))
   fun isOdd(i)    = not(isEven(i))

   (* to unsigned representation *)
   fun unsigned(sz, i) = if isNeg i then I.+(i, pow2 sz) else i

   (* to signed representation *)
   fun signed(sz, i) = if I.>(i, maxOfSize sz) then I.-(i, pow2 sz) else i

   (* Narrow to the representation of a given type *)
   fun narrow(sz, i) = signed(sz, I.andb(i, maskOf sz))

   (* Recognize 0x and 0b prefix and do the right thing *)
   fun fromString(sz, s) =
   let val n = S.size s
       fun conv(i,negate) =
       if n >= 2+i andalso S.sub(s, i) = #"0" then
         (case S.sub(s, i+1) of
           #"x" => (hexToInt (S.substring(s,2+i,n-2-i)), negate)
         | #"b" => (binToInt (S.substring(s,2+i,n-2-i)), negate)
         | _    => (I.fromString s, false)
         )
       else (I.fromString s, false)
       val (result, negate) =
           if s = "" then (NONE, false)
           else if S.sub(s, 0) = #"~" then conv(1, true)
           else conv(0, false)
   in  case (result, negate) of
         (SOME n, true) => SOME(narrow(sz, I.~ n))
       | (SOME n, false) => SOME(narrow(sz, n))
       | (NONE, _) => NONE
   end

   (* Convert types into IntInf without losing precision. *)
   structure Cvt =
   struct
      structure W   = Word
      structure W32 = Word32
      val wtoi   = W.toIntX
      val w32toi = W32.toIntX
      val fromInt    = I.fromInt
      val fromInt32  = Int32.toLarge
      val fromInt64  = Int64.toLarge
      fun fromWord w = I.fromLarge(Word.toLargeInt w)
(*
      fun fromWord32 w = I.+(I.<<(I.fromInt(w32toi(W32.>>(w,0w16))),0w16),
                                  I.fromInt(w32toi(W32.andb(w,0wxffff))))
*)
      fun fromWord32 w = I.fromLarge(Word32.toLargeInt w)
      fun fromWord64 w = I.fromLarge(Word64.toLargeInt w)
   end
   (* machine_int <-> other types *)
   fun fromInt(sz,i)      = narrow(sz,Cvt.fromInt i)
   fun fromInt32(sz,i)    = narrow(sz,Cvt.fromInt32 i)
   fun fromInt64(sz,i)    = narrow(sz,Cvt.fromInt64 i)
   fun fromWord(sz,w)     = narrow(sz,Cvt.fromWord w)
   fun fromWord32(sz,w)   = narrow(sz,Cvt.fromWord32 w)
   fun fromWord64(sz,w)   = narrow(sz,Cvt.fromWord64 w)
   fun toString(sz,i)     = I.toString i
   val toHex = I.fmt StringCvt.HEX
   val toBin = I.fmt StringCvt.BIN
   fun toHexString(sz, i) = "0x"^toHex(unsigned(sz, i))
   fun toBinString(sz, i) = "0b"^toBin(unsigned(sz, i))
   fun toInt(sz, i)       = I.toInt(narrow(sz, i))
   fun toInt32(sz, i)     = Int32.fromLarge(narrow(sz, i))
   fun toInt64(sz, i)     = Int64.fromLarge(narrow(sz, i))
   fun toWord(sz, i)      = Word.fromLargeInt(I.toLarge(unsigned(sz, i)))
   fun toWord32(sz, i)    = Word32.fromLargeInt(I.toLarge(unsigned(sz, i)))
(*
       let val i  = unsigned(sz, i)
           val lo = I.andb(i,0xffff)
           val hi = I.~>>(i,0w16)
           fun tow32 i = Word32.fromLargeInt(I.toLarge i)
       in  tow32 lo + Word32.<<(tow32 hi, 0w16) end
*)
   fun toWord64(sz, i)    = Word64.fromLargeInt(I.toLarge(unsigned(sz, i)))

   fun hash i = Word.fromInt(I.toInt(I.andb(i,0x1fffffff)))

   fun isInRange(sz, i) = I.<=(minOfSize sz,i) andalso I.<=(i,maxOfSize sz)

   fun signedBinOp f (sz,i,j) = narrow(sz, f(i, j))

   fun signedUnaryOp f (sz,i) = narrow(sz, f i)

   fun unsignedBinOp f (sz,i,j) = narrow(sz, f(unsigned(sz,i), unsigned(sz,j)))

   fun trappingUnaryOp f (sz,i) =
       let val x = f i
       in  if isInRange(sz, x) then x else raise Overflow
       end

   fun trappingBinOp f (sz,i,j) =
       let val x = f(i,j)
       in  if isInRange(sz, x) then x else raise Overflow
       end

   (* two's complement operators *)
   val NEG   = signedUnaryOp I.~
   val ABS   = signedUnaryOp I.abs
   val ADD   = signedBinOp I.+
   val SUB   = signedBinOp I.-
   val MULS  = signedBinOp I.*
   fun DIVS (DIV_TO_ZERO, ty, x, y) = signedBinOp I.quot (ty, x, y)
     | DIVS (DIV_TO_NEGINF, ty, x, y) = signedBinOp I.div (ty, x, y)
   fun REMS (DIV_TO_ZERO, ty, x, y) = signedBinOp I.rem (ty, x, y)
     | REMS (DIV_TO_NEGINF, ty, x, y) = signedBinOp I.mod (ty, x, y)
   val MULU  = unsignedBinOp I.*
   val DIVU  = unsignedBinOp I.div
(*
   val QUOTU = unsignedBinOp I.quot
*)
   val REMU  = unsignedBinOp I.rem

   val NEGT  = trappingUnaryOp I.~
   val ABST  = trappingUnaryOp I.abs
   val ADDT  = trappingBinOp I.+
   val SUBT  = trappingBinOp I.-
   val MULT  = trappingBinOp I.*
   fun DIVT (DIV_TO_ZERO, ty, x, y) = trappingBinOp I.quot (ty, x, y)
     | DIVT (DIV_TO_NEGINF, ty, x, y) = trappingBinOp I.div (ty, x, y)

   fun NOTB(sz,x)   = narrow(sz,I.notb x)
   fun ANDB(sz,x,y) = narrow(sz,I.andb(x,y))
   fun ORB(sz,x,y)  = narrow(sz,I.orb(x,y))
   fun XORB(sz,x,y) = narrow(sz,I.xorb(x,y))
   fun EQVB(sz,x,y) = narrow(sz,I.xorb(I.notb x,y))
   fun Sll(sz,x,y)  = narrow(sz,I.<<(x, y))
   fun Srl(sz,x,y)  = narrow(sz,I.~>>(unsigned(sz, x), y))
   fun Sra(sz,x,y)  = narrow(sz,I.~>>(x, y))
   fun SLL(sz,x,y)  = Sll(sz,x,toWord(sz, y))
   fun SRL(sz,x,y)  = Srl(sz,x,toWord(sz, y))
   fun SRA(sz,x,y)  = Sra(sz,x,toWord(sz, y))

   fun BITSLICE(sz,sl,x) =
   let fun slice([],n) = n
         | slice((from,to)::sl,n) =
            slice(sl, ORB(sz, narrow(to-from+1,
                                Srl(sz, x, Word.fromInt from)), n))
   in  slice(sl, 0)
   end

   fun bitOf(sz, i, b) =
       toWord(1, narrow(1, Srl(sz, i, Word.fromInt b)))
   fun byteOf(sz, i, b) =
       toWord(8, narrow(8, Srl(sz, i, Word.fromInt(b*8))))
   fun halfOf(sz, i, h) =
       toWord(16, narrow(16, Srl(sz, i, Word.fromInt(h*16))))
   fun wordOf(sz, i, w) =
       toWord32(32, narrow(32, Srl(sz, i, Word.fromInt(w*32))))

   (* type promotion *)
   fun SX(toSz,fromSz,i) = narrow(toSz, narrow(fromSz, i))
   fun ZX(toSz,fromSz,i) = narrow(toSz, unsigned(fromSz, narrow(fromSz, i)))

   (* comparisions *)
   fun EQ(sz,i:I.int,j)  = i = j
   fun NE(sz,i:I.int,j)  = i <> j
   fun GT(sz,i:I.int,j)  = i > j
   fun GE(sz,i:I.int,j)  = i >= j
   fun LT(sz,i:I.int,j)  = i < j
   fun LE(sz,i:I.int,j)  = i <= j
   fun LTU(sz,i,j) = unsigned(sz, i) < unsigned(sz, j)
   fun GTU(sz,i,j) = unsigned(sz, i) > unsigned(sz, j)
   fun LEU(sz,i,j) = unsigned(sz, i) <= unsigned(sz, j)
   fun GEU(sz,i,j) = unsigned(sz, i) >= unsigned(sz, j)
   (*
    * Split an integer "i" of size "sz" into words of size "wordSize"
    *)
   fun split{sz, wordSize, i} =
   let fun loop(sz, i, ws) =
           if sz <= 0 then rev ws
           else
           let val w = narrow(wordSize, i)
               val i = IntInf.~>>(i, Word.fromInt wordSize)
           in  loop(sz - wordSize, i, w::ws)
           end
   in  loop(sz, unsigned(sz, i), [])
   end

end

end

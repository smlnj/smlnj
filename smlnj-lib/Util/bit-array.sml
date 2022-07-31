(* bit-array.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure BitArray :> BIT_ARRAY =
  struct

    structure Vector =
      struct
        local
          structure W8A = Word8Array
          structure W8V = Word8Vector
          val sub = W8A.sub
          val << = Word8.<<
          val >> = Word8.>>
          val ++ = Word8.orb
          val & = Word8.andb
          infix sub << >> ++ &

          fun badArg(f,msg) = LibBase.failure{module="BitArray",func=f,msg=msg}

          val hexs = Byte.stringToBytes "0123456789abcdef"
          val lomask = W8V.fromList [0wx00, 0wx01, 0wx03, 0wx07,
                                     0wx0f, 0wx1f, 0wx3f, 0wx7f, 0wxff]
          val himask = W8V.fromList [0wxff, 0wxfe, 0wxfc, 0wxf8,
                                     0wxf0, 0wxe0, 0wxc0, 0wx80, 0wx00]
          fun hibits i = W8V.sub(himask,i)
          fun lobits i = W8V.sub(lomask,i)
          fun wmask7 i = Word.andb(Word.fromInt i, 0w7)
          val mask7 = Word.toIntX o wmask7

        (* return the number of bytes needed to represent the given number of bits *)
          fun sizeOf n = ((n + 7) div 8)

        (* return the index of byte that holds bit i *)
          fun byteOf i = (i div 8)

        (* return the mask for bit i in a byte *)
          fun bit i : Word8.word = (0w1 << Word.andb(Word.fromInt i, 0w7))

        in

        (* A bitvector is stored in a Word8Array.array.
         * Bit n is stored in bit (n mod 8) of word (n div 8).
         * We maintain the invariant that all bits >= nbits are 0.
         *)
          type elem = bool
          datatype vector = BA of {nbits : int, bits : W8A.array}

          val maxLen = 8*Word8Array.maxLen
		handle Overflow => Word8Array.maxLen (* would valOf Int.maxInt be better? *)

          fun array (0, init) = BA{nbits=0,bits=W8A.array(0, 0w0)}
            | array (len, false) = BA{nbits=len,bits=W8A.array(sizeOf len, 0w0)}
            | array (len, true) = let
                val sz = sizeOf len
                val bits = W8A.array (sz, 0wxff)
                in
                  case len mod 8 of 0 => () | idx => W8A.update(bits,sz-1,lobits idx);
                  BA{nbits = len, bits = bits}
                end

          val char0 = Byte.charToByte #"0"
          val char9 = Byte.charToByte #"9"
          val charA = Byte.charToByte #"A"
          val charF = Byte.charToByte #"F"
          val chara = Byte.charToByte #"a"
          val charf = Byte.charToByte #"f"
          fun fromString s = let
                val len = 4*(size s)          (* 4 bits per hex digit *)
                val (bv as BA{bits, ...}) = array (len, false)
                fun nibble x = let
                      val d = Byte.charToByte x
                      in
                        if (char0 <= d) andalso (d <= char9)
                          then d - char0
                        else if (charA <= d) andalso (d <= charF)
                          then d - charA + 0w10
                        else if (chara <= d) andalso (d <= charf)
                          then d - chara + 0w10
                        else raise Domain
                      end
                fun init ([], _) = SOME bv
                  | init ([x], i) = (W8A.update(bits, i, nibble x); SOME bv)
                  | init (x1::x2::r, i) = (
                      W8A.update(bits, i, ((nibble x2) << 0w4) ++ (nibble x1));
                      init (r, i+1))
                in
                  init (rev(explode s), 0)
                end
		  handle _ => NONE

          fun toString (BA{nbits=0,...}) = ""
            | toString (BA{nbits,bits}) = let
                val len = (nbits + 3) div 4
                val buf = W8A.array (len, 0w0)
                fun put (i,j) = let
                      val v = bits sub i
                      in
                        W8A.update (buf, j, W8V.sub(hexs, Word8.toInt(v & 0wx0f)));
                        W8A.update (buf, j-1, W8V.sub(hexs, Word8.toInt(v >> 0w4)));
                        put (i+1, j-2)
                      end
                in
                  (put(0,len-1)) handle _ => ();
                  Byte.bytesToString (W8A.vector buf)
                end

          fun bits (len,l) = let
                val (bv as BA{bits, ...}) = array (len, false)
                fun init i = let
                      val idx = byteOf i
                      val b = 0w1 << Word.andb(Word.fromInt i, 0w7)
                      in
                        W8A.update (bits, idx, ((bits sub idx) ++ b))
                      end
                in
                  List.app init l;
                  bv
                end

          fun fromList [] = array (0, false)
            | fromList l = let
                val len = length l
                val ba as BA{bits,...} = array (len, false)
                fun getbyte ([],_,b) = ([],b)
                  | getbyte (l,0w0,b) = (l,b)
                  | getbyte (false::r,bit,b) = getbyte(r,bit << 0w1,b)
                  | getbyte (true::r,bit,b) = getbyte(r,bit << 0w1,b ++ bit)
                fun fill ([],_) = ()
                  | fill (l,idx) = let
                      val (l',byte) = getbyte (l,0w1,0w0)
                      in
                        if byte <> 0w0 then W8A.update(bits,idx,byte) else ();
                        fill (l',idx+1)
                      end
                in
                  fill (l,0);
                  ba
                end

          fun tabulate (len, genf) = let
                val ba as BA{bits,...} = array (len, false)
                fun getbyte (cnt,0w0,b) = (cnt,b)
                  | getbyte (cnt,bit,b) =
                      if cnt = len then (cnt,b)
                      else case genf cnt of
                        false => getbyte(cnt+1,bit << 0w1,b)
                      | true => getbyte(cnt+1,bit << 0w1,b ++ bit)
                fun fill (cnt,idx) =
                      if cnt = len then ()
                      else let
                        val (cnt',byte) = getbyte (cnt,0w1,0w0)
                        in
                          if byte <> 0w0 then W8A.update(bits,idx,byte) else ();
                          fill (cnt',idx+1)
                        end
                in
                  fill (0,0);
                  ba
                end

          fun getBits (BA{nbits = 0, ...}) = []
            | getBits (BA{nbits, bits}) = let
                fun extractBits (_, 0w0, l) = l
                  | extractBits (bit, data, l) = let
                      val l' = if (data & 0wx80) = 0w0 then l else bit :: l
                      in
                       extractBits (bit-1, data<<0w1, l')
                      end
                fun extract (~1, _, l) = l
                  | extract (i, bitnum, l) =
                      extract (i-1,bitnum-8,extractBits (bitnum, bits sub i, l))
                val maxbit = nbits-1
                val hiByte = byteOf maxbit
                val delta = Word.andb(Word.fromInt maxbit, 0w7)
                in
                  extract (hiByte-1, maxbit - (Word.toIntX delta) - 1,
                    extractBits(maxbit,(bits sub hiByte) << (0w7-delta),[]))
                end

          fun bitOf (BA{nbits, bits}, i) =
                if i >= nbits
                  then raise Subscript
                  else ((W8A.sub (bits,byteOf i)) & (bit i)) <> 0w0

          fun isZero (BA{bits,...}) = let
                fun isz i = (bits sub i) = 0w0 andalso (isz (i+1))
                in
                  isz 0
                end handle _ => true

          fun copybits (bits, newbits) = let
                fun cpy i = (W8A.update(newbits, i, bits sub i); cpy(i+1))
                in
                  (cpy 0) handle _ => ()
                end

          fun mkCopy (BA{nbits, bits}) = let
                val ba as BA{bits=newbits,...} = array (nbits, false)
                in
                  copybits(bits, newbits);
                  ba
                end

          fun eqBits arg = let
                fun order (arg as (ba as BA{nbits,...},ba' as BA{nbits=nbits',...})) =
                      if nbits >= nbits'
                        then arg
                        else (ba',ba)
                val (BA{nbits,bits}, BA{bits=bits',nbits=nbits'}) = order arg
                val minlen = W8A.length bits'
                fun iszero i = (bits sub i) = 0w0 andalso (iszero (i+1))
                fun cmp i =
                      if i = minlen
                        then iszero i
                        else (bits sub i) = (bits' sub i) andalso cmp(i+1)
                in
                  (cmp 0) handle _ => true
                end
          fun equal (arg as (BA{nbits,...},BA{nbits=nbits',...})) =
               nbits = nbits' andalso eqBits arg

          fun extend0 (ba as BA{nbits, bits}, n) =
                if (nbits >= n)
                  then mkCopy ba
                  else let
                    val newbits = W8A.array(sizeOf n, 0w0)
                    fun cpy i = (W8A.update(newbits, i, bits sub i); cpy(i+1))
                    in
                      (cpy 0) handle _ => ();
                      BA{nbits=n, bits=newbits}
                    end

          fun extend1 (ba as BA{nbits, bits}, n) =
                if (nbits >= n)
                  then mkCopy ba
                  else let
                    val len = sizeOf n
                    val newbits = W8A.array(len, 0wxff)
                    val nbytes = byteOf nbits
                    val left = mask7 nbits
                    fun last () = (case mask7 n
			   of 0 => ()
                            | lft => W8A.update(newbits, len-1, (newbits sub (len-1)) & (lobits lft))
			  (* end case *))
                    fun adjust j = (
                          if left = 0
                            then ()
                            else W8A.update(newbits, j, (bits sub j) ++ (hibits left));
                          last ())
                    fun cpy i =
                          if i = nbytes
                            then adjust i
                            else (W8A.update(newbits, i, bits sub i); cpy(i+1))
                    in
                      cpy 0;
                      BA{nbits=n, bits=newbits}
                    end

          fun fit(lb,rb,rbits) = (rb & (lobits rbits)) ++ (lb & (hibits rbits))

          fun simpleCopy (src,dest,lastbyte,len) arg = let
                fun last (s,d) = (case mask7 len
		       of 0 => W8A.update(dest,d,src sub s)
                        | lft => W8A.update(dest,d,fit(dest sub d,src sub s,lft))
		      (* end case *))
                fun cpy (arg as (s,d)) =
                      if d = lastbyte
                        then last arg
                        else (W8A.update(dest,d,src sub s);cpy(s+1,d+1))
                in
                  cpy arg
                end

            (* rightblt copies bits [shft,shft+len-1] of src to
             * bits [0,len-1] in target.
             * Assume all parameters and lengths are okay.
             *)
          fun rightblt (src,dest,shft,len) = let
                val byte = byteOf shft and bitshift = wmask7 shft
                fun copy lastbyte = let
                      val lshift = 0w8 - bitshift
                      fun finish (sb,s,d) = let
                            val left = mask7 (len - 1) + 1
                            in
                              if Word.fromInt left <= lshift  (* enough bits in sb *)
                                then W8A.update(dest,d,fit(dest sub d,sb >> bitshift,left))
                                else let
                                  val sb' = (sb >> bitshift) ++ ((src sub s) << lshift)
                                  in
                                    W8A.update(dest,d,fit(dest sub d,sb',left))
                                  end
                            end
                      fun loop (arg as (sb, s, d)) =
                            if d = lastbyte then finish arg
                            else let
                              val sb' = src sub s
                              in
                                W8A.update(dest,d,(sb >> bitshift) ++ ((sb' << lshift) & 0wxFF));
                                loop(sb',s+1,d+1)
                              end
                      in
                        loop (src sub byte, byte+1, 0)
                      end
                in
                  if bitshift = 0w0 then simpleCopy (src,dest,byteOf(len-1),len) (byte,0)
                  else copy (byteOf (len-1))
                end

            (* leftblt copies bits [0,len-1] of src to
             * bits [shft,shft+len-1] in target.
             * Assume all parameters and lengths are okay.
             *)
          fun leftblt (_,_,_,0) = ()
            | leftblt (src,dest,shft,len) = let
                val byte = byteOf shft and bitshift = wmask7 shft
                val lastbyte = byteOf (shft+len-1)
                fun sliceCopy (s,d,len) = let
                      val mask = (lobits len) << bitshift
                      val sb = ((src sub s) << bitshift) & mask
                      val db = (dest sub d) & (Word8.notb mask)
                      in
                        W8A.update(dest,d,sb ++ db)
                      end
                fun copy () = let
                      val sb = src sub 0
                      val rshift = 0w8 - bitshift
                      fun finish (sb,s,d) = let
                            val left = mask7(shft + len - 1) + 1
                            in
                              if Word.fromInt left <= bitshift  (* enough bits in sb *)
                                then W8A.update(dest,d,fit(dest sub d,sb >> rshift,left))
                                else let
                                  val sb' = (sb >> rshift) ++ ((src sub s) << bitshift)
                                  in
                                    W8A.update(dest,d,fit(dest sub d,sb',left))
                                  end
                            end
                      fun loop (arg as (sb, s, d)) =
                            if d = lastbyte then finish arg
                            else let
                              val sb' = src sub s
                              in
                                W8A.update(dest,d,(sb >> rshift) ++ ((sb' << bitshift) & 0wxFF));
                                loop(sb',s+1,d+1)
                              end
                      in
                        W8A.update(dest,byte,fit(sb << bitshift, dest sub byte, Word.toIntX bitshift));
                        loop (sb, 1, byte+1)
                      end
                in
                  if bitshift = 0w0 then simpleCopy (src,dest,lastbyte,len) (0,byte)
                  else if lastbyte = byte then sliceCopy (0,byte,len)
                  else copy ()
                end

	  fun lshift_w (ba, 0w0) = mkCopy ba
	    | lshift_w (BA{nbits, bits}, n) = let
		val shft = Word.toIntX n
		val newlen = nbits + shft
		val newbits = W8A.array(sizeOf newlen, 0w0)
		in
		  leftblt(bits,newbits,shft,nbits);
		  BA{nbits=newlen,bits=newbits}
		end

          fun lshift (ba, shft) = if shft < 0
		then badArg("lshift", "negative shift")
		else lshift_w(ba, Word.fromInt shft)

          fun op @ (BA{nbits,bits},BA{nbits=nbits',bits=bits'}) = let
                val newlen = nbits + nbits'
                val newbits = W8A.array(sizeOf newlen, 0w0)
                in
                  copybits(bits',newbits);
                  leftblt(bits,newbits,nbits',nbits);
                  BA{nbits=newlen,bits=newbits}
                end

          fun concat [] = array (0, false)
            | concat [ba] = mkCopy ba
            | concat (l as (BA{bits,nbits}::tl)) = let
                val newlen = (foldl (fn (BA{nbits,...},a) => a+nbits) 0 l)
                            handle Overflow => raise Size
                val newbits = W8A.array(sizeOf newlen,0w0)
                fun cpy (BA{bits,nbits}, shft) = (
                        leftblt(bits,newbits,shft,nbits);
                        shft+nbits
                      )
                in
                  copybits(bits,newbits);
                  foldl cpy nbits tl;
                  BA{nbits=newlen,bits=newbits}
                end

          fun slice (ba as BA{nbits,bits},sbit,0) = array (0, false)
            | slice (ba as BA{nbits,bits},sbit,len) = let
                val newbits = W8A.array(sizeOf len,0w0)
                in
                  rightblt(bits,newbits,sbit,len);
                  BA{nbits=len,bits=newbits}
                end

          fun extract (ba as BA{nbits,bits},sbit,SOME len) =
                if sbit < 0 orelse len < 0 orelse sbit > nbits - len
                  then raise Subscript
                  else slice (ba,sbit,len)
            | extract (ba as BA{nbits,bits},sbit,NONE) =
                if sbit < 0 orelse sbit > nbits
                  then raise Subscript
                  else slice (ba,sbit,nbits-sbit)

	  fun rshift_w (ba, 0w0) = mkCopy ba
	    | rshift_w (BA{nbits, bits}, n) = let
		val shft = Word.toIntX n
                in
		  if shft >= nbits
		    then array (0, false)
		    else let
		      val newlen = nbits - shft
		      val newbits = W8A.array(sizeOf newlen,0w0)
		      in
			rightblt (bits, newbits, shft, newlen);
			BA{nbits=newlen, bits=newbits}
		      end
		end

          fun rshift (ba, shft) = if shft < 0
		then badArg("rshift", "negative shift")
		else rshift_w(ba, Word.fromInt shft)

          fun trim (tgt,len) =
                case mask7 len of
                  0 => ()
                | lft => let
                    val n = (W8A.length tgt) - 1
                    in
                      W8A.update(tgt, n, (tgt sub n) & (lobits lft))
                    end

          fun andBlend (BA{nbits,bits},BA{bits=bits',nbits=nbits'},tgt,len) = let
                fun copy i = (W8A.update(tgt,i,(bits sub i)&(bits' sub i));copy(i+1))
                in
                  (copy 0) handle _ => ();
                  trim (tgt,len)
                end

          fun orBlend f (ba,ba',tgt,len) = let
                fun order (arg as (ba as BA{nbits,...},ba' as BA{nbits=nbits',...})) =
                      if nbits >= nbits'
                        then arg
                        else (ba',ba)
                val (BA{nbits,bits},BA{bits=bits',nbits=nbits'}) = order (ba,ba')
                val bnd = W8A.length bits' (* number of bytes in smaller array *)
                fun copy2 i = (W8A.update(tgt,i,bits sub i);copy2(i+1))
                fun copy1 i =
                      if i = bnd
                        then copy2 bnd
                        else (W8A.update(tgt,i,f(bits sub i, bits' sub i));copy1(i+1))
                in
                  (copy1 0) handle _ => ();
                  trim (tgt,len)
                end

          fun newblend blendf (ba,ba',len) = let
                val nb as BA{bits,...} = array (len, false)
                in
                  blendf(ba,ba',bits,len);
                  nb
                end

          val orb = newblend (orBlend Word8.orb)
          val xorb = newblend (orBlend Word8.xorb)
          val andb = newblend andBlend

          fun union ba ba' = let
                val BA{bits,nbits} = ba
                val BA{bits=bits',nbits=nbits'} = ba'
                val nbytes = W8A.length bits
                val nbytes' = W8A.length bits'
                fun copy bnd = let
                      fun loop i =
                            if i = bnd
                              then ()
                              else (W8A.update(bits,i,(bits sub i) ++ (bits' sub i));loop(i+1))
                      in
                        loop 0
                      end
                in
                  if nbytes <= nbytes'
                    then (copy nbytes; trim (bits,nbits))
                    else copy nbytes'
                end

          fun intersection ba ba' = let
                val BA{bits,nbits} = ba
                val BA{bits=bits',nbits=nbits'} = ba'
                val nbytes = W8A.length bits
                val nbytes' = W8A.length bits'
                fun zeroFrom(b,j) = let
                      fun loop i = (W8A.update(b,i,0w0);loop(i+1))
                      in
                        (loop j) handle _ => ()
                      end
                in
                  if nbytes <= nbytes'
                    then andBlend(ba,ba',bits,nbytes * 8)
                    else (
                      andBlend(ba,ba',bits,nbytes' * 8);
                      zeroFrom (bits,nbytes')
                    )
                end

          fun flip (nbits, src, tgt) = let
                val nbytes = byteOf nbits and left = mask7 nbits
                fun last j =
                      W8A.update(tgt,j,(Word8.notb(src sub j)) & (lobits left))
                fun flp i =
                      if i = nbytes
                        then if left = 0 then () else last i
                        else (W8A.update(tgt,i,Word8.notb(src sub i) & 0wxff); flp(i+1))
                in
                  flp 0
                end

          fun notb (BA{nbits, bits}) = let
                val ba as BA{bits=newbits,...} = array (nbits, false)
                in
                  flip (nbits,bits,newbits);
                  ba
                end

          fun setBit (BA{nbits, bits}, i) = let
                val j = byteOf i and b = bit i
                in
                  if i >= nbits
                    then raise Subscript
                    else W8A.update (bits, j, ((bits sub j) ++ b))
                end

          fun clrBit (BA{nbits, bits}, i) = let
            val j = byteOf i and b = Word8.notb(bit i)
            in
              if i >= nbits
                then raise Subscript
                else W8A.update (bits, j, ((bits sub j) & b))
            end

          fun complement (BA{bits,nbits}) = flip(nbits, bits, bits)

          fun update (ba,i,true) = setBit (ba,i)
            | update (ba,i,_) = clrBit (ba,i)

          fun op sub arg = bitOf arg

          fun length (BA{nbits, ...}) = nbits

          fun app f (BA{nbits=0,bits}) = ()
            | app f (BA{nbits,bits}) = let
                val last = byteOf (nbits-1)
                fun loop (0,_) = ()
                  | loop (n,byte) = (f ((byte&0w1) = 0w1); loop (n-1,byte >> 0w1))
                fun f' (i,byte) =
                       if i < last then loop (8,byte)
                       else loop (mask7 (nbits - 1) + 1, byte)
                in
                  W8A.appi f' bits
                end

            (* FIX: Reimplement using W8A.foldi *)
          fun foldl f a (BA{nbits,bits}) = let
                fun loop (i,a) =
                      if i = nbits then a
                      else let
                        val b = ((W8A.sub (bits,byteOf i)) & (bit i)) <> 0w0
                        in
                          loop (i+1, f(b,a))
                        end
                in
                  loop (0,a)
                end

            (* FIX: Reimplement using W8A.foldr *)
          fun foldr f a (BA{nbits,bits}) = let
                fun loop (~1,a) = a
                  | loop (i,a) = let
                      val b = ((W8A.sub (bits,byteOf i)) & (bit i)) <> 0w0
                      in
                        loop (i-1, f(b,a))
                      end
                in
                  loop (nbits-1,a)
                end

          fun valid (nbits,sbit,NONE) =
                if sbit < 0 orelse sbit > nbits
                  then raise Subscript
                  else nbits - sbit
            | valid (nbits,sbit,SOME len) =
                if sbit < 0 orelse len < 0 orelse sbit > nbits - len
                  then raise Subscript
                  else len

            (* FIX: Reimplement using W8A.appi *)
          fun appi' f (BA{nbits=0,bits},_,_) = ()
            | appi' f (BA{nbits,bits},sbit,l) = let
                val len = valid (nbits, sbit, l)
                fun loop (_, 0) = ()
                  | loop (i, n) = let
                      val b = ((W8A.sub (bits,byteOf i)) & (bit i)) <> 0w0
                      in
                        f(i,b);
                        loop (i+1,n-1)
                      end
                in
                  loop (sbit,len)
                end

            (* FIX: Reimplement using W8A.foldi *)
          fun foldli' f a (BA{nbits,bits},sbit,l) = let
                val len = valid (nbits, sbit, l)
                val last = sbit+len
                fun loop (i,a) =
                      if i = last then a
                      else let
                        val b = ((W8A.sub (bits,byteOf i)) & (bit i)) <> 0w0
                        in
                          loop (i+1, f(i,b,a))
                        end
                in
                  loop (sbit,a)
                end

            (* FIX: Reimplement using W8A.foldr *)
          fun foldri' f a (BA{nbits,bits},sbit,l) = let
                val len = valid (nbits, sbit, l)
                fun loop (i,a) =
                      if i < sbit then a
                      else let
                        val b = ((W8A.sub (bits,byteOf i)) & (bit i)) <> 0w0
                        in
                          loop (i-1, f(i,b,a))
                        end
                in
                  loop (sbit+len-1,a)
                end

            (* FIX: Reimplement using general-purpose copy *)
          fun copy' {src = src as BA{nbits,bits},si,len,dst,di} = let
                val l = valid (nbits, si, len)
                val BA{nbits=nbits',bits=bits'} = dst
                val _ = if di < 0 orelse nbits' - di < l then raise Subscript
                        else ()
                val last = si + l
                fun loop (si,di) =
                      if si = last then ()
                      else (
                        if bitOf (src, si) then setBit(dst,di) else clrBit(dst,di);
                        loop (si+1,di+1)
                      )
                in
                  loop (si,di)
                end

          fun modify f (BA{nbits=0,bits}) = ()
            | modify f (BA{nbits,bits}) = let
                val last = byteOf (nbits-1)
                fun loop (0,_,a,_) = a
                  | loop (n,byte,a,mask) =
                      if f ((byte&mask) = mask)
                        then loop (n-1,byte, a&mask, mask << 0w1)
                        else loop (n-1,byte, a, mask << 0w1)
                fun f' (i,byte) =
                       if i < last then loop (8,byte,0w0,0w1)
                       else loop (mask7 (nbits - 1) + 1, byte,0w0,0w1)
                in
                  W8A.modifyi f' bits
                end

            (* FIX: Reimplement using W8A.modifyi *)
          fun modifyi' f (BA{nbits=0,bits},sbit,l) = ()
            | modifyi' f (BA{nbits,bits},sbit,l) = let
                val len = valid (nbits, sbit, l)
                val last = sbit+len
                fun loop i =
                      if i = last then ()
                      else let
                        val index = byteOf i
                        val biti = bit i
                        val byte = W8A.sub (bits,index)
                        val b = (byte & biti) <> 0w0
                        val b' = f(i,b)
                        in
                          if b = b' then ()
                          else if b' then W8A.update(bits,index,byte ++ biti)
                          else W8A.update(bits,index,byte & (Word8.notb biti));
                          loop (i+1)
                        end
                in
                  loop sbit
                end

	(* additional operation from Basis Library proposal 2015-003 *)
	  fun toList (BA{nbits, bits}) = let
		fun loop (~1, a) = a
		  | loop (i, a) = let
		      val b = ((Word8Array.sub (bits,byteOf i)) & (bit i)) <> 0w0
		      in
			loop (i-1, b :: a)
		      end
		in
		  loop (nbits-1, [])
		end

          end (* local *)

	  val << = lshift_w
	  val >> = rshift_w
        end (* structure Vector *)

    open Vector
    type array = vector

    fun vector a = a

    fun copy { src, dst, di } = copy' { src = src, dst = dst, di = di,
					si = 0, len = NONE }

    val copyVec = copy

    fun appi f a = appi' f (a, 0, NONE)
    fun modifyi f a = modifyi' f (a, 0, NONE)
    fun foldli f init a = foldli' f init (a, 0, NONE)
    fun foldri f init a = foldri' f init (a, 0, NONE)

    (* These are slow, pedestrian implementations.... *)
    fun findi p a = let
	val len = length a
	fun fnd i =
	    if i >= len then NONE
	    else let val x = sub (a, i)
		 in
		     if p (i, x) then SOME (i, x) else fnd (i + 1)
		 end
    in
	fnd 0
    end

    fun find p a = let
	val len = length a
	fun fnd i =
	    if i >= len then NONE
	    else let val x = sub (a, i)
		 in
		     if p x then SOME x else fnd (i + 1)
		 end
    in
	fnd 0
    end

    fun exists p a = let
	val len = length a
	fun ex i = i < len andalso (p (sub (a, i)) orelse ex (i + 1))
    in
	ex 0
    end

    fun all p a = let
	val len = length a
	fun al i = i >= len orelse (p (sub (a, i)) andalso al (i + 1))
    in
	al 0
    end

    fun collate c (a1, a2) = let
	val l1 = length a1
	val l2 = length a2
	val l12 = Int.min (l1, l2)
	fun col i =
	    if i >= l12 then Int.compare (l1, l2)
	    else case c (sub (a1, i), sub (a2, i)) of
		     EQUAL => col (i + 1)
		   | unequal => unequal
    in
	col 0
    end

  (* additional operations from Basis Library proposal 2015-003 *)
    fun fromVector (Vector.BA{nbits, bits}) = let
	  val newBits = Unsafe.Word8Array.create(Word8Array.length bits)
	  in
	    Word8Array.copy{di = 0, dst = newBits, src = bits};
	    BA{nbits = nbits, bits = newBits}
	  end

    val toVector = fromVector

  end (* structure BitArray *)

(* crc.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

signature CRC =
sig
  type crc
  val zero: crc
  val append : crc * char -> crc
  val N : int  (* size, in bytes, of CRC strings *)
  val fromString : string -> crc    (* computes the CRC of a string *)
  val toString: crc -> string
       (* Axiom:  fromString(toString(x)) = x *)
  val compare : crc * crc -> order
  val combine: crc list ->crc
  val hashToInt: crc -> int
  val * : crc * crc -> crc
  val + : crc * crc -> crc
      (* 0 <= hashToInt N c < N *)

  val suffix: {start: crc, finish: crc, length: int} -> crc

   (* Suffix allows you to compute CRC of the string B
             when you already know the CRC's of A and AB
  
      For any strings a,b,    test(a,b) = true
        fun test(a,b) =
	    let fun crcstring(start,a) = 
		      foldr (fn(x,y)=>CRC.append(y,x)) start (explode a)
                val x = crcstring(CRC.zero,a)
                val y = crcstring(x,b)
                val z = crcstring(CRC.zero,b)
                val z' = CRC.suffix{start=x,finish=y,length=size b}
             in CRC.toString z = CRC.toString z'
            end

    For a hash-consing application, I want to know the CRC of a string b
    knowing only:

      X = CRC of a
      Y = CRC of a^b
      N = size of b  (in bytes)

   The CRC of a string s is really a polynomial in the field ZF(2):

       (  Sum_i (s[i] * x^i)  )  mod P

   where s[i] is the i'th bit of the string and P is a primitive polynomial;
 
   then we can compute   Z = CRC of b  as follows:

      Z = (X * x^(8N) + Y) mod P

    where addition (+) is in the field of polynomials over ZF(2).

    Let's define this operation as  suffix{start=X,finish=Y,length=N}
    and we can do it in constant time (though the constant depends on the
    size of the polynomial P).

  *)



end

structure CRC :> CRC =
  struct

    val wtoi = Word.toIntX
    val itow = Word.fromInt

  (* 128-bit CRC.  
   * The call `append crc c' corresponds to eight steps of a shift register
   * circuit, shifting in one bit of character c from the left in each step.
   * See Figure 2.16 in Bertsekas and Gallager: Data Networks (1987), 
   * or Figure 3-32 in Siewiorek and Swarz: The Theory and Practice 
   * of Reliable System Design (Digital Press, 1982). 
   *)

    type crc = {high: int list, low: int list, lowest: int}
               (* Invariant: size(high @ rev low @ [lowest]) = 16 *)

  (* The prime generator polynomial is 1 + x + x^2 + x^7 + x^128.
   * Reversing the low coefficient bits we have 1110.0001 = 225
   *)
    val poly = 0w225

    val table : int Vector.vector = 
         let
	  fun init n = let
	        fun f (0w0,_,r) = wtoi r
		  | f (i,p,r) = if Word.andb(i,0w1)<>0w0
		      then f(Word.>>(i,0w1),p+p,Word.xorb(p,r))
		      else f(Word.>>(i,0w1),p+p,r)
	        in
		  f(itow n,poly,0w0)
	        end
	  in
	    Vector.tabulate(256, init)
	  end

    val N = 16

    val zero = {high=[0,0,0,0,0,0,0,0,0,0,0,0,0,0],low=[0],lowest=0}

    fun toString{high,low,lowest} = implode(map chr (high @ rev low @ [lowest]))

    fun append'({high=0::high', low, lowest}, c) =
              {high=high',low=lowest::low,lowest=c}
      | append'({high=h::high', low, lowest}, c) =
       let val hilo = Vector.sub(table, h)
           val hi = Word.>>(itow hilo,0w8)
           val lo = Word.andb(itow hilo,0w255)
        in {high=high', low= wtoi(Word.xorb(itow lowest,hi)) :: low, 
	    lowest=wtoi(Word.xorb(itow c, lo))}
       end
      | append'({high=nil, low, lowest}, c) = 
              append'({high=rev low, low=nil, lowest=lowest}, c)

    fun append(crc, c) = append'(crc, ord c)

    val z14 = [0,0,0,0,0,0,0,0,0,0,0,0,0,0]

    fun fromString s = 
       let fun get i = ord(String.sub(s,i))
           fun loop(high,i) = if i=0 then high
                              else let val i' = i-1
                                    in loop((get i')::high,i')
                                   end
           val len = size s
        in if len >= 16
            then let val crc0 = {high=loop(nil,14),low=[get 14],lowest=get 15}
                     fun aloop(crc,i) = if i = len
                                   then crc else aloop(append'(crc,get i),i+1)
                  in aloop(crc0,16)
                 end
           else if len > 2
            then let fun zloop(high,0) = high
		       | zloop(high,n) = zloop(0::high, n-1)
                  in {high=zloop(loop(nil,len-2),16-len),low=[get(len-2)],
		      lowest=get(len-1)}
                 end
           else if len=2
            then {high=z14,low=[get(0)], lowest=get(1)}
           else if len=1
	       then let val {high,low,...} = zero
                      in {high=high,low=low,lowest=get 0}
		    end
           else zero

       end
                       
   val one = append'(zero,1)

   fun map2w f = ListPair.map (fn (a, b) => wtoi (f (itow a, itow b)))

   fun xor({high=h1,low=l1,lowest=x1},
	   {high=h2,low=l2,lowest=x2}) =
            {high=map2w Word.xorb (h1 @ rev l1, h2 @ rev l2),
	     low=nil, lowest=wtoi(Word.xorb(itow x1,itow x2))}

(* buggy
   fun prod1(x,y) =
   let fun f(0,x,y,u) = u
         | f(n,x,y,u) = let val odd = Bits.andb(y,1)
                       in f(n-1,Bits.lshift(x,1),Bits.rshift(y,1),
			    Bits.xorb(u,Bits.andb(~odd,y)))
                       end
    in f(8,x,y,0)
   end
*)

   fun prod1(x,0) = 0
     | prod1(x,y) =
       let val u = prod1(x, wtoi(Word.>>(itow y,0w1)))
	   val odd = wtoi(Word.andb(itow y,0w1))
        in wtoi(Word.xorb(Word.<<(itow u,0w1),
		     Word.andb(itow x, itow(~odd))))
       end

   fun product(crc1,crc2) =
          let 
	      fun expand crc =
		  (* list of bytes from low to high, dropping high zeros *)
	         let fun f({high=0::h',low,lowest},nil) = 
		            f({high=h',low=low,lowest=lowest},nil)
                       | f({high=h::h',low,lowest},r) = 
		            f({high=h',low=low,lowest=lowest},h::r)
                       | f({high=nil,low=nil,lowest=0},nil) = nil
                       | f({high=nil,low=nil,lowest},r) = lowest::r
                       | f({high=nil,low,lowest},r) = 
                             f({high=rev low, low=nil,lowest=lowest},r)
                  in  f(crc,nil)
                 end

              fun prodN(x, carry, lowest::rest) =
                   let val hilo= prod1(x,lowest)
                       val lo = Word.andb(itow hilo,0w255)
		       val hi = Word.>>(itow hilo,0w8)
                    in append'(prodN(x, wtoi hi, rest),
                               wtoi(Word.xorb(lo,itow carry)))
                   end
                | prodN(x, carry, nil) = append'(zero, carry)

	       fun prodNN(x::xx,yy) =
                     xor (prodN(x,0,yy), append'(prodNN(xx,yy), 0))
                 | prodNN(nil,yy) = zero

            in prodNN(expand crc1, expand crc2)
           end

   val MAX = 64  (* such that the "length" argument to "suffix" 
                    is never larger than 2^MAX *)

   val expsqr = let
       fun loop(i,v::vl) =
	   if i<MAX then
	       (* precondition: v = append(one,zerostring(2^(i-1)))
		* where zerostring(n) is a string of n null bytes 
		* postcondition: loop(i+1,append(one,zerostring(2^i))::v::vl)*)
	       loop(i+1, product(v,v)::v::vl)
	   else Vector.fromList(rev (v::vl))
	 | loop _ = raise Fail "CRC: internal error (expsqr)"
   in
       loop(1,[append'(one, 0)])
   end

   fun odd(n) = Word.andb(itow n,0w1) <> 0w0

   fun shift(crc,n) =
      let fun scan(i,accum) = 
	      let val j = wtoi (Word.<<(0w1,itow i))
	       in if j>n then accum
		  else if Word.andb(itow j,itow n) <> 0w0
			   then scan(i+1,product(accum,Vector.sub(expsqr,i)))
                           else scan(i+1,accum)
              end
       in product(crc,scan(0,one))
      end

   fun suffix{start, finish, length=n} = xor(shift(start,n), finish)


(*
   fun hashToInt n {high,low,lowest} = 
    let fun hashbyte(b, accum) = (accum*256 + b) mod n
     in accum(lowest,foldr hashbyte (foldl hashbyte 0 high) low)
    end
*)
   fun hashToInt {high,low,lowest} =
    let val op * = Word32.* and op + = Word32.+
        fun hashbyte(b, accum: Word32.word) = 
                          (accum*0w65599 + Word32.fromInt b)
	val h = hashbyte(lowest,foldr hashbyte (foldl hashbyte 0w0 high) low)
     in Word32.toInt(Word32.>>(h * 0w65599,0w2))
    end
    

   fun compare({high=ah::ar,low=al,lowest=at},{high=bh::br,low=bl,lowest=bt})=
          if ah<bh then
	      LESS else if (ah:int)>bh then GREATER
	    else compare({high=ar,low=al,lowest=at},{high=br,low=bl,lowest=bt})
     | compare({high=nil,low=al as _::_,lowest=at},b) =
           compare({high=rev al,low=nil,lowest=at},b)
     | compare(a,{high=nil,low=bl as _::_,lowest=bt}) =
            compare(a,{high=rev bl, low=nil,lowest=bt})
     | compare({high=nil,low=nil,lowest=at},{high=nil,low=nil,lowest=bt}) = 
             if at<(bt:int) then LESS 
	     else if at>bt then GREATER else EQUAL
     | compare _ = raise Fail "CRC: internal error (compare)"

(*
   fun {high=ah::ar,low=al,lowest=at} < {high=bh::br,low=bl,lowest=bt} =
                 Int.<(ah,bh)
         orelse ah=bh andalso
	         {high=ar,low=al,lowest=at} < {high=br,low=bl,lowest=bt}
     | {high=nil,low=al as _::_,lowest=at} < b =
           {high=rev al,low=nil,lowest=at} < b
     | a < {high=nil,low=bl as _::_,lowest=bt} =
            a < {high=rev bl, low=nil,lowest=bt}
     | {high=nil,low=nil,lowest=at} < {high=nil,low=nil,lowest=bt} = 
             Int.<(at,bt)
*)
(*   fun show crc = concat(map (fn c => Int.toString(ord c) ^ " ") (explode (toString crc)))
*)

   val A: Word32.word = 0wxff208489
   and B: Word32.word = 0wxf4872e10
   and C: Word32.word = 0wx402d619b
   and D: Word32.word = 0wx0bf359a7


   val perm = #[
    255, 254, 252, 251, 250, 248, 240, 245, 246, 238, 237, 244, 7, 189,
    214, 236, 235, 20, 33, 8, 227, 14, 233, 178, 172, 60, 229, 133, 152,
    19, 210, 203, 221, 208, 76, 18, 13, 199, 113, 62, 40, 190, 213, 194,
    43, 181, 21, 15, 201, 162, 90, 186, 71, 117, 107, 70, 191, 5, 173, 44,
    39, 12, 174, 183, 99, 11, 176, 163, 161, 72, 86, 105, 2, 83, 42, 52,
    179, 135, 103, 110, 151, 58, 108, 96, 166, 25, 115, 66, 142, 10, 141,
    48, 104, 34, 159, 120, 22, 140, 64, 82, 78, 68, 207, 125, 123, 150,
    144, 138, 128, 139, 136, 114, 119, 53, 148, 185, 41, 124, 216, 143,
    49, 92, 98, 51, 112, 73, 50, 63, 16, 46, 158, 126, 206, 122, 94, 132,
    88, 184, 28, 84, 127, 156, 167, 223, 118, 89, 116, 17, 111, 121, 109,
    77, 146, 61, 224, 101, 81, 218, 97, 188, 243, 155, 57, 102, 54, 129,
    93, 192, 153, 106, 36, 145, 79, 31, 137, 26, 67, 85, 175, 80, 168, 65,
    91, 1, 147, 149, 6, 29, 37, 69, 182, 165, 4, 74, 55, 47, 171, 169, 75,
    134, 193, 195, 198, 131, 38, 180, 56, 196, 23, 154, 177, 200, 205, 27,
    209, 95, 204, 160, 3, 30, 157, 32, 9, 212, 211, 45, 202, 170, 0, 219,
    187, 87, 35, 100, 217, 232, 164, 228, 220, 197, 231, 215, 226, 130,
    225, 234, 241, 239, 59, 230, 247, 24, 249, 242, 222, 253 ]
  

  fun combine [] = zero
    | combine [crc] = crc
    | combine (crc1::crcs) = 
    let fun expand{high,low,lowest} = lowest :: low @ rev high
        fun mash(crc1,crc2) = foldr (fn (c,x)=>append'(x,c)) crc1 (expand crc2)
        val x = foldr mash crc1 crcs

        fun w32(a::b::c::d::rest) =
              (Word32.xorb(Word32.<<(Word32.fromInt d, 0w24),
               Word32.xorb(Word32.<<(Word32.fromInt c, 0w16),
                Word32.xorb(Word32.<<(Word32.fromInt b, 0w8),
                           Word32.fromInt a))),
	       rest)
	  | w32 _ = raise Fail "CRC: internal error (w32)"

        val (u0,r0) = w32 (expand x)
        val (u1,r1) = w32 r0
        val (u2,r2) = w32 r1
        val (u3,r3) = w32 r2
	val _ = case r3 of
	    [] => () | _ => raise Fail "CRC: internal error (w32 rest)"

        val v0 = Word32.+(Word32.*(u0,A),u1)
        val v1 = Word32.+(Word32.*(u1,B),u2)
        val v2 = Word32.+(Word32.*(u2,C),u3)
        val v3 = Word32.+(Word32.*(u3,D),u0)
        
        fun byte(b,k) = 
	    Vector.sub(perm,
		       Word32.toInt(Word32.andb(0w255,Word32.>>(b,Word.fromInt k))))
        fun b32(n,rest) = byte(n,0) :: byte(n,8) :: byte(n,16) :: byte(n,24)
                           :: rest
        val x' = b32(v3,b32(v2,b32(v1,b32(v0,nil))))
    in
        case x' of
	    y0 :: y1 :: y' => { high = y', low = [y1], lowest = y0 }
	  | _ => raise Fail "CRC: internal error (y0,y1,y')"
     end

   val op * = product
   val op + = xor
end
(*
structure Test = 
struct
 

 fun test(a,b) =
   let fun crcstring(a) = 
              foldl (fn(x,y)=>CRC.append(y,x)) CRC.zero (explode a)
       val zeros = crcstring(implode (chr 1 :: map (fn _ => chr 0) (explode b)))
       val x = crcstring a
       val y = crcstring b
       val z = crcstring (a^b)
       val z' = CRC.+(CRC.*(x,zeros),y)
   in CRC.toString z = CRC.toString z'
  end

end
*)


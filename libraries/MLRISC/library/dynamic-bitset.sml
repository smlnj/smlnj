(*
 * Growable bitset.
 *
 * -- Allen
 *)

structure DynamicBitSet :> BITSET =
struct

   structure A = Word8Array
   structure W = Word8
   open A

   infix << >> & ||
   infix sub

   type bitset = array ref

   val word  = Word.fromInt 
   val int   = Word.toInt
   val op &  = Word.andb
   val op >> = Word.>>
   val op << = W.<<

   fun create n = ref(array((n+7)div 8, 0wx0))

   fun size a = length(! a) * 8

   fun grow (r as ref a, i) =
   let val new_size  = Int.max(length a * 2, i)
       val new_array = array(new_size, 0wx0)
       val _         = copy { src = a, si = 0, dst = new_array, di = 0, 
                              len = NONE }
   in  r := new_array
   end

   fun set (r as ref a, i) =
   let val byte = int((word i) >> 0w3)
       val mask = W.<< (0w1, (word i) & 0w7)
   in  update(a, byte, W.orb(a sub byte, mask)) end
   handle Subscript => (grow (r, i+1); set(r,i))

   fun reset (r as ref a, i) =
   let val byte = int((word i) >> 0w3)
       val mask = W.notb(W.<< (0w1, (word i) & 0w7))
   in  update(a, byte, W.andb(a sub byte, mask)) end
   handle Subscript => ()

   fun clear (ref a) = modify (fn _ => 0wx0) a

   fun negate (ref a) = ref(tabulate (length a, fn i => W.notb(a sub i)))

   fun union (ref a, ref b) =
   let val m         = Int.max(length a, length b)
       val n         = Int.min(length a, length b)
       val c         = array(m, 0wx0)
       fun f ~1      = ()
         | f i       = update(c, i, W.orb(a sub i, b sub i))
   in  f n;
       copy { src = if length a > length b then a else b,
              si  = n,  dst = c, di  = n, len = NONE };
       ref c 
   end

   fun intersect (ref a, ref b) =
   let val n         = Int.min(length a, length b)
       val c         = array(n, 0wx0)
       fun f ~1      = ()
         | f i       = update(c, i, W.andb(a sub i, b sub i))
   in  f n;
       ref c 
   end

   fun diff (ref a, ref b) =
   let val m         = length a
       val c         = array(m, 0wx0)
       fun f ~1      = ()
         | f i       = update(c, i, W.andb(a sub i, W.notb(b sub i)))
   in  f m; ref c
   end

   fun unionWith (r as ref a, ref b) =
      (if length b > length a then grow(r, length b) else ();
       modifyi (fn (i,x) => W.orb(x,b sub i)) (a, 0, NONE))
   
   fun intersectWith (ref a, ref b) =
      modifyi (fn (i,x) => W.andb(x,b sub i)) (a, 0, NONE)

   fun diffWith (ref a, ref b) =
      modifyi (fn (i,x) => W.andb(x,W.notb(b sub i))) (a, 0, NONE)

   fun complement (ref a) = modify W.notb a

   fun copy (ref a) = ref(tabulate (length a, fn i => a sub i))

   fun toString (ref a) = 
   let fun f i = if i < length a then W.toString(a sub i)::f(i+1) else []
       val s = String.concat(f 0)
   in  "[" ^ s ^ "]" end

   fun contains (r as ref a, i) = 
   let val byte = int((word i) >> 0w3)
       val mask = W.<<(0w1, (word i) & 0w7)
   in  W.andb(A.sub(a, byte), mask) <> 0wx0 end
   handle Subscript => false
 
   fun markAndTest (r as ref a, i) =
   let val byte = int((word i) >> 0w3)
       val mask = W.<<(0w1, (word i) & 0w7)
       val word = A.sub(a,byte)
   in  if W.andb(word, mask) <> 0wx0 then
          true
       else 
          (A.update(a, byte, W.orb(word, mask)); false)
   end handle Subscript => (grow (r, i+1); markAndTest(r,i))

   fun unmarkAndTest (r as ref a, i) =
   let val byte = int(word i >> 0w3)
       val mask = W.<<(0w1, (word i) & 0w7)
       val word = A.sub(a,byte)
   in  if W.andb(word, mask) <> 0wx0 then
          (A.update(a, byte, W.andb(word,W.notb mask)); true)
       else 
          false
   end handle Subscript => false

end


(* dynamic-array.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Dynamic (dense) array.
 *
 * -- Allen
 *)

structure DynArray : sig

    include ARRAY

    val fromArray : 'a Array.array * 'a * int -> 'a array
    val baseArray : 'a array -> 'a Array.array
    val checkArray: 'a array * 'a Array.array -> unit
    val clear     : 'a array * int -> unit
    val expandTo  : 'a array * int -> unit

  end = struct
     structure A = Array
     structure AS = ArraySlice
     type 'a vector = 'a A.vector 
     datatype 'a array = ARRAY of 'a A.array ref * 'a * int ref

     exception Subscript = General.Subscript
     exception Size      = General.Size
     exception Unimplemented

     infix 9 sub

     val maxLen = A.maxLen

     fun array (n,d) = ARRAY(ref(A.array (n,d)), d, ref 0) 
     fun clear (ARRAY(a,def,cnt),n) = (a := A.array(n,def); cnt := n)
     fun fromArray(a,d,n) = ARRAY(ref a, d, ref n)

     fun baseArray(ARRAY(ref a,_,_)) = a
     fun checkArray(ARRAY(ref a,_,_),a') = if a = a' then () else raise Match

     fun length (ARRAY (ref a,_,ref n)) = n

     fun (ARRAY(ref a, d, _)) sub i = A.sub(a,i) handle _ => d
    
     fun update (ARRAY(r as ref a, d, n), i, e) =
        (A.update(a,i,e); n := Int.max(!n,i+1)) handle _ =>
            let val new_size  = Int.max(i+1,!n*2)
                val new_size  = if new_size < 10 then 10 else new_size
                val new_array = A.array(new_size,d)
            in  A.copy {src = a, dst = new_array, di = 0};
                r := new_array;
                n := i+1;
                A.update(new_array, i, e)
            end

     fun expandTo(arr as ARRAY(_, d, _), N) = update(arr, N-1, d)

     fun tabulate (n, f) = 
         let val array   = A.tabulate(n, f)
             val default = A.sub(array,0)
         in
             ARRAY(ref array, default, ref n)
         end handle _ => raise Size

     fun fromList l =
         let val array   = A.fromList l
             val default = A.sub(array,0)
         in
             ARRAY(ref array, default, ref (A.length array))
         end handle _ => raise Size

     fun slice (ARRAY (ref a, _, ref n)) = AS.slice (a, 0, SOME n)

     fun appi f arr = AS.appi f (slice arr)
     fun app f arr = AS.app f (slice arr)

     fun copy { src, dst, di } =
	 appi (fn (i, x) => update (dst, i + di, x)) src

     fun copyVec { src, dst, di } =
	 Vector.appi (fn (i, x) => update (dst, i + di, x)) src

     fun foldli f init arr = AS.foldli f init (slice arr)
     fun foldri f init arr = AS.foldri f init (slice arr)
     fun foldl f init arr = AS.foldl f init (slice arr)
     fun foldr f init arr = AS.foldr f init (slice arr)
     fun modifyi f arr = AS.modifyi f (slice arr)
     fun modify f arr = AS.modify f (slice arr)
     fun findi p arr = AS.findi p (slice arr)
     fun find p arr = AS.find p (slice arr)
     fun exists p arr = AS.exists p (slice arr)
     fun all p arr = AS.all p (slice arr)
     fun collate c (a1, a2) = AS.collate c (slice a1, slice a2)
     fun vector arr = AS.vector (slice arr)

   (* additional operations from Basis Library proposal 2015-003 *)
     fun toList arr = foldr (op ::) [] arr

     fun fromVector v = let
	    val arr = A.fromVector v
	    val default = A.sub(arr, 0)
	    in
	      ARRAY(ref arr, default, ref (A.length arr))
	    end
	      handle _ => raise Size

     val toVector = vector

end

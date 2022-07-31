(*
 * Bag datatype that uses hashing
 * 
 * -- Allen
 *)

structure HashBag :> HASH_BAG =
struct

   structure S = HashMap

   type 'a bag = ('a,int) S.map * int ref

   fun create x n = (S.create x n, ref 0)

   fun insert (bag,c) i =
      (S.update bag ((i,1),fn x => x + 1); c := !c + 1)
  
   fun insertN (bag,c) (i,n:int) =
      (S.update bag ((i,n),fn x => x + n); c := !c + n)

   fun size (_,c) = !c

   fun bucketSize (bag,_) = S.bucketSize bag

   fun isEmpty (_,c) = !c = 0

   fun remove (bag,c) i = 
      let val x = S.lookupOrElse bag 0 i
      in  if x > 0 then (S.insert bag (i,x-1); c := !c - 1) else ()
      end

   fun removeN (bag,c) (i,n) =  
      let val x = S.lookupOrElse bag 0 i
      in  if x > n then (S.insert bag (i,x-n); c := !c - n) 
          else (c := !c - Int.min(x,n); S.remove bag i)
      end

   fun removeAll (bag,c) i = S.remove bag i

   fun toList (bag,_) = S.toList bag

   fun clear (bag,c) = (S.clear bag; c := 0)

   fun contains (bag,_) i = S.contains bag i

   fun count (bag,_) i = S.lookupOrElse bag 0 i

   fun app f (bag,_) = S.app f bag

   fun dupApp f (bag,_) =
   let fun f' (x,0) = ()
         | f' (x,n) = (f x; f'(x,n-1))
   in
       S.app f' bag
   end

   fun fold f x (bag,_) = S.fold f x bag

   fun dupFold f x (bag,_) =
   let fun f' ((x,0),l) = l
         | f' ((x,n),l) = f'((x,n-1),f(x,l))
   in  S.fold f' x bag
   end

   fun toDupList bag = dupFold (op::) [] bag

   fun toString str bag =
      "{" ^ dupFold (fn (x,"") => str x
                      | (x,l)  => str x ^ ", " ^ l) "" bag ^ "}"
end


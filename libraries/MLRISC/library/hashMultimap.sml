(*
 * Multimap datatype that uses hashing.
 *
 * -- allen
 *)

structure HashMultimap :> HASH_MULTIMAP =
struct

   structure S = HashMap

   type ('a,'b) multimap = ('a,'b list) S.map * int ref


   fun create x n = (S.create x n, ref 0)

   fun size (_,c)    = !c
   fun bucketSize (m,_) = S.bucketSize m
   fun isEmpty (_,c) = !c = 0

   fun insert (m,c) (e as (x,y)) =
      (S.update m ((x,[y]),fn ys => y::ys); c := !c + 1)

   fun removeAll (m,c) i =
   let val stuff = S.lookup m i
   in  S.remove m i; c := !c - length stuff
   end handle _ => ()

   fun update (m,c) (e as (x,ys)) =
   let val stuff = S.lookupOrElse m [] x
   in  S.insert m e; c := !c - length stuff + length ys
   end

   fun lookup (m,_) i = S.lookup m i

   fun contains (m,_) i = S.contains m i

   fun count (m,_) i = length(S.lookupOrElse m [] i)

   fun toList (m,_) = S.toList m

   fun toDupList (m,_) =
   let fun collect (x,[],l)   = l
         | collect (x,h::t,l) = (x,h)::collect(x,t,l)
   in
       S.fold (fn ((x,ys),l) => collect (x,ys,l)) [] m 
   end

   fun clear (m,c) = (S.clear m; c := 0)

   fun dupApp f (m,_) =
   let fun call (x,[])   = ()
         | call (x,h::t) = (f(x,h); call(x,t))
   in
       S.app call m
   end

   fun app f (m,_) = S.app f m

   fun dupFold f x (m,_) =
   let fun collect((x,[]),l)   = l
         | collect((x,h::t),l) = collect((x,t),f((x,h),l))
   in
       S.fold collect x m
   end

   fun fold f x (m,_) = S.fold f x m

   fun toString (f,g) m =
      "{" ^
          dupFold (fn ((x,y),"") => "(" ^ f x ^ ", " ^ g y ^ ")"
                   | ((x,y),l)  => "(" ^ f x ^ ", " ^ g y ^ "), " ^ l) 
                   "" m ^ "}"

end


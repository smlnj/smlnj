(*
 * Sorting
 *
 * -- Allen
 *)

signature SORTING =
sig

   val sort        : ('a * 'a -> bool) -> 'a list -> 'a list
   val sort_uniq   : ('a * 'a -> bool) -> 
                     ('a * 'a -> bool) -> 'a list -> 'a list
   val merge       : ('a * 'a -> bool) -> 'a list * 'a list -> 'a list
   val merge_uniq  : ('a * 'a -> bool) -> 
                     ('a * 'a -> bool) -> 'a list * 'a list -> 'a list
   val merge_uniqs : ('a * 'a -> bool) -> 
                     ('a * 'a -> bool) -> 'a list list -> 'a list
   val uniq        : ('a * 'a -> bool) -> 'a list -> 'a list

end 

structure Sorting : SORTING =
struct

   infix ==

   fun gensort merge op< l =
   let fun sort [] = []
         | sort (l as [x])   = l
         | sort (l as [x,y]) = if x < y then l else [y,x]
         | sort l =
           let fun split([],a,b)    = (a,b)
                 | split(x::xs,a,b) = split(xs,b,x::a)
               val (a,b) = split(l,[],[])
           in  merge (sort a, sort b)
           end
   in  sort l
   end

   fun merge op< (a,b) =
   let fun m ([],a) = a
         | m (a,[]) = a
         | m (a as (u::v), b as (w::x)) =
            if u < w then u::m(v,b) else w::m(a,x)
   in  m(a,b)
   end

   fun merge_uniq op< op== (a,b) =
   let fun m ([],a) = uniq op== a
         | m (a,[]) = uniq op== a
         | m (a as (u::v), b as (w::x)) =
            if u == w then m(a,x)
            else if u < w then u::m(v,b) 
            else w::m(a,x)
   in  m(a,b)
   end

   and uniq op== l =
   let fun f []                 = []
         | f (l as [x])         = l
         | f (x::(l as (y::z))) = if x == y then f l else x::f l
   in  f l
   end


   fun sort op< l = gensort (merge op<) op< l

   fun sort_uniq op< op== l = gensort (merge_uniq op< op==) op< l

   fun merge_uniqs op< op== l = sort_uniq op< op== (List.concat l)

end

(*
 * Newer merge sort.  Stolen from a NJPLS meeting.
 *
 * -- Allen
 *)

signature SORTING =
sig

   val sort        : ('a * 'a -> bool) -> 'a list -> 'a list
   val uniq        : ('a * 'a -> bool) -> 'a list -> 'a list
   val sort_uniq   : ('a * 'a -> bool) -> 
                     ('a * 'a -> bool) -> 'a list -> 'a list

end 

structure Sorting : SORTING =
struct

   infix ==
 
   val SOME maxInt = Int.maxInt

   fun sort op< =
   let fun getRun [] = (0,[])
         | getRun (h::t) = 
           let fun loop(last,n,[])   = (n,[])
                 | loop(last,n,l as h::t) = 
                   if h < last then (n,l) else loop(h,n+1,t) 
           in  loop(h,1,t) end
       fun head([],_) = []
         | head(_,0)  = []
         | head(h::t,n) = h::head(t,n-1)
       fun merge(a,alen,b,blen) =
       let fun loop([],_,b,blen) = head(b,blen)
             | loop(_,0,b,blen)  = head(b,blen)
             | loop(a,alen,[],_) = head(a,alen)
             | loop(a,alen,_,0) = head(a,alen)
             | loop(a as ah::at,alen,b as bh::bt,blen) =
               if ah < bh then ah::loop(at,alen-1,b,blen)
               else bh::loop(a,alen,bt,blen-1)
       in  loop(a,alen,b,blen) end
       fun iter(sorted,slen,[],want) = (sorted,slen,[])
         | iter(sorted,slen,unsorted,want) = 
           if slen >= want then (sorted,slen,unsorted) 
           else
           let val (runlen,runtail) = getRun unsorted
               val (sorted',slen',unsorted) =
                  if runlen >= slen then
                      (unsorted,runlen,runtail)
                  else 
                      iter(unsorted,runlen,runtail,runlen)
           in  iter(merge(sorted,slen,sorted',slen'),
                    slen+slen',unsorted,want)
           end
       fun main list = 
       let val (sorted,_,_) = iter([],0,list,maxInt) 
       in  sorted end
   in  main end

   fun uniq op== =
   let fun f []                 = []
         | f (l as [x])         = l
         | f (x::(l as (y::z))) = if x == y then f l else x::f l
   in  f
   end

   fun sort_uniq op< op== l = uniq op== (sort op< l)

end

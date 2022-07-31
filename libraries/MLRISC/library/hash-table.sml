(*
 * Hash table.
 *
 * -- Allen
 *)
structure Hashtable :> HASHTABLE =
struct

   structure A = Array

   type ('a,'b) table = ('a -> word) * 
                        ('a * 'a -> bool) *
                        exn *
                        ('a * 'b) list A.array ref * 
                        int ref

   infix ==

   fun create{hash,==,exn,size} = (hash,op==,exn,ref(A.array(size,[])),ref 0)
   fun copy(hash,op==,exn,ref a,ref c) = 
         (hash,op==,exn,ref(A.tabulate(A.length a,fn i => A.sub(a,i))), ref c)
   fun size (_,_,_,_,ref n) = n
   fun clear (_,_,_,ref a,c) =
       let fun f ~1 = ()
             | f i  = (A.update(a,i,[]); f(i-1))
       in  f(A.length a - 1); c := 0 end
   fun insert (hash,op==,exn,A as ref a,c) (k,v) =
   let val N  = A.length a
       val h  = Word.toIntX(hash k) mod N
       val es = A.sub(a,h)
       fun ins ([],es') = (A.update(a,h,(k,v)::es'); 
                           c := !c + 1;
                           if !c >= N then grow(hash,A,N) else ()
                          )
         | ins ((e as (k',_))::es,es') = 
            if k == k' then A.update(a,h,(k,v)::es'@es) 
            else ins(es,e::es')
   in  ins (es,[])
   end

   and grow(hash,A as ref a,N) =
       let val M = N + N
           val M = if M < 13 then 13 else M
           val a' = A.array(M,[])
           fun ins (k,v) = let val h = Word.toIntX(hash k) mod M
                           in  A.update(a',h,(k,v)::A.sub(a',h)) end
       in  A.app (fn es => app ins es) a;
           A := a'
       end

   fun remove (hash,op==,exn,ref a,c) k =
   let val N  = A.length a
       val h  = Word.toIntX(hash k) mod N
       val es = A.sub(a,h)
       fun del ([],es') = ()
         | del ((e as (k',_))::es,es') = 
            if k == k' then (A.update(a,h,es'@es); c := !c - 1)
            else del(es,e::es')
   in  del (es,[])
   end
 
   fun lookup(hash,op==,exn,ref a,_) k =
   let val N  = A.length a
       val h  = Word.toIntX(hash k) mod N
       fun find [] = raise exn
         | find ((k',v)::es) = if k == k' then v else find es
   in  find(A.sub(a,h))
   end

   fun app f (_,_,_,ref A,_) = A.app (List.app f) A

   fun map f (_,_,_,ref A,_) =
   let fun fl([],x) = x
         | fl((k,v)::es,x) = f(k,v)::fl(es,x)
   in  A.foldr fl [] A end

   fun fold f x (_,_,_,ref A,_) = 
   let fun fl([],x) = x
         | fl((k,v)::es,x) = f(k,v,fl(es,x))
   in  A.foldr fl x A end

end


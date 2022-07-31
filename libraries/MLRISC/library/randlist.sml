(*
 * Random Access Lists  (due to Chris Okasaki)
 *
 * -- Allen
 *)

signature RANDOM_ACCESS_LIST =
sig

   type 'a rand_list

                 (* O(1) operations *)
   val empty  : 'a rand_list           
   val length : 'a rand_list -> int
   val null   : 'a rand_list -> bool
   val cons   : 'a * 'a rand_list -> 'a rand_list
   val hd     : 'a rand_list -> 'a
   val tl     : 'a rand_list -> 'a rand_list
  
                 (* O(log n) operations *)
   val sub       : 'a rand_list * int -> 'a
   val update    : 'a rand_list * int * 'a -> 'a rand_list
  
                 (* O(n) operations *)
   val fromList  : 'a list -> 'a rand_list
   val toList    : 'a rand_list -> 'a list

                 (* O(n) operations *)
   val map       : ('a -> 'b) -> 'a rand_list -> 'b rand_list
   val app       : ('a -> unit) -> 'a rand_list -> unit
   val foldl     : ('a * 'b -> 'b) -> 'b -> 'a rand_list -> 'b
   val foldr     : ('a * 'b -> 'b) -> 'b -> 'a rand_list -> 'b
end  

structure RandomAccessList :> RANDOM_ACCESS_LIST =
struct

   datatype 'a tree = LEAF of 'a | NODE of 'a tree * 'a * 'a tree

   type 'a rand_list = (int * 'a tree) list
    
   fun tree_sub (LEAF x,0,_) = x
     | tree_sub (LEAF _,_,_) = raise Subscript
     | tree_sub (NODE(_,x,_),0,_) = x
     | tree_sub (NODE(l,x,r),i,N) = 
       let val N' = N div 2
       in  if i <= N' then tree_sub(l,i-1,N')
                      else tree_sub(r,i-1-N',N')
       end

   fun tree_update (LEAF _,0,x,_) = LEAF x
     | tree_update (LEAF _,_,_,_) = raise Subscript
     | tree_update (NODE(l,_,r),0,x,_) = NODE(l,x,r)
     | tree_update (NODE(l,y,r),i,x,N) = 
       let val N' = N div 2
       in  if i <= N' then NODE(tree_update(l,i-1,x,N'),y,r)
                      else NODE(l,y,tree_update(r,i-1-N',x,N'))
       end

   val empty = []

   fun null [] = true | null _ = false

   fun length rl =
   let fun f([],n) = n
         | f((m,_)::l,n) = f(l,m+n)
   in  f(rl,0)
   end

   fun cons (x, rl as ((m,t)::(n,u)::l)) = 
        if m = n then (m+n+1,NODE(t,x,u))::l
                 else (1,LEAF x)::rl
     | cons (x, rl) = (1,LEAF x)::rl

   fun hd ((_,LEAF x)::_) = x
     | hd ((_,NODE(_,x,_))::_) = x
     | hd [] = raise Empty

   fun tl ((_,LEAF x)::rl) = rl
     | tl ((n,NODE(l,x,r))::rl) = 
       let val n' = n div 2
       in  (n',l)::(n',r)::rl
       end
     | tl [] = raise Empty
         
   fun sub([],_)        = raise Subscript
     | sub((n,t)::rl,i) = if i < n then tree_sub(t,i,n)
                          else sub(rl,i-n)

   fun update([],_,_)   = raise Subscript
     | update((p as (n,t))::rl,i,x) =
         if i < n then (n,tree_update(t,i,x,n))::rl
         else p::update(rl,i-n,x)

   fun map f rl = 
   let fun g (LEAF x)      = LEAF(f x)
         | g (NODE(l,x,r)) = NODE(g l,f x,g r)
   in  List.map (fn (n,t) => (n,g t)) rl
   end

   fun app f rl =
   let fun g (LEAF x)      = f x
         | g (NODE(l,x,r)) = (f x; g l; g r)
   in  List.app (fn (_,t) => g t) rl
   end

   fun foldl f u rl =
   let fun g (LEAF x,u)      = f(x,u)
         | g (NODE(l,x,r),u) = g(r,g(l,f(x,u)))
   in  List.foldl (fn ((_,t),x) => g(t,x)) u rl
   end

   fun foldr f u rl =
   let fun g (LEAF x,u)      = f(x,u)
         | g (NODE(l,x,r),u) = f(x,g(l,g(r,u)))
   in  List.foldr (fn ((_,t),x) => g(t,x)) u rl
   end

   fun fromList l = List.foldr cons empty l
   fun toList rl  = foldr op:: [] rl
end


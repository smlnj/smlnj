(*
 * A fixed capacity integer set datatype
 *
 * -- Allen
 *)

signature INTSET =
sig

   type intset

   val intset   : int -> intset
   val contains : intset * int -> bool
   val add      : intset * int -> unit
   val remove   : intset * int -> unit
   val clear    : intset -> unit
   val size     : intset -> int
   val capacity : intset -> int
   val is_empty : intset -> bool
   val app      : (int -> unit) -> intset -> unit
   val fold     : (int * 'a -> 'a) -> 'a -> intset -> 'a
   val toList   : intset -> int list
   val toString : intset -> string
   val copy     : intset -> intset
   val +        : intset * intset -> intset
   val -        : intset * intset -> intset
   val *        : intset * intset -> intset
   val union    : intset * intset -> unit
   val diff     : intset * intset -> unit

end

structure IntSet :> INTSET =
struct

   structure A = Array
   datatype intset = SET of {stack : int A.array,
                             pos   : int A.array,
                             count : int ref
                            }
   fun intset n = SET{stack=A.array(n,0),pos=A.array(n,0),count=ref 0}
   fun contains(SET{stack,pos,count},i) =
       let val j = A.sub(pos,i)
       in  j < !count andalso A.sub(stack,j) = i end

   fun add(SET{stack,pos,count},i) =
       let val j = A.sub(pos,i)
           val n = !count
       in  if j < n andalso A.sub(stack,j) = i then ()
           else (A.update(stack,n,i); A.update(pos,i,n); count := n + 1)
       end
   fun remove(SET{stack,pos,count},i) =
       let val j = A.sub(pos,i)
           val n = !count
           val k = A.sub(stack,j) 
       in  if j < n andalso i = k then 
             let val k' = A.sub(stack,n-1)
             in  A.update(stack,j,k');
                 A.update(pos,k',j);
                 count := n - 1
             end 
           else ()
       end

   fun clear(SET{count,...}) = count := 0
   fun size(SET{count,...}) = !count
   fun capacity(SET{stack,...}) = A.length stack
   fun is_empty(SET{count,...}) = !count = 0
   fun app f (SET{count,stack,...}) =
       let fun g ~1 = ()
             | g i  = (f(A.sub(stack,i)); g(i-1))
       in  g(!count - 1) end
   fun fold f x (SET{count,stack,...}) =
       let fun g(~1,x) = x
             | g(i,x)  = g(i-1,f(A.sub(stack,i),x))
       in  g(!count - 1,x) end
   fun toList set = fold op:: [] set 
   fun toString set = 
        String.concat(
            "{"::fold (fn (i,[x]) => [Int.toString i,x]
                        | (i,s)   => Int.toString i::","::s) ["}"] set)

   fun copy (SET{stack,pos,count}) = 
       let val N      = A.length stack
           val stack' = A.array(N,0)
           val pos'   = A.array(N,0)
           val n      = !count
           fun f(i,x) = (A.update(stack',i,x);A.update(pos',x,i))
       in  A.appi f (stack,0,SOME n);
           SET{stack=stack',
               pos  =pos',
               count=ref n
              }
       end

   fun union(s1,s2) = app (fn x => add(s2,x)) s1
   fun diff(s1,s2)  = app (fn x => remove(s2,x)) s1
   fun s1 + s2 = let val s3 = copy s1
                 in  union(s2,s3); s3 end
   fun s1 - s2 = let val s3 = copy s1
                 in  diff(s2,s3); s3 end
   fun s1 * s2 = let val s3 = intset(capacity s1)
                 in  app (fn x => if contains(s2,x) then add(s3,x) else ())
                         s1;
                     s3
                 end
       


end 


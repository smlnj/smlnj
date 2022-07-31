(*
 * map datatype that uses hashing.
 *
 * -- allen
 *)

structure HashMap :> HASH_MAP =
struct

   structure A = Array

   datatype 'a tree = NODE of 'a * 'a tree * 'a tree | EMPTY 

   datatype ('a,'b) map = 
      MAP of
      { table : ('a * 'b) tree Array.array ref,
        size  : int ref,
        order : 'a * 'a -> order,
        hash  : 'a -> int,
        exn   : exn
      }

   fun create { order, hash, exn } N =
   let val N = if N <= 10 then 10 else N
   in
      MAP { table    = ref(Array.array(N,EMPTY)),
            size     = ref 0,
            order    = order,
            hash     = hash,
            exn      = exn
          }
   end

   fun size (MAP { size, ... }) = !size

   fun bucketSize (MAP { table, ... }) = Array.length (!table)

   fun isEmpty (MAP { size, ... }) = !size = 0

   fun clear (MAP { size, table, ... }) = 
       (table := A.array(A.length(!table),EMPTY); size := 0)

   and insert (m as MAP { size, table = ref T, order, hash, exn,...})
              (e as (x,y)) = 
   let val pos = hash x mod A.length T
       fun ins EMPTY = (size := !size + 1; NODE(e,EMPTY,EMPTY))
         | ins (NODE(e' as (x',y'),l,r)) =
           case order(x,x') of
              LESS    => NODE(e',ins l,r)
           |  EQUAL   => NODE(e,l,r)
           |  GREATER => NODE(e',l,ins r)
   in  A.update(T,pos,ins(A.sub(T,pos)));
       if !size > 6 * A.length T then
          grow m
       else ()
   end

   and grow (MAP { size, table = table as ref T, order, hash, exn, ... }) =
   let val m2 as 
           MAP{table = ref T',...} = create{ order=order, hash=hash, exn=exn } 
                   (!size * 2 + 10) (* : ('a,'b) map  *)
       val ins = insert m2 
       fun loop EMPTY = ()
         | loop (NODE(e,l,r)) = (ins e; loop l; loop r)
   in  A.app loop T; table := T'
   end

   and update (m as MAP { size, table = ref T, order, hash, exn,...})
              (e as (x,y), f) = 
   let val pos = hash x mod A.length T
       fun ins EMPTY = (size := !size + 1; NODE(e,EMPTY,EMPTY))
         | ins (NODE(e' as (x',y'),l,r)) =
           case order(x,x') of
              LESS    => NODE(e',ins l,r)
           |  EQUAL   => NODE((x',f y'),l,r)
           |  GREATER => NODE(e',l,ins r)
   in  A.update(T,pos,ins(A.sub(T,pos)));
       if !size > 6 * A.length T then
          grow m
       else ()
   end

   fun remove (MAP { size, table = ref T, order, hash, exn,...}) x =
   let val pos = hash x mod A.length T
       fun del EMPTY = EMPTY
         | del (NODE(e' as (x',_),l,r)) =
           case order(x,x') of
              LESS    => NODE(e',del l,r)
           |  EQUAL   => (size := !size - 1;
                         case (l,r) of
                            (EMPTY,r) => r
                         |  (l,EMPTY) => l
                         |  _         => let val (leftmost,r') = delLeftMost r
                                         in  NODE(leftmost,l,r')
                                         end
                         )
           |  GREATER => NODE(e',l,del r)
       and delLeftMost EMPTY = raise exn
         | delLeftMost (NODE(e,EMPTY,r)) = (e,r)
         | delLeftMost (NODE(e,l,r)) = 
           let val (e',r') = delLeftMost r
           in  (e',NODE(e,l,r'))
           end
 
   in  A.update(T,pos,del(A.sub(T,pos)))
   end

   fun lookup (MAP { table = ref T, order, hash, exn, ... }) x =
   let val pos = hash x mod A.length T
       fun look EMPTY = raise exn
         | look (NODE(e' as (x',y'),l,r)) =
            case order(x,x') of
               LESS    => look l
            |  EQUAL   => y'
            |  GREATER => look r
   in  look (A.sub(T,pos))
   end

   fun lookupOrElse m default x = lookup m x handle _ => default

   fun contains (MAP { table = ref T, order, hash, ... }) x =
   let val pos = hash x mod A.length T
       fun find EMPTY = false
         | find (NODE(e' as (x',y'),l,r)) =
            case order(x,x') of
               LESS    => find l
            |  EQUAL   => true
            |  GREATER => find r
   in  find(A.sub(T,pos))
   end

   fun fold f x =
      fn (MAP { table = ref T, ... }) =>
      let fun collect (EMPTY,L)           = L
            | collect (NODE(e,l,r),L) = collect(l,collect(r,f(e,L)))
      in  A.foldl (fn (t,l) => collect(t,l)) x T
      end

   fun app f = 
      fn (MAP { table = ref T, ... }) =>
      let fun appTree EMPTY         = ()
            | appTree (NODE(e,l,r)) = (f e; appTree l; appTree r)
      in  A.app appTree T
      end

   fun toList map = fold (op::) [] map

   fun toString (f,g) map =
      "{" ^ fold (fn ((x,y),"") => "(" ^ f x ^ ", " ^ g y ^ ")"
                   | ((x,y),l)  => "(" ^ f x ^ ", " ^ g y ^ "), " ^ l
                 ) "" map ^ "}"  

end


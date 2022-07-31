(*
 * A set datatype that uses hashing
 *
 * -- Allen
 *)

structure HashSet :> HASH_SET =
struct

   structure A = Array

   datatype 'a set = 
      SET of
      { table : 'a list Array.array ref,
        size  : int ref,
        order : 'a * 'a -> order,
        hash  : 'a -> int
      }

   fun create { order, hash } N =
   let val N = if N <= 10 then 10 else N
   in
      SET { table    = ref(Array.array(N,[])),
            size     = ref 0,
            order    = order,
            hash     = hash
          }
   end

   fun size (SET { size, ... }) = !size

   fun bucketSize (SET { table, ... }) = Array.length (!table)

   fun isEmpty (SET { size, ... }) = !size = 0

   fun clear (SET { size, table, ... }) = 
       (table := A.array(A.length(!table),[]); size := 0)

   and insert (m as SET { size, table = ref T, order, hash,...}) x =
   let val pos = hash x mod A.length T
       val list = A.sub(T,pos)
       fun ins [] = (size := !size + 1; 
                     A.update(T,pos,x::list);
                     if !size > 6 * A.length T then grow m else ())
         | ins (x'::rest) =
           case order(x,x') of
              EQUAL => ()
           |  _     => ins rest
   in  
       ins list
   end

   and grow (SET { size, table = table as ref T, order, hash, ... }) =
   let val m2 as 
           SET{table = ref T',...} = create{ order=order, hash=hash }
                   (!size * 2 + 10)
   in  A.app (app (insert m2)) T; table := T'
   end

   fun remove (SET { size, table = ref T, order, hash,...}) x =
   let val pos = hash x mod A.length T
       val list = A.sub(T,pos)
       fun del ([],list) = ()
         | del (x'::rest,list) =
           case order(x,x') of
              EQUAL   => (size := !size - 1;
                          A.update(T,pos,rest@list)
                         )
           |  _       => del (rest,x'::list)
 
   in  del(list,[])
   end

   fun contains (SET { table = ref T, order, hash, ... }) x =
   let val pos = hash x mod A.length T
       fun find [] = false
         | find (x'::rest) =
            case order(x,x') of
               EQUAL => true
            |  _     => find rest
   in  find(A.sub(T,pos))
   end

   fun fold f x =
      fn (SET { table = ref T, ... }) =>
          A.foldl (fn (t,l) => List.foldl f l t) x T

   fun app f = 
      fn (SET { table = ref T, ... }) =>
          A.app (List.app f) T

   fun toList set = fold (op::) [] set

   fun toString f set =
      "{" ^ fold (fn (x,"") => f x
                   | (x,l)  => f x ^ ", " ^ l
                 ) "" set ^ "}"  

end


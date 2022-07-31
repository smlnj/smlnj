(*
 * This module implements Tarjan's fast path computation algorithm.
 *
 * -- Allen
 *)

functor TarjanFastPath(Dom : DOMINATOR_TREE) : TARJAN_FAST_PATH =
struct

   structure Dom = Dom
   structure G   = Graph
   structure A   = Array

   datatype 'e pexp = 
      NULLSET
   |  LAMBDA 
   |  EDGE of 'e Graph.edge
   |  || of 'e pexp * 'e pexp 
   |  ++ of 'e pexp * 'e pexp

   type 'e pseq = ('e pexp * int * int) list

   infix || ++

   fun simp (NUL || x) = x
     | simp (x || NUL) = x
     | simp (NUL ++ x) = NUL
     | simp (x ++ NUL) = NUL
     | simp (EMP ++ x) = x
     | simp x = x

   fun solve P s = 

   fun decompose_and_sequence (G as G.GRAPH G,Dom as G.GRAPH dom) =
   let val N        = #capacity dom ()
       val ancestor = A.array(N,~1)
       val derived  = A.array(N,~1)
       val S        = A.array(N,LAMBDA)
       val R        = A.array(N,NULLSET)
       fun eliminate (G.GRAPH G) =

       fun walk(u,lvl,sequence) =
   in
   end

end

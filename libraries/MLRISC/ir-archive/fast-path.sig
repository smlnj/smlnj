(*
 * This module implements Tarjan's fast path computation algorithm.
 *
 * -- Allen
 *)

signature TARJAN_FAST_PATH =
sig

   structure Dom : DOMINATOR_TREE

   (* path expression *)
   datatype 'e pexp =  
      NUL 
   |  EMP  
   |  EDGE of 'e Graph.edge
   |  || of 'e pexp * 'e pexp
   |  ++ of 'e pexp * 'e pexp

   (* path sequence *)
   type 'e pseq = ('e pexp * Graph.node_id * Graph.node_id) list 

   (* Given a path sequence and a source node $s$, solve for the path of $s$ *)
   val solve : 'e pseq -> Graph.node_id -> 'e pexp

   (* decompose a graph into a path sequence *)
   val eliminate : ('n,'e,'g) Graph.graph -> 'e pseq

end


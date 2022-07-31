(*
 *  This module implenents max cardinality matching.  
 *  Each edge of the matching are folded together with a user supplied
 *  function.
 *
 * -- Allen
 *) 

signature BIPARTITE_MATCHING =
sig

   val matching : ('n,'e,'g) Graph.graph -> 
                  ('e Graph.edge * 'a -> 'a) -> 'a -> 'a * int

end

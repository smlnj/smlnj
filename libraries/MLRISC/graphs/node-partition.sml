(*
 * This implenments node partitions (i.e. a union-find data structure)
 * on nodes.
 *
 * -- Allen
 *)

signature NODE_PARTITION =
sig

   type 'n node_partition 

   val node_partition : ('n,'e,'g) Graph.graph -> 'n node_partition
   val !!    : 'n node_partition -> Graph.node_id -> 'n Graph.node
   val ==    : 'n node_partition -> Graph.node_id * Graph.node_id -> bool
   val union : 'n node_partition -> ('n Graph.node * 'n Graph.node ->
                                        'n Graph.node) ->
                                        Graph.node_id * Graph.node_id -> bool
   val union': 'n node_partition -> Graph.node_id * Graph.node_id -> bool

end

structure NodePartition :> NODE_PARTITION =
struct

   structure U = URef
   structure H = HashTable
   structure G = Graph

   type 'n node_partition = (G.node_id,'n G.node U.uref) H.hash_table

   fun node_partition (G.GRAPH G) =
   let val P   = H.mkTable (Word.fromInt,op =) (#order G () * 2,G.NotFound)
       val ins = H.insert P
       val _   = #forall_nodes G (fn n as (i,_) => ins(i,U.uRef n))
   in  P
   end

   fun !! P x          = U.!! (H.lookup P x)
   fun == P (x,y)      = U.equal (H.lookup P x, H.lookup P y)
   fun union P f (x,y) = U.unify f (H.lookup P x, H.lookup P y)
   fun union' P (x,y)  = U.union (H.lookup P x, H.lookup P y)
end


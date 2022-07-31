(*
 * This implements a priority queue for nodes in a graph
 * 
 * -- Allen
 *)

signature NODE_PRIORITY_QUEUE =
sig

   type node_priority_queue

   exception EmptyPriorityQueue

   val create         : int -> (Graph.node_id * Graph.node_id -> bool) -> 
                           node_priority_queue 
   val fromGraph      : (Graph.node_id * Graph.node_id -> bool) -> 
                          ('n,'e,'g) Graph.graph -> node_priority_queue
   val isEmpty        : node_priority_queue -> bool
   val clear          : node_priority_queue -> unit
   val min            : node_priority_queue -> Graph.node_id 
   val deleteMin      : node_priority_queue -> Graph.node_id
   val decreaseWeight : node_priority_queue * Graph.node_id -> unit
   val insert         : node_priority_queue * Graph.node_id -> unit
end

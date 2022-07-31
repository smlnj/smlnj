(*
 *  A generic directed graph data structure.  
 *  Implemented in an ``object oriented style''
 *  All graphs are based on this interface.
 * 
 *  -- Allen
 *)

structure Graph : GRAPH =
struct

   exception Graph of string
   exception Subgraph
   exception NotFound
   exception Unimplemented
   exception Readonly     
   exception NotSingleEntry
   exception NotSingleExit

   fun unimplemented _ = raise Unimplemented

   type node_id = int 
   type 'n node = node_id * 'n
   type 'e edge = node_id * node_id * 'e

   datatype ('n,'e,'g) graph = GRAPH of ('n,'e,'g) graph_methods
   withtype ('n,'e,'g) graph_methods =
       {  name            : string,
          graph_info      : 'g,

          (* inserting/removing nodes and edges *)
          new_id          : unit -> node_id,
          add_node        : 'n node -> unit,
          add_edge        : 'e edge -> unit, 
          remove_node     : node_id -> unit,
          set_out_edges   : node_id * 'e edge list -> unit,
          set_in_edges    : node_id * 'e edge list -> unit,
          set_entries     : node_id list -> unit,
          set_exits       : node_id list -> unit,

          garbage_collect : unit -> unit,

          nodes           : unit -> 'n node list,
          edges           : unit -> 'e edge list,
          order           : unit -> int,
          size            : unit -> int,
          capacity        : unit -> int,
          succ            : node_id -> node_id list,
          pred            : node_id -> node_id list,
          out_edges       : node_id -> 'e edge list,
          in_edges        : node_id -> 'e edge list,
          has_edge        : node_id * node_id -> bool,
          has_node        : node_id -> bool,
          node_info       : node_id -> 'n,
          entries         : unit -> node_id list,
          exits           : unit -> node_id list,
          entry_edges     : node_id -> 'e edge list,
          exit_edges      : node_id -> 'e edge list,

          forall_nodes    : ('n node -> unit) -> unit,
          forall_edges    : ('e edge -> unit) -> unit
       }

   fun remove_all_edges (GRAPH G) (i,j) =
        #set_out_edges G (i,List.filter (fn (_,k,_) => k = j) (#out_edges G i))
   fun remove_all_edges' (GRAPH G) (i,j,p) =
        #set_out_edges G (i,List.filter (fn (_,k,e) => k = j andalso p e) 
                           (#out_edges G i))
   fun remove_edge (GRAPH G) (i,j) =
       let fun filter [] = []
             | filter((e as (_,k,_))::es) = 
               if j = k then es else e::filter es
       in  #set_out_edges G (i,filter(#out_edges G i)) end
   fun remove_edge' (GRAPH G) (i,j,p) =
       let fun filter [] = []
             | filter((e as (_,k,e'))::es) = 
               if j = k andalso p e' then es else e::filter es
       in  #set_out_edges G (i,filter(#out_edges G i)) end

end


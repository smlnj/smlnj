(*
 * This is the signature of a bipartite graph
 *
 * -- Allen
 *)

signature BIPARTITE_GRAPH =
sig

   include GRAPH

   datatype ('m,'n,'e,'g) bigraph = BIGRAPH of ('m,'n,'e,'g) bigraph_methods
   withtype ('m,'n,'e,'g) bigraph_methods =
       {  name        : string,
          graph_info  : 'g,

          (* inserting/removing nodes and edges *)
          new_src     : 'm -> 'm node,
          new_dst     : 'n -> 'n node,
          add_src     : 'm node -> unit,
          add_dst     : 'n node -> unit,
          add_edge    : 'e edge -> unit, 
          remove_src  : node_id -> unit,
          remove_dst  : node_id -> unit,
          remove_edge : 'e edge -> unit,

          (* collect deleted node ids *)
          garbage_collect : unit -> unit,

          (* selectors *)
          src_nodes  : unit -> 'm node list,
          dst_nodes  : unit -> 'n node list,
          edges      : unit -> 'e edge list,
          src_order  : unit -> int,
          dst_order  : unit -> int,
          size       : unit -> int,
          capacity   : unit -> int,
          succ       : node_id -> node_id list,
          pred       : node_id -> node_id list,
          out_edges  : node_id -> 'e edge list,
          in_edges   : node_id -> 'e edge list,
          has_edge   : node_id * node_id -> bool,
          has_src    : node_id -> bool,
          has_dst    : node_id -> bool,
          src_node   : node_id -> 'm,
          dst_node   : node_id -> 'n,

          (* iterators *)
          forall_src   : ('m node -> unit) -> unit,
          forall_dst   : ('n node -> unit) -> unit,
          forall_edges : ('e edge -> unit) -> unit
       }
end


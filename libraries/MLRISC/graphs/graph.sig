(* graph.sig
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 *  A generic directed graph data structure.  
 *  Implemented in an ``object oriented style''
 *  All graphs are based on this interface.
 * 
 *  -- Allen
 *)

signature GRAPH =
sig

   exception Graph of string (* Bug *)
   exception Subgraph        (* subgraph constraint failure *)
   exception NotFound        (* element not located *)
   exception Unimplemented   (* method is not implemented *)
   exception Readonly        (* modification fails *)
   exception NotSingleEntry  (* should be single entry *)
   exception NotSingleExit   (* should be single exit *)


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

          (* collect deleted node ids *)
          garbage_collect : unit -> unit,

          (* selectors *)
          nodes           : unit -> 'n node list,
          edges           : unit -> 'e edge list,
          order           : unit -> int,	(* # nodes *)
          size            : unit -> int,	(* # edges *)
          capacity        : unit -> int,	(* max. node_id < capacity *)
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

          (* iterators *)
          forall_nodes    : ('n node -> unit) -> unit,
          forall_edges    : ('e edge -> unit) -> unit
       }
   val unimplemented : 'a -> 'b

         (* remove one edge i->j from graph *)
   val remove_edge : ('n,'e,'g) graph -> node_id * node_id -> unit

   val remove_edge' : ('n,'e,'g) graph -> node_id * node_id * 
                                            ('e -> bool) -> unit

         (* remove all edges i->j from graph *)
   val remove_all_edges : ('n,'e,'g) graph -> node_id * node_id -> unit

   val remove_all_edges' : ('n,'e,'g) graph -> 
                node_id * node_id * ('e -> bool) -> unit
end


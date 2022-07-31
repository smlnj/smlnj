(*
 * These graph wrappers allow the client to attach triggers to 
 * various graph methods.
 *
 * -- Allen
 *)

signature GRAPH_WRAPPERS =
sig

   val do_before_new_id : 
       (unit -> unit) -> ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_after_new_id : 
       (Graph.node_id -> unit) -> 
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_before_add_node : 
       ('n Graph.node -> unit) -> 
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_after_add_node : 
       ('n Graph.node -> unit) -> 
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_before_add_edge : 
       ('e Graph.edge -> unit) -> 
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_after_add_edge : 
       ('e Graph.edge -> unit) -> 
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_before_remove_node : 
       (Graph.node_id -> unit) -> 
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_after_remove_node : 
       (Graph.node_id -> unit) -> 
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_before_set_in_edges : 
       (Graph.node_id * 'e Graph.edge list -> unit) -> 
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_after_set_in_edges : 
       (Graph.node_id * 'e Graph.edge list -> unit) -> 
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_before_set_out_edges : 
       (Graph.node_id * 'e Graph.edge list -> unit) -> 
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_after_set_out_edges : 
       (Graph.node_id * 'e Graph.edge list -> unit) -> 
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_before_set_entries : 
       (Graph.node_id list -> unit) ->
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_after_set_entries : 
       (Graph.node_id list -> unit) ->
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_before_set_exits : 
       (Graph.node_id list -> unit) ->
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_after_set_exits : 
       (Graph.node_id list -> unit) ->
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_before_changed : (('n,'e,'g) Graph.graph -> unit) ->
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
   val do_after_changed : (('n,'e,'g) Graph.graph -> unit) ->
	   ('n,'e,'g) Graph.graph -> ('n,'e,'g) Graph.graph
end

structure GraphWrappers : GRAPH_WRAPPERS =
struct

   structure G = Graph

   fun do_before_new_id f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = fn () => (f(); #new_id G ()),
         add_node        = #add_node G,
         add_edge        = #add_edge G,
         remove_node     = #remove_node G,
         set_in_edges    = #set_in_edges G,
         set_out_edges   = #set_out_edges G,
         set_entries     = #set_entries G,
         set_exits       = #set_exits G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_after_new_id f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = fn () => let val x = #new_id G () in f x; x end,
         add_node        = #add_node G,
         add_edge        = #add_edge G,
         remove_node     = #remove_node G,
         set_in_edges    = #set_in_edges G,
         set_out_edges   = #set_out_edges G,
         set_entries     = #set_entries G,
         set_exits       = #set_exits G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_before_add_node f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = fn n => (f n; #add_node G n),
         add_edge        = #add_edge G,
         remove_node     = #remove_node G,
         set_in_edges    = #set_in_edges G,
         set_out_edges   = #set_out_edges G,
         set_entries     = #set_entries G,
         set_exits       = #set_exits G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_after_add_node f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = fn n => (#add_node G n; f n),
         add_edge        = #add_edge G,
         remove_node     = #remove_node G,
         set_in_edges    = #set_in_edges G,
         set_out_edges   = #set_out_edges G,
         set_entries     = #set_entries G,
         set_exits       = #set_exits G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_before_add_edge f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = #add_node G,
         add_edge        = fn e => (f e; #add_edge G e),
         remove_node     = #remove_node G,
         set_in_edges    = #set_in_edges G,
         set_out_edges   = #set_out_edges G,
         set_entries     = #set_entries G,
         set_exits       = #set_exits G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_after_add_edge f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = #add_node G,
         add_edge        = fn e => (#add_edge G e; f e),
         remove_node     = #remove_node G,
         set_in_edges    = #set_in_edges G,
         set_out_edges   = #set_out_edges G,
         set_entries     = #set_entries G,
         set_exits       = #set_exits G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_before_remove_node f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = #add_node G,
         add_edge        = #add_edge G,
         remove_node     = fn n => (f n; #remove_node G n),
         set_in_edges    = #set_in_edges G,
         set_out_edges   = #set_out_edges G,
         set_entries     = #set_entries G,
         set_exits       = #set_exits G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_after_remove_node f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = #add_node G,
         add_edge        = #add_edge G,
         remove_node     = fn n => (#remove_node G n; f n),
         set_in_edges    = #set_in_edges G,
         set_out_edges   = #set_out_edges G,
         set_entries     = #set_entries G,
         set_exits       = #set_exits G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_before_set_in_edges f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = #add_node G,
         add_edge        = #add_edge G,
         remove_node     = #remove_node G,
         set_in_edges    = fn e => (f e; #set_in_edges G e),
         set_out_edges   = #set_out_edges G,
         set_entries     = #set_entries G,
         set_exits       = #set_exits G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_after_set_in_edges f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = #add_node G,
         add_edge        = #add_edge G,
         remove_node     = #remove_node G,
         set_in_edges    = fn e => (#set_in_edges G e; f e),
         set_out_edges   = #set_out_edges G,
         set_entries     = #set_entries G,
         set_exits       = #set_exits G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_before_set_out_edges f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = #add_node G,
         add_edge        = #add_edge G,
         remove_node     = #remove_node G,
         set_out_edges   = fn e => (f e; #set_out_edges G e),
         set_in_edges    = #set_in_edges G,
         set_entries     = #set_entries G,
         set_exits       = #set_exits G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_after_set_out_edges f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = #add_node G,
         add_edge        = #add_edge G,
         remove_node     = #remove_node G,
         set_out_edges   = fn e => (#set_out_edges G e; f e),
         set_in_edges    = #set_in_edges G,
         set_entries     = #set_entries G,
         set_exits       = #set_exits G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_before_set_entries f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = #add_node G,
         add_edge        = #add_edge G,
         remove_node     = #remove_node G,
         set_in_edges    = #set_in_edges G,
         set_out_edges   = #set_out_edges G,
         set_entries     = fn ns => (f ns; #set_entries G ns),
         set_exits       = #set_exits G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_after_set_entries f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = #add_node G,
         add_edge        = #add_edge G,
         remove_node     = #remove_node G,
         set_in_edges    = #set_in_edges G,
         set_out_edges   = #set_out_edges G,
         set_entries     = fn ns => (#set_entries G ns; f ns),
         set_exits       = #set_exits G,
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_before_set_exits f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = #add_node G,
         add_edge        = #add_edge G,
         remove_node     = #remove_node G,
         set_in_edges    = #set_in_edges G,
         set_out_edges   = #set_out_edges G,
         set_entries     = #set_entries G,
         set_exits       = fn ns => (f ns; #set_exits G ns),
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_after_set_exits f (G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = #new_id G,
         add_node        = #add_node G,
         add_edge        = #add_edge G,
         remove_node     = #remove_node G,
         set_in_edges    = #set_in_edges G,
         set_out_edges   = #set_out_edges G,
         set_entries     = #set_entries G,
         set_exits       = fn ns => (#set_exits G ns; f ns),
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_before_changed f (G' as G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = (fn x => (f G'; #new_id G x)),
         add_node        = (fn x => (f G'; #add_node G x)),
         add_edge        = (fn x => (f G'; #add_edge G x)),
         remove_node     = (fn x => (f G'; #remove_node G x)),
         set_in_edges    = (fn x => (f G'; #set_in_edges G x)),
         set_out_edges   = (fn x => (f G'; #set_out_edges G x)),
         set_entries     = (fn x => (f G'; #set_entries G x)),
         set_exits       = (fn x => (f G'; #set_exits G x)),
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

   fun do_after_changed f (G' as G.GRAPH G) =
       G.GRAPH
       { name            = #name G,
         graph_info      = #graph_info G,
         new_id          = (fn x => (#new_id G x before f G')),
         add_node        = (fn x => (#add_node G x before f G')),
         add_edge        = (fn x => (#add_edge G x before f G')),
         remove_node     = (fn x => (#remove_node G x before f G')),
         set_out_edges   = (fn x => (#set_out_edges G x before f G')),
         set_in_edges    = (fn x => (#set_in_edges G x before f G')),
         set_entries     = (fn x => (#set_entries G x before f G')),
         set_exits       = (fn x => (#set_exits G x before f G')),
         garbage_collect = #garbage_collect G,
         nodes           = #nodes G,
         edges           = #edges G,
         order           = #order G,
         size            = #size G,
         capacity        = #capacity G,
         out_edges       = #out_edges G,
         in_edges        = #in_edges G,
         succ            = #succ G,
         pred            = #pred G,
         has_edge        = #has_edge G,
         has_node        = #has_node G,
         node_info       = #node_info G,
         entries         = #entries G,
         exits           = #exits G,
         entry_edges     = #entry_edges G,
         exit_edges      = #exit_edges G,
         forall_nodes    = #forall_nodes G,
         forall_edges    = #forall_edges G
       }

end


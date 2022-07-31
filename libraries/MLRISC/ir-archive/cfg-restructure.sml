(*
 * This module inserts preheaders and other stuff.
 * This is probably no longer used.
 *
 * -- Allen
 *)

functor ControlFlowGraphRestructure
    (structure Loop : LOOP_STRUCTURE) : CONTROL_FLOW_GRAPH_RESTRUCTURE =
struct
   structure Loop = Loop
   structure G    = Graph

   fun restructure (G.GRAPH cfg,G.GRAPH loop) 
          { add_preheader, 
            add_landing_pad
          } =
   let val add_node = #add_node cfg
       fun preheader f =
           fn {header,backedges} =>
              let val in_edges = #in_edges cfg header
                  fun g([],entries) = entries
                    | g((e as (i,j,_))::es,entries) = 
                       if List.exists (fn (i',j',_) => i=i' andalso j=j') 
                            backedges then g(es,entries)
                                      else g(es,e::entries)
              in  f{header =(header,#node_info cfg header),
                    entries=g(in_edges,[])
                   }
              end

       fun landing_pads f = fn {exits} => app (fn e => f {exit=e}) exits

       fun nop _ = ()
       val insert_preheader    = case add_preheader of
                                   SOME f => preheader f
                                 | NONE   => nop
       val insert_landing_pads = case add_landing_pad of
                                   SOME f => landing_pads f
                                 | NONE   => nop
       fun process_loop(i,Loop.LOOP{header,backedges=[],exits,...}) = ()
         | process_loop(i,Loop.LOOP{header,backedges,exits,...}) =
          (insert_preheader{header=header,backedges=backedges};
           insert_landing_pads{exits=exits}
          )
   in 
       #forall_nodes loop process_loop
   end

end


(*
 * Module for adding preheaders and splitting critical edges
 *
 * -- Allen
 *)

functor CFGStructuring(IR : MLRISC_IR) : CFG_STRUCTURING =
struct

   structure IR   = IR
   structure CFG  = IR.CFG
   structure Loop = IR.Loop
   structure Util = IR.Util
   structure W    = CFG.W
   structure G    = Graph

   structure Reshape = ControlFlowGraphRestructureFn(structure Loop = Loop)

   fun reshape { add_preheader, split_critical_edges } IR =
   let  val CFG as G.GRAPH cfg = IR
        val loop               = IR.loop IR

        fun is_falls_thru(_,_,CFG.EDGE{k=CFG.BRANCH false,...}) = true
          | is_falls_thru(_,_,CFG.EDGE{k=CFG.FALLSTHRU,...}) = true
          | is_falls_thru(_,_,_) = false

        fun ins_preheader{header,entries=[]} = ()
          | ins_preheader{header,entries=[_]} = ()
          | ins_preheader{header=(h,h'),entries} =
            let fun sum_weights([],n) = n
                  | sum_weights((_,_,CFG.EDGE{w,a,...})::es,n) = 
                      sum_weights(es,!w + n)
                val w = sum_weights(entries,W.zero)
                val CFG.BLOCK{annotations=old_an,...} = h'
                val p = #new_id cfg ()
                val (preheader as CFG.BLOCK{freq,annotations,...}, new_edge) = 
                   if List.exists is_falls_thru entries then
                     (CFG.empty_block(p,w), 
                     (p,h,CFG.EDGE{k=CFG.FALLSTHRU,w=ref w,a=a}))
                   else
                     (CFG.jump_block(p,CFG.define_label h',w),
                     (p,h,CFG.EDGE{k=CFG.JUMP,w=ref w,a=a}))
                val new_entries = map (fn (i,j,e) => (i,p,e)) entries
            in  annotations := !old_an; 
                app (fn (i,j,_) => #remove_edge cfg (i,j)) entries;
                app (#add_edge cfg) new_entries;
                #add_node cfg (p,preheader);
                #add_edge cfg new_edge;
                app (fn (i,_,_) => Util.set_target(CFG,i)) new_entries
            end

    in  Reshape.restructure (CFG,loop)
             { add_preheader   = if add_preheader then SOME ins_preheader 
                                 else NONE,
               add_landing_pad = NONE
             };
        CFG.changed CFG
    end

end
    

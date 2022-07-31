(*
 * This module inserts preheaders
 *
 * -- Allen
 *)

functor InsertPreheaders
  (structure IR        : MLRISC_IR
   structure InsnProps : INSN_PROPERTIES
     sharing IR.CFG.I = InsnProps.I
  ) : MLRISC_IR_OPTIMIZATION =
struct

   structure IR   = IR
   structure CFG  = IR.CFG
   structure Loop = IR.Loop
   structure Util = IR.Util
   structure W    = CFG.W
   structure G    = Graph

   type flowgraph = IR.IR 

   val preheaders = MLRiscControl.getCounter "preheaders-inserted"

   val name = "InsertPreheaders"

   fun error msg = MLRiscErrorMsg.error(name,msg)

   fun run IR =
   let  val CFG as G.GRAPH cfg = IR
        val G.GRAPH loop = IR.loop IR

        fun is_falls_thru(_,_,CFG.EDGE{k=CFG.BRANCH false,...}) = true
          | is_falls_thru(_,_,CFG.EDGE{k=CFG.FALLSTHRU,...}) = true
          | is_falls_thru(_,_,_) = false

        val changed = ref false

        fun process_loop(_,Loop.LOOP{backedges=[],...}) = ()
          | process_loop(_,Loop.LOOP{header,backedges,...}) =
        let fun find_entries([],entries,freq) = (entries,freq)
              | find_entries((e as (i,j,CFG.EDGE{k=CFG.ENTRY,...}))::es,_,_) = 
                 error "insert_preheaders"
              | find_entries((e as (i,j,CFG.EDGE{w,k,...}))::es,entries,freq) = 
                 if List.exists (fn (i',j',_) => i=i' andalso j=j') backedges
                 then find_entries(es,entries,freq)
                 else find_entries(es,e::entries,!w + freq)
            val (entries,w) = find_entries(#in_edges cfg header,[],0)
            val header_node = #node_info cfg header
            val preheader   = #new_id cfg ()
            val preheader_node as CFG.BLOCK{freq,annotations,insns,...} =
                 CFG.newBlock(preheader,ref w)
            val no_jump = List.exists is_falls_thru entries
            val new_edge = CFG.EDGE{k=if no_jump then CFG.FALLSTHRU 
                                      else CFG.JUMP, w=ref w,a=ref []}
        in  changed := true;
            preheaders := !preheaders + 1;
            #add_node cfg (preheader,preheader_node);
            #add_edge cfg (preheader,header,new_edge);
            if no_jump then () 
               else insns := [InsnProps.jump(CFG.defineLabel header_node)];
            app (fn (i,_,_) => 
                let fun edge(i,j,e) = 
                         (i,if j = header then preheader else j,e)
                in  #set_out_edges cfg (i,map edge (#out_edges cfg i));
                    Util.updateJumpLabel CFG i
                end) entries
        end 
   in   
        #forall_nodes loop process_loop;
        if !changed then IR.changed IR else ();
        IR
   end

end


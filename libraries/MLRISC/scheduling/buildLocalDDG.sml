(*
 *  Build a DDG from a basic block
 *)
functor BasicBlockSchedulerDDGBuilder
   (structure DDG        : SCHEDULER_DDG
    structure InsnProps  : INSN_PROPERTIES
      sharing DDG.I = InsnProps.I
   ) : BASIC_BLOCK_SCHEDULER_DDG_BUILDER =
struct

   structure DDG        = DDG
   structure I          = DDG.I
   structure C          = I.C
   structure SchedProps = DDG.SchedProps
   structure H          = C.ColorTable
   structure G          = Graph

   type architecture = string
   type ddg = (I.instruction,DDG.latency) DDG.ddg

   fun error msg = MLRiscErrorMsg.error("BasicBlockSchedulerDDGBuilder",msg)

   val COPY_LATENCY = 0

   exception NotThere

  (*
   * Build a DAG from a list of instructions (in reverse order)
   * This is just a simple def/use analysis.
   *)
   fun buildDDG{cpu_info,ddg=G.GRAPH ddg} =
   let val SchedProps.CPU_INFO{defUse,...} = cpu_info
       fun buildDAG insns =
       let val defMap    = H.mkTable(31,NotThere) 
           val useMap    = H.mkTable(31,NotThere) 
           val findUse   = H.find useMap
           val findDef   = H.find defMap
           val rmvUse    = H.remove useMap
           val rmvDef    = H.remove defMap
           fun lookupUse r = case findUse r of NONE => [] | SOME x => x
           fun lookupDef r = case findDef r of NONE => [] | SOME x => x
           val insertUse = H.insert useMap
           val insertDef = H.insert defMap

           fun flowDep i (r,latency) = 
               app (fn j => #add_edge ddg (i,j,latency)) (lookupUse r)
           fun outputDep i (r,_) = 
               app (fn j => #add_edge ddg (i,j,~1)) (lookupDef r)
           fun antiDep i r = 
               app (fn j => #add_edge ddg (i,j,~1)) (lookupDef r)
           fun ctrlDep i j = #add_edge ddg (i,j,~1)
           fun addDefs n (r,l) = (rmvUse r; insertDef(r, [n]))
           fun addUses n r = insertUse(r,n::lookupUse r)

           fun copyDstSrc i' =
           let val (dst, src) = InsnProps.moveDstSrc i'
               fun coalesce(d::ds, s::ss, dst, src) = 
                   if C.sameColor(d,s) then coalesce(ds, ss, dst, src)
                   else coalesce(ds, ss, (d,COPY_LATENCY)::dst, s::src)
                 | coalesce([], [], dst, src) = (dst, src)
                 | coalesce _ = error "coalesce"

               val (dst, src) = coalesce(dst, src, [], [])
               val dst = case InsnProps.moveTmpR i' of
                           NONE => dst
                         | SOME tmp => (tmp,~1)::dst
           in  (dst, src) end

           fun scan(i,[],branches,succs) = ()
             | scan(i,i'::insns,branches,succs) = 
           let val _    = #add_node ddg (i,i') (* create node *)
               val kind = InsnProps.instrKind i' 
               val (defs,uses) = 
                   case kind of
                     InsnProps.IK_COPY => copyDstSrc i'
                   | _ => defUse i'
               val _ = #add_node ddg (i,i')
               val _ = app (flowDep i) defs
               val _ = app (outputDep i) defs
               val _ = app (antiDep i) uses
               val _ = app (ctrlDep i) branches
               val _ = app (addDefs i) defs
               val _ = app (addUses i) uses
               val branches = 
                   case kind of
                     InsnProps.IK_JUMP => [i]
                   | InsnProps.IK_CALL => (app (ctrlDep i) succs; [i])
                   | _  => branches
           in  scan(i+1,insns,branches,i::succs)
           end
       in  scan(0,insns,[],[])
       end
   in  buildDAG
   end

end

(*
 * This module performs branches chaining 
 *
 * -- Allen
 *)

functor BranchChaining
    (structure IR        : MLRISC_IR
     structure InsnProps : INSN_PROPERTIES
        sharing IR.I = InsnProps.I
    ) : MLRISC_IR_OPTIMIZATION =
struct

   structure IR   = IR
   structure CFG  = IR.CFG
   structure G    = Graph
   structure Util = IR.Util
   structure A    = Array
   structure C    = IR.I.C

   type flowgraph = IR.IR
 
   val name = "BranchChaining"

   fun error msg = MLRiscErrorMsg.error("BranchChaining",msg)
   
   val branchChainingCount = MLRiscControl.getCounter "branch-chaining-count"

   fun run (IR as G.GRAPH cfg : IR.IR) = 
   let exception NoTarget
       val N = #capacity cfg ()

       (* Performs branch chaining *)  
       val branchMap = IntHashTable.mkTable(13, NoTarget)
       val addBranch = IntHashTable.insert branchMap 
       val lookupBranch = IntHashTable.lookup branchMap

       val visited = A.array(N, ~1)
       val stamp   = ref 0

       val changed = ref false

       fun isTrivialCopy(instr) =  
           let fun isTrivial([], []) = true
                 | isTrivial(d::ds, s::ss) =
                   C.sameColor(d,s) andalso isTrivial(ds,ss)
                 | isTrivial _ = error "isTrivialCopy"
               val (dst, src) = InsnProps.moveDstSrc instr
           in  isTrivial(dst, src) 
           end

       fun isNop(instr) = 
           case InsnProps.instrKind instr of
             InsnProps.IK_NOP => true 
           | InsnProps.IK_COPY => isTrivialCopy(instr) 
           | _ => false

       fun isAllNop [] = true
         | isAllNop (i::is) = isNop i andalso isAllNop is

       (* Given a blockId, finds out which block it really branches to
        * eventually.  The visited array is to prevent looping in programs
        * with self-loops.   If NO_BRANCH_CHAINING is set on a jump, we also
        * terminate there. 
        *) 
       fun chase blockId = 
       let val st = !stamp
           val _ = stamp := !stamp + 1;
           fun follow blockId =
               lookupBranch blockId 
               handle NoTarget =>
               if A.sub(visited,blockId) = st then blockId
               else
                 (A.update(visited, blockId, st);
                  case #node_info cfg blockId of
                    CFG.BLOCK{insns=ref [], ...} =>
                       (* falls thru to next *)
                       (case #out_edges cfg blockId of 
                          [(_,next,CFG.EDGE{k=CFG.FALLSTHRU, ...})] => 
                             follow next 
                        | _ => blockId (* terminates here *) 
                       )
                  | CFG.BLOCK{insns=ref(insns as jmp::rest), ...} =>
                          (* may be a jump *)
                    let val (_, a) = InsnProps.getAnnotations jmp
                    in  if #contains MLRiscAnnotations.NO_BRANCH_CHAINING a then
                            blockId (* no branch chaining! *)
                        else 
                        (case #out_edges cfg blockId of
                           [(_,next,CFG.EDGE{k=CFG.JUMP, ...})] => 
                              if isAllNop rest then follow next 
                              else blockId (* terminates here *)
                         | [(_,next,CFG.EDGE{k=CFG.FALLSTHRU, ...})] => 
                              if isAllNop insns then follow next
                              else blockId (* terminates here *)
                         | _ => blockId (* terminates here *)
                        )
                    end
                 )
           val targetBlockId = follow blockId
       in  addBranch(blockId, targetBlockId);
           if blockId <> targetBlockId then 
              ((* print "BRANCHING CHAINING\n"; *)
               branchChainingCount := !branchChainingCount + 1;
               changed := true) 
           else ();
           targetBlockId 
       end

       fun branchChaining(i,CFG.BLOCK{insns=ref [], ...}) = ()
         | branchChaining(i,CFG.BLOCK{insns=ref(jmp::_), ...}) = 
           if InsnProps.instrKind jmp = InsnProps.IK_JUMP then
           let fun get(i,j,e as CFG.EDGE{k=CFG.JUMP,...}) = (i,chase j,e)
                 | get(i,j,e as CFG.EDGE{k=CFG.BRANCH true,...}) = (i,chase j,e)
                 | get(i,j,e as CFG.EDGE{k=CFG.SWITCH _,...}) = (i,chase j,e) 
                 | get e = e
               val out_edges = #out_edges cfg i
           in  case out_edges of
                 ([_] | [_,_]) =>
                  let val edges = map get out_edges
                  in  #set_out_edges cfg (i,edges);
                      Util.updateJumpLabel IR i
                  end
               | _ => () (* Can't do branch chaining on multiway jumps yet! *)
           end
           else ()

   in  #forall_nodes cfg branchChaining;
       if !changed then (Util.removeUnreachableCode IR; IR.changed IR) else ();
       IR
   end

end


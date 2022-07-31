(*
 * This module rearranges and eliminate branches in a problem to
 * get better locality. 
 *
 * -- Allen
 *)

functor ReshapeBranches
    ( structure IR        : MLRISC_IR
      structure InsnProps : INSN_PROPERTIES
         sharing IR.I = InsnProps.I
    ) : MLRISC_IR_OPTIMIZATION =
struct

   structure IR       = IR
   structure CFG      = IR.CFG
   structure Dom      = IR.Dom
   structure Loop     = IR.Loop
   structure W        = CFG.W
   structure G        = Graph
   structure Util     = IR.Util

   type flowgraph = IR.IR

   (*
    * Restructure branches to in order to get better locality.
    *)
   val name = "ReshapeBranches"

   fun run IR =
   let val CFG as G.GRAPH cfg   = IR
       val Dom as G.GRAPH dom   = IR.dom  IR
       val Loop as G.GRAPH loop = IR.loop IR
       val dominates            = Dom.dominates Dom
       val labelOf              = Util.labelOf CFG
       val changed              = ref false

       exception GiveUp

       (* Check that a block does not have stupid pseudo ops that
        * get in the way *)
       fun no_pseudo_ops j =
           let val CFG.BLOCK{data,...} = #node_info cfg j
           in  case !data of
                  [] => true
               |  _  => false
           end

       (* Can block j has a new fallsthru entry? *)
       fun can_fallsthru j =
           case CFG.fallsThruFrom(CFG,j) of
              NONE   => no_pseudo_ops j
           |  SOME _ => false

       (* flip conditionals around *)
       fun flip_cond should_flip (i,CFG.BLOCK{insns,...}) =
          case (#out_edges cfg i,!insns) of
            ([e1 as (_,j,CFG.EDGE{w=w1,k=k1 as CFG.BRANCH b1,a=a1}),
              e2 as (_,k,CFG.EDGE{w=w2,k=k2 as CFG.BRANCH b2,a=a2})], 
              branch::rest) =>
             if j = k then (* targets are the same *)
             let val a = ref(!a1 @ !a2)
             in  #set_out_edges cfg 
                    (i, [(i,j,CFG.EDGE{w=ref(!w1 + !w2),k=CFG.JUMP,a=a})]);
                 insns := InsnProps.jump(labelOf j)::rest;
                 changed := true
             end
             else if should_flip(e1,e2) then 
                let val branch' = InsnProps.negateConditional branch
                in  if b1 andalso not(can_fallsthru j) orelse
                       b2 andalso not(can_fallsthru k) then
                       raise GiveUp
                    else ();
                    insns := branch'::rest;
                    CFG.removeEdge CFG e1;
                    CFG.removeEdge CFG e2;
                    #add_edge cfg (i,j,CFG.EDGE{w=w1,k=k2,a=a1});
                    #add_edge cfg (i,k,CFG.EDGE{w=w2,k=k1,a=a2});
                    Util.updateJumpLabel CFG i;
                    changed := true
                end 
             else ()
         | _ => ()

       fun follow i = 
       let fun chase j = 
               case #out_edges cfg j of
                 [(_,k,_)] => if k = i then k else chase k
               | _ => j
       in chase i end

       (* Falls thru case should be likely for forward branches. *)
       fun should_flip_forward_branches(
           (i,j,CFG.EDGE{w=w1,k=CFG.BRANCH b1,...}),
           (_,k,CFG.EDGE{w=w2,k=CFG.BRANCH b2,...})) =
             (b1 andalso !w1 > !w2 andalso not(dominates(follow j,i)))
             orelse
             (b2 andalso !w2 > !w1 andalso not(dominates(follow k,i)))
        | should_flip_forward_branches _ = false

       (*
        *  Eliminate all fallsthru into a block j
        *)
       fun elim_fallsthru j =
           let fun elim(e as (i,j,CFG.EDGE{k=CFG.BRANCH false,...})) =
                      flip_cond (fn _ => true) (i,#node_info cfg i)
                 | elim(e as (i,j,CFG.EDGE{k=CFG.FALLSTHRU,w,a,...})) =
                      let val i' as CFG.BLOCK{insns,...} = #node_info cfg i
                      in  insns := InsnProps.jump(
                                CFG.defineLabel(#node_info cfg i))::(!insns);
                          CFG.removeEdge CFG e;
                          #add_edge cfg (i,j,CFG.EDGE{k=CFG.JUMP,a=a,w=w});
                          Util.updateJumpLabel CFG i;
                          changed := true
                      end
                 | elim _ = ()
           in  app elim (#in_edges cfg j)
           end

       (* 
        * If a backedge is an unconditional jump,
        * Try to eliminate it by changing it into a fallsthru.
        *)
       fun restructure_loop(_,Loop.LOOP{header,backedges=[],...}) = ()
         | restructure_loop(_,Loop.LOOP{header,backedges=e::es,...}) =
       if no_pseudo_ops header then
       let fun find_best((e as (_,_,CFG.EDGE{w=w1,...}))::es,
                         best_e as (_,_,CFG.EDGE{w=w2,...})) =
                  find_best(es,if !w1 > !w2 then e else best_e)
             | find_best([],best_e) = best_e
       in  case find_best(es,e) of
              best_e as (i,j,CFG.EDGE{k=CFG.JUMP,w,a}) =>
                  if i <> header then 
                  (let val _ = elim_fallsthru header
                       val _ = elim_fallsthru i
                       val CFG.BLOCK{insns,...} = #node_info cfg i
                       fun remove_jump(insns as jmp::rest) =
                           if InsnProps.instrKind jmp = InsnProps.IK_JUMP then
                              rest else insns
                         | remove_jump [] = []
                   in  insns := remove_jump(!insns);
                       CFG.removeEdge CFG best_e;
                       #add_edge cfg (i,j,CFG.EDGE{k=CFG.FALLSTHRU,w=w,a=a});
                       changed := true
                   end handle _ => ())
                  else ()
           | _ => () 
       end
       else ()

       (* Restructure conditionals branches  *)
       val restructure_conditionals =
              flip_cond should_flip_forward_branches

   in  #forall_nodes cfg (fn x => restructure_conditionals x handle _ => ());
       #forall_nodes loop restructure_loop; 
       if !changed then IR.changed IR else ();
       Util.mergeAllEdges IR;
       IR
   end

end


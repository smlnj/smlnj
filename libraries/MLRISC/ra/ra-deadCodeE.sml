(*
 * This is a hack module for removing dead code that are discovered by 
 * the register allocator.  This module acts as a wrapper
 * for the generic RA flowgraph module.
 *
 * -- Allen
 *)

functor RADeadCodeElim
   (Flowgraph : RA_FLOWGRAPH)
   (  (* check for dead code on these cellkinds only *)
    val cellkind : CellsBasis.cellkind -> bool
      (* Dead registers are stored here. *)
    val deadRegs : bool IntHashTable.hash_table 
      (* Affected blocks *)
    val affectedBlocks : bool IntHashTable.hash_table 
    val spillInit : Flowgraph.G.interferenceGraph * CellsBasis.cellkind 
                      -> unit 
   ) : RA_FLOWGRAPH =
struct
   structure F = Flowgraph

   open F

   (* We must save all the copy temporaries for this to work *)
   val mode = RACore.SAVE_COPY_TEMPS

   fun isOn(flag,mask) = Word.andb(flag,mask) <> 0w0

   (*
    * New services that also removes dead code 
    *)
   fun services f =
   let val {build, spill, blockNum, instrNum, programPoint} = F.services f
       (* 
        * The following build method marks all pseudo registers
        * that are dead, and record their definition points.
        *)
       fun findDeadCode(G.GRAPH{nodes, copyTmps, mode, ...}) = 
       let val dead     = IntHashTable.insert deadRegs 
           val affected = IntHashTable.insert affectedBlocks
           val affectedList = app (fn d => affected(blockNum d, true))

           (* Mark all copy temporaries *)
           val marker = [{block=0,insn=0}]
           fun markCopyTmps [] = ()  
             | markCopyTmps(G.NODE{uses, ...}::tmps) =
                 (uses := marker; markCopyTmps tmps)
           fun unmarkCopyTmps [] = ()
             | unmarkCopyTmps(G.NODE{uses, ...}::tmps) =
                 (uses := []; unmarkCopyTmps tmps)

           fun enter(_, G.NODE{uses=ref [], defs, number=reg, ...}) =
               (* This is dead, but make sure it is not a copy temporary.
                * Those cannot be eliminated.
                *)
                (affectedList (!defs); dead(reg, true))
             | enter _ = ()

       in  markCopyTmps(!copyTmps);
           IntHashTable.appi enter nodes;
           unmarkCopyTmps(!copyTmps);
           if isOn(mode, RACore.HAS_PARALLEL_COPIES) then ()
           else copyTmps := [] (* clean up now *)
       end

       (*
        * Build the graph, then remove dead code.
        *)
       fun buildIt(graph, kind) =  
       let val moves = build(graph, kind)
       in  if cellkind kind then findDeadCode(graph) else ();
           moves
       end

       fun spillIt(arg as {graph, cellkind, ...}) = 
           (spillInit(graph, cellkind); spill arg)

   in  {build=buildIt, spill=spillIt, programPoint=programPoint,
        blockNum=blockNum, instrNum=instrNum}
   end

end

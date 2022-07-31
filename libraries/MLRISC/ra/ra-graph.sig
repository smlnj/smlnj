(* ra-graph.sig
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies.
 *
 * This is the new interference graph used by the new register allocator.
 * 
 * -- Allen
 *)

signature RA_GRAPH = 
sig

  structure C : CELLS_BASIS
  structure BM : RA_BITMATRIX = RaBitmatrix
  (*
   * The following are the data structures used in the register allocator.
   *)
  
  exception Nodes

  type priority = real
  type cost = real         

  (*
   * The following represent a program point in the program.
   *
   * The last instruction in the block is numbered 1, i.e. the instruction
   * numbering is in reverse.  The number 0 is reserved for "live-out".
   *
   *)
  type programPoint = {block:int, insn:int} 

  (* Hash table indexed by program point *)
  structure PPtHashTable : MONO_HASH_TABLE 
      where type Key.hash_key = programPoint

  type frame_offset = int
  type logical_spill_id = int

  datatype spillLoc = 
     FRAME   of logical_spill_id  (* spill to a new frame location *)
   | MEM_REG of C.cell            (* spill to a memory register *)

  (* Hash table indexed by spill location *)
  structure SpillLocHashTable : MONO_HASH_TABLE 
      where type Key.hash_key = spillLoc

  type mode = word

  datatype interferenceGraph = 
     GRAPH of 
     { bitMatrix    : BM.bitMatrix ref,
       nodes        : node IntHashTable.hash_table,
       K            : int,
       firstPseudoR : int,
       dedicated    : int -> bool,
       getreg       : {pref:int list, stamp:int, proh:int Array.array} -> int,
       getpair      : {pref:int list, stamp:int, proh:int Array.array} -> int,
       proh         : int Array.array,
       stamp        : int ref,

       (* Info to undo a spill when an optimistic spill has occurred *)
       spillFlag    : bool ref,

       spilledRegs  : bool IntHashTable.hash_table, 
                           (*registers that have been spilled*)
       trail        : trailInfo ref,

       (* how to pretty print a register *)
       showReg      : C.cell -> string,

       (* how many registers there are? *)
       numRegs      : int,
       maxRegs      : unit -> int,

       (* dead copies *)
       deadCopies   : C.cell list ref,
       copyTmps     : node list ref,
       memMoves     : move list ref,
       memRegs      : node list ref,

       (* spill locations *)
       spillLoc     : int ref,

       (* span indexed by node id *)
       span         : cost IntHashTable.hash_table option ref,

       (* mode *)
       mode         : mode,

       pseudoCount  : int ref
     }

  and moveStatus = BRIGGS_MOVE             (* not yet coalesceable *)
                 | GEORGE_MOVE             (* not yet coalesceable *)
                 | COALESCED               (* coalesced *)
                 | CONSTRAINED             (* src and target intefere *)
                 | LOST                    (* frozen moves *)
                 | WORKLIST                (* on the move worklist *)

  and move = 
    MV of {src    : node,  		(* source register of move *)
	   dst    : node,		(* destination register of move *)
(*           kind   : moveKind,           (* kind of move *) *)
           cost   : cost,               (* cost *)
	   status : moveStatus ref,     (* coalesced? *)
           hicount: int ref             (* neighbors of high degree *)
	  }

  and moveKind = REG_TO_REG      (* register to register *)
               | EVEN_TO_REG     (* even register in pair to register *)
               | ODD_TO_REG      (* odd register in pair to register *)
               | PAIR_TO_PAIR    (* register pair to register pair *)
               | REG_TO_EVEN     (* register to even register in pair *)
               | REG_TO_ODD      (* register to odd register in pair *)
(*  and moveKind = REGmvk		 (* register-register move *)
               | MEMREGmvk       (* move involving memReg  *)
 
*)

  and nodeStatus =
        PSEUDO                (* pseudo register *)
      | REMOVED               (* removed from the interference graph *)
      | ALIASED of node       (* coalesced *)
      | COLORED of int        (* colored *)
      | MEMREG of int * C.cell(* register implemented in memory *)
      | SPILLED		      (* spilled *)
      | SPILL_LOC of int      (* spilled at logical location *)

       (* Note on SPILLED:
        *  SPILLED ~1 means that the spill location is still undetermined
        *  SPILLED c, c >= 0 means that c is a fixed "memory register"
        *  SPILLED c, c < ~1 means that c is a logical spill location
        *                    assigned by the register allocator
        *)

  and node = 
    NODE of { number : int, 	        (* node number *)
	      cell:    C.cell,
	      movecnt: int ref,		(* #moves this node is involved in *)
	      movelist: move list ref,	(* moves associated with this node *)
	      degree : int ref,		(* current degree *)
	      color : nodeStatus ref,	(* status *)
	      adj : node list ref,	(* adjacency list *)
              pri : priority ref,       (* priority *)
              movecost : cost ref,      (* move cost *)
              (* pair : bool, *)        (* register pair? *)
              defs : programPoint list ref, 
              uses : programPoint list ref
            }

  and trailInfo = END | UNDO of node * moveStatus ref * trailInfo

  (* Create a new bitMatrix *)
  val newBitMatrix : {edges : int, maxRegs : int} -> BM.bitMatrix

  (* Create a new interference graph *)
  val newGraph : { nodes        : node IntHashTable.hash_table,
                   numRegs      : int,
                   maxRegs      : unit -> int,
                   K            : int,
                   firstPseudoR : int,
                   dedicated    : int -> bool,
                   showReg      : C.cell -> string,
                   getreg       : 
                     {pref:int list,stamp:int,proh:int Array.array} -> int,
                   getpair      : 
                     {pref:int list,stamp:int,proh:int Array.array} -> int,
                   proh         : int Array.array,
                   mode         : mode,
                   spillLoc     : int ref,
                   memRegs      : C.cell list
                 } -> interferenceGraph

end

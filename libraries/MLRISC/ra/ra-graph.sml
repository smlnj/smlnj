(*
 * This is the new interference graph used by the new register allocator.
 * 
 * -- Allen
 *)

structure RAGraph : RA_GRAPH =
struct

  structure C = CellsBasis

  structure BM = RaBitmatrix

  type priority = real

  type programPoint = {block:int, insn:int}

  structure PPtHashTable = HashTableFn
     (type hash_key = programPoint
      fun hashVal{block,insn} = 
             Word.<<(Word.fromInt block,0w7) + Word.fromInt insn
      fun sameKey(x:programPoint,y) = x = y
      )

  type frame_offset = int
  type logical_spill_id = int
  datatype spillLoc = FRAME of logical_spill_id
                    | MEM_REG of C.cell 

  structure SpillLocHashTable = HashTableFn 
     (type hash_key = spillLoc
      fun hashVal(FRAME i) = Word.fromInt i
        | hashVal(MEM_REG r) = C.hashCell r
      fun sameKey(FRAME i, FRAME j) = i = j
        | sameKey(MEM_REG x,MEM_REG y) = C.sameColor(x, y)
        | sameKey _ = false
      )

  type cost = real

  type mode = word

  datatype interferenceGraph = 
   GRAPH of { bitMatrix    : BM.bitMatrix ref,
              nodes        : node IntHashTable.hash_table,
              K            : int,
              firstPseudoR : int,
              dedicated    : int -> bool,
              getreg       : 
                  {pref:int list, stamp:int, proh:int Array.array} -> int,
              getpair       : 
                  {pref:int list, stamp:int, proh:int Array.array} -> int,
              proh         : int Array.array,
              stamp        : int ref,

             (* Info to undo a spill when an optimistic spill has occurred *)
              spillFlag    : bool ref,
              spilledRegs  : bool IntHashTable.hash_table,
              trail        : trailInfo ref,

              showReg      : C.cell -> string,
              numRegs      : int,
              maxRegs      : unit -> int,
 
              deadCopies   : C.cell list ref,
              copyTmps     : node list ref,
              memMoves     : move list ref,
              memRegs      : node list ref,

              spillLoc     : int ref,
              span         : cost IntHashTable.hash_table option ref,
              mode         : mode,
              pseudoCount  : int ref
            }

  and moveStatus = BRIGGS_MOVE | GEORGE_MOVE
                 | COALESCED | CONSTRAINED | LOST | WORKLIST

  and move = 
    MV of {src : node,			(* source register of move *)
	   dst : node,			(* destination register of move *)
           (* kind: moveKind, *)        (* what kind of move *)
           cost : cost,                 (* cost *)
	   status : moveStatus ref,	(* coalesced? *)
	   hicount : int ref	        (* neighbors of high degree *)
	  }

  and moveKind = REG_TO_REG      (* register to register *)
               | EVEN_TO_REG     (* even register in pair to register *)
               | ODD_TO_REG      (* odd register in pair to register *)
               | PAIR_TO_PAIR    (* register pair to register pair *)
               | REG_TO_EVEN     (* register to even register in pair *)
               | REG_TO_ODD      (* register to odd register in pair *)

  and nodeStatus =
        PSEUDO                (* pseudo register *)
      | REMOVED               (* removed from the interference graph *)
      | ALIASED of node       (* coalesced *)
      | COLORED of int        (* colored *)
      | MEMREG of int * C.cell(* register implemented in memory *)
      | SPILLED		      (* spilled *)
      | SPILL_LOC of int      (* spilled at logical location *)

  and node = 
    NODE of { number : int,  	        (* node number *)
              cell   : C.cell,
	      movecnt: int ref,		(* #moves this node is involved in *)
	      movelist: move list ref,	(* moves associated with this node *)
	      degree : int ref,		(* current degree *)
	      color : nodeStatus ref,	(* status *)
	      adj : node list ref,      (* adjacency list *)
              pri : priority ref,       (* priority *)
              movecost : cost ref,      (* move cost *)
              (* pair : bool, *)        (* register pair? *)
              defs : programPoint list ref,
              uses : programPoint list ref
            }

  and trailInfo = END | UNDO of node * moveStatus ref * trailInfo

  exception Nodes

  fun error msg = MLRiscErrorMsg.error("NewRAGraph", msg)

  val stampCounter = ref 0

  (* Create a new bitMatrix *)
  fun roundSize size = 
  let fun f(x, shift) =
        if x >= size then (x, Word.>>(shift, 0w1))
        else f(x+x, shift+0w1)
  in f(64, 0w6) end

  val max = Word.<<(0w1,Word.>>(Word.fromInt Word.wordSize,0w1)) 
  val _ = if max < Word.<<(0w1,0w15) 
          then error "word size too small" else ()

  fun newBitMatrix{edges, maxRegs} =
  let val table = 
        (* if maxRegs < 1024 then
          let val denseBytes  = (maxRegs * (maxRegs + 1) + 15) div 16
          in  BITMATRIX(Word8Array.array(denseBytes,0w0))
          end 
          else *)
          let val (tableSize, shift) = roundSize edges
          in  if Word.fromInt maxRegs < max then
                 BM.SMALL(ref(Array.array(tableSize,[])),shift)
              else  
                 BM.LARGE(ref(Array.array(tableSize, BM.NIL)),shift)
          end
  in  BM.BM{table=table, elems=ref 0, edges=edges}
  end

  (* Create a new interference graph *)
  fun newGraph{nodes,K,firstPseudoR,dedicated,spillLoc,
               getreg,getpair,showReg,maxRegs,numRegs,proh,
               memRegs,mode} =
  let (* lower triangular bitmatrix primitives *)
      (* NOTE: The average ratio of E/N is about 16 *)
      val bitMatrix = newBitMatrix{edges=numRegs * 16,maxRegs=maxRegs()}

      (* Make memory register nodes *)
      fun makeMemRegs [] = []
        | makeMemRegs(cells) = 
          let val add = IntHashTable.insert nodes
              fun loop([], ns) = ns
                | loop(cell::cells, ns) = 
                  let val id = C.registerId cell
                      val node = 
                      NODE{number=id,
                           pri=ref 0.0,adj=ref [],degree=ref 0,movecnt=ref 0,
                           color=ref(MEMREG(id,cell)), 
                           defs=ref [], uses=ref [],
                           movecost=ref 0.0,movelist=ref [], cell=cell}
                  in  add(id, node); loop(cells, node::ns)
                  end
          in  loop(cells, [])
          end 

      val memRegs = makeMemRegs memRegs

  in  if !stampCounter > 10000000 then stampCounter := 0 else ();
      GRAPH{ bitMatrix    = ref bitMatrix,
             nodes        = nodes,
             K            = K,
             firstPseudoR = firstPseudoR,
             dedicated    = dedicated,
             getreg       = getreg,
             getpair      = getpair,
             proh         = proh,
             stamp        = stampCounter,
             spillFlag    = ref false,
             spilledRegs  = IntHashTable.mkTable(2,Nodes),
             trail        = ref END,
             showReg      = fn _ => raise Match,
             numRegs      = numRegs,
             maxRegs      = maxRegs,
             deadCopies   = ref [],
             copyTmps     = ref [],
             memMoves     = ref [],
             memRegs      = ref memRegs,
             spillLoc     = spillLoc,
             span         = ref NONE,
             mode         = mode,
             pseudoCount  = ref 0
           }
  end
  
end

(* ra-core.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies.
 *
 * Overview
 * ========
 * This implementation of iterated coalescing differ from the old one in
 * various substantial ways:
 *
 * 1. The move list is prioritized.  Higher ranking moves are coalesced first.
 *    This tends to favor coalescing of moves that has higher priority.
 *
 * 2. The freeze list is prioritized.  Lower ranking nodes are unfrozen
 *    first.  Since freeze disable moves, this tends to disable moves
 *    of low priority.
 *
 * 3. The simplify worklist is not kept explicitly during the 
 *    simplify/coalesce/freeze phases.  Instead, whenever a non-move
 *    related node with degree < K is discovered, we call simplify
 *    to remove it from the graph immediately.  
 *
 *    I think this has a few advantages.
 *    (a) There is less bookkeeping.
 *    (b) Simplify adds coalescable moves to the move list.
 *        By doing simplify eagerly, moves are added to the move list
 *        faster, allowing higher ranking moves to ``preempt'' low
 *        ranking moves.
 *
 * 4. Support for register pairs
 *
 * Important Invariants
 * ====================
 *   1. Adjacency list
 *      a. All nodes on the adjacency list are distinct
 *      b. nodes with color ALIASED or REMOVED are NOT consider to be
 *         on the adjacency list
 *      c. If a node x is COLORED, then we DON'T keep track of 
 *         its adjacency list 
 *      d. When a node has been removed, there aren't any moves associated
 *         with it.    
 *   2. Moves
 *      a. Moves marked WORKLIST are on the worklist.
 *      b. Moves marked MOVE are NOT on the worklist.
 *      c. Moves marked LOST are frozen and are in fact never considered again.
 *      d. Moves marked CONSTRAINED cannot be coalesced because the src and dst
 *         interfere
 *      e. Moves marked COALESCED have been coalesced.  
 *      f. The movecnt in a node is always the number of nodes 
 *         currently marked as WORKLIST or MOVE, i.e. the moves that
 *         are associated with the node.  When this is zero, the node is
 *         considered to be non-move related.
 *      g. Moves on the move worklist are always distinct.
 *   3.
 *
 * Allen.
 *
 *)

local

   val debug = false
   val tally = false 

in

structure RACore : RA_CORE =
struct

  structure G    = RAGraph
  structure A    = Array
  structure W    = Word
  structure W8A  = Word8Array
  structure W8   = Word8
  structure C    = RAGraph.C

  (* For debugging, uncomment Unsafe. *)
  structure UA   = Unsafe.Array 
  structure UW8A = Unsafe.Word8Array

  open G 

  val verbose       = MLRiscControl.mkFlag ("ra-verbose", "RA chattiness")
  val ra_spill_coal = MLRiscControl.mkCounter ("ra-spill-coalescing",
					       "RA spill coalescing counter")
  val ra_spill_prop = MLRiscControl.mkCounter ("ra-spill-propagation",
					       "RA spill propagation counter")

(*
  val good_briggs   = MLRiscControl.getCounter "good-briggs"
  val bad_briggs    = MLRiscControl.getCounter "bad-briggs"
  val good_george   = MLRiscControl.getCounter "good-george"
  val bad_george    = MLRiscControl.getCounter "bad-george"
  val good_freeze   = MLRiscControl.getCounter "good-freeze"
  val bad_freeze    = MLRiscControl.getCounter "bad-freeze"
 *)

  val NO_OPTIMIZATION     = 0wx0
  val BIASED_SELECTION    = 0wx1
  val DEAD_COPY_ELIM      = 0wx2
  val COMPUTE_SPAN        = 0wx4
  val SAVE_COPY_TEMPS     = 0wx8 
  val HAS_PARALLEL_COPIES = 0wx10
  val SPILL_COALESCING       = 0wx100
  val SPILL_COLORING         = 0wx200
  val SPILL_PROPAGATION      = 0wx400
  val MEMORY_COALESCING      = 
      SPILL_COALESCING + SPILL_COLORING + SPILL_PROPAGATION

  val i2s = Int.toString
  val r2s = Real.toString

  local

  fun isOn(flag,mask) = Word.andb(flag,mask) <> 0w0

  fun error msg = MLRiscErrorMsg.error("RACore", msg)
 
  fun concat([], b) = b
    | concat(x::a, b) = concat(a, x::b)

  in

    
  structure FZ = RaPriQueue
     (type elem=node 
      fun less(NODE{movecost=ref p1,...}, NODE{movecost=ref p2,...}) = p1 <= p2
     )
  structure MV = RaPriQueue
     (type elem=G.move 
      fun less(MV{cost=p1,...}, MV{cost=p2,...}) = p1 >= p2
     )

  type move_queue = MV.pri_queue
  type freeze_queue = FZ.pri_queue


  (*  
   * Utility functions
   *)
  fun chase(NODE{color=ref(ALIASED r), ...}) = chase r
    | chase x = x

  fun cellId(C.CELL{id, ...}) = id

  fun col2s col =
       case col of
         PSEUDO     => ""
       | REMOVED    => "r"
       | ALIASED _  => "a"
       | COLORED c  => "["^i2s c^"]"
       | MEMREG (_,m)  => "m" ^ "{" ^ C.toString m ^ "}"
       | SPILLED    => "s"
       | SPILL_LOC c  => "s" ^ "{" ^ i2s c ^ "}"

  fun node2s (NODE{cell, color, pri,...}) = i2s(cellId cell)^col2s(!color)

  fun show G (node as NODE{pri,...}) = 
      node2s node^(if !verbose then "("^r2s(!pri)^")" else "")

  (*
   * Dump the interference graph
   *)
  fun dumpGraph(G as G.GRAPH{nodes, showReg, K,...}) stream =
  let fun pr s = TextIO.output(stream, s)
      val show = show G
      fun prMove(MV{src, dst, status=ref(WORKLIST | BRIGGS_MOVE | GEORGE_MOVE),
                    cost,...}) = 
            pr(node2s(chase dst)^" <- "^node2s(chase src)^
               "("^r2s(cost)^") ")
        | prMove _ = ()

      fun prAdj(n,n' as NODE{adj, degree, uses, defs,
                             color, pri, movecnt, movelist, ...}) =
          (pr(show n');
           if !verbose then pr(" deg="^i2s(!degree)) else ();
           (case !color of
              ALIASED n => (pr " => "; pr(show n); pr "\n")
            | _ =>
              (pr(" <-->");
               app (fn n => (pr " "; pr(show n))) (!adj); pr "\n";
               if !verbose andalso !movecnt > 0 then 
                 (pr("\tmoves "^i2s(!movecnt)^": ");
                  app prMove (!movelist);
                  pr "\n"
                 ) 
               else ()
              )
           )
         )
 
  in  pr("=========== K="^i2s K^" ===========\n");
      app prAdj (ListMergeSort.sort (fn ((x, _),(y, _)) => x > y)
                    (IntHashTable.listItemsi nodes))
  end


  (*
   * Function to create new nodes.
   * Note: it is up to the caller to remove all dedicated registers.
   *)
  fun newNodes (G.GRAPH{nodes, firstPseudoR,  ...}) = let
      val getnode = IntHashTable.lookup nodes
      val addnode = IntHashTable.insert nodes

      fun colorOf(C.CELL{col=ref(C.MACHINE r), ...}) = r
	| colorOf(C.CELL{id, ...}) = id

      fun getNode(cell as C.CELL{col, ...}) = 
	(getnode(colorOf cell))
	  handle _ => let
	       val reg = colorOf cell
	       val col = 
		 case !col 
		  of C.MACHINE r => G.COLORED r
		   | C.PSEUDO    => G.PSEUDO
		   | C.ALIASED _ => error "getNode: ALIASED"
		   | C.SPILLED   => error "getNode: SPILLED"
	       val node = 	         
		 NODE{number=reg,
		      cell=cell, color= ref col, degree=ref 0,
		      adj=ref[], movecnt=ref 0, movelist=ref[],
		      movecost=ref 0.0, pri=ref 0.0, defs=ref[],
		      uses=ref[]}

	    in addnode(reg, node); node
	    end


      fun defUse{defs, uses, pt, cost} = let
	   fun def cell = let
	     val node as NODE{pri, defs, ...} = getNode (cell)
	   in
	       pri := !pri + cost;
	       defs := pt :: !defs;
	       node
	   end
	   fun use cell = let
	     val node as NODE{pri, uses, ...} = getNode(cell)
	   in
	       pri := !pri + cost;
	       uses := pt :: !uses
	   end
      in  
	  List.app use uses;
	  List.map def defs     
      end
  in  defUse
  end

  (*
   * Add an edge (x, y) to the interference graph.
   * Nop if the edge already exists.
   * Note: adjacency lists of colored nodes are not stored 
   *       within the interference graph to save space.
   * Now we allow spilled node to be added to the edge; these do not
   * count toward the degree. 
   *)
  fun addEdge(GRAPH{bitMatrix,...}) = 
  let val addBitMatrix = BM.add(!bitMatrix)
  in  fn (x as NODE{number=xn, color=colx, adj=adjx, degree=degx, ...}, 
          y as NODE{number=yn, color=coly, adj=adjy, degree=degy, ...}) => 
          if xn = yn then ()
          else if addBitMatrix(xn, yn) then
           (case (!colx, !coly) of
             (PSEUDO,      PSEUDO) => (adjx := y:: !adjx; degx := !degx+1;
                                       adjy := x:: !adjy; degy := !degy+1)
           | (PSEUDO,   COLORED _) => (adjx := y:: !adjx; degx := !degx+1)
           | (PSEUDO,    MEMREG _) => (adjx := y:: !adjx; adjy := x:: !adjy)
           | (PSEUDO, SPILL_LOC _) => (adjx := y:: !adjx; adjy := x:: !adjy)
           | (PSEUDO,     SPILLED) => ()
           | (COLORED _,   PSEUDO) => (adjy := x:: !adjy; degy := !degy+1)
           | (COLORED _, COLORED _) => () (* x<>y, can't alias *)
           | (COLORED _, MEMREG _) => () (* x<>y, can't alias *)
           | (COLORED _, SPILL_LOC _) => () (* x<>y, can't alias *)
           | (COLORED _,   SPILLED) => ()
           | (MEMREG _,    PSEUDO) => (adjx := y:: !adjx; adjy := x:: !adjy)
           | (MEMREG _, COLORED _) => ()   (* x<>y, can't alias *)
           | (MEMREG _,  MEMREG _) => ()   (* x<>y, can't alias *)
           | (MEMREG _, SPILL_LOC _) => () (* x<>y, can't alias *)
           | (MEMREG _,   SPILLED) => ()
           | (SPILL_LOC _, PSEUDO) => (adjx := y:: !adjx; adjy := x:: !adjy)
           | (SPILL_LOC _, COLORED _) => ()     (* x<>y, can't alias *)
           | (SPILL_LOC _, MEMREG _) => ()    (* x<>y, can't alias *)
           | (SPILL_LOC _, SPILL_LOC _) => () (* x<>y, can't alias *)
           | (SPILL_LOC _, SPILLED) => () (* x<>y, can't alias *)
           | (SPILLED,  _) => ()
           | (colx, coly) => 
               error("addEdge x="^i2s xn^col2s colx^" y="^i2s yn^col2s coly)
           )
          else () (* edge already there *)
  end

  fun isFixedMem(SPILL_LOC _) = true
    | isFixedMem(MEMREG _) = true
    | isFixedMem(SPILLED) = true
    | isFixedMem _ = false

  fun isFixed(COLORED _) = true
    | isFixed c = isFixedMem(c) 

  (*
   * Initialize a list of worklists
   *)
  fun initWorkLists 
        (GRAPH{nodes, K, bitMatrix, pseudoCount, 
               firstPseudoR, deadCopies, memMoves, mode, ...}) {moves} =
  let 
      (* Filter moves that already have an interference
       * Also initialize the movelist and movecnt fields at this time.
       *)
      val member = BM.member(!bitMatrix)

      fun setInfo(NODE{color=ref PSEUDO, movecost, movecnt, movelist,...}, 
                  mv, cost) =
           (movelist := mv :: !movelist; 
            movecnt := !movecnt + 1;
            movecost := !movecost + cost
           )
        | setInfo _ = ()


      (* filter moves that cannot be coalesced *)
      fun filter([], mvs', mem) = (mvs', mem)
        | filter((mv as MV{src as NODE{number=x, color=ref colSrc,...},
                           dst as NODE{number=y, color=ref colDst,...}, 
                           cost, ...})::mvs, 
                 mvs', mem) =
          if isFixed colSrc andalso isFixed colDst then
            filter(mvs, mvs', mem)
          else if isFixedMem colSrc orelse isFixedMem colDst then
            filter(mvs, mvs', mv::mem)
          else if member(x, y) then  
            filter(mvs, mvs', mem) 
          else 
            (setInfo(src, mv, cost);
             setInfo(dst, mv, cost);
             filter(mvs, MV.add(mv, mvs'), mem))

      (* like filter but does dead copy elimination *)
      fun filterDead([], mvs', mem, dead) = (mvs', mem, dead)
        | filterDead((mv as 
                  MV{src as NODE{number=x, color as ref colSrc, 
                                 pri, adj, uses,...},
                     dst as NODE{number=y, cell=celly, color=ref colDst, 
                                 defs=dstDefs, uses=dstUses,...},
                     cost, ...})::mvs, 
                 mvs', mem, dead) =  
          if (isFixed colSrc andalso isFixed colDst) then
            filterDead(mvs, mvs', mem, dead)
          else if isFixedMem colSrc orelse isFixedMem colDst then
            filterDead(mvs, mvs', mv::mem, dead)
          else (case (colSrc, colDst, dstDefs, dstUses) 
            of (_, PSEUDO, ref [pt], ref [])=> 
               (* eliminate dead copy *)
               let fun decDegree [] = ()
                     | decDegree(NODE{color=ref PSEUDO, degree, ...}::adj) =
                         (degree := !degree - 1; decDegree adj)
                     | decDegree(_::adj) = decDegree adj
                   fun elimUses([], _, uses, pri, cost) = (uses, pri)
                     | elimUses(pt::pts, pt':G.programPoint, uses, pri, cost) =
                       if pt = pt' then elimUses(pts, pt', uses, pri-cost, cost)
                       else elimUses(pts, pt', pt::uses, pri, cost)
                   val (uses', pri') = elimUses(!uses, pt, [], !pri, cost);
               in  pri := pri';
                   uses := uses';
                   color := ALIASED src;
                   decDegree(!adj);
                   filterDead(mvs, mvs', mem, celly::dead)
               end
             | _ =>  (* normal moves *)
               if member(x, y)     (* moves that interfere *)
               then filterDead(mvs, mvs', mem, dead) 
               else (setInfo(src, mv, cost);
                     setInfo(dst, mv, cost);
                     filterDead(mvs, MV.add(mv, mvs'), mem, dead)
                    )
            )
            
      (*
       * Scan all nodes in the graph and check which worklist they should
       * go into.
       *)
      fun collect([], simp, fz, moves, spill, pseudos) =
         (pseudoCount := pseudos;
          {simplifyWkl = simp,
           moveWkl     = moves,
           freezeWkl   = fz,
           spillWkl    = spill
          }
         )
        | collect(node::rest, simp, fz, moves, spill, pseudos) = 
          (case node of
              NODE{color=ref PSEUDO, movecnt, degree, ...} =>
                 if !degree >= K then
                    collect(rest, simp, fz, moves, node::spill, pseudos+1)
                 else if !movecnt > 0 then
                    collect(rest, simp, FZ.add(node, fz), 
                            moves, spill, pseudos+1)
                 else
                    collect(rest, node::simp, fz, moves, spill, 
                            pseudos+1)  
           |  _ => collect(rest, simp, fz, moves, spill, pseudos)
          )

      (* First build the move priqueue *)
      val (mvs, mem) = 
                if isOn(mode, DEAD_COPY_ELIM) then
                let val (mvs, mem, dead) = filterDead(moves, MV.EMPTY, [], [])
                in  deadCopies := dead; (mvs, mem)
                end
                else filter(moves, MV.EMPTY, [])

  in  memMoves := mem;  (* memory moves *)
      collect(IntHashTable.listItems nodes, [], FZ.EMPTY, mvs, [], 0)
  end

  (*
   * Return a regmap that returns the current spill location
   * during spilling.
   *)
  fun spillLoc(G.GRAPH{nodes,...}) = 
  let val getnode = IntHashTable.lookup nodes
      fun num(NODE{color=ref(ALIASED n), ...}) = num n
        | num(NODE{color=ref(SPILLED), number, ...}) = number
        | num(NODE{color=ref(SPILL_LOC s), number, ...}) = ~s
        | num(NODE{color=ref(MEMREG(m, _)), number, ...}) = m
        | num(NODE{number, ...}) = number
      fun lookup r = num(getnode r) handle _ => r
  in  lookup 
  end

  fun spillLocToString(G.GRAPH{nodes,...}) = 
  let val getnode = IntHashTable.lookup nodes
      fun num(NODE{color=ref(ALIASED n), ...}) = num n
        | num(NODE{color=ref(SPILLED), cell, ...}) = "spilled "^C.toString cell
        | num(NODE{color=ref(SPILL_LOC s), number, ...}) = "frame "^i2s s
        | num(NODE{color=ref(MEMREG(_,m)), ...}) = "memreg "^C.toString m 
        | num(NODE{number, ...}) = "error "^i2s number
      fun lookup r = num(getnode r) 
  in  lookup 
  end

  (*
   * Core phases:
   *   Simplify, coalesce, freeze.
   *
   * NOTE: When a node's color is REMOVED or ALIASED, 
   *       it is not considered to be part of the adjacency list
   *
   *  1.  The move list has no duplicate
   *  2.  The freeze list may have duplicates
   *)
  fun iteratedCoalescingPhases
       (G as GRAPH{K, bitMatrix, spillFlag, trail, stamp, mode,
                   pseudoCount,  ...}) =
  let val member = BM.member(!bitMatrix)
      val addEdge = addEdge G
      val show = show G
      val memoryCoalescingOn = isOn(mode, MEMORY_COALESCING)

      (*
       * SIMPLIFY node:
       *   precondition: node must be part of the interference graph (PSEUDO)
       *)
      fun simplify(node as NODE{color, number, adj, degree, (*pair,*)...},
                   mv, fz, stack) =
      let val _ = if debug then print("Simplifying "^show node^"\n") else ()
          fun forallAdj([], mv, fz, stack) = (mv, fz, stack)
            | forallAdj((n as NODE{color=ref PSEUDO, degree as ref d,...})::adj,
                        mv, fz, stack) =
              if d = K then 
              let val (mv, fz, stack) = lowDegree(n, mv, fz, stack)
              in  forallAdj(adj, mv, fz, stack) end
              else (degree := d - 1; forallAdj(adj, mv, fz, stack))
            | forallAdj(_::adj, mv, fz, stack) = forallAdj(adj, mv, fz, stack)
      in  color := REMOVED;
          pseudoCount := !pseudoCount - 1;
          forallAdj(!adj, mv, fz, node::stack) (* push onto stack *)
      end (* simplify *)

      and simplifyAll([], mv, fz, stack) = (mv, fz, stack)
        | simplifyAll(node::simp, mv, fz, stack) =
          let val (mv, fz, stack) = simplify(node, mv, fz, stack)
          in  simplifyAll(simp, mv, fz, stack) end

      (*
       * Decrement the degree of a pseudo node.
       *   precondition: node must be part of the interference graph
       *   If the degree of the node is now K-1.
       *   Then if (a) the node is move related, freeze it.
       *           (b) the node is non-move related, simplify it
       *
       *   node  -- the node to decrement degree
       *   mv    -- queue of move candidates to be coalesced
       *   fz    -- queue of freeze candidates
       *   stack -- stack of removed nodes
       *)
      and lowDegree(node as NODE{degree as ref d, movecnt, adj, color,...},
                    (* false, *) mv, fz, stack) = 
           (* normal edge *)
          (if debug then 
           print("DecDegree "^show node^" d="^i2s(d-1)^"\n") else (); 
           degree := K - 1;
           (* node is now low degree!!! *)
           let val mv = enableMoves(!adj, mv)
           in  if !movecnt > 0 then (* move related *)
                  (mv, FZ.add(node, fz), stack)
               else (* non-move related, simplify now! *)
                  simplify(node, mv, fz, stack)
           end
          )
       (*
        | decDegree(node as NODE{degree as ref d, movecnt, adj, color,...},
                    true, mv, fz, stack) = (* register pair edge *)
          (degree := d - 2;
           if d >= K andalso !degree < K then 
             (* node is now low degree!!! *)
             let val mv = enableMoves(node :: !adj, mv)
             in  if !movecnt > 0 then (* move related *)
                    (mv, FZ.add(node, fz), stack)
                 else (* non-move related, simplify now! *)
                    simplify(node, mv, fz, stack)
             end
           else
             (mv, fz, stack)
          )
        *)

      (*
       * Enable moves:
       *   given: a list of nodes (some of which are not in the graph)
       *   do:    all moves associated with these nodes are inserted
       *          into the move worklist
       *)
      and enableMoves([], mv) = mv
        | enableMoves(n::ns, mv) =
          let (* add valid moves onto the worklist.
               * there are no duplicates on the move worklist!
               *)
              fun addMv([], ns, mv) = enableMoves(ns, mv)
                | addMv((m as MV{status, hicount as ref hi, ...})::rest,
                        ns, mv) = 
                  (case !status of
                     (BRIGGS_MOVE | GEORGE_MOVE) => 
                       (* decrements hi, when hi <= 0 enable move *)
                       if hi <= 1 then
                         (status := WORKLIST; addMv(rest, ns, MV.add(m, mv)))
                       else
                         (hicount := hi-1; addMv(rest, ns, mv))
                   | _    => addMv(rest, ns, mv)
                  )
          in  (* make sure the nodes are actually in the graph *)
              case n of
                NODE{movelist, color=ref PSEUDO, movecnt,...} =>
                  if !movecnt > 0 then (* is it move related? *)
                     addMv(!movelist, ns, mv)
                  else
                     enableMoves(ns, mv)
              | _ => enableMoves(ns, mv)
          end (* enableMoves *)

     (*
      *  Brigg's conservative coalescing test:
      *    given: an unconstrained move (x, y)  
      *    return: true or false
      *)
     fun conservative(hicount,
                      x as NODE{degree=ref dx, adj=xadj, (* pair=px, *) ...},
                      y as NODE{degree=ref dy, adj=yadj, (* pair=py, *) ...}) =
         dx + dy < K orelse
         let (*  
              *  hi -- is the number of nodes with deg > K (without duplicates)
              *  n -- the number of nodes that have deg = K but not neighbors
              *         of both x and y
              *  We use the movecnt as a flag indicating whether
              *  a node has been visited.  A negative count is used to mark
              *  a visited node.
              *)
             fun undo([], extraHi) = 
                 extraHi <= 0 orelse (hicount := extraHi; false)
               | undo(movecnt::tr, extraHi) = 
                   (movecnt := ~1 - !movecnt; undo(tr, extraHi))
             fun loop([], [], hi, n, tr) = undo(tr, (hi + n) - K + 1)
               | loop([], yadj, hi, n, tr) = loop(yadj, [], hi, n, tr)
               | loop(NODE{color, movecnt as ref m, degree=ref deg, ...}::vs, 
                      yadj, hi, n, tr) =
                 (case !color of
                    COLORED _ =>
                      if m < 0 then
                         (* node has been visited before *)
                         loop(vs, yadj, hi, n, tr)
                      else
                        (movecnt := ~1 - m;  (* mark as visited *)
                         loop(vs, yadj, hi+1, n, movecnt::tr))
                  | PSEUDO =>
                      if deg < K then loop(vs, yadj, hi, n, tr)
                      else if m >= 0 then
                         (* node has never been visited before *)
                         (movecnt := ~1 - m;  (* mark as visited *)
                          if deg = K 
                          then loop(vs, yadj, hi, n+1, movecnt::tr)
                          else loop(vs, yadj, hi+1, n, movecnt::tr)
                         )
                      else
                         (* node has been visited before *)
                         if deg = K then loop(vs, yadj, hi, n-1, tr)
                         else loop(vs, yadj, hi, n, tr)
                  | _ => loop(vs, yadj, hi, n, tr) (* REMOVED/ALIASED *)
                 )
         in loop(!xadj, !yadj, 0, 0, []) end

     (*
      *  Heuristic used to determine whether a pseudo and machine register     
      *  can be coalesced. 
      *  Precondition:
      *     The two nodes are assumed not to interfere.
      *)
     fun safe(hicount, reg, NODE{adj, ...}) =
     let fun loop([], hi) = hi = 0 orelse (hicount := hi; false)
           | loop(n::adj, hi) =
             (case n of
               (* Note: Actively we only have to consider pseudo nodes and not
                * nodes that are removed, since removed nodes either have
                * deg < K or else optimistic spilling must be in effect!
                *)
               NODE{degree,number,color=ref(PSEUDO | REMOVED), ...} => 
               if !degree < K orelse member(reg, number) then loop(adj, hi)
               else loop(adj, hi+1)
             | _ => loop(adj, hi)
             )
     in  loop(!adj, 0) end

     (*
      *  Decrement the active move count of a node.
      *  When the move count reaches 0 and the degree < K
      *  simplify the node immediately.    
      *      Precondition: node must be a node in the interference graph
      *      The node can become a non-move related node.
      *)
     fun decMoveCnt
         (node as NODE{movecnt, color=ref PSEUDO, degree, movecost,...}, 
          cnt, cost, mv, fz, stack) =
         let val newCnt = !movecnt - cnt
         in  movecnt := newCnt;
             movecost := !movecost - cost;
             if newCnt = 0 andalso !degree < K (* low degree and movecnt = 0 *)
             then (simplify(node, mv, fz, stack))
             else (mv, fz, stack)
         end
       | decMoveCnt(_, _, _, mv, fz, stack) = (mv, fz, stack)

     (*
      * Combine two nodes u and v into one.
      *   v is replaced by u  
      *   u is the new combined node
      *   Precondition: u <> v and u and v must be unconstrained
      *
      *  u, v   -- two nodes to be merged, must be distinct!
      *  coloingv -- is u a colored node?
      *  mvcost -- the cost of the move that has been eliminated
      *  mv     -- the queue of moves
      *  fz     -- the queue of freeze candidates
      *  stack  -- stack of removed nodes
      *)
     fun combine(u, v, coloringv, mvcost, mv, fz, stack) =
     let val NODE{color=vcol, pri=pv, movecnt=cntv, movelist=movev, adj=adjv,
                  defs=defsv, uses=usesv, degree=degv, ...} = v
         val NODE{color=ucol, pri=pu, movecnt=cntu, movelist=moveu, adj=adju,
                  defs=defsu, uses=usesu, degree=degu, ...} = u

         (* merge movelists together, taking the opportunity
          * to prune the lists
          *)
         fun mergeMoveList([], mv) = mv
           | mergeMoveList((m as MV{status,hicount,src,dst,...})::rest, mv) = 
              (case !status of
                BRIGGS_MOVE =>  
                  (* if we are changing a copy from v <-> w to uv <-> w
                   * makes sure we reset its trigger count, so that it
                   * will be tested next.
                   *)
                  (if coloringv then 
                      (status := GEORGE_MOVE; 
                       hicount := 0;
                       if debug then 
                          print ("New george "^show src^"<->"^show dst^"\n")
                       else ()
                      )
                   else ();
                   mergeMoveList(rest, m::mv)
                  )
              | GEORGE_MOVE => 
                  (* if u is colored and v is not, then the move v <-> w
                   * becomes uv <-> w where w is colored.  This can always
                   * be discarded.
                   *)
                  (if coloringv then mergeMoveList(rest, mv)
                   else mergeMoveList(rest, m::mv)
                  )
              | WORKLIST => mergeMoveList(rest, m::mv)
              | _ => mergeMoveList(rest, mv)
              )

         (* Form combined node; add the adjacency list of v to u *)
         fun union([], mv, fz, stack) = (mv, fz, stack)
           | union((t as NODE{color, degree, ...})::adj, 
                   mv, fz, stack) =
              (case !color of
                 (COLORED _ | SPILL_LOC _ | MEMREG _ | SPILLED) => 
                   (addEdge(t, u); union(adj, mv, fz, stack))
               | PSEUDO =>
                   (addEdge(t, u);
                    let 
                      val d = !degree
                    in 
                      if d = K then 
                        let val (mv, fz, stack) = lowDegree(t, mv, fz, stack)
                        in  union(adj, mv, fz, stack) 
                        end
                      else (degree := d - 1; union(adj, mv, fz, stack))
                    end
                   ) 
               | _ => union(adj, mv, fz, stack)
              )        
     in  vcol    := ALIASED u; 
                  (* combine the priority of both: 
                   * note that since the mvcost has been counted twice
                   * in the original priority, we substract it twice
                   * from the new priority.
                   *)
         pu    := !pu + !pv - mvcost - mvcost;
                  (* combine the def/use pts of both nodes.
                   * Strictly speaking, the def/use points of the move
                   * should also be removed.  But since we never spill
                   * a coalesced node and only spilling makes use of these
                   * def/use points, we are safe for now.  
                   *
                   * New comment: with spill propagation, it is necessary
                   * to keep track of the spilled program points.
                   *)
         if memoryCoalescingOn then
           (defsu := concat(!defsu, !defsv); 
            usesu := concat(!usesu, !usesv)
           )
         else ();
         case !ucol of
           PSEUDO => 
             (if !cntv > 0 then moveu := mergeMoveList(!movev, !moveu) 
              else (); 
              movev := []; (* XXX kill the list to free space *)
              cntu  := !cntu + !cntv
             )
         | _ => ()
         ;
         cntv := 0;

         let val removingHi = !degv >= K andalso (!degu >= K orelse coloringv) 
             (* Update the move count of the combined node *)
             val (mv, fz, stack) = union(!adjv, mv, fz, stack)
             val (mv, fz, stack) = 
                 decMoveCnt(u, 2, mvcost + mvcost, mv, fz, stack)  
             (* If either v or u are high degree then at least one high degree
              * node is removed from the neighbors of uv after coalescing
              *)
             val mv = if removingHi then enableMoves(!adju, mv) else mv
         in  coalesce(mv, fz, stack)
         end
     end

     (*
      *  COALESCE:
      *    Repeat coalescing and simplification until mv is empty.
      *)
     and coalesce(MV.EMPTY, fz, stack) = (fz, stack)
       | coalesce(MV.TREE(MV{src, dst, status, hicount, cost, ...}, _, l, r), 
                  fz, stack) = 
         let (* val _ = coalesce_count := !coalesce_count + 1 *)
             val u = chase src
             val v as NODE{color=ref vcol, ...} = chase dst
               (* make u the colored one *)
             val (u as NODE{number=u', color=ref ucol, ...},
                  v as NODE{number=v', color=ref vcol, ...}) = 
                     case vcol of
                       COLORED _ => (v, u)
                     | _         => (u, v)
             val _ = if debug then print ("Coalescing "^show u^"<->"^show v
                         ^" ("^r2s cost^")") else ()
             val mv = MV.merge(l, r)
             fun coalesceIt(status, v) = 
                (status := COALESCED;
                 if !spillFlag then trail := UNDO(v, status, !trail) else ()
                )
         in  if u' = v' then (* trivial move *)
                let val _ = if debug then print(" Trivial\n") else ()
                    val _ = coalesceIt(status, v)
                in  coalesce(decMoveCnt(u, 2, cost+cost, mv, fz, stack))
                end
             else 
                (case vcol of
                  COLORED _ => 
                      (* two colored nodes cannot be coalesced *)
                     (status := CONSTRAINED;
                      if debug then print(" Both Colored\n") else (); 
                      coalesce(mv, fz, stack))
                | _ =>
                  if member(u', v') then 
                     (* u and v interfere *)
                    let val _ = status := CONSTRAINED
                        val _ = if debug then print(" Interfere\n") else ();  
                        val (mv, fz, stack) = 
                                decMoveCnt(u, 1, cost, mv, fz, stack)
                    in  coalesce(decMoveCnt(v, 1, cost, mv, fz, stack)) end
                  else
                  case ucol of 
                    COLORED _ =>  (* u is colored, v is not *)
                    if safe(hicount, u', v) then 
                      (if debug then print(" Safe\n") else (); 
                       (*if tally then good_george := !good_george+1 else ();*)
                       coalesceIt(status, v);
                       combine(u, v, true, cost, mv, fz, stack)
                      ) 
                    else
                      ((* remove it from the move list *)
                       status := GEORGE_MOVE;
                       (*if tally then bad_george := !bad_george + 1 else ();*)
                       if debug then print(" Unsafe\n") else (); 
                       coalesce(mv, fz, stack)
                      )
                  |  _ => (* u, v are not colored *)
                   if conservative(hicount, u, v) then 
                      (if debug then print(" OK\n") else (); 
                       (*if tally then good_briggs := !good_briggs+1 else ();*)
                       coalesceIt(status, v);
                       combine(u, v, false, cost, mv, fz, stack)
                      )
                   else (* conservative test failed *)
                      ((* remove it from the move list *)
                       status := BRIGGS_MOVE;
                       (*if tally then bad_briggs := !bad_briggs + 1 else ();*)
                       if debug then print(" Non-conservative\n") else (); 
                       coalesce(mv, fz, stack)
                      )
                )
         end

      (* mark a node n as frozen: 
       *  Go thru all the moves (n, m), decrement the move count of m
       *  precondition: degree must be < K
       *                movecnt must be > 0
       *    node  -- the node to be frozen
       *    fz    -- a queue of freeze candidates
       *    stack -- stack of removed nodes
       *)
      fun markAsFrozen(
            node as NODE{number=me, degree, 
                         adj, movelist, movecnt as ref mc,...},
            fz, stack) = 
      let val _ = if debug then print("Mark as frozen "^i2s me^"\n")
                  else ()
          (* eliminate all moves, return a list of nodes that
           * can be simplified
           *)
          fun elimMoves([], simp) = simp
            | elimMoves(MV{status, src, dst, ...}::mvs, simp) =
              case !status of 
                WORKLIST => error "elimMoves"
              | (BRIGGS_MOVE | GEORGE_MOVE) => (* mark move as lost *)
                let val _ = status := LOST
                    val src as NODE{number=s,...} = chase src
                    val you = if s = me then chase dst else src
                in  case you of
                      NODE{color=ref(COLORED _),...} => 
                        elimMoves(mvs, simp)
                    | NODE{movecnt as ref c, degree, ...} => (* pseudo *)
                        (movecnt := c - 1; 
                         if c = 1 andalso !degree < K then 
                            elimMoves(mvs, you::simp)
                         else 
                            elimMoves(mvs, simp)
                        )
                end
              |  _   => elimMoves(mvs, simp)

          (* Note:
           * We are removing a high degree node, so try to enable all moves 
           * associated with its neighbors.
           *)

          val mv = if !degree >= K then enableMoves(!adj, MV.EMPTY) 
                   else MV.EMPTY

      in  if mc = 0 
          then simplify(node, mv, fz, stack)
          else 
             (movecnt := 0; 
              simplifyAll(node::elimMoves(!movelist, []), mv, fz, stack) 
             )
      end

      (*
       * FREEZE: 
       *   Repeat picking 
       *   a node with degree < K from the freeze list and freeze it.
       *   fz    -- queue of freezable nodes 
       *   stack -- stack of removed nodes
       *   undo  -- trail of coalesced moves after potential spill
       *)
      fun freeze(fz, stack) = 
      let fun loop(FZ.EMPTY, FZ.EMPTY, stack) = stack
            | loop(FZ.EMPTY, newFz, _) = error "no freeze candidate"
            | loop(FZ.TREE(node, _, l, r), newFz, stack) =
              let val fz = FZ.merge(l, r)
              in  case node of
                     (* This node has not been simplified 
                      * This must be a move-related node.
                      *)
                     NODE{color=ref PSEUDO, degree, ...} =>
                     if !degree >= K (* can't be frozen yet? *)
                     then 
                        ((*if tally then bad_freeze := !bad_freeze+1 else ();*)
                         loop(fz, FZ.add(node,newFz), stack))
                     else (* freeze node *)
                     let val _ = 
                            if debug then print("Freezing "^show node^"\n")
                            else ()
                         (*val _ = 
                            if tally then good_freeze := !good_freeze + 1
                            else ()*)
                         val (mv, fz, stack) = markAsFrozen(node, fz, stack)
                         val (fz, stack) = coalesce(mv, fz, stack)
                     in  ((* print("[freezing again "^
                           i2s(!blocked)^"]"); *)
                           loop(FZ.merge(fz, newFz), FZ.EMPTY, stack))
                     end
                  | _ => 
                    ((*if tally then bad_freeze := !bad_freeze + 1 else ();*)
                     loop(fz, newFz, stack))
              end
      in  (* print("[freezing "^i2s(!blocked)^"]"); *)
          loop(fz, FZ.EMPTY, stack)
      end

      (* 
       * Sort simplify worklist in increasing degree.
       * Matula and Beck suggests that we should always remove the
       * node with the lowest degree first.  This is an approximation of
       * the idea. 
       *)
      (*
      val buckets = A.array(K, []) : G.node list A.array
      fun sortByDegree nodes =
      let fun insert [] = ()
            | insert((n as NODE{degree=ref deg, ...})::rest) =
              (UA.update(buckets, deg, n::UA.sub(buckets, deg)); insert rest)
          fun collect(~1, L) = L
            | collect(deg, L) = collect(deg-1, concat(UA.sub(buckets, deg), L))
      in  insert nodes; 
          collect(K-1, [])
      end
       *)

      (*
       * Iterate over simplify, coalesce, freeze
       *)
      fun iterate{simplifyWkl, moveWkl, freezeWkl, stack} =
      let (* simplify everything *)
          val (mv, fz, stack) = 
                 simplifyAll((* sortByDegree *) simplifyWkl, 
                             moveWkl, freezeWkl, stack)
          val (fz, stack) = coalesce(mv, fz, stack)
          val stack       = freeze(fz, stack)
      in  {stack=stack}
      end
  in  {markAsFrozen=markAsFrozen, iterate=iterate}
  end

  (*
   * The main entry point for the iterated coalescing algorithm
   *)
  fun iteratedCoalescing G = 
  let val {iterate,...} = iteratedCoalescingPhases G
  in  iterate end


  (*
   * Potential Spill:
   *   Find some node on the spill list and just optimistically
   * remove it from the graph.
   *)
  fun potentialSpillNode (G as G.GRAPH{spillFlag,...}) = let
      val {markAsFrozen,...} = iteratedCoalescingPhases G
  in  fn {node, cost, stack} => 
      let val _ = spillFlag := true (* potential spill found *)
          val (mv, fz, stack) = markAsFrozen(node, FZ.EMPTY, stack)
      in  if cost < 0.0 then
             let val NODE{color, ...} = node in color := SPILLED end
          else ();
          {moveWkl=mv, freezeWkl=fz, stack=stack}
      end
  end



  (*
   *  SELECT: 
   *    Using optimistic spilling
   *)
  fun select(G as GRAPH{getreg, getpair, trail, firstPseudoR, stamp, 
                        spillFlag, proh, mode, ...}) {stack} =
  let 
      fun undoCoalesced END = ()
        | undoCoalesced(UNDO(NODE{number, color, ...}, status, trail)) =
          (status := BRIGGS_MOVE;
           if number < firstPseudoR then () else color := PSEUDO;
           undoCoalesced trail
          )
      val show = show G

      (* Fast coloring, assume no spilling can occur *)
      fun fastcoloring([], stamp) = ([], stamp)
        | fastcoloring((node as NODE{color, (* pair, *) adj, ...})::stack, 
                       stamp) =
          let (* set up the proh array *)
              fun neighbors [] = ()
                | neighbors(r::rs) = 
                  let fun mark(NODE{color=ref(COLORED c), ...}) =
                           (UA.update(proh, c, stamp); neighbors rs)
                        | mark(NODE{color=ref(ALIASED n), ...}) = mark n
                        | mark _ = neighbors rs
                  in  mark r end
              val _ = neighbors(!adj)
          in  color := COLORED(getreg{pref=[], proh=proh, stamp=stamp});
              fastcoloring(stack, stamp+1) 
          end

      (* Briggs' optimistic spilling heuristic *)
      fun optimistic([], spills, stamp) = (spills, stamp)
        | optimistic((node as NODE{color=ref(SPILLED), ...})::stack,  
                     spills, stamp) =
             optimistic(stack, node::spills, stamp)
        | optimistic((node as NODE{color as ref REMOVED, (* pair, *) adj, ...})::stack, 
                     spills, stamp) = let
           (* set up the proh array *)
              fun neighbors [] = ()
                | neighbors(r::rs) = 
                  let fun mark(NODE{color=ref(COLORED c), ...}) =
                           (UA.update(proh, c, stamp); neighbors rs)
                        | mark(NODE{color=ref(ALIASED n), ...}) = mark n
                        | mark _ = neighbors rs
                  in  mark r end
              val _ = neighbors(!adj)
              val spills = 
                  let val col = getreg{pref=[], proh=proh, stamp=stamp}
                  in  color := COLORED col; spills
                  end handle _ => node::spills
          in  optimistic(stack, spills, stamp+1) 
          end
        | optimistic _ = error "optimistic"

      (* Briggs' optimistic spilling heuristic, with biased coloring *)
      fun biasedColoring([], spills, stamp) = (spills, stamp)
        | biasedColoring((node as NODE{color=ref(SPILLED), ...})::stack,  
                         spills, stamp) =
             biasedColoring(stack, node::spills, stamp)
        | biasedColoring((node as NODE{color=ref(SPILL_LOC _), ...})::stack,  
                         spills, stamp) =
             biasedColoring(stack, node::spills, stamp)
        | biasedColoring((node as NODE{color=ref(MEMREG _), ...})::stack,  
                         spills, stamp) =
             biasedColoring(stack, node::spills, stamp)
        | biasedColoring(
             (node as NODE{number, color, adj, 
                           (* pair, *) movecnt, movelist,...})::stack, 
             spills, stamp) =
          let (* set up the proh array *)
              fun neighbors [] = ()
                | neighbors(r::rs) = 
                  (case chase r of
                     NODE{color=ref(COLORED c), ...} => 
                        (UA.update(proh, c, stamp); neighbors rs)
                   | _ => neighbors rs
                  )
              (* 
               * Look at lost moves and see if it is possible to 
               * color the move with the same color
               *)
              fun getPref([], pref) = pref
                | getPref(MV{status=ref(LOST | BRIGGS_MOVE | GEORGE_MOVE), 
                             src, dst, ...}::mvs, pref) =
                  let val src as NODE{number=s,...} = chase src
                      val other = if s = number then chase dst else src 
                  in  case other of
                        NODE{color=ref(COLORED c),...} => getPref(mvs, c::pref)
                      | _ => getPref(mvs, pref)
                  end
                | getPref(_::mvs, pref) = getPref(mvs, pref)

              val _    = neighbors(!adj)
              val pref = getPref(!movelist,[])
              val spills = 
                  let val col = getreg{pref=[], proh=proh, stamp=stamp}
                  in  color := COLORED col; spills
                  end handle _ => node::spills
          in  biasedColoring(stack, spills, stamp+1) end

      val (spills, st) = 
        if isOn(mode, BIASED_SELECTION)  then
          biasedColoring(stack, [], !stamp)
        else if !spillFlag then
          optimistic(stack, [], !stamp)
        else
          fastcoloring(stack, !stamp)

  in  stamp := st;
      case spills of
        [] => {spills=[]}
      | spills => 
        let fun undo [] = ()
              | undo(NODE{color,...}::nodes) = (color := PSEUDO; undo nodes)
        in  undo stack;
            undoCoalesced (!trail);
            trail := END;
            {spills=spills}
        end
  end (*select*)

  (*
   * Incorporate memory<->register moves into the interference graph
   *)
  fun initMemMoves(GRAPH{memMoves, ...}) =
  let fun move(NODE{movelist, movecost, ...}, mv, cost) = 
          (movelist := mv :: !movelist;
           movecost := cost + !movecost
          )

      fun setMove(dst, src, mv, cost) = 
          (move(dst, mv, cost); move(src, mv, cost))

      fun init [] = ()
        | init((mv as MV{dst, src, cost, ...})::mvs) = 
          let val dst as NODE{color=ref dstCol, ...} = chase dst
              val src as NODE{color=ref srcCol, ...} = chase src
          in
            if isFixedMem(srcCol) andalso isFixedMem(dstCol) then
              setMove(dst, src, mv, cost)
            else (case (srcCol, dstCol)
              of (PSEUDO, _) =>
                 if isFixedMem dstCol then setMove(dst, src, mv, cost) 
                 else error "initMemMoves"
               | (_, PSEUDO) => 
                 if isFixedMem srcCol then setMove(dst, src, mv, cost) 
                 else error "initMemMoves"
               | (COLORED _, _) => 
                 if isFixedMem dstCol then () else error "initMemMoves"
               | (_, COLORED _) => 
                 if isFixedMem srcCol then () else error "initMemMoves"
               | _  => error "initMemMoves"
             (*esac*));
              init mvs
          end
      val moves = !memMoves 
  in  memMoves := [];
      init moves
  end


  (*
   * Compute savings due to memory<->register moves
   *)
  fun moveSavings(GRAPH{memMoves=ref [], ...}) = (fn node => 0.0)
    | moveSavings(GRAPH{memMoves, bitMatrix, ...}) = 
  let exception Savings
      val savingsMap = IntHashTable.mkTable(32, Savings)
               : {pinned:int,cost:cost} IntHashTable.hash_table
      val savings = IntHashTable.find savingsMap
      val savings = fn r => case savings r of NONE => {pinned= ~1, cost=0.0}
                                            | SOME s => s
      val addSavings = IntHashTable.insert savingsMap
      val member     = BM.member(!bitMatrix)
      fun incSavings(u, v, c) =
      let val {pinned, cost} = savings u
      in  if pinned <> ~1 andalso v <> pinned orelse member(u, v)
          then ()
          else addSavings(u, {pinned=v, cost=cost + c + c})
      end
      fun computeSavings [] = ()
        | computeSavings(MV{dst, src, cost, ...}::mvs) =
          let val src as NODE{number=u, color=cu, ...} = chase src
              val dst as NODE{number=v, color=cv, ...} = chase dst
          in  case (!cu, !cv) 
              of (cu, PSEUDO) => 
                 if isFixedMem (cu) then incSavings(v, u, cost) else ()
               | (PSEUDO, cv) => 
                 if isFixedMem (cv) then incSavings(u, v, cost) else ()
               | _ => ();
              computeSavings mvs
          end
  in  computeSavings (!memMoves);
      fn node => #cost(savings node)
  end

  (*
   * Update the color of cells 
   *)
  fun updateCellColors(GRAPH{nodes, deadCopies, ...}) = 
  let fun enter(C.CELL{col, ...},c) = col := c
      fun cellOf(NODE{cell, ...}) = cell
      fun set(NODE{cell, color=ref(COLORED c),...}) = 
            enter(cell, C.MACHINE c)
        | set(NODE{cell, color=ref(ALIASED alias),...}) = 
            enter(cell, C.ALIASED(cellOf alias))
        | set(NODE{cell, color=ref(SPILLED),...}) = 
            enter(cell, C.SPILLED)
        | set(NODE{cell, color=ref(SPILL_LOC s),...}) = 
            enter(cell, C.SPILLED)
        | set(NODE{cell, color=ref(MEMREG(m, _)),...})= 
            enter(cell, C.MACHINE m)
        | set(NODE{cell, color=ref PSEUDO, ...}) = ()
        | set(_) = error("updateCellColors")
  in  IntHashTable.app set nodes
  end

  (*
   * Update aliases before spill rewriting.
   *)
  fun updateCellAliases(GRAPH{nodes, deadCopies, ...}) = 
  let fun enter(C.CELL{col, ...},c) = col := c
      fun cellOf(NODE{cell, ...}) = cell
      fun set(NODE{cell, color=ref(COLORED c),...}) = ()
        | set(NODE{cell, color=ref(ALIASED alias),...}) = 
            enter(cell, C.ALIASED(cellOf alias))
        | set(NODE{cell, color=ref(SPILLED),...}) = ()
        | set(NODE{cell, color=ref(SPILL_LOC s),...}) = ()
        | set(NODE{cell, color=ref(MEMREG _),...})= ()
        | set(NODE{cell, color=ref PSEUDO, ...}) = ()
        | set(_) = error("updateCellAliases")
  in  IntHashTable.app set nodes
  end

  fun markDeadCopiesAsSpilled(GRAPH{deadCopies, ...}) = 
  let fun enter(C.CELL{col, ...},c) = col := c
  in  case !deadCopies of
        [] => ()
      | dead => app (fn r => enter(r, C.SPILLED)) dead
  end 

  (*
   * Clear the interference graph, but keep the nodes 
   *)
  fun clearGraph(GRAPH{bitMatrix, maxRegs, trail, spillFlag, 
                       deadCopies, memMoves, copyTmps, ...}) =
  let val edges = BM.edges(!bitMatrix)
  in  trail      := END;
      spillFlag  := false;
      deadCopies := [];
      memMoves   := [];
      copyTmps   := [];
      bitMatrix  := BM.empty;
      bitMatrix  := G.newBitMatrix{edges=edges, maxRegs=maxRegs()}
  end 

  fun clearNodes(GRAPH{nodes,...}) =
  let fun init(_, NODE{pri, degree, adj, movecnt, movelist,
                       movecost, defs, uses, ...}) =
            (pri := 0.0; degree := 0; adj := []; movecnt := 0; movelist := [];
             defs := []; uses := []; movecost := 0.0)
  in  IntHashTable.appi init nodes
  end

  end (* local *)

end 

end (* local *)

(* 
 * This module implements the memory coalescing capability of the 
 * register allocator.
 *)
functor MemoryRA(Flowgraph : RA_FLOWGRAPH) : RA_FLOWGRAPH =
struct

  structure G = RAGraph
  structure A = Array
  structure W = Word

  val debug = false

  open G RACore

  val ra_spill_coal = MLRiscControl.mkCounter ("ra-spill-coalescing",
					       "RA spill coalesce count")
  val ra_spill_prop = MLRiscControl.mkCounter ("ra-spill-propagation",
					       "RA spill propagation count")

  local

  fun error msg = MLRiscErrorMsg.error("RACore", msg)
 
  fun concat([], b) = b
    | concat(x::a, b) = concat(a, x::b)

  fun chase(NODE{color=ref(ALIASED n),...}) = chase n
    | chase n = n

  in

  fun isOn(flag,mask) = Word.andb(flag,mask) <> 0w0

  fun isMemLoc(SPILLED) = true
    | isMemLoc(SPILL_LOC _) = true
    | isMemLoc(MEMREG _) = true
    | isMemLoc _ = false

  (*
   * Spill coalescing.
   * Coalesce non-interfering moves between spilled nodes, 
   * in non-increasing order of move cost.
   *)
  fun spillCoalescing(GRAPH{bitMatrix, ...}) = let
      val member = BM.member(!bitMatrix)
      val addEdge = BM.add(!bitMatrix)
  in 
    fn nodesToSpill => let
      (* Find moves between two spilled nodes *)
      fun collectMoves([], mv') = mv'
	| collectMoves(NODE{movelist, color, ...}::ns, mv') = let
	    fun ins([], mv') = collectMoves(ns, mv')
	      | ins(MV{status=ref(COALESCED | CONSTRAINED), ...}::mvs, mv') = 
		  ins(mvs, mv')
	      | ins((mv as MV{dst, src, ...})::mvs, mv') = let
		  val NODE{color=ref cd, number=nd, ...} = chase dst
		  val NODE{color=ref cs, number=ns, ...} = chase src
		in
		  if nd=ns then ins(mvs, mv')
		  else (case (cd, cs)
		    of (MEMREG _, MEMREG _) => ins(mvs, mv')
		     |  _ => 
			if isMemLoc cd andalso isMemLoc cs then
			  ins(mvs, MV.add(mv, mv'))
			else
			  ins(mvs, mv')
		   (*esac*))
		end
	  in 
	    if isMemLoc (!color) then ins(!movelist, mv')
	    else collectMoves(ns, mv')
	  end

      (* Coalesce moves between two spilled nodes *)
      fun coalesceMoves(MV.EMPTY) = ()
	| coalesceMoves(MV.TREE(MV{dst, src, cost, ...}, _, l, r)) =
	  let val dst as NODE{color=colorDst, ...} = chase dst
	      val src = chase src

	      (* Make sure that dst has not been assigned a spill location *)
	      val (dst, src) =
		case !colorDst of SPILLED => (dst, src) | _ => (src, dst)

	      val dst as NODE{number=d, color=colorDst, adj=adjDst, 
			      defs=defsDst, uses=usesDst,  ...} = dst
	      val src as NODE{number=s, color=colorSrc, adj=adjSrc, 
			      defs=defsSrc, uses=usesSrc, ...} = src

	      (* combine adjacency lists *)
	      fun union([], adjSrc) = adjSrc
		| union((n as NODE{color, adj=adjT, 
				   number=t, ...})::adjDst, adjSrc) = 
		  (case !color of
		     (SPILLED | MEMREG _ | SPILL_LOC _ | PSEUDO) =>
		       if addEdge(s, t) then 
			  (adjT := src :: !adjT; union(adjDst, n::adjSrc))
		       else union(adjDst, adjSrc)
		   | COLORED _ =>
		       if addEdge(s, t) then union(adjDst, n::adjSrc) 
		       else union(adjDst, adjSrc)
		   | _ => union(adjDst, adjSrc)
		  )

	      val mvs = MV.merge(l,r)

	      fun f() = 
		((* print(Int.toString d ^"<->"^Int.toString s^"\n");*)
		 ra_spill_coal := !ra_spill_coal + 1;
		  (* unify *)
		 colorDst := ALIASED src; 
		 adjSrc := union(!adjDst, !adjSrc);
		 defsSrc := concat(!defsDst, !defsSrc);
		 usesSrc := concat(!usesDst, !usesSrc);
		 coalesceMoves mvs)
	  in  
	      if d = s then coalesceMoves mvs
	      else (case !colorDst 
		of MEMREG _ => coalesceMoves mvs
	         | SPILLED => 
		    if member(d,s) then coalesceMoves mvs else f()
		 | SPILL_LOC loc => 
		    if member(d,s) then coalesceMoves mvs else f()
		 | _ => error "coalesceMoves"
               (*esac*))		      
	  end
     in coalesceMoves(collectMoves(nodesToSpill, MV.EMPTY))
     end
  end (*spillCoalesce*)

  (*
   * Spill propagation.
   * This one uses a simple local lookahead algorithm.
   *)
  fun spillPropagation(G as GRAPH{bitMatrix, memRegs, ...}) nodesToSpill =
  let val spillCoalescing = spillCoalescing G
      exception SpillProp
      val visited = IntHashTable.mkTable(32, SpillProp) 
                     : bool IntHashTable.hash_table
      val hasBeenVisited = IntHashTable.find visited
      val hasBeenVisited = fn r => case hasBeenVisited r of NONE => false
                                                          | SOME _ => true
      val markAsVisited = IntHashTable.insert visited
      val member = BM.member(!bitMatrix)  

      (* compute savings due to spill coalescing.
       * The move list must be associated with a colorable node.
       * The pinned flag is to prevent the spill node from coalescing
       * two different fixed memory registers.
       *)
      fun coalescingSavings
           (node as NODE{number=me, movelist, pri=ref spillcost, ...}) =
      let fun interferes(x,[]) = false
            | interferes(x,NODE{number=y, ...}::ns) = 
                 x = y orelse member(x,y) orelse interferes(x, ns)

          fun moveSavings([], pinned, total) = (pinned, total+total)
            | moveSavings(MV{status=ref(CONSTRAINED | COALESCED), ...}::mvs,
                          pinned, total) = 
                 moveSavings(mvs, pinned, total)
            | moveSavings(MV{dst, src, cost, ...}::mvs, pinned, total) =
              let val NODE{number=d, color=dstCol, ...} = chase dst
                  val NODE{number=s, color=srcCol, ...} = chase src

                  (* How much can be saved by coalescing with the memory 
                   * location x.
                   *)
                  fun savings(x) =
                      if member(d, s) then 
                        (if debug then print "interfere\n" else (); 
                         moveSavings(mvs, pinned, total))
                      else if x = ~1 then 
                        (if debug then print (Real.toString cost^"\n") else ();
                         moveSavings(mvs, pinned, total+cost))
                      else if pinned >= 0 andalso pinned <> x then 
                        (* already coalesced with another mem reg *)
                        (if debug then print "pinned\n" else ();
                         moveSavings(mvs, pinned, total))
                     else
                        (if debug then print (Real.toString cost^"\n") else ();
                         moveSavings(mvs, x, total+cost))

                 val _ = if debug then
                            (print("Savings "^Int.toString d^" <-> "^
                                              Int.toString s^"=")
                            ) else ()
              in  if d = s then
                    (if debug then print "0 (trivial)\n" else ();
                     moveSavings(mvs, pinned, total)
                    )
                 else
                    case (!dstCol, !srcCol) of
		      (SPILLED, PSEUDO) => savings(~1)
		    | (MEMREG(m, _), PSEUDO) => savings(m)
		    | (SPILL_LOC s, PSEUDO) => savings(~s)
		    | (PSEUDO, SPILLED) => savings(~1)
		    | (PSEUDO, MEMREG(m, _)) => savings(m)
		    | (PSEUDO, SPILL_LOC s) => savings(~s)
                    | _ => (if debug then print "0 (other)\n" else ();
                            moveSavings(mvs, pinned, total))
              end

          (* Find initial budget *)
          val _ = if debug then
                      print("Trying to propagate "^Int.toString me^
                            " spill cost="^Real.toString spillcost^"\n")
                  else ()
                  
          val (pinned, savings) = moveSavings(!movelist, ~1, 0.0)
          val budget = spillcost - savings
          val S      = [node]

          (* Find lookahead nodes *)
          fun lookaheads([], L) = L
            | lookaheads(MV{cost, dst, src, ...}::mvs, L) =
              let val dst as NODE{number=d, ...} = chase dst
                  val src as NODE{number=s, ...} = chase src
                  fun check(n, node as NODE{color=ref PSEUDO, ...}) = 
                      if n = me orelse member(n, me) then
                          lookaheads(mvs, L)       
                      else
                          add(n, node, L, []) 
                    | check _ = lookaheads(mvs, L)
                  and add(x, x', (l as (c,n' as NODE{number=y, ...}))::L, L') =
                       if x = y then 
                          lookaheads(mvs, (cost+c, n')::List.revAppend(L', L))
                       else add(x, x', L, l::L')
                    | add(x, x', [], L') = 
                          lookaheads(mvs, (cost, x')::L')
              in  if d = me then check(s, src) else check(d, dst)
              end

          (* Now try to improve it by also propagating the lookahead nodes *)
          fun improve([], pinned, budget, S) = (budget, S)
            | improve((cost, node as NODE{number=n, movelist, pri, ...})::L, 
                      pinned, budget, S) = 
              if interferes(n, S) then
                  (if debug then 
                      print ("Excluding "^Int.toString n^" (interferes)\n")
                   else ();
                  improve(L, pinned, budget, S))
              else
              let val (pinned', savings) = moveSavings(!movelist, pinned, 0.0)
                  val defUseSavings = cost+cost
                  val spillcost     = !pri
                  val budget' = budget - savings - defUseSavings + spillcost
              in  if budget' <= budget then 
                     (if debug then print ("Including "^Int.toString n^"\n")
                      else ();
                      improve(L, pinned', budget', node::S)
                     )
                  else
                     (if debug then print ("Excluding "^Int.toString n^"\n")
                      else ();
                      improve(L, pinned, budget, S))
              end

      in  if budget <= 0.0 then (budget, S)
          else improve(lookaheads(!movelist, []), pinned, budget, S)
      end

      (* Insert all spillable neighbors onto the worklist *)
      fun insert([], worklist) = worklist
        | insert((node as NODE{color=ref PSEUDO, number, ...})::adj, worklist) =
          if hasBeenVisited number 
          then insert(adj, worklist)
          else (markAsVisited (number, true);
                insert(adj, node::worklist))
        | insert(_::adj, worklist) = insert(adj, worklist)

      fun insertAll([], worklist) = worklist
        | insertAll(NODE{adj, ...}::nodes, worklist) = 
             insertAll(nodes, insert(!adj, worklist))

      val marker = SPILLED

      (* Process all nodes from the worklist *)
      fun propagate([], spilled) = spilled
        | propagate((node as NODE{color=ref PSEUDO, ...})::worklist, 
                    spilled) =
          let val (budget, S) = coalescingSavings(node)
              fun spillNodes([]) = ()
                | spillNodes(NODE{color, ...}::nodes) = 
                  (ra_spill_prop := !ra_spill_prop + 1;
                   color := marker; (* spill the node *)
                   spillNodes nodes
                  )
                    
          in  if budget <= 0.0
              then  (* propagate spill *)
                 (if debug then
                    (print("Propagating ");
                     app (fn NODE{number=x, ...} => print(Int.toString x^" "))
                         S;
                     print "\n") 
                  else ();
                  spillNodes S;
                  (* run spill coalescing *)
                  spillCoalescing S;
                  propagate(insertAll(S, worklist), List.revAppend(S,spilled))
                 )
              else
                 propagate(worklist, spilled)
          end
        | propagate(_::worklist, spilled) = 
            propagate(worklist, spilled)

      (* Initialize worklist *)
      fun init([], worklist) = worklist
        | init(NODE{adj, color=ref(c), ...}::rest, worklist) =
	   if isMemLoc (c) then 
	     init(rest, insert(!adj, worklist))
	   else 
	     init(rest, worklist)

      (* 
       * Iterate between spill coalescing and propagation 
       *)
      fun iterate(spillWorkList, spilled) = 
      let (* run one round of coalescing first *)
          val _ = spillCoalescing spillWorkList
          val propagationWorkList = init(spillWorkList, []) 
          (* iterate on our own spill nodes *)
          val spilled = propagate(propagationWorkList, spilled)
          (* try the memory registers too *)
          val spilled = propagate(!memRegs, spilled)
      in  spilled
      end

  in  iterate(nodesToSpill, nodesToSpill)
  end


  (*
   * Spill coloring.
   * Assign logical spill locations to all the spill nodes.
   *
   * IMPORTANT BUG FIX:
   *    Spilled copy temporaries are assigned its own set of colors and
   * cannot share with another other nodes.   They can share colors with 
   * themselves however.
   *
   * spillLoc is the first available (logical) spill location.
   *)

  fun spillColoring(GRAPH{spillLoc, copyTmps, mode, ...}) nodesToSpill = let
    val proh = A.array(length nodesToSpill, ~1)
    val firstColor= !spillLoc

    fun colorCopyTmps(tmps) = let
      fun spillTmp(NODE{color as ref(SPILLED), ...}, found) = 
	   (color := SPILL_LOC(firstColor); true)
	| spillTmp(_, found) = found
    in  
      if List.foldl spillTmp false tmps then
	(spillLoc := !spillLoc + 1; firstColor + 1)
      else firstColor
    end

    (* color the copy temporaries first *)
    val firstColor = 
      if isOn(mode, RACore.HAS_PARALLEL_COPIES) then
	colorCopyTmps(!copyTmps) 
      else firstColor

    fun selectColor([], _, lastLoc) = (spillLoc := lastLoc)
      | selectColor(NODE{color as ref(SPILLED), number, adj, ...}::nodes, 
		    currLoc, lastLoc) = 
        let
          fun neighbors(NODE{color=ref(SPILL_LOC s), ...}) = 
		A.update(proh, s - firstColor, number)
	    | neighbors(NODE{color=ref(ALIASED n), ...}) = neighbors n
	    | neighbors _ = ()

	  val _ =  app neighbors (!adj)

	  fun findColor(loc, startingPt) =
	    if loc = lastLoc then findColor(firstColor, startingPt)
	    else if A.sub(proh, loc-firstColor) <> number then (loc, lastLoc)
  	    else if loc  = startingPt then (lastLoc, lastLoc+1)
		 else findColor(loc+1, startingPt)

	  val (loc, lastLoc) = findColor(currLoc + 1, currLoc)

        in
	  color := SPILL_LOC(loc); (* mark with color *)
	  selectColor(nodes, loc, lastLoc)
        end
      | selectColor(_::nodes, currLoc, lastLoc) = 
	  selectColor(nodes, currLoc, lastLoc)
  in
    (* color the rest of the spilled nodes *)
    selectColor(nodesToSpill, firstColor, !spillLoc + 1)     
  end (* spillColoring *)

  end (* local *)
  
  structure F = Flowgraph

  open F 

  val SPILL_COALESCING     = 0wx100
  val SPILL_COLORING       = 0wx200
  val SPILL_PROPAGATION    = 0wx400

  (*
   * New services that also perform memory allocation 
   *)
  fun services f =
  let val {build, spill=spillMethod, 
           blockNum, instrNum, programPoint} = F.services f

      (* Mark nodes that are immediately aliased to mem regs;
       * These are nodes that need also to be spilled
       *)
      fun markMemRegs [] = ()
        | markMemRegs(NODE{number=r, 
			   color as ref(ALIASED
					(NODE{color=ref(col), ...})), ...}::ns) =
	   (case col of MEMREG _ => color := col | _ => ();
	    markMemRegs(ns))
        | markMemRegs(_::ns) = markMemRegs ns
 
      (*
       * Actual spill phase.  
       *   Perform the memory coalescing phases first, before doing an 
       *   actual spill.
       *)
      fun spillIt{graph = G as GRAPH{mode, ...}, nodes,
                  copyInstr, spill, spillSrc, spillCopyTmp,
                  reload, reloadDst, renameSrc, cellkind} =
      let 
	  val nodes = if isOn(mode,SPILL_PROPAGATION) then   
                          spillPropagation G nodes else nodes
          val _ = if isOn(mode,SPILL_COALESCING) then 
                     spillCoalescing G nodes else ()
          val _ = if isOn(mode,SPILL_COLORING) then 
                     spillColoring G nodes else ()
          val _ = if isOn(mode,SPILL_COALESCING+SPILL_PROPAGATION) 
                  then markMemRegs nodes else ()
      in  spillMethod
               {graph=G, nodes=nodes, copyInstr=copyInstr,
                spill=spill, spillSrc=spillSrc, spillCopyTmp=spillCopyTmp,
                reload=reload, reloadDst=reloadDst, 
                renameSrc=renameSrc, cellkind=cellkind} 
      end
   in  {build=build, spill=spillIt, programPoint=programPoint,
        blockNum=blockNum, instrNum=instrNum}
   end

end 

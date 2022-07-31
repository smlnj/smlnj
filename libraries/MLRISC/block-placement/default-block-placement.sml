(* default-block-placement.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * Place blocks in an order that respects the FALLSTHRU and BRANCH(false)
 * edges and is otherwise the order of block generation.
 *)

functor DefaultBlockPlacement (CFG : CONTROL_FLOW_GRAPH) : BLOCK_PLACEMENT =
  struct

    structure CFG=CFG
    structure G = Graph

  (* flags *)
    val dumpBlocks = MLRiscControl.mkFlag (
	  "dump-block-list",
	  "whether block list is shown")
    val dumpCFG = MLRiscControl.mkFlag (
	  "dump-cfg-after-placement",
	  "whether CFG is shown after block placement")
    val dumpStrm = MLRiscControl.debug_stream

    fun blockToString (id', CFG.BLOCK{id, ...}) =
	  concat["<", Int.toString id', ":", Int.toString id, ">"]

    fun error msg = MLRiscErrorMsg.error ("DefaultBlockPlacement", msg)

    fun blockPlacement (cfg as G.GRAPH graph) = let
	  val placed = Array.array(#capacity graph (), false)
	  fun isMarked id = Array.sub(placed, id)
	  fun mark id = Array.update(placed, id, true)
	  fun assertNotMarked id = if isMarked id
		then error "conflicting placement constraints"
		else ()
	(* special case the entry and exit blocks *)
	  fun getBlk id = (id, #node_info graph id)
	  val entry = CFG.entry cfg
	  val exit = CFG.exit cfg
	  val _ = mark(#1 exit)	(* we place exit at the end *)
	(* return true if the edge must connect adjacent nodes *)
	  fun adjEdge (_, _, CFG.EDGE{k=CFG.FALLSTHRU, ...}) = true
	    | adjEdge (_, _, CFG.EDGE{k=CFG.BRANCH false, ...}) = true
	    | adjEdge _ = false
	  val findAdjEdge = List.find adjEdge
	(* place nodes by assuming that the initial order is close to
	 * correct.
	 *)
	  fun placeNodes ([], l) = List.rev (exit::l)
	    | placeNodes ((nd1 as (id1, b1))::r1, l) = if isMarked id1
		then placeNodes (r1, l)
		else (
		  case r1
		   of [] => List.rev (exit::nd1::l)
		    | (nd2 as (id2, b2))::r2 => if isMarked id2
			  then placeNodes(nd1::r2, l)
			  else (
			  (* Here we know that both nd1 and nd2 have not been
			   * placed.  We need to check for placement constraints
			   * in nd1's out edges and nd2's in edges.
			   *)
			    mark id1;
			    case findAdjEdge (#out_edges graph id1)
			     of NONE => let
				  fun pushPredChain (nd as (id, _), r) = (
					case findAdjEdge (#in_edges graph id)
					 of NONE => nd::r
					  | SOME(src, _, _) => (
					      assertNotMarked src;
					      pushPredChain (getBlk src, nd::r))
					(* end case *))
				  in
				    placeNodes (pushPredChain(nd2, r2), nd1::l)
				  end
			      | SOME(_, dst, _) => if (dst = id2)
				  then placeNodes(r1, nd1::l)
				  else (
				    assertNotMarked dst;
				    placeNodes (getBlk dst::r1, nd1::l))
			    (* end case *))
		    (* end case *))
	  val blocks = placeNodes (entry :: #nodes graph (), [])
	  in
	    if !dumpBlocks
	      then let
		fun say s = TextIO.output(!dumpStrm, s)
		in
		  say "Block placement order:\n";
		  List.app
		    (fn b => say(concat["  ", blockToString b, "\n"]))
		      blocks
		end
	      else ();
	    if !dumpCFG
	      then let
		val prBlock = CFG.dumpBlock (!dumpStrm, cfg)
		in
		  TextIO.output(!dumpStrm, "[ after block placement ]\n");
		  List.app prBlock blocks
		end
	      else ();
	    (cfg, blocks)
	  end

  end

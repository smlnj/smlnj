(* check-placement-fn.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * This functor implements code to check that a block placement is
 * correct.
 *)

functor CheckPlacementFn (

    structure CFG : CONTROL_FLOW_GRAPH
    structure InsnProps : INSN_PROPERTIES
      where I = CFG.I

  ) : sig

    structure CFG : CONTROL_FLOW_GRAPH

    val check : (CFG.cfg * CFG.node list) -> unit

  end = struct

    structure CFG=CFG
    structure IP = InsnProps
    structure G = Graph

    val dumpStrm = MLRiscControl.debug_stream

    fun blockToString (id', CFG.BLOCK{id, ...}) =
	  concat["<", Int.toString id', ":", Int.toString id, ">"]

    fun check (cfg as G.GRAPH graph, blocks) = let
	(* an array that maps from block id to position in the placement (starting
	 * from 1).  Nodes that have no placement have index ~1.
	 *)
	  val order = let
		val arr = Array.array(#capacity graph (), ~1)
		fun init ((id, _), i) = (Array.update(arr, id, i); i+1)
		in
		  ignore (List.foldl init 1 blocks);
		  arr
		end
	  fun adjacentNodes (a, b) = (Array.sub(order, a) + 1 = Array.sub(order, b))
	  val anyErrors = ref false
	(* report an error and dump the cfg *)
	  fun reportError msg = let
		fun say s = TextIO.output(!dumpStrm, s)
		in
		  if !anyErrors
		    then ()
		    else (
		      anyErrors := true;
		      say "********** Bogus block placement **********\n");
		  say(concat("** "::msg))
		end
	  fun reportNotAdjacent (src, dst) = let
		fun b2s id = concat[
			Int.toString id, "@", Int.toString(Array.sub(order, id))
		      ]
		in
		  reportError [
		      "Blocks ", b2s src, " and ", b2s dst,
		      " are not adjacent\n"
		    ]
		end
	(* return true if the edge must connect adjacent nodes *)
	  fun adjEdge (CFG.EDGE{k=CFG.FALLSTHRU, ...}) = true
	    | adjEdge (CFG.EDGE{k=CFG.BRANCH false, ...}) = true
	    | adjEdge _ = false
	(* entry and exit nodes *)
	  val entryId = CFG.entryId cfg
	  val exitId = CFG.exitId cfg
	(* get the jump targets from the last instruction in a block *)
	  fun getJumpTargets id = (case #node_info graph id
		 of CFG.BLOCK{insns=ref(i::_), ...} => (
		      case IP.instrKind i
		       of IP.IK_JUMP => IP.branchTargets i
			| _ => []
		      (* end case *))
		  | _ => []
		(* end case *))
	(* check that FALLSTHRU and BRANCH false edges connect adjacent nodes *)
	  fun chkEdge (src, dst, CFG.EDGE{k, ...}) = (case k
		 of (CFG.FALLSTHRU | CFG.BRANCH false) =>
		      if adjacentNodes(src, dst)
			then ()
			else reportNotAdjacent(src, dst)
		  | CFG.BRANCH true => (case getJumpTargets src
		       of [IP.FALLTHROUGH, IP.LABELLED _] => ()
			| [IP.LABELLED _, IP.FALLTHROUGH] => ()
			| _ => reportError[
			      "Block ", Int.toString src,
			      " doesn't end in conditiona branch\n"
			    ]
		      (* end case *))
		  | CFG.JUMP => (case getJumpTargets src
		       of [IP.LABELLED _] => ()
			| _ => reportError[
			      "Block ", Int.toString src, " doesn't end in jump\n"
			    ]
		      (* end case *))
		  | CFG.ENTRY => if (src <> entryId)
		      then reportError[
			  "Block ", Int.toString src, " is not ENTRY\n"
			]
		      else ()
		  | CFG.EXIT => if (dst <> exitId)
		      then reportError[
			  "Block ", Int.toString dst, " is not EXIT\n"
			]
		      else (case getJumpTargets src
			 of [IP.ESCAPES] => ()
			  | _ => reportError [
				"Block ", Int.toString src,
				"doesn't end in an escaping jump\n"
			      ]
			(* end case *))
		  | _ => () (* no checking for SWITCH or FLOWSTO *)
		(* end case *))
	  in
	    #forall_edges graph chkEdge;
	    if (!anyErrors)
	      then let
		fun say s = TextIO.output(!dumpStrm, s)
		val prBlock = CFG.dumpBlock (!dumpStrm, cfg)
		in
		  say "Block placement order:\n";
		  List.app
		    (fn b => say(concat["  ", blockToString b, "\n"]))
		      blocks;
		  TextIO.output(!dumpStrm, "[ control-flow-graph ]\n");
		  List.app prBlock blocks;
		  say "**********\n";
		  MLRiscErrorMsg.error ("CheckPlacementFn", "bogus placement")
		end
	      else ()
	  end

  end

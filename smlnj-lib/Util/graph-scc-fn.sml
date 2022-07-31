(* graph-scc-fn.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 *   Calculate strongly-connected components of directed graph.
 *   The graph can have nodes with self-loops.
 *
 * author: Matthias Blume
 *)

functor GraphSCCFn (Nd: ORD_KEY) :> GRAPH_SCC where Nd = Nd =
  struct
    structure Nd = Nd

    type node = Nd.ord_key

    structure Map = RedBlackMapFn (Nd)

    datatype component
      = SIMPLE of node
      | RECURSIVE of node list

    fun eq x y = (Nd.compare(x, y) = EQUAL)

    fun topOrder' { roots, follow } = let

	fun getNode (n, nm as (npre, m)) = (
	      case Map.find (m, n)
	       of NONE => let
		    val r = { pre = npre, low = ref npre }
		    val m' = Map.insert (m, n, r)
		    in
		      ((npre + 1, m'), r)
		    end
		| SOME r => (nm, r)
	      (* end case *))

	fun component (x, []) =
	    if List.exists (eq x) (follow x) then RECURSIVE [x]
	    else SIMPLE x
	  | component (x, xl) = RECURSIVE (x :: xl)

	(* depth-first search in continuation-passing, state-passing style *)
	fun dfs args = let

	    (* the nodemap represents the mapping from nodes to
	     * pre-order numbers and low-numbers. The latter are ref-cells.
	     * nodemap also remembers the next available pre-order number.
	     * The current node itself is not given as an argument.
	     * Instead, it is represented by grab_cont -- a function
	     * that "grabs" a component from the current stack and then
	     * continues with the regular continuation.  We do it this
	     * way to be able to handle the topmost virtual component --
	     * the one whose sole element is the virtual root node. *)
	    val { follow_nodes, grab_cont,
		  node_pre, node_low, parent_low, nodemap,
		  stack, sccl, nograb_cont } = args

	    (* loop over the follow-set of a node *)
	    fun loop (tn :: tnl) (nodemap as (npre, theMap), stack, sccl) =
		let val is_tn = eq tn
		in
		    case Map.find (theMap, tn) of
			SOME{ pre = tn_pre, low = tn_low } => let
			    val tl = !tn_low
			in
			    if tl < (!node_low) andalso
			       List.exists is_tn stack then
				node_low := tl
			    else ();
			    loop tnl (nodemap, stack, sccl)
			end
                      | NONE =>let
			    (* lookup failed -> tn is a new node *)
			    val tn_pre = npre
			    val tn_low = ref npre
			    val npre = npre + 1
			    val theMap =
				Map.insert (theMap, tn,
					    { pre = tn_pre, low = tn_low })
			    val nodemap = (npre, theMap)
			    val tn_nograb_cont = loop tnl
			    fun tn_grab_cont (nodemap, sccl) = let
				fun grab (top :: stack, scc) =
				    if eq tn top then
					tn_nograb_cont
					    (nodemap, stack,
					     component (top, scc) :: sccl)
				    else
					grab (stack, top :: scc)
				  | grab _ =
				    raise Fail "scc:grab: empty stack"
			    in
				grab
			    end
			in
			    dfs { follow_nodes = follow tn,
				  grab_cont = tn_grab_cont,
				  node_pre = tn_pre, node_low = tn_low,
				  parent_low = node_low,
				  nodemap = nodemap,
				  stack = tn :: stack,
				  sccl = sccl,
				  nograb_cont = tn_nograb_cont }
			end
		end
	      | loop [] (nodemap, stack, sccl) =
		let val nl = !node_low
		in
		    if nl = node_pre then
			grab_cont (nodemap, sccl) (stack, [])
		    else
			((* propagate node_low up *)
			 if nl < (!parent_low) then parent_low := nl else ();
			 (* `return' *)
			 nograb_cont (nodemap, stack, sccl))
		end
	in
	    loop (rev follow_nodes) (nodemap, stack, sccl)
	end
	fun top_grab_cont (nodemap, sccl) ([], []) = sccl
	  | top_grab_cont _ _ = raise Fail "scc:top_grab: stack not empty"
    in
	dfs { follow_nodes = roots,
	      grab_cont = top_grab_cont,
	      node_pre = 0,
	      node_low = ref 0,	    (* low of virtual root *)
	      parent_low = ref 0,   (* low of virtual parent of virtual root *)
	      nodemap = (1, Map.empty),
	      stack = [],
	      sccl = [],
	      nograb_cont = fn (_, _, _) => raise Fail "scc:top_nograb_cont" }
    end

    fun topOrder { root, follow } =
	topOrder' { roots = [root], follow = follow }
  end

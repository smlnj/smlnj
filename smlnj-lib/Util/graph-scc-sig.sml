(* graph-scc-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 *   Calculate strongly-connected components of directed graph.
 *   The graph can have nodes with self-loops.
 *
 * author: Matthias Blume
 *)

signature GRAPH_SCC =
  sig

    structure Nd : ORD_KEY

    type node = Nd.ord_key

    datatype component
      = SIMPLE of node			(* singleton, no self-loop *)
      | RECURSIVE of node list

    (* given a list of root nodes and a follow function, return the
     * topologically sorted list of strongly-connected components.
     * Assuming that the roots list does not contain duplicates, then
     * the first component in the result will contain the first root.
     *)
    val topOrder': { roots: node list, follow: node -> node list }
		   -> component list

    (* for backward compatibility;
     * AXIOM: topOrder{root,follow}==topOrder'{roots=[root],follow=follow}
     *)
    val topOrder : { root: node, follow: node -> node list }
		   -> component list

  end

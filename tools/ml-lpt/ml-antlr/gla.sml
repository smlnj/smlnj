(* gla.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * An interpretation of Parr's Grammar Lookahead Automata.
 * MORE COMMENTS COMING SOON
 *)

structure GLA :>
  sig 

    type gla
    type prepath = Token.Set.set list

    val mkGLA : LLKSpec.grammar -> gla
    val lookK : (gla * LLKSpec.prod * prepath * Int.int) -> Token.Set.set
    val lookKFollow : (gla * LLKSpec.nonterm * prepath * Int.int) -> Token.Set.set

    val dumpGraph : (string * LLKSpec.grammar * gla) -> unit

  end = struct

    structure S = LLKSpec
    structure NMap = Nonterm.Map
    structure PMap = Prod.Map
    structure TSet = Token.Set

    datatype node_type
      = Initial 
      | Final 
      | Normal

    datatype node = Node of {
        id : Int.int,
	edges : edge list ref,
	ty : node_type,
	nt : S.nonterm
      }
    and edge = Edge of (label * node) 
    and label     
      = Epsilon 
      | Tok of Token.token 
      | Call of S.nonterm

    datatype gla = GLA of {
        nodes : node list,
	prodMap : node PMap.map,
	startOf : S.nonterm -> node,
	finEBNF : S.nonterm -> node
      }

    type prepath = Token.Set.set list

    fun isFinal (Node {ty = Final, ...}) = true
      | isFinal _ = false
    fun isInitial (Node {ty = Initial, ...}) = true
      | isInitial _ = false
    fun edgesOf (Node {edges, ...}) = !edges
    fun idOf (Node {id, ...}) = id
    fun ntOf (Node {nt, ...}) = nt
    fun sameNode (n1, n2) = (idOf n1 = idOf n2)

    structure Ord = 
      struct
	type ord_key = node
	fun compare (Node {id = id1, ...}, Node {id = id2, ...}) = Int.compare (id1, id2)
      end

    structure SCC = GraphSCCFn(Ord)
    structure NodeSet = RedBlackSetFn (Ord)

  (* Build an NFA for the grammar *)
    fun mkGLA grm = let
          val S.Grammar {toks, nterms, prods, eof, startnt, ...} = grm
	  val nextId = ref 0
	  val nodes : node list ref = ref []
	  fun newNode (nt, ty) = let 
	        val _ = nextId := !nextId + 1
		val n = Node {id = !nextId, edges = ref [], ty = ty, nt = nt}
		in
	          nodes := n :: (!nodes);
		  n
                end
	  fun makeNTs (f) = List.foldl NMap.insert' NMap.empty
  		(ListPair.zip (nterms, List.map f nterms))
	(* create initial, final state for each nonterminal *)
	  val ntMap     = makeNTs (fn nt => newNode (nt, Initial))
	  val finalMap  = makeNTs (fn nt => newNode (nt, Final))
	  fun mkOf map nt = valOf (NMap.find (map, nt)) 
	  val startOf = mkOf ntMap
	  val finalOf = mkOf finalMap
	  val finEBNF = ref NMap.empty
	  fun addFinEBNF (nt, n) = (finEBNF := NMap.insert (!finEBNF, nt, n))
	  fun addEdge (Node {edges, ...}, target, label) = (
		edges := (Edge (label, target))::(!edges);
		case label
		 of Call nt => 
		    (* add follow edge *)
		    addEdge (finalOf nt, target, Epsilon)
		  | _ => ())
	(* iterate through the items of a production, creating both nodes
	 * and edges.
	 *)
	  fun iter (nt, lastNode, []) = addEdge (lastNode, finalOf nt, Epsilon)
	    | iter (nt, lastNode, itm::itms) = let
		val n = newNode (nt, Normal)
	        in
		  (case Item.sym itm
		    of S.TOK t => 
		         addEdge (lastNode, n, Tok t)
		     | S.NONTERM (nt, _) =>
			 addEdge (lastNode, n, Call nt)
		     | S.CLOS nt => let 
			 val nback = newNode (nt, Normal)
		         in
			   addEdge (lastNode, n, Epsilon);
			   addEdge (lastNode, nback, Call nt);
			   addEdge (nback, lastNode, Epsilon);
			   addFinEBNF (nt, n)
		         end
		     | S.POSCLOS nt => let
			 val nback = newNode (nt, Normal)
		         in
			   addEdge (lastNode, nback, Call nt);
		           addEdge (nback, n, Epsilon);
			   addEdge (nback, lastNode, Epsilon);
			   addFinEBNF (nt, n)
		         end
		     | S.OPT nt => (
			 addEdge (lastNode, n, Epsilon);
			 addEdge (lastNode, n, Call nt);
			 addFinEBNF (nt, n))
		   (* end case *));
		  iter (nt, n, itms)
		end
	(* process a production *)
	  fun doProd prod = let
		val lhs = Prod.lhs prod
	        val n = newNode (lhs, Normal)
		in
	          iter (lhs, n, Prod.items prod);
	          addEdge (startOf lhs, n, Epsilon);
		  (prod, n)
		end
	  val prodMap = foldl PMap.insert' PMap.empty (map doProd prods)
	  val eofNode = newNode (startnt, Normal)
	  in
            addEdge (eofNode, eofNode, Tok eof);
	    addEdge (finalOf startnt, eofNode, Epsilon);
            GLA {nodes = !nodes, prodMap = prodMap, 
		 startOf = startOf, finEBNF = mkOf (!finEBNF)}
	  end

    fun unionAll sets = foldl TSet.union TSet.empty sets

  (* Traverse the GLA, finding all possible tokens that can appear
   * k tokens ahead of node n.  The stack holds sharper follow information
   * when available.  See the implementation notes for more detail.
   *)
    fun look (startOf, n, k, stack, prePath, leftSet, acc) = let
	  val leftSet' = NodeSet.add (leftSet, n)
	  val lookRepl = (fn (tokSet, leftSet) => (tokSet, leftSet')) o look
          fun follow (Edge (Tok t, n'), (tokSet, leftSet)) = 
	        if k = 1 then (TSet.add (tokSet, t), leftSet)
		else if TSet.member(hd prePath, t) then
		  lookRepl (startOf, n', k - 1, stack, 
			    tl prePath, NodeSet.empty, tokSet)
		else (tokSet, leftSet)
	    | follow (Edge (Epsilon, n'), (tokSet, leftSet)) = 
	        look (startOf, n', k, stack, prePath, leftSet, tokSet)
	    | follow (Edge (Call nt, n'), (tokSet, leftSet)) = 
	        lookRepl (startOf, startOf nt, k, 
			  n'::stack, prePath, leftSet, tokSet)
          in
            if NodeSet.member (leftSet, n) then
	      if isInitial n then (
		Err.leftRecur (Nonterm.qualName (ntOf n));
		(TSet.empty, leftSet'))
	      else 
		(acc, leftSet')
            else if isFinal n andalso length stack > 0 then 
	      look (startOf, hd stack, k, tl stack, prePath, leftSet', acc)
	    else 
	      foldl follow (acc, leftSet') (edgesOf n)
          end

  (* compute lookahead set k tokens into the given production *)
    fun lookK (GLA {prodMap, startOf, ...}, prod, prePath, k) = 
	  #1 (look (startOf, valOf (PMap.find (prodMap, prod)), k, [], 
		    prePath, NodeSet.singleton (startOf (Prod.lhs prod)), 
	            TSet.empty))

  (* compute lookahead set k tokens AFTER the given EBNF subrule *)
    fun lookKFollow (GLA {finEBNF, startOf, ...}, nt, prePath, k) =
	  #1 (look (startOf, finEBNF nt, k, [], prePath, NodeSet.empty, TSet.empty))

  (* the below code dumps the graph to a .DOT file *)
  (* TODO: move this to its own module *)

    datatype attribute = ATTR of string * string
    datatype d_node = NODE of string * attribute list
    datatype di_edge = EDGE of string * string * attribute list
    datatype di_graph = GRAPH of string * d_node list * di_edge list * attribute list

    fun replBS str =
	  String.translate 
	    (fn #"\\" => "\\\\" | c => String.str c) 
	    str

    fun writeGraph (out, graph) = let
        (* output a string *)
          fun wr s = TextIO.output (out, s)
        (* output a string list *)
	  fun wrs ss = wr (String.concat ss)
        (* indent to some level *)
	  fun wrIndent 0 = ()
	    | wrIndent lvl = (wr "  "; wrIndent (lvl - 1))
        (* apply output functions, indenting each time *)
	  fun app indent f list = 
	        List.app (fn x => (wrIndent indent; f x)) list
          fun wrAttr (ATTR (name, value)) = wrs ([
		  "[ ", name, " = \"", value, "\" ]", "\n"
	        ])
	  fun wrNode (NODE (name, atts)) = 
	        (wr name;
		 wr "\n";
		 app 2 wrAttr atts)
	  fun wrEdge (EDGE (no1, no2, atts)) =
	        (wrs ([no1, " -> ", no2, "\n"]);
		 app 2 wrAttr atts)
	  fun wrGraphAttr attr = 
	        (wr "graph\n";
		 wrIndent 2;
		 wrAttr attr)
	  fun wrGraph (GRAPH (name, nodes, edges, atts)) = 
	        (wrs (["digraph ", name, " {\n"]);
		 app 1 wrGraphAttr atts;
		 app 1 wrNode nodes;
		 app 1 wrEdge edges;
		 wr "}")
          in wrGraph graph
          end

    fun dumpGraph (fname, grm, gla) = let
          val S.Grammar {toks, nterms, prods, eof, ...} = grm
	  val GLA {startOf, nodes, ...} = gla
	  val seen = ref NodeSet.empty
	  fun addSeen n = seen := NodeSet.add (!seen, n)
	  fun idOf (Node {id, ...}) = id
	  fun nameOf n = Int.toString (idOf n)
	  fun doNode n = 
	        if NodeSet.member(!seen, n) then
		  []
		else (
		  addSeen n;
		  if isFinal n then []
		  else List.concat (map (doEdge n) (edgesOf n)))
(*
	  and doEdge n (Edge (Call nt, _)) =
		[EDGE (nameOf n, nameOf (startOf nt), [])]
	    | doEdge n (Edge (lbl, n')) =
		(EDGE (nameOf n, nameOf n', [
		   ATTR ("label", case lbl
				   of Epsilon => ""
				    | Tok t => Token.name t)])) 
		:: (doNode n')
*)
	  and doEdge n (Edge (lbl, n')) =
		(EDGE (nameOf n, nameOf n', [
		   ATTR ("label", case lbl
				   of Epsilon => ""
				    | Tok t => Token.name t
				    | Call nt => Nonterm.qualName nt)])) 
		:: (doNode n')
	  val edges = List.concat (map (doNode o startOf) nterms)
	  fun doNT nt = NODE (nameOf (startOf nt), [ATTR ("label", Nonterm.qualName nt), ATTR ("shape", "box")])
	  fun doFinal node = if isFinal node then
			       SOME (NODE (nameOf node, 
					   [ATTR ("shape", "doublecircle")]))
			     else NONE
	  val nodes = (map doNT nterms) @ (List.mapPartial doFinal nodes)
	  val graph = GRAPH ("dump", nodes, edges, [ATTR ("rankdir", "LR")])
	  val out = TextIO.openOut (fname ^ ".dot")
          in 
            TextIO.output (TextIO.stdErr, concat[" writing ", fname, ".dot\n"]);
            writeGraph (out, graph)
	    before TextIO.closeOut out
          end

  end

(* dot-output.sml
 *
 * COPYRIGHT (c) 2005
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Produce a .dot file from a DFA.
 * (See www.graphviz.org for details about DOT)
 *)

structure DotOutput : OUTPUT =
  struct

    structure RE = RegExp
    structure Lex = LexGen
    structure LO = LexOutputSpec

    datatype attribute = ATTR of string * string
    datatype node = NODE of string * attribute list
    datatype di_edge = EDGE of string * string * attribute list
    datatype di_graph = GRAPH of string * node list * di_edge list * attribute list

    fun replBS str =
	  String.translate
	    (fn #"\\" => "\\\\" | #"\"" => "\\\"" | c => String.str c)
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

    fun mkGraph states = let
        (* node id -> node name *)
	  fun name id = "Q" ^ Int.toString id
          fun mkNode (LO.State{id, label, final = [], ...}) =
	        NODE (name id, [ATTR ("shape", "circle")])
	    | mkNode (LO.State{id, label, final = i::_, ...}) =
	        NODE (name id,
		  [ATTR ("shape", "doublecircle"),
		   ATTR ("label", (name id) ^ "/" ^ (Int.toString i))])
	  fun mkEdge fromID (symSet, LO.State{id, ...}) =
	        EDGE (name fromID, name id,
		  [ATTR ("label", replBS (RE.toString (RE.mkSymSet symSet)))])
	  fun mkEdges (LO.State{id, next, ...}) =
	        List.map (mkEdge id) (List.rev (!next))
	  fun mkRule (i, re) = String.concat (
		["Rule ",
		 Int.toString i,
		 ": ",
		 replBS (RE.toString re),
		 "\\n"])
        (* node for input REs *)
	  fun mkRules res =
	        NODE ("Rules",
		  [ATTR ("label", Vector.foldli
				    (fn (i, r, s) => s ^ (mkRule (i, r)))
				    "" res),
		   ATTR ("shape", "plaintext"),
		   ATTR ("fontname", "Courier")])
	  val nodes' = List.map mkNode states
	  val nodes = nodes'
	  val edges = List.concat (List.map mkEdges states)
          in GRAPH ("DFA", nodes, edges,
	       [ATTR ("size", "7,10"),
		ATTR ("rankdir", "LR")])
          end

    fun output (spec, fname) = let
          val LO.Spec {dfa, startStates, ...} = spec
	  val out = TextIO.openOut (fname ^ ".dot")
	  val graph = mkGraph dfa
          in
            print (" writing " ^ fname ^ ".dot\n");
            writeGraph (out, graph)
	    before TextIO.closeOut out
          end

  end

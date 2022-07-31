(* compute-freqs-fn.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies.
 *
 * Compute block and edge weights (frequencies) from edge probabilities.
 * This algorithm uses symbolic simplification of the frequency equations.
 * It handles unstructured loops.
 *)

functor ComputeFreqsFn (
    structure CFG : CONTROL_FLOW_GRAPH
  ) : COMPUTE_EXECUTION_FREQUENCIES =
  struct

    structure CFG = CFG
    structure Prob = Probability
    structure F = Format

  (* flags *)
    val dumpFreqs = MLRiscControl.mkFlag (
	  "dump-frequencies",
	  "when true, block and edge frequencies are output")
    val dumpCFG = MLRiscControl.mkFlag (
	  "dump-cfg-after-frequencies",
	  "when true, the CFG is output after frequency computation")
    fun pr s = TextIO.output(!MLRiscControl.debug_stream, s)
    fun prf (fmt, items) = pr(F.format fmt items)

  (* Complete edge probabilities; we use the edge weights to store this
   * information.
   *)
    structure CompleteProbs = CompleteProbsFn (
	structure CFG = CFG
	fun recordProb (CFG.EDGE{w, ...}, p) = (w := p))

    fun getProb (CFG.EDGE{w, ...}) = !w

  (* fudge factor for infinite loops. *)
    val epsilon = 1.0e~6

  (***** Representation of equations *****)
    type var = Graph.node_id
    datatype def = Unknown | Sum of sum
    withtype term = (real * var)
	 and sum = {terms : term list, c : real}

    val zero = {c = 0.0, terms = []}
    val one = {c = 1.0, terms = []}

  (* multiply a term by a scalar *)
    fun scale (coeff : real) (a, x) = (coeff*a, x)

    fun compute (cfg as Graph.GRAPH methods) = let
	  val {in_edges, out_edges, node_info, capacity, ...} = methods
	  val defs = Array.array(capacity(), Unknown)
	  fun getVar id = Array.sub(defs, id)
	  fun setVar (id, s) = Array.update(defs, id, s)
	(* if a node has been visited, then it has a definition *)
	  fun visited id = (
		case Array.sub(defs, id) of Unknown => false | _ => true)
	(** computations on sums **)
	(* if a variable is defined, compute the normal form of its definition
	 * and return it.  If the variable is unknown or its definition is
	 * already in normal form, then return NONE.
	 *)
	  fun normalizeVar v = (case getVar v
		 of Unknown => Unknown
		  | Sum s => (case normalizeSum s
		       of NONE => Sum s
			| SOME s' => let val sum = Sum s'
			    in
			      setVar(v, sum); sum
			    end
		      (* end case *))
		(* end case *))
	(* normalize a sum of scaled variables; if the sum is already normalized,
	 * then return NONE.
	 *)
	  and normalizeSum ({terms, c} : sum) = let
		fun extract ((t as (b, y))::r, ts, todo : (real * sum) list) = (
		      case normalizeVar y
		       of Unknown => extract(r, t::ts, todo)
			| Sum s => extract(r, ts, (b, s)::todo)
		      (* end case *))
		  | extract ([], _, []) = NONE
		  | extract ([], ts, todo) =
		      SOME(addDefs ({terms=List.rev ts, c=c}, todo))
		and addDefs (acc, []) = acc
		  | addDefs (acc, (coeff, sum)::r) =
		      addDefs (addScaled(acc, coeff, sum), r)
		in
		  extract (terms, [], [])
		end
	(* compute r1 + coeff*r2, where r1 and r2 are normalized; the result
	 * is normalized.
	 *)
	  and addScaled (r1 : sum, coeff : real, r2 : sum) = let
		fun combine ([], ts) = List.map (scale coeff) ts
		  | combine (ts, []) = ts
		  | combine (ts1 as (t1::r1), ts2 as (t2::r2)) =
		      if (#2 t1 < #2 t2)
			then t1 :: combine(r1, ts2)
		      else if (#2 t1 = #2 t2)
			then (#1 t1 + (coeff * #1 t2), #2 t1) :: combine (r1, r2)
			else (scale coeff t2) :: combine(ts1, r2)
		in
		  { c = #c r1 + coeff * #c r2,
		    terms = combine(#terms r1, #terms r2)
		  }
		end
	(* add the term (a*x) to a normalized term; we assume that x is Undefined. *)
	  fun addScaledVar ({c, terms}, a : real, x) = let
		fun insert [] = [(a, x)]
		  | insert ((t as (b, y))::r) =
		      if (y < x)
			then  t :: insert r
		      else if (y = x)
			then  (a+b, x) :: r
			else (a, x) :: t :: r
		in
		  {c = c, terms = insert terms}
		end
	(* given a list of incoming edges, create the rhs sum. *)
	  fun makeRHS preds = let
		fun f ((src, _, e), acc) = let
		      val prob = getProb e
		      in
			case normalizeVar src
			 of Unknown => addScaledVar (acc, prob, src)
			  | Sum sum => addScaled (acc, prob, sum)
			(* end case *)
		      end
		in
		  List.foldl f zero preds
		end
	(* Simplify the equation "x = rhs" by checking for x in rhs.  We assume that
	 * x is undefined and that the rhs is normaized.  We return the simplified
	 * rhs.
	 *)
	  fun simplify (x, rhs as {terms, c}) = let
		fun removeX ([], _) = rhs
		  | removeX ((t as (a, y))::r, ts) =
		      if (x < y)
			then rhs
		      else if (x = y)
			then let
			  val s = 1.0 / Real.max(1.0 - a, epsilon)
			  val terms = List.revAppend(ts, r)
			  in
			    {c = s*c, terms = List.map (scale s) terms}
			  end
			else removeX(r, t::ts)
		in
		  removeX (terms, [])
		end
	(* INVARIANT: the variables corresponding to marked nodes are not Unknown
	 * in the rhs of any equation.
	 *)
	  fun dfs id = if (visited id)
		then ()
		else let
		  val rhs = makeRHS (in_edges id)
		  val rhs = simplify (id, rhs)
		  in
		    setVar (id, Sum rhs);
		    followEdges (out_edges id)
		  end
	  and followEdges [] = ()
	    | followEdges ((_, dst, _)::r) = (dfs dst; followEdges r)
	  val root =
	      case #entries methods () of
		  [root] => root
		| _ => raise Fail "ComputeFreqsFn: root"
	  in
	  (* initialize edge probabilities *)
	    CompleteProbs.completeProbs cfg;
	  (* initialize the root *)
	    setVar (root, Sum one);
	  (* traverse the successors of the root *)
	    followEdges (out_edges root);
	  (* record block and edge frequencies in CFG *)
	    #forall_nodes methods (fn (id, CFG.BLOCK{freq, ...}) => (
		case normalizeVar id
		 of Unknown => freq := 0.0
		  | Sum{c, terms=[]} => freq := c
		  | _ => raise Fail (concat[
			"block ", Int.toString id, " unresolved"
		      ])
		(* end case *)));
	    #forall_edges methods (fn (src, _, CFG.EDGE{w, ...}) => let
		val CFG.BLOCK{freq, ...} = node_info src
		in
		  w := !w * !freq
		end);
	    if !dumpFreqs
	      then let
		fun bfreq (id, CFG.BLOCK{kind, freq, ...}) =
		      prf("\tbfreq(%s %d) = %f\n", [
			  F.STR(CFG.kindName kind), F.INT id, F.REAL(!freq)
			])
		fun freq (src, dst, info as CFG.EDGE{w, ...}) =
		      prf("\tfreq(%d->%d:%s) = %f\n", [
			  F.INT src, F.INT dst, F.STR(CFG.show_edge info),
			  F.REAL(!w)
			])
		in
		  pr "[ computed frequencies ]\n";
		  #forall_nodes methods bfreq;
		  #forall_edges methods freq
		end
	      else ();
	    if !dumpCFG
	      then CFG.dump (
		  !MLRiscControl.debug_stream,
		  "after frequency computation", cfg)
	      else ()
	  end

  end

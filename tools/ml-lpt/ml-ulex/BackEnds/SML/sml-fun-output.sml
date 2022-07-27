(* sml-fun-output.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Code generation for SML, using mutually recursive functions for states.
 *)

structure SMLFunOutput : OUTPUT = 
  struct

    open SMLOutputSupport

  (* transition interval representation *)
    datatype transition_interval = TI of SIS.interval * int * ml_exp
    fun intervalOf (TI (i, t, e)) = i
    fun tagOf      (TI (i, t, e)) = t
    fun actionOf   (TI (i, t, e)) = e
    fun sameTag    (TI (_, t1, _), TI (_, t2, _)) = t1 = t2
    fun singleton  (TI ((i, j), _, _)) = i = j

  (* generate code for transitions: generate a hard-coded binary
   * search on accepting characters
   *)
    fun mkTrans ([], _) = raise Fail "(BUG) SMLFunOutput: alphabet not covered"
      | mkTrans ([t], _)  = actionOf t
      | mkTrans ([t1, t2], _) = 
	  if sameTag (t1, t2) then actionOf t1
	  else let
	    val (_, t1end) = intervalOf t1
	    val (t2start, _) = intervalOf t2
	    in 
	      if singleton t1 then
		ML_If (ML_Cmp (ML.EQ, inpVar, ML_Sym t1end),
		       actionOf t1,
		       actionOf t2)
	      else if singleton t2 then
		ML_If (ML_Cmp (ML.EQ, inpVar, ML_Sym t2start),
		       actionOf t2,
		       actionOf t1)
	      else
		ML_If (ML_Cmp (ML.LEQ, inpVar, ML_Sym t1end),
		       actionOf t1,
		       actionOf t2)
            end
      | mkTrans (ts, len) = let
	  val lh = len div 2
	  fun split (ls, 0, l1) = (List.rev l1, ls)
	    | split (l::ls, cnt, l1) = split (ls, cnt-1, l::l1)
	    | split _ = raise Fail "(BUG) SMLFunOutput: split failed"
	  val (ts1, ts2) = split (ts, lh, [])
	  val (ts2start, ts2end) = intervalOf (List.hd ts2)
	  val (ts2', ts2len) = if ts2start = ts2end
			       then (List.tl ts2, len - lh - 1)
			       else (ts2, len - lh)
	(* we want to take advantage of the special case when 
	 * len = 3 and hd ts2 is a singleton.  this case often
	 * occurs when we have an arrow for a single character.
	 *)
	  val elseClause = 
	        if lh = 1 andalso ts2len = 1 
		then mkTrans ([List.hd ts1, List.hd ts2'], 2)
		else ML_If (ML_Cmp (ML.LT, inpVar, ML_Sym ts2start),
			    mkTrans (ts1, lh),
			    mkTrans (ts2', ts2len))
	  in
	    ML_If (ML_Cmp (ML.EQ, inpVar, ML_Sym ts2start),
		   actionOf (List.hd ts2),
		   elseClause)
          end

    fun mkState (arg, eofRules, actionVec) (s, k) = let
          val LO.State {id, startState, label, final, next} = s
	  fun addMatch (i, lastMatch) = let
		val lastMatch' = if hasREJECT (Vector.sub (actionVec, i))
				 then lastMatch
				 else ML_Var "yyNO_MATCH"
		in
	          ML_App ("yyMATCH",
			  [ML_Var "strm",
			   ML_Var (actName i),
			   lastMatch'])
		end
	  val (curMatch, nextMatches) = (case final
					  of [] => (NONE, [])
					   | f::fs => (SOME f, fs)
					 (* end case *))
	  val lastMatch = List.foldr addMatch (ML_Var "lastMatch") nextMatches
	(* collect all valid transition symbols *)
	  val labels = List.foldl SIS.union SIS.empty (List.map #1 (!next))
        (* pair transition intervals with associated actions/transitions *)
	  val newFinal = (case curMatch
			   of SOME j => addMatch (j, lastMatch)
			    | NONE => lastMatch
			  (* end case *))
	  fun arrows (syms, s) = 
	        mapInt 
		  (fn i => TI (i, idOf s, 
		     ML_App (nameOf s, [ML_Var "strm'", newFinal])))
		  syms
	  val TIs = List.map arrows (!next)
	  val errAct' = 
	        (case curMatch
		  of SOME j => 
		       ML_App (actName j, 
			       [ML_Var "strm", 
				if hasREJECT (Vector.sub (actionVec, j))
				then lastMatch
				else ML_Var "yyNO_MATCH"])
		   | NONE =>  ML_App ("yystuck", [lastMatch])
		 (* end case *))
        (* if start state, check for eof *)
	  val errAct = if startState
		       then mkEOF (eofRules, errAct')
		       else errAct'
        (* error transitions = complement(valid transitions) *)
	  val error = SIS.complement labels
	  val errTIs = mapInt (fn i => TI (i, ~1, errAct)) error
	(* the arrows represent intervals that partition the entire
	 * alphabet, with each interval mapped to some transition or
	 * action.  we sort the intervals by their smallest member.
	 *)
	  fun gt (a, b) = (#1 (intervalOf a)) > (#1 (intervalOf b))
	  val sorted = ListMergeSort.sort gt (List.concat (errTIs :: TIs))
        (* now we want to find adjacent partitions with the same 
	 * action, and merge their intervals
	 *)
	  fun merge [] = []
	    | merge [t] = [t]
	    | merge (t1::t2::ts) = 
	        if sameTag (t1, t2) then let
		    val TI ((i, _), tag, act) = t1
		    val TI ((_, j), _,   _  ) = t2
		    val t = TI ((i, j), tag, act)
		    in
		      merge (t::ts)
		    end
		else
		  t1::(merge (t2::ts))
	  val merged = merge sorted
        (* create the transition code, which at least has an error transition *)
	  val trans = mkTrans(merged, List.length merged)
        (* create the input code *)
	  val getInp = 
	        ML_Case (ML_App ("yygetc", [ML_Var "strm"]),
			 [(ML_ConPat ("NONE", []), errAct),
			  (ML_ConPat ("SOME", [ML_VarPat (inp ^ ", strm'")]), 
			   trans)])
	  in
            ML_Fun (nameOf s, ["strm", "lastMatch : yymatch"], getInp, k)
          end

    structure SCC = GraphSCCFn (
      struct
        type ord_key = LO.dfa_state
        fun compare (LO.State{id = id1, ...}, LO.State{id = id2, ...}) =
	      Int.compare (id1, id2)
      end)

    fun mkStates (arg, eofRules, actions, dfa, startStates, k) = let
          fun follow (LO.State {next, ...}) = 
	        #2 (ListPair.unzip (!next))
          val scc = SCC.topOrder' { roots = startStates, follow = follow }
	  val mkState' = mkState (arg, eofRules, actions)
	  fun mkGrp (SCC.SIMPLE state, k) = ML_NewGroup (mkState' (state, k))
	    | mkGrp (SCC.RECURSIVE states, k) = 
	        ML_NewGroup (List.foldr mkState' k states)
          in
            List.foldl mkGrp k scc
          end

    fun lexerHook spec strm = let
          val LO.Spec {arg, actions, dfa, startStates, eofRules, ...} = spec
	  fun matchSS (label, state) =
	        (ML_ConPat (label, []), 
		   ML_App (nameOf state, 
				[ML_RefGet (ML_Var "yystrm"), 
				 ML_Var "yyNO_MATCH"]))
	  val innerExp = ML_Case (ML_RefGet (ML_Var "yyss"),
				  List.map matchSS startStates)
	  val statesExp = mkStates 
			    (arg, eofRules, actions, dfa, 
			     #2 (ListPair.unzip startStates), innerExp)
	  val lexerExp = Vector.foldri mkAction statesExp actions
          val ppStrm = TextIOPP.openOut {dst = strm, wid = 80}
          in
            ML.ppML (ppStrm, lexerExp)
          end

    fun tableHook _ strm = TextIO.output (strm, "Vector.fromList []");

    fun output (spec, fname) = 
          ExpandFile.expandTemplate {
	      src = if !Options.lexCompat 
		    then lexTemplate else ulexTemplate,
	      dst = fname ^ ".sml",
	      hooks = [("lexer", lexerHook spec),
		       ("startstates", startStatesHook spec),
		       ("userdecls", userDeclsHook spec),
		       ("header", headerHook spec),
		       ("args", argsHook spec),
		       ("pargs", pargsHook spec),
		       ("table", tableHook spec)]
	    }

  end

(* thompson-engine.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is an implementation of Ken Thompson's RE matchine algorithm from
 * CACM (1968).  It is based on the description of the algorithm by Russ
 * Cox at http://swtch.com/~rsc/regexp/regexp1.html.
 *)

structure ThompsonEngine : REGEXP_ENGINE =
  struct

    structure RE = RegExpSyntax
    structure CSet = RE.CharSet
    structure M = MatchTree

  (* a match specifies the position (as a stream) and the length of the match *)
    type 'a match = {pos : 'a, len : int} MatchTree.match_tree

  (* intermediate representation of states *)
    datatype state_kind
      = CHR' of (char * state' ref)
      | CSET' of (CSet.set * state' ref)
      | NCSET' of (CSet.set * state' ref)
      | SPLIT' of (state' ref * state' ref)
      | BOL' of state' ref			(* assert beginning of line *)
      | EOL' of state' ref			(* assert end of line *)
      | FINAL'

    withtype state' = {id : int, kind : state_kind}

    type frag = {start : state', out : state' ref list}

  (* return the ID of a state *)
    fun idOf {id, kind} = id

    val final = {id = 0, kind = FINAL'}

  (* interpreter representation of states *)
    datatype state
      = CHR of (char * int)
      | CSET of (CSet.set * int)
      | NCSET of (CSet.set * int)
      | SPLIT of (int * int)
      | BOL of int			(* assert beginning of line *)
      | EOL of int			(* assert end of line *)
      | FINAL

    fun cvtState {id, kind} = (case kind
	   of CHR'(c, out) => CHR(c, idOf(!out))
	    | CSET'(cset, out) => CSET(cset, idOf(!out))
	    | NCSET'(cset, out) => NCSET(cset, idOf(!out))
	    | SPLIT'(out1, out2) => SPLIT(idOf(!out1), idOf(!out2))
	    | BOL' out => BOL(idOf(!out))
	    | EOL' out => EOL(idOf(!out))
	    | FINAL' => FINAL
	  (* end case *))

    datatype regexp = RE of {start : int, states : state vector}

    fun compile re = let
	(* the list of states; state 0 is always the accepting state *)
	  val nStates = ref 1
	  val states = ref [final]
	(* create new states *)
	  fun new kind = let
		val id = !nStates
		val s = {id = id, kind = kind}
		in
		  states := s :: !states;
		  nStates := id+1;
		  s
		end
	  fun newChr (c, out) = new (CHR'(c, out))
	  fun newCset (cset, out) = new (CSET'(cset, out))
	  fun newNcset (cset, out) = new (NCSET'(cset, out))
	  fun newSplit (out1, out2) = new (SPLIT'(out1, out2))
	  fun newBOL out = new (BOL' out)
	  fun newEOL out = new (EOL' out)
	(* update the outputs of a fragment *)
	  fun setOuts (f : frag, s : state') = List.app (fn r => r := s) (#out f)
	(* compile an RE *)
	  fun reComp re = (case re
		 of RE.Group re => reComp re
		  | RE.Alt[] => raise RE.CannotCompile
		  | RE.Alt[re] => reComp re
		  | RE.Alt(re::rest) =>  let
		      val f1 = reComp re
		      val f2 = reComp (RE.Alt rest)
		      val s = newSplit(ref(#start f1), ref(#start f2))
		      in
			{start = s, out = #out f1 @ #out f2}
		      end
		  | RE.Concat[] => raise RE.CannotCompile
		  | RE.Concat[re] => reComp re
		  | RE.Concat(re::rest) => cat (re, RE.Concat rest)
		  | RE.Interval(re, 0, SOME 1) => option re
		  | RE.Interval(re, 0, NONE) => closure re
		  | RE.Interval(re, 1, NONE) => posClosure re
		  | RE.Interval _ => raise RE.CannotCompile
		  | RE.MatchSet cset => let
		      val out = ref final
		      in
			{start = newCset(cset, out), out = [out]}
		      end
		  | RE.NonmatchSet cset => let
		      val out = ref final
		      in
			{start = newNcset(cset, out), out = [out]}
		      end
		  | RE.Char c => let
		      val out = ref final
		      in
			{start = newChr(c, out), out = [out]}
		      end
		  | RE.Begin => let
		      val out = ref final
		      in
			{start = newBOL out, out = [out]}
		      end
		  | RE.End => raise RE.CannotCompile
		(* end case *))
	(* compile re1 . re2 *)
	  and cat (re1, re2) = let
		val f1 = reComp re1
		val f2 = reComp re2
		in
		  setOuts (f1, #start f2);
		  {start = #start f1, out = #out f2}
		end
	(* compile re? *)
	  and option re = let
		val f = reComp re
		val out = ref final
		val s = newSplit(ref(#start f), out)
		in
		  {start = s, out = out :: #out f}
		end
        (* compile re* *)
	  and closure re = let
		val f = reComp re
		val out = ref final
		val s = newSplit(ref(#start f), out)
		in
		  setOuts (f, s);
		  {start = s, out = [out]}
		end
        (* compile re+ *)
	  and posClosure re = let
		val f = reComp re
		val out = ref final
		val s = newSplit(ref(#start f), out)
		in
		  setOuts (f, s);
		  {start = #start f, out = [out]}
		end
	(* generate the intermediate state representation *)
	  val frag = reComp re
	  val _ = setOuts (frag, final)
	(* convert the states to the final representation; note that we reverse the list
	 * so that the states are now in increasing order.
	 *)
	  val states = List.foldl (fn (s, l) => cvtState s :: l) [] (!states)
	  in
	    RE{ start = idOf(#start frag), states = Vector.fromList states }
	  end

(* +DEBUG *)
    fun stateToString (CHR(c, out)) =
	  concat["CHR (#\"", Char.toString c, "\", ", Int.toString out, ")"]
      | stateToString (CSET(cs, out)) = concat["CSET (-, ", Int.toString out, ")"]
      | stateToString (NCSET(cs, out)) = concat["NCSET (-, ", Int.toString out, ")"]
      | stateToString (SPLIT(out1, out2)) =
	  concat["SPLIT (", Int.toString out1, ", ", Int.toString out2, ")"]
      | stateToString (BOL out) = concat["BOL ", Int.toString out]
      | stateToString (EOL out) = concat["EOL ", Int.toString out]
      | stateToString FINAL = "FINAL"
    fun dump (RE{start, states}) = let
	  fun prState st = print(stateToString st)
	  in
	    print(concat["start = ", Int.toString start, "\n"]);
	    Vector.appi (fn (i, st) => (print(Int.toString i ^ ": "); prState st; print "\n"))
	      states
	  end
(* -DEBUG *)

  (* scan the stream for the first occurrence of the regular expression *)
    fun scan (RE{start, states}, getc : (char,'a) StringCvt.reader) = let
(*val _ = dump (RE{start=start, states=states})*)
	(* to make elimination of duplicates in a state set cheap, we map state IDs
	 * to a stamp of the last set that they were added to.
	 *)
	  val stamp = ref 0w1
	  val lastStamp = Array.array(Vector.length states, 0w0)
	  fun addState (stamp', stateList, id) =
		if (Array.sub(lastStamp, id) = stamp')
		  then stateList
		  else (
		    Array.update(lastStamp, id, stamp');
		    case Vector.sub(states, id)
		     of SPLIT(out1, out2) =>
			  addState (stamp', addState (stamp', stateList, out1), out2)
		      | state => state :: stateList
		    (* end case *))
	  fun startState () = let
		val stamp' = !stamp
		in
		  stamp := stamp' + 0w1;
		  addState (stamp', [], start)
		end
	  fun isMatch stamp' = (Array.sub(lastStamp, 0) = stamp')
	(* attempt to match the RE starting with the stream startPos *)
	  fun find' (isFirst, startPos) = let
		fun scan (_, _, _, lastAccepting, []) = lastAccepting
		  | scan (isFirst, n, strm, lastAccepting, nfaState) = (case getc strm
		       of NONE => if isMatch (!stamp)
			    then SOME(n, startPos)
			    else lastAccepting
			| SOME(c, strm') => let
			    val stamp' = !stamp
			    val _ = (stamp := stamp' + 0w1)
			    fun test ([], nextStates) = nextStates
			      | test (s::r, nextStates) = let
				  fun continue nextStates = test(r, nextStates)
				  fun add out = continue(addState (stamp', nextStates, out))
				  in
				    case s
				     of CHR(c', out) => if (c = c')
					  then add out
					  else continue nextStates
				      | CSET(cset, out) => if CSet.member(cset, c)
					  then add out
					  else continue nextStates
				      | NCSET(cset, out) => if CSet.member(cset, c)
					  then continue nextStates
					  else add out
				      | BOL out => if isFirst
					  then test(Vector.sub(states, out)::r, nextStates)
					  else continue nextStates
				      | EOL out => raise Fail "end-of-line not supported yet"
				      | _ => continue nextStates
				    (* end case *)
				  end
			    val nextNfaState = test (nfaState, [])
			    val lastAccepting = if isMatch stamp'
				  then SOME(n+1, startPos)
				  else lastAccepting
			    in
(*
print(concat[
"{", String.concatWith "," (List.map stateToString nfaState), "} -- ",
"#\"", Char.toString c, "\" --> {",
String.concatWith "," (List.map stateToString nextNfaState), "}\n"]);
*)
			      scan ((c = #"\n"), n+1, strm', lastAccepting, nextNfaState)
			    end
		      (* end case *))
		in
		  case scan (isFirst, 0, startPos, NONE, startState())
		   of NONE => NONE
		    | SOME(n, strm) => SOME(M.Match({pos=startPos, len=n}, []), strm)
		  (* end case *)
		end
	  in
	    find'
	  end

	fun find re getc stream = let
	      val scan = scan (re, getc)
	      fun loop (isFirst, s) = (case (scan (isFirst, s))
		     of NONE => (case (getc s)
			   of SOME(#"\n", s') => loop (true, s')
			    | SOME(_, s') => loop (false, s')
			    | NONE => NONE
			  (* end case *))
		      | SOME v => SOME v
		    (* end case *))
	      in
		loop (true, stream)
	      end

	fun prefix re getc strm = scan (re, getc) (true, strm)

	fun match [] = (fn getc => fn strm => NONE)
	  | match l = let
	    (* compile the REs *)
	      val l = List.map (fn (re, act) => (compile re, act)) l
	      fun match' getc strm = let
		  (* find the longest SOME *)
		    fun loop ([], max, _) = max
		      | loop ((re, act)::r, max, maxLen) = (case scan(re, getc) (true, strm)
			   of NONE => loop (r, max, maxLen)
			    | SOME(m as MatchTree.Match({len, ...}, _), cs) =>
				if (len > maxLen)
				  then loop (r, SOME(m, act, cs), len)
				  else loop (r, max, maxLen)
			  (* end case *))
		    in
		      case loop (l, NONE, ~1)
		       of NONE => NONE
			| SOME(m, act, cs) => SOME(act m, cs)
		      (* end case *)
		    end
	      in
		match'
	      end

  end

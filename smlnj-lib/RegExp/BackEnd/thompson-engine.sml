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

    (* the intermediate representation of an NFA *)
    type frag = {start : state', out : state' ref list}

(* +DEBUG **
    fun state'ToString {id, kind} = let
          fun kindToString (CHR'(c, out)) =
                concat["CHR (#\"", Char.toString c, "\", -)"]
            | kindToString (CSET'(cs, out)) = "CSET -"
            | kindToString (NCSET'(cs, out)) = "NCSET -"
            | kindToString (SPLIT'(out1, out2)) = "SPLIT -"
            | kindToString (BOL' out) = "BOL -"
            | kindToString (EOL' out) = "EOL -"
            | kindToString FINAL' = "FINAL"
          in
            concat["(", Int.toString id, ": ", kindToString kind, ")"]
          end
    fun fragToString {start, out} = concat[
            "{start = ", state'ToString start, ", out = [",
            String.concatWithMap ", " (state'ToString o !) out, "]}"
          ]
** -DEBUG *)

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

    (* the representation of the NFA.  The `start` field is the start state.
     * The `state` vector maps state indices to states.  By convention,
     * state 0 is the accepting state.
     *)
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
	  (* update the outputs of a fragment to the state s *)
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
		  | RE.Interval(re, min, optMax) => let
                      (* the suffix matches instances of `re` after the first `min`
                       * iterations.  It is either `re*` (when `optMax` is `NONE`)
                       * or a sequence of `m - min` SPLITs, where one edge goes to
                       * out and the other goes to next SPLIT in the sequence
                       * (when `optMax` is `SOME m`).
                       *)
                      val suffix : frag = (case optMax
                             of NONE => closure re
                              | SOME m => let
                                  val out = ref final
                                  fun mkSuffix 1 = reComp re
                                    | mkSuffix i = let
                                        val f = reComp re
                                        val f' = mkSuffix(i-1)
                                        val s = newSplit(out, ref(#start f))
                                        in
                                          setOuts (f, #start f');
                                          {start = s, out = out :: #out f'}
                                        end
                                  in
                                    if (m <= min) then raise RE.CannotCompile else ();
                                    mkSuffix (m - min)
                                  end
                            (* end case *))
                      (* the prefix is `min` iterations of `re` *)
                      fun mkPrefix 0 = suffix
                        | mkPrefix i = let
                            val f = reComp re
                            val f' = mkPrefix (i-1)
                            in
                              setOuts (f, #start f');
                              {start = #start f, out = #out f'}
                            end
                      in
                        mkPrefix min
                      end
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
		  | RE.End => let
		      val out = ref final
		      in
			{start = newEOL out, out = [out]}
		      end
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

(* +DEBUG **
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
	    Vector.appi
              (fn (i, st) => (print(Int.toString i ^ ": "); prState st; print "\n"))
	        states
	  end
** -DEBUG *)

    (* is a stream at the end of line? *)
    fun isEOL NONE = true
      | isEOL (SOME(#"\n", _)) = true
      | isEOL _ = false

  (* scan the stream for the first occurrence of the regular expression *)
    fun scan (RE{start, states}, getc : (char,'a) StringCvt.reader) = let
	(* to make elimination of duplicates in a state set cheap, we map state IDs
	 * to a stamp of the last set that they were added to.
	 *)
	  val stamp = ref 0w1
          fun incr () = let val s = !stamp + 0w1 in stamp := s; s end
	  val lastStamp = Array.array(Vector.length states, 0w0)
          (* conditionally add the epsilon closure of the state `id` to `stateList` *)
	  fun addState (isFirst, strm, stamp', stateList, id) = let
                fun add (stateList, id) =
                      if (Array.sub(lastStamp, id) = stamp')
                        (* the state is already in the list *)
                        then stateList
                        else (
                          Array.update(lastStamp, id, stamp');
                          case Vector.sub(states, id)
                           of SPLIT(out1, out2) => add (add (stateList, out1), out2)
                            | BOL out => if isFirst
                                then add(stateList, out)
                                else stateList
                            | EOL out => if isEOL (getc strm)
                                then add(stateList, out)
                                else stateList
                            | state => state :: stateList
                          (* end case *))
                in
                  add (stateList, id)
                end
          (* get the list of start states by performing epsilon moves *)
	  fun startStates strm = let
		val stamp' = incr()
		in
		  addState (true, strm, stamp', [], start)
		end
          (* is the accepting state in the current set of states? *)
	  fun isMatch stamp = (Array.sub(lastStamp, 0) = stamp)
	  (* attempt to match the RE; the parameters are
           *   - isFirst      true if the current stream position is the start of
           *                  a line (or the input)
           *   - strm         the initial stream to scan
           *)
	  fun find' (isFirst, strm) = let
                (* scanning the input; the parameters are
                 *   - isFirst      true if the current position is the start of
                 *                  a line (or the input)
                 *   - n            the number of characters matched so far
                 *   - strm         the current position of the input stream
                 *   - nfaStates    the current list of NFA states (guaranteed to be
                 *                  non-empty)
                 *   - lastMatch    if we were previously in a accepting state during
                 *                  this scan, then `lastMatch` is the value
                 *                  `SOME(startPos, k, strm')`, where `k` is the length
                 *                  of the match and `strm'` is stream position
                 *                  immediately following the match.
                 *)
                fun scan (isFirst, n, strm, nfaStates, lastMatch) = (case getc strm
                       of NONE => lastMatch
                        | SOME(c, strm') => let
                            (* bump the stamp counter *)
			    val stamp' = incr()
                            (* compute the next set of NFA states by seeing if there
                             * is a transition labeled with the character `c`
                             *)
                            fun test ([], nextStates) = nextStates
                              | test (s::r, nextStates) = let
				  fun continue nextStates = test(r, nextStates)
				  fun add out =
                                        continue(
                                          addState (
                                            isFirst, strm', stamp', nextStates, out))
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
				      | _ => continue nextStates
				    (* end case *)
				  end
                            val next = test (nfaStates, [])
                            in
(* +DEBUG **
print(concat[
"{", String.concatWithMap "," stateToString nfaStates, "} -- ",
"#\"", Char.toString c, "\" --> {",
String.concatWithMap "," stateToString next, "}\n"]);
let val stamps = Array.foldri (fn (i, s, stmps) => (i, s)::stmps) [] lastStamp
fun w2s w = Int.toString(Word.toIntX w)
fun stamp2s (i, s) = concat[Int.toString i, ":", w2s s]
in
print(concat[
"  stamps [", String.concatWithMap "," stamp2s stamps, "] @ ", w2s stamp', "\n"]);
case lastMatch
 of NONE => print "  lastMatch = NONE\n"
  | SOME(n, _) => print(concat["  lastMatch = SOME(", Int.toString n, ", -)\n"])
(* end case *)
end;
** -DEBUG *)
                              case next
                               of [] => lastMatch
                                | _ => let
                                    val n = n+1
                                    val isFirst = (c = #"\n")
                                    val lastMatch = if isMatch stamp'
                                          then SOME(n, strm')
                                          else lastMatch
                                    in
                                      scan (isFirst, n, strm', next, lastMatch)
                                    end
                              (* end case *)
                            end
                      (* end case *))
                val nfaStart = startStates strm
                val lastMatch = if isMatch(!stamp)
                      then SOME(0, strm)
                      else NONE
                in
                  scan (isFirst, 0, strm, nfaStart, lastMatch)
                end (* find' *)
	  in
	    fn (isFirst, strm) => (case find'(isFirst, strm)
                 of SOME(n, strm') => SOME(M.Match({pos=strm, len=n}, []), strm')
                  | NONE => NONE
                (* end case *))
	  end

    fun find re getc strm = let
          val scan = scan (re, getc)
(* TODO: this is potentially expensive backtracking at the top level; if we had
 * support for groups, then we could modify the state machine to match ".*|(re)",
 * which would avoid backtracking.
 *)
          fun loop (isFirst, s) = (case (scan (isFirst, s))
                 of NONE => (case (getc s)
                       of SOME(#"\n", s') => loop (true, s')
                        | SOME(_, s') => loop (false, s')
                        | NONE => NONE
                      (* end case *))
                  | someMatch => someMatch
                (* end case *))
          in
            loop (true, strm)
          end

    fun prefix re getc strm = scan (re, getc) (true, strm)

    fun match [] = (fn getc => fn strm => NONE)
      | match l = let
        (* compile the REs *)
          val l = List.map (fn (re, act) => (compile re, act)) l
          fun match' getc strm = let
              (* find the longest SOME *)
                fun loop ([], max, _) = max
                  | loop ((re, act)::r, max, maxLen) = (
                      case scan(re, getc) (true, strm)
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

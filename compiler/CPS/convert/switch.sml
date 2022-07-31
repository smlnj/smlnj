(* switch.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * TODO:
 *   - we might be able to exploit range information for Word8.word and Int8.int
 *     types (and Char.char), but it is not clear that we have that much type information
 *     at this point in the compiler pipeline.
 *   - if CPS.SWITCH supported boxed int types, we could merge the taggedNumSwitch and
 *     boxedNumSwitch functions
 *   - change CPS to replace streql/strneq with a STRCMP three-way branch and then
 *     implement binary search for strings.
 *)

structure Switch : sig

    val switch : {
	  (* variable renaming *)
	    rename : CPS.lvar -> CPS.value
	  } -> {
	    arg     : CPS.value,
	    sign    : Access.consig,
	    cases   : (PLambda.con * CPS.cexp) list,
	    default : CPS.cexp
	  } -> CPS.cexp

  end = struct

    structure LT = Lty
    structure PL = PLambda
    structure F = FLINT
    structure A = Access

    fun bug s = ErrorMsg.impossible ("Switch: " ^ s)

  (* make a fresh variable *)
    val mkv = LambdaVar.mkLvar

  (* make a PURE expression, where `k` is the continuation of the operation *)
    fun pure (rator, args, ty, k) = let
	  val x = mkv()
	  in
	    CPS.PURE(rator, args, x, ty, k(CPS.VAR x))
	  end

  (* minimum number of cases required to make a SWITCH (instead of conditional branches) *)
    val switchLimit = 4

  (* convert to/from IntInf.int *)
    val toII = IntInf.fromInt
    val toI = IntInf.toInt

  (* default integer type/values *)
    local
      val tt = {sz = Target.defaultIntSz, tag = true}
    in
    val tagNumTy = CPS.NUMt tt
    fun tagNum n = CPS.NUM{ival = n, ty = tt}
    fun boxNumTy sz = CPS.NUMt{sz = sz, tag = false}
  (* operator numkinds for default tagged ints and words *)
    val tagIntKind = CPS.P.INT Target.defaultIntSz
    val tagWordKind = CPS.P.UINT Target.defaultIntSz
    end

  (* sort cases tagged by integers *)
    val numSort = ListMergeSort.sort (fn ((i, _) : (IntInf.int * CPS.cexp), (j,_)) => (i > j))

  (* A chunk is a tuple `(lb, ub, n, cases)`, where `lb` is the smallest value
   * covered by `cases`, `ub` is the largest value covered by `cases`, `n` is
   * the number of items in the `cases` list, and where `cases` list is a dense
   * (i.e., no gaps) list of integer-cexp pairs.
   *)
    type chunk = (IntInf.int * IntInf.int * int * (IntInf.int * CPS.cexp) list)

  (* group a sorted list of integer cases into a list of chunks. The resulting
   * chunks will have at least switchLimit items.
   *)
    fun groupCases (cases : (IntInf.int * CPS.cexp) list, default) : chunk list = let
	(* merge a chunk into a list of chunks, where the chunks are ordered in increasing
	 * range.
         *)
	  fun merge (lb, ub, n, cases, []) = [(lb, ub, n, cases)]
	    | merge (lb, ub, n, cases, chunks as (lb', ub', n', cases')::chunkr) =
	      (* check if merged chunk will have approx 50% coverage or more *)
		if (toII(2 * (n + n')) < (ub' - lb))
		  then (lb, ub, n, cases) :: chunks
		  else let (* fill in any gap *)
		    fun fill (i, n, cases) = if (ub < i)
			  then fill (i - 1, n + 1, (i, default) :: cases)
			  else (n, cases)
		    val (n', cases') = fill (lb' - 1, n', cases')
		    in
		      merge (lb, ub', n + n', cases@cases', chunkr)
		    end
	(* compute an initial list of chunks, where each chunk has at least 50% coverage
	 * of its range
	 *)
	  val initialChunks = List.foldr
		(fn (ic, chunks) => merge(#1 ic, #1 ic, 1, [ic], chunks))
		  [] cases
	(* split chunks that are too small for a SWITCH into singletons *)
	  fun separate (chunk as (_, _, n, cases), chunks) =
		if (n < switchLimit)
		  then List.foldr
		    (fn (ic as (i, _), chunks) => (i, i, 1, [ic])::chunks)
		      chunks
			cases
		  else chunk :: chunks
	  in
	    List.foldr separate [] initialChunks
	  end

  (* generate a switch for a tagged integer/word type.  If the optRange is `SOME n`, then
   * the range of possible values is `0..n`.
   *)
    fun taggedNumSwitch (_, _, [], default, _) = default
      | taggedNumSwitch (arg, nk, cases, default, optRange) = let
	  val nCases = List.length cases
	(* sort cases *)
	  val cases = numSort cases
	(* equality test branch *)
	  fun ifeq (i, tr, fl) =
		CPS.BRANCH(CPS.P.CMP{oper=CPS.P.EQL, kind=nk}, [arg, tagNum i], mkv(), tr, fl)
	(* less-than test branch *)
	  fun ifless (a, b, tr, fl) =
		CPS.BRANCH(CPS.P.CMP{oper=CPS.P.LT, kind=nk}, [a, b], mkv(), tr, fl)
	(* map cases to CPS.SWITCH, where we know that lo0 <= arg <= hi0 *)
	  fun switch' (lo0, hi0) = let
	      (* group cases into dense chunks *)
		val chunks = groupCases (cases, default)
	      (* generate switch for chunks, where we know that lo <= arg <= hi *)
		fun gen (_, [(_, _, _, [(i, act)])], lo, hi) = (* one singleton chunk *)
		      if (lo = i) andalso (hi = i)
			then act
			else ifeq(i, act, default)
		  | gen (_, [(lb, ub, n, cases)], lo, hi) = let (* one chunk with multiple cases *)
		    (* project out actions from cases *)
		      val actions = List.map #2 cases
		    (* the switch *)
		      val exp = if (lb = 0)
			    then CPS.SWITCH(arg, mkv(), actions)
			    else pure(
			    (* NOTE: because lb <= arg, this subtraction cannot Overflow *)
			      CPS.P.PURE_ARITH{oper=CPS.P.SUB, kind=nk}, [arg, tagNum lb], tagNumTy,
			      fn arg' => CPS.SWITCH(arg', mkv(), actions))
		    (* add lower-bound check (if necessary) *)
		      val exp = if (lo < lb)
			    then ifless(arg, tagNum lb, default, exp)
			    else exp
		    (* add upper-bound check (if necessary) *)
		      val exp = if (ub < hi)
			    then ifless(tagNum ub, arg, default, exp)
			    else exp
		      in
			exp
		      end
		  | gen (nChunks, chunks, lo, hi) = let (* two or more chunks *)
		      val m = nChunks div 2
		      val (c1, midVal, c2) = (case List.splitAt(chunks, m)
			     of (c1, c2 as (lb, _, _, _)::_) => (c1, lb, c2)
			      | _ => bug "taggedNumSwitch.switch: split"
			    (* end case *))
		    (* INV: case-labels-of(c1) < midVal <= case-labels-of(c2) *)
		      in
			ifless(arg, tagNum midVal,
			  gen (m, c1, lo, midVal-1),
			  gen (nChunks-m, c2, midVal, hi))
		      end
		in
		  gen (length chunks, chunks, lo0, hi0)
		end
	(* map non-exhaustice cases to if-then-else sequence *)
	  fun ifelseWDefault [] = default
	    | ifelseWDefault ((i, act)::r) = ifeq(i, act, ifelseWDefault r)
	(* map exhaustice cases to if-then-else sequence *)
	  fun ifelse [] = bug "taggedNumSwitch: impossible"
	    | ifelse [(_, act)] = act
	    | ifelse ((i, act)::r) = ifeq(i, act, ifelse r)
	  in
	    case (nCases < switchLimit, optRange)
	     of (true, NONE) => ifelseWDefault cases
	      | (true, SOME n) => if nCases <= n then ifelseWDefault cases else ifelse cases
	      | (false, NONE) => let
		  val (lo, hi) = (case cases
			 of (i, _)::_ => (i, #1(List.last cases))
			  | [] => bug "taggedNumSwitch: empty cases"
			(* end case *))
		  val unsigned = (case nk of CPS.P.UINT _ => true | _ => false)
		(* switch with upper-bound test *)
		  val exp = ifless(tagNum hi, arg, default, switch'(lo, hi))
		(* add lower-bound test, if necessary *)
		  val exp = if unsigned andalso lo = 0
			then exp (* no test required *)
			else ifless(arg, tagNum lo, default, exp)
		  in
		    exp
		  end
	      | (false, SOME n) => switch' (0, toII n)
	    (* end case *)
	  end

  (* generate a switch for a boxed integer/word type. *)
    fun boxedNumSwitch (arg, CPS.NUMt ty, nk, cases, default) = let
	  fun branch (cmpOp, i, tr, fl) = CPS.BRANCH(
		CPS.P.CMP{oper=cmpOp, kind=nk}, [arg, CPS.NUM{ival = i, ty = ty}], mkv(),
		tr,
		fl)
	  val cases = numSort cases
	  fun gen (n, cases) = if (n > 4)
		then let (* binary search *)
		  val m = n div 2
		  val (cases1, (i, act), cases2) = let
			fun split (0, prefix, ic::r) = (List.rev prefix, ic, r)
			  | split (n, prefix, ic::r) = split (n-1, ic::prefix, r)
			  | split _ = bug "bogus split"
			in
			  split (m, [], cases)
			end
		  in
		    branch(CPS.P.EQL, i,
		      act,
		      branch(CPS.P.LT, i,
			gen (m, cases1),
			gen (n-m-1, cases2)))
		  end
		else let (* linear search *)
		  fun genCase [] = default
		    | genCase ((i, act)::r) = branch(CPS.P.EQL, i, act, genCase r)
		  in
		    genCase cases
		  end
	  in
	    gen (List.length cases, cases)
	  end (* boxedNumSwitch *)

  (* generate a switch for string patterns *)
    fun stringSwitch (arg, cases, default : CPS.cexp) = let
	  fun ifeq (s, tr, fl) = CPS.BRANCH(CPS.P.STREQL s, [arg], mkv(), tr, fl)
	  fun un_str (PL.STRINGcon s, act) = (s, act)
	    | un_str _ = bug "un_str"
	(* group cases by length of the string *)
	  fun coalesce cases = let
	      (* first sort by length *)
		val cases' = ListMergeSort.sort (fn ((s1,_),(s2,_)) => size s1 > size s2) cases
	      (* get length of first string *)
		val firstLen = size(#1 (List.hd cases'))
	      (* group strings by length *)
		fun gather (n, [], current, acc) = (toII n, current) :: acc
		  | gather (n, (x as (s, a))::rest, current, acc) = let
		      val n' = size s
		      in
			if n' = n
			  then gather(n, rest, x::current, acc)
			  else gather(n', rest, [x], (toII n, current)::acc)
		      end
		in
		  gather (size(#1 (List.hd cases')), cases', [], [])
		end
	(* generate tests for a given group of strings *)
	  fun genGrp (0, (_, act)::_) = (0 : IntInf.int, act)
	    | genGrp (n, cases) = let
		fun try [] = default
		  | try ((s, act) :: r) = ifeq(s, act, try r)
		in
		  (n, try cases)
		end
	(* cases by length *)
	  val bylength = List.map genGrp (coalesce (List.map un_str cases))
	  in
	    pure(CPS.P.LENGTH, [arg], tagNumTy,
	      fn len => taggedNumSwitch(len, tagWordKind, bylength, default, NONE))
	  end

  (* does a datatype constructor have a boxed representation? *)
    fun isboxed (PL.DATAcon((_,A.CONSTANT _, _),_,_)) = false
      | isboxed (PL.DATAcon((_,A.LISTNIL,_),_,_)) = false
      | isboxed (PL.DATAcon((_,rep,_),_,_)) = true
      | isboxed _ = bug "isboxed"

  (* generate switch code for a datatype with the given signature *)
    fun dataconSwitch (arg, sign, cases, default) = let
	  fun tag (PL.DATAcon((_, A.CONSTANT i, _), _, _), act) = (toII i, act)
	    | tag (PL.DATAcon((_, A.TAGGED i, _), _, _), act) = (toII i, act)
	    | tag (_, act) = (0, act)
	  val (boxed, unboxed) = List.partition (isboxed o #1) cases
	  val boxed = List.map tag boxed
	  val unboxed = List.map tag unboxed
	  in
	    case sign
	     of A.CSIG(0, n) =>
		  pure(CPS.P.UNBOX, [arg], tagNumTy,
		    fn x => taggedNumSwitch(x, tagWordKind, unboxed, default, SOME(n-1)))
	      | A.CSIG(n, 0) =>
		  pure(CPS.P.GETCON, [arg], tagNumTy,
		    fn x => taggedNumSwitch(x, tagWordKind, boxed, default, SOME(n-1)))
	      | A.CSIG(1, nu) => let
		(* only one boxed constructor, so get the action for that case *)
		  val boxedAct = (case boxed
			 of [] => default
			  | (_, act)::_ => act
			(* end case *))
		  val unboxedAct = (case unboxed
			 of [] => default
			  | _ => pure(CPS.P.UNBOX, [arg], tagNumTy,
			      fn x => taggedNumSwitch(x, tagWordKind, unboxed, default, SOME(nu-1)))
			(* end case *))
		  in
		    CPS.BRANCH(CPS.P.BOXED, [arg], mkv(), boxedAct, unboxedAct)
		  end
	      | A.CSIG(nb, nu) => let
		  val boxedAct = (case boxed
			 of [] => default
			  | _ => pure(CPS.P.GETCON, [arg], tagNumTy,
			      fn x => taggedNumSwitch(x, tagWordKind, boxed, default, SOME(nb-1)))
			(* end case *))
		  val unboxedAct = (case unboxed
			 of [] => default
			  | _ => pure(CPS.P.UNBOX, [arg], tagNumTy,
			      fn x => taggedNumSwitch(x, tagWordKind, unboxed, default, SOME(nu-1)))
			(* end case *))
		  in
		    CPS.BRANCH(CPS.P.BOXED, [arg], mkv(), boxedAct, unboxedAct)
		  end
	      | A.CNIL => bug "dataconSwitch"
	    (* end case *)
	  end

  (* generate switch code for the given argument and cases *)
    fun switch {rename} {cases=[], default, ...} = default
      | switch {rename} {arg, sign, cases as (c, _)::_, default} = (case c
	   of PL.INTcon{ival, ty} => let
		fun un_int (PL.INTcon{ival, ...}, act) = (ival, act)
		  | un_int _ = bug "un_int"
		val cases = List.map un_int cases
		in
		  if (ty <= Target.defaultIntSz)
		    then taggedNumSwitch(arg, tagIntKind, cases, default, NONE)
		    else boxedNumSwitch(arg, boxNumTy ty, CPS.P.INT ty, cases, default)
		end
	    | PL.WORDcon{ival, ty} => let
		fun un_word (PL.WORDcon{ival, ...}, act) = (ival, act)
		  | un_word _ = bug "un_int"
		val cases = List.map un_word cases
		in
		  if (ty <= Target.defaultIntSz)
		    then taggedNumSwitch(arg, tagWordKind, cases, default, NONE)
		    else boxedNumSwitch(arg, boxNumTy ty, CPS.P.UINT ty, cases, default)
		end
	    | PL.STRINGcon _ => stringSwitch(arg, cases, default)
	    | PL.DATAcon((_, A.EXN _, _), _, _) => let
		val x = mkv()
		fun gen [] = default
		  | gen ((PL.DATAcon((_, A.EXN(A.LVAR p), _), _, _), act)::r) =
		      CPS.BRANCH(CPS.P.PNEQ, [CPS.VAR x, rename p], mkv(), gen r, act)
		  | gen _ = bug "exnSwitch"
		in
		  CPS.PURE(CPS.P.GETEXN, [arg], x, CPSUtil.BOGt, gen cases)
		end
	    | PL.DATAcon _ => dataconSwitch(arg, sign, cases, default)
	  (* end case *))

  end

(* err-handler.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Error repair for ml-antlr
 *)

functor AntlrErrHandler (
    structure Tok : ANTLR_TOKENS
    structure Lex : ANTLR_LEXER
  ) : sig

  exception ParseError

  type 'a err_handler
  type wstream
  type lexer = Lex.strm -> Tok.token * AntlrStreamPos.span * Lex.strm
  type 'a wreader = wstream -> 'a * AntlrStreamPos.span * wstream

  val mkErrHandler : { get : unit -> 'a, put : 'a -> unit }
		     -> 'a err_handler * Tok.token wreader
  val launch  : 'a err_handler * lexer * 'b wreader * bool ->
	        Lex.strm -> ('b option * Lex.strm * Tok.token AntlrRepair.repair list)
  val failure : 'a err_handler -> 'b

  val getPos    : wstream -> AntlrStreamPos.pos
  val getSpan   : wstream -> AntlrStreamPos.span

  val whileDisabled : 'b err_handler -> (unit -> 'a) -> 'a 

(*
  val wrap   : err_handler -> (R.strm -> ('a * R.strm)) -> R.strm -> ('a * R.strm)
  val tryProds : 'b err_handler -> (R.strm -> 'a) list -> R.strm -> 'a
*)

end = struct

  exception ParseError
  exception Unrepairable

  structure AR = AntlrRepair

  structure WS = AntlrWrappedStream(
    structure Tok = Tok
    structure Lex = Lex)

  type wstream = WS.wstream
  val getPos = WS.getPos
  val getSpan = WS.getSpan

  type lexer = Lex.strm -> Tok.token * AntlrStreamPos.span * Lex.strm
  type 'a wreader = wstream -> 'a * AntlrStreamPos.span * wstream
  type 'a checkpoint = 'a * unit SMLofNJ.Cont.cont * wstream

  datatype 'a err_handler = EH of {				  
    checkpoints : 'a checkpoint list ref,
    maxPos : WS.tok_pos ref,
    cont : unit SMLofNJ.Cont.cont option ref,
    get : unit -> 'a,
    put : 'a -> unit,
    rs : WS.repair_state,
    enabled : bool ref
  }

  fun getGet (EH {get,  ...}) = get
  fun getPut (EH {put,  ...}) = put
  fun getRS  (EH {rs,   ...}) = rs

  fun getCont (EH {cont, ...}) = !cont
  fun setCont (EH {cont, ...}, new) = cont := new

  fun getCheckpoints (EH {checkpoints,   ...}) = !checkpoints
  fun setCheckpoints (EH {checkpoints,   ...}, new) = checkpoints := new

  fun getMaxPos (EH {maxPos,   ...}) = !maxPos
  fun setMaxPos (EH {maxPos,   ...}, new) = maxPos := new
 
  fun getEnabled (EH {enabled, ...}) = !enabled
  fun setEnabled (EH {enabled, ...}, n) = enabled := n
(*
  fun getRepairs (EH {repairs, ...}) = !repairs 
  fun addRepair  (EH {repairs, ...}, n) = repairs := (!repairs) @ [n] *)

  fun mkErrHandler {get, put} = let
        val cont = ref NONE
	val checkpoints = ref []
	val maxPos = ref ~1
        val eh = EH {
	      cont = cont, checkpoints = checkpoints,
	      maxPos = maxPos, get = get, put = put,
	      rs = WS.mkRepairState(), enabled = ref true
	    }
        fun lex ws = (case !cont
	       of SOME _ => (
		    maxPos := Int.max (WS.getTokPos ws, !maxPos);
		    WS.get1 ws)
		| NONE => if WS.getTokPos ws > !maxPos 
		    then let
		      val () = SMLofNJ.Cont.callcc (fn k => (
				checkpoints := (get(), k, ws) :: !checkpoints;
				maxPos := WS.getTokPos ws))
		      in
			WS.get1 ws
		      end
		    else WS.get1 ws
	      (* end case *))
        in
	  (eh, lex)
        end

  val isEOF = Tok.isEOF o #1 o WS.get1

  val minAdvance = 1

  fun restoreCheckpoint (eh, (x, cont, strm)) = (
	getPut eh x;  (* restore refcell data for checkpoint *)
	setMaxPos (eh, WS.getTokPos strm);
	SMLofNJ.Cont.throw cont ())

  fun tryRepair (eh, c) = let
        val oldMax = getMaxPos eh
	val firstTime = ref true
	val () = SMLofNJ.Cont.callcc (fn k => (setCont (eh, SOME k)))
        in
	  if !firstTime
	    then ( (* first time through, try the repair *)
	      firstTime := false; restoreCheckpoint (eh, c))
	    else ( (* second time through, return the distance achieved *)
	      setCont (eh, NONE); getMaxPos eh - oldMax)
        end

  local

    val allToks = List.filter (not o Tok.isEOF) Tok.allToks
    fun mkDelete strm = (WS.getPos strm, AR.Delete [#1 (WS.get1 strm)])
    fun mkInsert strm tok = (WS.getPos strm, AR.Insert [tok])
    fun mkSubst  strm tok = (WS.getPos strm, AR.Subst { old = [#1 (WS.get1 strm)], new = [tok] })
    fun allRepairs strm = 
	  (if isEOF strm then [] else [mkDelete strm]) @
	  map (mkInsert strm) allToks @
	  (if isEOF strm then [] else map (mkSubst strm) allToks)

    fun involvesKW (_, r) = (case r
	  of AR.Insert toks => List.exists Tok.isKW toks
	   | AR.Delete toks => List.exists Tok.isKW toks
	   | AR.Subst {old,new} => List.exists Tok.isKW (old @ new)
	   | AR.FailureAt _ => false
         (* end case *))

  in
  fun trySingleToken eh = let
	val RS = getRS eh
	val oldRepairs = WS.getRepairs RS
	val oldMax = getMaxPos eh
	val oldMaxRepair = WS.maxRepairPos RS
	val oldCheckpoints = getCheckpoints eh
	fun restoreToErr() = (WS.setRepairs (RS, oldRepairs); setMaxPos (eh, oldMax))
      (* stream value for checkpoint *)
	fun strmOf (_, _, strm) = strm
	fun setupRepair (r, c::cs) = 
	      WS.setRepairs (RS, WS.addRepair (oldRepairs, WS.getTokPos (strmOf c), r))
	  | setupRepair _ = raise Fail "bug"
	fun try (_::c::cs, [], best, n) = 
	      if n < 15 andalso WS.getTokPos (strmOf c) > oldMaxRepair
	      then try (c::cs, allRepairs (strmOf c), best, n+1)
	      else try ([], [], best, n)
	  | try (c::cs, r::rs, best, n) = (
	      restoreToErr(); setupRepair (r, c::cs);
	      let val score = tryRepair (eh, c) 
			        - (if involvesKW r then 2 else 0)
			        + (case #2 r
				    of AR.Insert _ => ~1
				     | AR.Delete _ => 1
				     | AR.Subst  _ => 0
				     | _ => 0)
		  val oldScore = case best of NONE => 0 
					    | SOME (_, _, s) => s
	      in if score > oldScore andalso score > minAdvance
		 then try (c::cs, rs, SOME (c::cs, r, score), n)
		 else try (c::cs, rs, best, n)
	      end)
	  | try (_, [], SOME (c::cs, r, score), _) = 
	      (setupRepair (r, c::cs); 
	       setCheckpoints (eh, c::cs);
	       setMaxPos (eh, List.length cs); 
	       restoreCheckpoint (eh, c))
	  | try _ = restoreToErr()
	val curStrm = strmOf (hd oldCheckpoints)
        in if WS.getTokPos curStrm <= WS.maxRepairPos RS then ()
	   else try (oldCheckpoints, allRepairs curStrm, NONE, 1)
        end
  end

  val maxDel = 50

  fun tryDeletion eh = let
        fun getn (strm, 0, acc) = SOME (rev acc)
	  | getn (strm, n, acc) = let
	      val (tok, _, strm') = WS.get1 strm
	      in
	        if Tok.isEOF tok then NONE
		else getn (strm', n-1, tok::acc)
	      end
	val rs = getRS eh
	val oldRepairs = WS.getRepairs rs
	val oldMax = getMaxPos eh
	val oldRepairMax = WS.maxRepairPos rs
	fun restoreToErr() = (WS.setRepairs (rs, oldRepairs); setMaxPos (eh, oldMax))
      (* stream value for checkpoint *)
	fun strmOf (_, _, strm) = strm
	val cs = getCheckpoints eh
	fun tryCS ([], n, max) = ()
	  | tryCS (c::cs, n, max) = 
	      if WS.getTokPos (strmOf c) <= oldRepairMax 
	         orelse oldMax - WS.getTokPos (strmOf c) > maxDel then () 
	      else
	        (WS.setRepairs (rs, 
		   WS.addRepair (oldRepairs, WS.getTokPos (strmOf c),
	             (WS.getPos (strmOf c), AR.Delete (valOf (getn (strmOf c, n, []))))));
		 setMaxPos (eh, WS.getTokPos (strmOf c));
		 if tryRepair (eh, c) > minAdvance + 2
		 then (setCheckpoints (eh, c::cs); 
		       restoreCheckpoint (eh, c))
		 else (restoreToErr(); tryCS (cs, n+1, max)))
	and tryN (n, c::cs, max) = (case getn (strmOf c, n, [])
              of NONE => ()
	       | SOME toks => (tryCS (c::cs, n, max);
			       if n > max then () else tryN (n+1, c::cs, max))
             (* end case *))
	  | tryN _ = raise Fail "bug"
        in 
          tryN (1, [hd cs], 5);
	  tryN (1, cs, maxDel)
        end

  fun failure eh = 
        if getEnabled eh 
	then (case getCont eh
               of NONE => (trySingleToken eh;
			   tryDeletion eh;
			   raise Unrepairable)
		| SOME k => SMLofNJ.Cont.throw k ()
	      (* end case *))
	else raise ParseError

  fun launch (eh, lex, parse, reqEOF) strm = let
	val wstrm = WS.wrap (getRS eh, strm, lex)
        in let val (result, _, wstrm') = parse wstrm
	       val (strm', repairs) = WS.unwrap wstrm'
	   in 
	     if reqEOF andalso not (isEOF wstrm') then failure eh
	     else ();
	     (SOME result, strm', repairs) 
	   end
	   handle Unrepairable => let
	     val (_, repairs) = WS.unwrap wstrm
	     val (tok, (pos, _), _) = (WS.get1 o #3 o hd o getCheckpoints) eh
	     in (NONE, strm, repairs @ [(pos, AR.FailureAt tok)]) end
        end

  fun whileDisabled eh f = let
        val oldEnabled = getEnabled eh
        in
	  setEnabled (eh, false);
	  (f () handle e => (setEnabled (eh, oldEnabled);
			     raise e))
	  before setEnabled (eh, oldEnabled)
        end

(*
  fun throwIfEH (eh, t) = 
        if getEnabled eh then 
	  Option.app (fn k => SMLofNJ.Cont.throw k (SOME t)) (getCont eh)
	else ()

  fun wrap eh f t = if not (getEnabled eh) then f t else let
	val cont_ref : retry_cont option ref = ref NONE
	val state = (getGet eh) () 
	val t' = SMLofNJ.Cont.callcc (fn k => (cont_ref := SOME k; t))
	val retry = (t', valOf (!cont_ref)) 
        in
	  getPut eh state; 
	  f t'
	  handle RepairableError => (
	    throwIfEH (eh, t');
	    raise JumpOut [retry])
	| JumpOut stack => (
	    throwIfEH (eh, t');
	    raise JumpOut (retry::stack))
        end

  fun findWindow (stack) = let
	val revStack = rev stack
	val rightMost = hd revStack
	fun TOf (t, _) = t
	fun find [] = raise (Fail "BUG: findWindow given an empty stack")
	  | find [top] = (top, rightMost)
	  | find (top::stack) = 
	      if R.farEnoughWindow {startAt = TOf top, endAt = TOf rightMost}
	      then (top, rightMost)
	      else find stack
        in
	  find revStack
        end

  fun tryRepair (eh, cont) t = 
        (case SMLofNJ.Cont.callcc (fn k => (setCont (eh, SOME k); NONE))
	  of NONE => 
	     (* first time through, try the repair *)
	       SMLofNJ.Cont.throw cont t
	   | SOME t' => 
	     (* second time through, return the new right-most strm *)
	       (setCont (eh, NONE); t')
	 (* end case *))

  fun primaryRepair (eh, stack) = let
	val ((leftT, leftCont), (rightT, rightCont)) = 
	    findWindow stack
	val repair = R.chooseRepair {
			startAt = leftT,
			endAt = rightT,
			try = tryRepair (eh, leftCont)
		     }
        in case repair
	    of SOME {repair, repaired} => 
	         SOME (repair, leftCont, repaired)
	     | NONE => NONE
        end

  fun secondaryRepair (eh, revStack) = let
	val stack = rev revStack
	val (errStrm, errCont) = hd stack
	fun try ((strm, cont), strm', next) = let
	      val strm'' = tryRepair (eh, cont) strm'
    	      in case (R.tryDeletion {oldStartAt = strm, 
				      startAt = strm', 
				      endAt = strm''})
		  of SOME r => SOME (r, cont, strm')
		   | NONE => next()
	      end
	fun rightRepair (strm, n) = 
	      if n = 0 then NONE
	      else let 
	        val strm' = R.skip (strm, 1)
		in 
		  try (hd stack, strm', fn () => rightRepair (strm', n-1))
		end
	fun leftRightRepair (strm, []) = 
	      if R.isEmpty strm then
		(addRepair (eh, (R.getPos errStrm, 
				 Repair.FailureAt (#1 (R.get1 errStrm))));
		 raise UnrepairableError)
	      else leftRightRepair (R.skip (strm, 1), stack)
	  | leftRightRepair (strm, top::stack) = 
	      try (top, strm, fn () => leftRightRepair (strm, stack))
        in case rightRepair (errStrm, 5)
	    of SOME r => r
	     | _      => valOf (leftRightRepair (errStrm, []))
        end

  fun repair (eh, stack) = (case primaryRepair (eh, stack)
	of SOME r => r
	 | NONE => secondaryRepair (eh, stack)
       (* end case *))

  fun launch eh f t = let
        val (x, _, t') = wrap eh f t 
	    handle JumpOut stack => let
	        val (r, cont, t') = repair (eh, stack)
		in
		  addRepair (eh, r);
		  SMLofNJ.Cont.throw cont t'
		end
        in
	  throwIfEH (eh, t');
	  (SOME x, t', getRepairs eh)
        end
    handle UnrepairableError =>
      (NONE, t, getRepairs eh)
*)

(*
  fun tryProds eh prods strm = let
	fun try [] = raise RepairableError
	  | try (prod :: prods) = let 
	      val state = (getGet eh) ()
	      in
	        whileDisabled eh (fn () => prod strm)
		handle _ => 
		  (getPut eh state;
		   try (prods))
	      end
        in
          try prods
        end
*)

end

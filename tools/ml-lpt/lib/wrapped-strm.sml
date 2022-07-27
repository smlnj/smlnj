(* wrapped-strm.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * "wrapped" streams, which track the number of tokens read
 * and allow "prepending" a sequence of tokens.
 *)

functor AntlrWrappedStream (
    structure Tok : ANTLR_TOKENS
    structure Lex : ANTLR_LEXER
  ) :> sig

  type tok_pos = Int.int	(* position in terms of number of tokens *)
  type lexer = Lex.strm -> Tok.token * AntlrStreamPos.span * Lex.strm

  type repairs
  val addRepair : repairs * tok_pos * Tok.token AntlrRepair.repair -> repairs

  type repair_state
  val mkRepairState : unit -> repair_state
  val getRepairs : repair_state -> repairs
  val setRepairs : repair_state * repairs -> unit
  val maxRepairPos : repair_state -> tok_pos

  type wstream
  val wrap   : repair_state * Lex.strm * lexer -> wstream
  val unwrap : wstream -> Lex.strm * Tok.token AntlrRepair.repair list

  val get1      : wstream -> Tok.token * AntlrStreamPos.span * wstream
  val getPos    : wstream -> AntlrStreamPos.pos
  val getSpan   : wstream -> AntlrStreamPos.span
  val getTokPos : wstream -> tok_pos

end = struct

  type tok_pos = Int.int	(* position in terms of number of tokens *)
  type repair = tok_pos * Tok.token AntlrRepair.repair
  type repairs = repair list
  type repair_state = repairs ref (* invariant: at most one repair per tok_pos *)
  type lexer = Lex.strm -> Tok.token * AntlrStreamPos.span * Lex.strm

  datatype global_state = GS of {
    lex : (Lex.strm -> Tok.token * AntlrStreamPos.span * Lex.strm),
    repairs : repair_state
  }

  datatype wstream = WSTREAM of {
    curTok : tok_pos,
    strm : Lex.strm,
    gs : global_state
  }

  fun mkRepairState() = ref []
  fun getRepairs repairs = !repairs
  fun setRepairs (repairs, new) = repairs := new
  fun maxRepairPos (ref []) = ~1
    | maxRepairPos (ref ((p, _)::_)) = p

  open AntlrRepair

  fun addRepair (rs, pos, r) =
        if pos > maxRepairPos (ref rs) then (pos, r)::rs
	else raise Fail (String.concat [
		"bug: repairs not monotonic adding at ",
		Int.toString pos, " to a max pos of ",
		Int.toString (maxRepairPos (ref rs))])

  fun wrap (repairs, strm, lex) = 
        WSTREAM {strm = strm, curTok = 0, gs = GS {lex = lex, repairs = repairs}}
  fun unwrap (WSTREAM {strm, gs = GS {repairs, ...}, ...}) = 
        (strm, rev (#2 (ListPair.unzip (!repairs))))

  fun skip1 lex strm = let 
        val (_, _, strm') = lex strm 
        in strm' end
  fun get1 (WSTREAM {strm, curTok, gs = gs as GS {lex, repairs}}) = let
        fun findRepair [] = NONE
	  | findRepair ((pos, r)::rs) = if curTok = pos then SOME r 
					else findRepair rs
        in case findRepair (!repairs)
	    of NONE => let
		 val (tok, span, strm') = lex strm
	         in 
		   (tok, span, WSTREAM {strm = strm', curTok = curTok + 1, gs = gs})
	         end
	     | SOME (p, Insert [tok]) => 
	         (tok, (p, p), WSTREAM {strm = strm, curTok = curTok + 1, gs = gs})
	     | SOME (p, Delete toks) => let
		 val strm' = foldl (fn (_, s) => (skip1 lex) s) strm toks
		 val (tok, span, strm'') = lex strm'
	         in 
		   (tok, span, WSTREAM {strm = strm'', curTok = curTok + 1, gs = gs})
	         end
	     | SOME (p, Subst {old = [old], new = [new]}) => 
	         (new, (p, p), WSTREAM {strm = skip1 lex strm, curTok = curTok + 1, gs = gs})
	     | SOME (p, FailureAt _) => raise Fail "bug: findRepair found FailureAt"
	     | _ => raise Fail "bug: unimplemented"
        end

  (* get position AFTER trimming whitespace *)
  fun getPos ws = let val (_, (left, _), _) = get1 ws in left end
  fun getSpan ws = (getPos ws, getPos ws)
  fun getTokPos (WSTREAM {curTok, ...}) = curTok
        

end

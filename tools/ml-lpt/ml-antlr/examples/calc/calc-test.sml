(* calc-test.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CalcTest = 
  struct

    structure Tok = CalcParseTokens

    structure ListLex = struct
      type strm = Tok.token list
      fun lex [] = (Tok.EOF, (0, 0), [])
	| lex (t::ts) = (t, (0, 0), ts)
      type pos = AntlrStreamPos.pos
      type span = pos * pos
      fun getPos _ = 0
    end

    structure CP = CalcParseFn(ListLex)

    fun fragToToks (SMLofNJ.QUOTE s) = let
          val sref = ref true
          fun input _ = if !sref then
			  (sref := false; s)
			else ""
	  val lex = CalcLex.lex (AntlrStreamPos.mkSourcemap())
          fun loop ((Tok.EOF, _, _), accum) = rev accum
	    | loop ((s, _, strm), accum) = loop (lex strm, s::accum)
          in
            loop (lex (CalcLex.streamify input), [])
          end
      | fragToToks (SMLofNJ.ANTIQUOTE i) = [Tok.DummyExp i]

    fun % frags = let
      val (r, s', errs, {vars, nums}) = CP.parseexp ListLex.lex AtomMap.empty (List.concat (map fragToToks frags))
    in
      app (fn (pos, repair) => print (AntlrRepair.actionToString Tok.toString repair ^ "\n")) errs;
      print (" -- VARS: " ^ (String.concatWith ", " vars) ^ "\n");
      print (" -- NUMS: " ^ (String.concatWith ", " (map Int.toString nums)) ^ "\n");
      (r, s')
    end

(*    val _ = Control.quotation := true *)

  end

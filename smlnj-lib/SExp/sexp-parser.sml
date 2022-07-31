(* sexp-parser.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Author: Damon Wang (with modifications by John Reppy)
 *)

structure SExpParser : sig

    val parse : TextIO.instream -> SExp.value list

    val parseFile : string -> SExp.value list

  end = struct

    structure Lex = SExpLexer
    structure T = SExpTokens
    structure S = SExp

    fun parse' (srcMap, inStrm) = let
	  fun error (pos, msg, tok) = raise Fail(concat[
		  "error ", AntlrStreamPos.spanToString srcMap pos, ": ",
		  msg, ", found '", T.toString tok, "'"
		])
(* TODO: add support for lexer errors *)
          (* the lexer *)
	  val lexer = Lex.lex srcMap
	(* returns (tok, pos, nextStrm, strm) where the difference between
	 * nextStrm and strm is that tok is the next token in strm
	 * whereas the token _after_ tok is the next up in nextStrm.
	 *
	 * In other words, strm has had all leading whitespace consumed.
	 *)
	  fun lexNWS (strm : Lex.strm) = (case lexer strm
		 of (T.WHITE, _, strm) => lexNWS strm
		  | (tok, pos, nextStrm) => (tok, pos, nextStrm, strm)
		(* end case *))
	  fun parseValue (strm : Lex.strm) = let
		val (tok, pos, strm) = lexer strm
		in
		  case tok
		   of T.DELIM(delim, T.OPEN) => parseList (delim, strm)
		    | T.QUOTE => let val (strm', value) = parseValue strm
			in
			  (strm, S.QUOTE value)
			end
		    | T.KW_true => (strm, S.BOOL true)
		    | T.KW_false => (strm, S.BOOL false)
		    | T.INT n => (strm, S.INT n)
		    | T.FLOAT f => (strm, S.FLOAT f)
		    | T.STRING s => (strm, S.STRING s)
		    | T.SYMBOL s => (strm, S.SYMBOL (Atom.atom s))
		    | _ => error (pos, "parsing value", tok)
		  (* end case *)
		end
	(* parse a list, where delim is the type of delimiter enclosing the list *)
	  and parseList (delim : T.delim_type, strm : Lex.strm) = let
		fun matchDelim (T.DELIM(dType, T.CLOSE)) = (dType = delim)
		  | matchDelim tok = false
		val (tok, _, nextStrm, strm) = lexNWS strm
		in
		  if matchDelim tok
		    then (nextStrm, S.LIST [])
		    else let
		      fun loop (strm, items) = let
(* FIXME: better error reporting for lists; unclosed vs mismatched delims *)
			    val (strm, v) = parseValue strm
			  (* expect either a separator (whitespace) or a delimiter *)
			    val (tok, pos, nextStrm, strm) = lexNWS strm
			    in
			      if matchDelim tok
				then (nextStrm, v::items)
				else loop(strm, v::items)
			    end
		      val (strm, items) = loop (strm, [])
		      in
			(strm, S.LIST (List.rev items))
		      end
		end
	(* parse top-level s-expressions until EOF *)
	  fun parseSExps (strm, sexps) = (case lexer strm
		 of (T.WHITE, _, strm) => parseSExps (strm, sexps)
                  | (T.EOF, _, _) => List.rev sexps
		  | _ => let
                      val (strm, sexp) = parseValue strm
                      in
                        parseSExps (strm, sexp::sexps)
                      end
                (* end case *))
	  in
	    parseSExps (Lex.streamifyInstream inStrm, [])
	  end

    fun parse inStrm = parse' (AntlrStreamPos.mkSourcemap (), inStrm)

    fun parseFile fileName = let
	  val inStrm = TextIO.openIn fileName
	  val v = parse' (AntlrStreamPos.mkSourcemap' fileName, inStrm)
		handle ex => (TextIO.closeIn inStrm; raise ex)
	  in
	    TextIO.closeIn inStrm;
	    v
	  end

  end

(* json-parser.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure JSONParser :> sig

  (* abstract type of JSON input *)
    type source = JSONSource.source

  (* open a text input stream as a source *)
    val openStream : TextIO.instream -> source

  (* open a text file as a source *)
    val openFile : string -> source

  (* open a string as a source *)
    val openString : string -> source

  (* close a source *)
    val close : source -> unit

    val parse : source -> JSON.value

    val parseFile : string -> JSON.value

  end = struct

    structure Lex = JSONLexer
    structure T = JSONTokens
    structure J = JSON

    datatype source = datatype JSONSource.source

    val openStream = JSONSource.openStream
    val openFile = JSONSource.openFile
    val openString = JSONSource.openString
    val close = JSONSource.close

    fun parse (Src{closed = ref true, ...}) = raise Fail "closed JSON source"
      | parse (src as Src{srcMap, strm, ...}) = let
	  val errorMsg = JSONSource.errorMsg src
	  fun error arg = raise Fail(errorMsg arg)
	  val lexer = Lex.lex srcMap
	  fun parseValue (strm : Lex.strm) = let
		val (tok, span, strm) = lexer strm
		in
		  case tok
		   of T.LB => parseArray strm
		    | T.LCB => parseObject strm
		    | T.KW_null => (strm, J.NULL)
		    | T.KW_true => (strm, J.BOOL true)
		    | T.KW_false => (strm, J.BOOL false)
		    | T.INT n => (strm, J.INT n)
		    | T.FLOAT f => (strm, J.FLOAT f)
		    | T.STRING s => (strm, J.STRING s)
		    | _ => error (span, "parsing value", tok)
		  (* end case *)
		end
	  and parseArray (strm : Lex.strm) = (case lexer strm
		 of (T.RB, _, strm) => (strm, J.ARRAY[])
		  | _ => let
		      fun loop (strm, items) = let
			    val (strm, v) = parseValue strm
			  (* expect either a "," or a "]" *)
			    val (tok, span, strm) = lexer strm
			    in
			      case tok
			       of T.RB => (strm, v::items)
				| T.COMMA => loop (strm, v::items)
				| _ => error (span, "parsing array", tok)
			      (* end case *)
			    end
		      val (strm, items) = loop (strm, [])
		      in
			(strm, J.ARRAY(List.rev items))
		      end
		(* end case *))
	  and parseObject (strm : Lex.strm) = let
		fun parseField ((T.STRING s, _, strm), flds) = (case lexer strm
		       of (T.COLON, _, strm) => let
			    val (strm, v) = parseValue strm
			    in
			      parseFields (strm, (s, v)::flds)
			    end
			| (tok, span, _) => error (span, "parsing field", tok)
		      (* end case *))
		  | parseField ((tok, span, _), _) = error (span, "parsing field", tok)
		and parseFields (strm, flds) = (case lexer strm
		       of (T.RCB, span, strm) => (strm, J.OBJECT(List.rev flds))
			| (T.COMMA, span, strm) => parseField (lexer strm, flds)
			| (tok, span, _) => error (span, "parsing object", tok)
		      (* end case *))
		in
		  case lexer strm
		   of (T.RCB, span, strm) => (strm, J.OBJECT[])
		    | tokEtc => parseField (tokEtc, [])
		  (* end case *)
		end
	  val (inStrm, value) = parseValue (!strm)
	  in
	    strm := inStrm;
	    value
	  end

    fun parseFile fileName = let
	  val inStrm = openFile fileName
	  val v = parse inStrm
		handle ex => (close inStrm; raise ex)
	  in
	    close inStrm;
	    v
	  end

  end

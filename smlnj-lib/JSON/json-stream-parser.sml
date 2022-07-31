(* json-stream-parser.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * TODO: use the same "source" abstraction supported by the `JSONParser`
 * structure.
 *)

structure JSONStreamParser :> sig

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

  (* callback functions for the different parsing events *)
    type 'ctx callbacks = {
	null : 'ctx -> 'ctx,
	boolean : 'ctx * bool -> 'ctx,
	integer : 'ctx * IntInf.int -> 'ctx,
	float : 'ctx * real -> 'ctx,
	string : 'ctx * string -> 'ctx,
	startObject : 'ctx -> 'ctx,
	objectKey : 'ctx * string -> 'ctx,
	endObject : 'ctx -> 'ctx,
	startArray : 'ctx -> 'ctx,
	endArray : 'ctx -> 'ctx,
	error : 'ctx * string -> unit
      }

    val parse : 'ctx callbacks -> (source * 'ctx) -> 'ctx

    val parseFile : 'ctx callbacks -> (string * 'ctx) -> 'ctx

  end = struct

    structure Lex = JSONLexer
    structure T = JSONTokens

    datatype source = datatype JSONSource.source

    val openStream = JSONSource.openStream
    val openFile = JSONSource.openFile
    val openString = JSONSource.openString
    val close = JSONSource.close

  (* callback functions for the different parsing events *)
    type 'ctx callbacks = {
	null : 'ctx -> 'ctx,
	boolean : 'ctx * bool -> 'ctx,
	integer : 'ctx * IntInf.int -> 'ctx,
	float : 'ctx * real -> 'ctx,
	string : 'ctx * string -> 'ctx,
	startObject : 'ctx -> 'ctx,
	objectKey : 'ctx * string -> 'ctx,
	endObject : 'ctx -> 'ctx,
	startArray : 'ctx -> 'ctx,
	endArray : 'ctx -> 'ctx,
	error : 'ctx * string -> unit
      }

    fun error (cb : 'a callbacks, ctx, msg) = (
	  #error cb (ctx, msg);
	  raise Fail "error")

    fun parse (cb : 'a callbacks) (src as Src{srcMap, strm, ...}, ctx) = let
	  val lexer = Lex.lex srcMap
	  val errorMsg = JSONSource.errorMsg src
	  fun err (ctx, span, msg, tok) = error (cb, ctx, errorMsg (span, msg, tok))
	  fun parseValue (strm : Lex.strm, ctx) = let
		val (tok, span, strm) = lexer strm
		in
		  case tok
		   of T.LB => parseArray (strm, ctx)
		    | T.LCB => parseObject (strm, ctx)
		    | T.KW_null => (strm, #null cb ctx)
		    | T.KW_true => (strm, #boolean cb (ctx, true))
		    | T.KW_false => (strm, #boolean cb (ctx, false))
		    | T.INT n => (strm, #integer cb (ctx, n))
		    | T.FLOAT f => (strm, #float cb (ctx, f))
		    | T.STRING s => (strm, #string cb (ctx, s))
		    | _ => err (ctx, span, "error parsing value", tok)
		  (* end case *)
		end
	  and parseArray (strm : Lex.strm, ctx) = (case lexer strm
		 of (T.RB, _, strm) => (strm, #endArray cb (#startArray cb ctx))
		  | _ => let
		      fun loop (strm, ctx) = let
			    val (strm, ctx) = parseValue (strm, ctx)
			  (* expect either a "," or a "]" *)
			    val (tok, span, strm) = lexer strm
			    in
			      case tok
			       of T.RB => (strm, ctx)
				| T.COMMA => loop (strm, ctx)
				| _ => err (ctx, span, "error parsing array", tok)
			      (* end case *)
			    end
		      val ctx = #startArray cb ctx
		      val (strm, ctx) = loop (strm, #startArray cb ctx)
		      in
			(strm, #endArray cb ctx)
		      end
		(* end case *))
	  and parseObject (strm : Lex.strm, ctx) = let
		fun parseField (strm, ctx) = (case lexer strm
		       of (T.STRING s, span, strm) => let
			    val ctx = #objectKey cb (ctx, s)
			    in
			      case lexer strm
			       of (T.COLON, _, strm) => parseValue (strm, ctx)
				| (tok, span, _) =>
				    err (ctx, span, "error parsing field", tok)
			      (* end case *)
			    end
			| _ => (strm, ctx)
		      (* end case *))
		fun loop (strm, ctx) = let
		      val (strm, ctx) = parseField (strm, ctx)
		      in
			(* expect either "," or "}" *)
			case lexer strm
			 of (T.RCB, span, strm) => (strm, ctx)
			  | (T.COMMA, span, strm) => loop (strm, ctx)
			  | (tok, span, _) =>
			      err (ctx, span, "error parsing object", tok)
			(* end case *)
		      end
		val ctx = #startObject cb ctx
		val (strm, ctx) = loop (strm, #startObject cb ctx)
		in
		  (strm, #endObject cb ctx)
		end
	  val (inStrm, cxt) = parseValue (!strm, ctx)
	  in
	    strm := inStrm;
	    ctx
	  end

    fun parseFile cb = let
	  val parse = parse cb
	  fun parser (fileName, ctx) = let
		val inStrm = openFile fileName
		val ctx = parse (inStrm, ctx)
		      handle ex => (close inStrm; raise ex)
		in
		  close inStrm;
		  ctx
		end
	  in
	    parser
	  end

  end

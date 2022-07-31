(* json-source.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * JSON input sources.  Note that this module is internal the the library.
 *)

structure JSONSource : sig

    datatype source = Src of {
	srcMap : AntlrStreamPos.sourcemap,
	strm : JSONLexer.strm ref,
	closeFn : unit -> unit,
	closed : bool ref
      }

  (* open a text input stream as a source *)
    val openStream : TextIO.instream -> source

  (* open a text file as a source *)
    val openFile : string -> source

  (* open a string as a source *)
    val openString : string -> source

  (* close a source *)
    val close : source -> unit

    val errorMsg : source -> AntlrStreamPos.span * string * JSONTokens.token -> string

  end = struct

    structure Lex = JSONLexer
    structure T = JSONTokens

    datatype source = Src of {
	srcMap : AntlrStreamPos.sourcemap,
	strm : Lex.strm ref,
	closeFn : unit -> unit,
	closed : bool ref
      }

    fun openStream inS = let
	  val closed = ref false
	  in
	    Src{
		srcMap = AntlrStreamPos.mkSourcemap (),
		strm = ref(Lex.streamifyInstream inS),
		closeFn = fn () => (),
		closed = closed
	      }
	  end

    fun openFile file = let
	  val closed = ref false
	  val inStrm = TextIO.openIn file
	  in
	    Src{
		srcMap = AntlrStreamPos.mkSourcemap (),
		strm = ref(Lex.streamifyInstream inStrm),
		closeFn = fn () => TextIO.closeIn inStrm,
		closed = closed
	      }
	  end

    fun openString s = let
	  val closed = ref false
	  val data = ref s
	  fun input () = (!data before data := "")
	  in
	    Src{
		srcMap = AntlrStreamPos.mkSourcemap (),
		strm = ref(Lex.streamify input),
		closeFn = fn () => (),
		closed = closed
	      }
	  end

    fun close (Src{closed = ref true, ...}) = ()
      | close (Src{closed, closeFn, ...}) = (
	  closed := true;
	  closeFn())

    fun errorMsg (Src{srcMap, ...}) (span, _, T.ERROR msg) = concat(
	  "error " :: AntlrStreamPos.spanToString srcMap span :: ": " ::
	    msg)
      | errorMsg (Src{srcMap, ...}) (span, msg, tok) = concat[
	    "error ", AntlrStreamPos.spanToString srcMap span, ": ",
	    msg, ", found '", T.toString tok, "'"
	  ]

  end

(* parser.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Parser :> sig

    type file = {
	  name : string,		(* the file's name *)
	  errStrm : Error.err_stream,	(* the error stream for the file *)
	  decls : ParseTree.decl list	(* the top-level declarations in the file *)
	}

  (* takes the name of an ASDL file and parses it (and its includes).  Note
   * that redundant includes are skipped.
   *)
    val parse : string -> {
	    includes : file list,
	    file : file
	  }

  end = struct

    structure PT = ParseTree
    structure AR = AntlrRepair

    structure FileTbl = HashTableFn (
      struct
	type hash_key = string
	val hashVal = HashString.hashString
	val sameKey : string * string -> bool = (op =)
      end)

    type file = {
	  name : string,		(* the file's name *)
	  errStrm : Error.err_stream,	(* the error stream for the file *)
	  decls : ParseTree.decl list	(* the top-level declarations in the file *)
	}

  (* error function for lexers *)
    fun lexErr errStrm (pos, msg) = Error.errorAt(errStrm, (pos, pos), msg)

    local
    (* map tokens to strings; when adding a token, we use a generic name where it
     * makes sense
     *)
      fun tokToString AR.ADD (ASDLTokens.LID _) = "<lower-case identifier>"
	| tokToString AR.DEL (ASDLTokens.LID x) = Atom.toString x
	| tokToString AR.ADD (ASDLTokens.UID _) = "<upper-case identifier>"
	| tokToString AR.DEL (ASDLTokens.UID x) = Atom.toString x
	| tokToString _ (ASDLTokens.CODE _) = "<code>"
	| tokToString _ tok = ASDLTokens.toString tok

    (* error function for parsers *)
      val parseErr = Error.parseError tokToString

    (* glue together the lexer and parser *)
      structure ASDLParser = ASDLParseFn(ASDLLex)

    (* parse one file *)
      fun parseOne (errStrm, inStrm) = let
	    fun get () = TextIO.input inStrm
	    val lexer = ASDLLex.lex (Error.sourceMap errStrm) (lexErr errStrm)
	    val (res, _, errs) = ASDLParser.parse lexer (ASDLLex.streamify get)
	    in
	      TextIO.closeIn inStrm;
	      List.app (parseErr errStrm) errs;
	      res
	    end

    (* resolve a file to either a user file (relative to the current directory)
     * or an ASDL library file.  Return NONE if the file cannot be found exist.
     *)
      fun resolveFile srcDir file = let
	    fun tryPath path = if OS.FileSys.access(path, [OS.FileSys.A_READ])
		  then SOME(path, TextIO.openIn path)
		  else NONE
	    in
	      if OS.Path.isAbsolute file
		then tryPath file
		else (case tryPath (OS.Path.joinDirFile{dir=srcDir, file=file})
		   of NONE => raise Fail "TODO: library search"
		    | something => something
		  (* end case *))
	    end
    in

    fun parse file = let
	  val fileTbl = FileTbl.mkTable(16, Fail "file table")
	(* add a file to the file table *)
	  fun addFile (name, path, errStrm, decls) = let
		val file = {name = path, errStrm = errStrm, decls = decls}
		in
		  FileTbl.insert fileTbl (name, ());
		  file
		end
	(* list of included files in reverse left-to-right post-order *)
	  val files : file list ref = ref []
	  fun visitFile file = (files := file :: !files)
	(* specialize resolveFile to the directory where the source file is located *)
	  val resolveFile = resolveFile (OS.Path.dir file)
	(* parse an included file if it has not already been parsed *)
	  fun parseInclude (errStrm, outer) {span, tree} = (
		case FileTbl.find fileTbl tree
		 of SOME() => if List.exists (fn s => (s = tree)) outer
		      then Error.errorAt(errStrm, span, [
			  "recursive include of '", tree, "'"
			])
		      else ()
		  | NONE => (case resolveFile tree
		       of NONE => (
			    Error.errorAt(errStrm, span, [
				"unable to find file '", tree, "'"
			      ]);
			    FileTbl.insert fileTbl (tree, ()))
			| SOME(path, inStrm) => let
			    val errStrm = Error.mkErrStream path
			    in
			      case parseOne (errStrm, inStrm)
			       of SOME(PT.File{includes, decls}) => let
				  (* add this file to the table and file list *)
				    val file = addFile (tree, path, errStrm, decls)
				    in
				      (* parse included files *)
					List.app
					  (parseInclude (errStrm, tree::outer))
					    includes;
					visitFile file
				    end
				| NONE => (* add error placeholder *)
				    visitFile (addFile (tree, path, errStrm, []))
			      (* end case *)
			    end
		      (* end case *))
		(* end case *))
	  val errStrm = Error.mkErrStream file
	  fun return decls = {
		  includes = List.rev (!files),
		  file = {name = file, errStrm = errStrm, decls = decls}
		}
	  in
	    if OS.FileSys.access(file, [OS.FileSys.A_READ])
	      then (case parseOne (errStrm, TextIO.openIn file)
		 of SOME(PT.File{includes, decls}) => (
		    (* add file to file table to detect recusive includes *)
		      FileTbl.insert fileTbl (file, ());
		    (* parse included files *)
		      List.app (parseInclude (errStrm, [file])) includes;
		    (* return this file and includes *)
		      return decls)
		  | NONE => return []
		(* end case *))
	      else (
		Error.error(errStrm, ["unable to find file '", file, "'"]);
		return [])
	  end (* parse *)

    end (* local *)

  end

(* extract-index.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module implements a tree walker that extracts index information
 * from the documentation sources.
 *)

structure ExtractIndex : sig

  (* given the root directory of the documentation sources, extract
   * index information.
   *)
    val extract : string -> FileTree.t

  end = struct

    structure FT = FileTree
    structure P = OS.Path
    structure RE = RegExpFn(
	structure P = AwkSyntax
	structure E = BackTrackEngine)
    structure MT = MatchTree
    structure SS = Substring
    structure SIO = TextIO.StreamIO

  (* error reporting *)
    fun error msg = (
          TextIO.output(TextIO.stdErr, concat("Error: " :: msg));
          TextIO.output1 (TextIO.stdErr, #"\n");
          OS.Process.exit OS.Process.failure)

    val attrRE = RE.compileString "^:!?([^!:]+)!?:(.*)"
    val includeRE = RE.compileString "^include::([^.]+\\.adoc)\\[\\]"
    val xrefRE = RE.compileString
	  "[ ]*xref:([^.]+\\.adoc)\\[([^\\]]+|`\\[\\.kw\\]#[a-z]+# [^`]+`)\\]::"
  (* match the title text for a module xref *)
    val pageRefRE = RE.compileString "`\\[\\.kw\\]#([a-z]+)# ([^`]+)`"
  (* module specifications in the synopsis *)
    val moduleRE = RE.compileString "^(signature|structure|functor)[ \t]+([A-Za-z0-9_]+)"

  (* `match re ln` attempts to match the line of text `ln` using the regular
   * expression `re`.  If successful, the result will be a match tree of strings
   * corresponding to the matches (and submatches) of `re`.
   *)
    fun match re = let
	  val prefix = StringCvt.scanString (RE.prefix re)
	  fun getSubstrs s = MT.map (fn {pos, len} => String.substring(s, pos, len))
	  in
	    fn s => Option.map (getSubstrs s) (prefix s)
	  end

    fun openIn (rootDir, path) = let
          val file = P.concat(rootDir, path)
          in
            if OS.FileSys.access(file, [OS.FileSys.A_READ])
              then TextIO.openIn file
              else error ["file '", file, "' does not exist or is not readable"]
          end

    fun trimWS ss = SS.dropr Char.isSpace (SS.dropl Char.isSpace ss)

  (* match an asciidoctor atrribute *)
    val matchAttr = match attrRE

  (* extract attribute values from lines immediately following the title *)
    fun scanMeta inputStrm = let
	  val inS' = TextIO.getInstream inputStrm
	  val author = ref NONE
	  val keywords = ref []
	  val title = ref NONE
	  fun trim s = SS.string(trimWS(SS.full s))
	  fun scan inS = (case SIO.inputLine inS
		 of SOME(ln, inS') => (case matchAttr ln
		       of SOME(MT.Match(_, [MT.Match(a, []), MT.Match(v, [])])) => (
			    case String.map Char.toLower a
			     of "author" => author := SOME(trim v)
			      | "keywords" => keywords :=
				  List.map trim
				    (String.tokens (fn #"," => true | _ => false) v)
			      | "title" => title := SOME(trim v)
			      | _ => ()
			    (* end case *);
			    scan inS')
			| _ => inS
		      (* end case *))
		  | NONE => inS
		(* end case *))
	  in
	    TextIO.setInstream (inputStrm, scan (TextIO.getInstream inputStrm));
	    { author = !author, kws = !keywords, title = !title }
	  end

  (* `scanFile root path getContents processContents` scans the file
   * specified by `path` using the `getContents` function to read the
   * file and the `processContents` to process it.
   *)
    fun scanFile rootDir path getContents processContent = let
	  val dir = P.dir path
	  val stem = P.base(P.file path)
	  val inS = openIn (rootDir, path)
	  val SOME firstLn = TextIO.inputLine inS
	  val title = if String.isPrefix "= " firstLn
	        then SS.string(trimWS(SS.extract(firstLn, 2, NONE)))
		else "<title>"
	  val meta = scanMeta inS
	  val contents = getContents inS
	  in
	    TextIO.closeIn inS;
	    FT.FILE{
		dir = dir,
		stem = stem,
		title = title,
		meta = meta,
		info = processContent contents
	      }
	  end

  (* scan the input stream until a line for which f returns `SOME v` is encountered *)
    fun scanLines f inS = let
	  fun lp () = (case TextIO.inputLine inS
		 of SOME ln => (case f ln
		       of NONE => lp ()
			| someV => someV
		      (* end case *))
		  | NONE => NONE
		(* end case *))
	  in
	    lp ()
	  end

  (* scan the input until a line with the given prefix is encountered; this function
   * returns true if it finds such a line and false otherwise.
   *)
    fun existsPrefix prefix = let
	  val isPrefix = String.isPrefix prefix
	  fun lp inS = (case TextIO.inputLine inS
		   of NONE => false
		    | SOME ln => isPrefix ln orelse lp inS
		  (* end case *))
	  in
	    lp
	  end

  (* find the next "include" directive in the input stream *)
    fun findInclude inS = scanLines (match includeRE) inS

  (* find the next "xref" directive in the input stream *)
    fun findXRef inS = scanLines (match xrefRE) inS

  (* match a module page reference *)
    val matchPageRef = match pageRefRE

  (* match a module specification in the synopsis *)
    val matchModule = match moduleRE

  (* extract the modules that are listed in the "Synopsis" of a page.  We first look
   * for a line of the form
   *
   *	== Synopsis
   *
   * and then for an SML source-code block.  In the body of the source-code block
   * we expect to see lines that have prefixes of the form
   *
   *	signature NAME
   *	structure NAME ...
   *	functor NAME ...
   *)
    fun doPage rootDir libDir {file, info={kind, name}} = let
	  val pagePath = P.joinDirFile{dir = libDir, file = file}
	(* skip to the SML code block in the synopsis *)
	  fun findSynopsis inS =
		existsPrefix "== Synopsis" inS
		andalso existsPrefix "[source,sml]" inS
		andalso existsPrefix "----" inS
	(* extract module names from the code block *)
	  fun getSynopsis inS = let
		fun lp mods = (case TextIO.inputLine inS
		       of NONE => error ["unexpected EOF in synopsis"]
			| SOME ln => (case matchModule ln
			     of SOME(MT.Match(_, [MT.Match(mk, _), MT.Match(id, _)])) =>
				  let
				  val mk = (case mk
					 of "signature" => FT.SIGNATURE
					  | "structure" => FT.STRUCTURE
					  | "functor" => FT.FUNCTOR
                                          | _ => error ["expected module kind"]
					(* end case *))
				  in
				    lp ((mk, id)::mods)
				  end
			      | _ => if String.isPrefix "----" ln
				  then List.rev mods
				  else lp mods
			    (* end case *))
		      (* end case *))
		in
		  lp []
		end
	  fun scanContents inS = (case kind
		 of FT.OtherPage => {kind = kind, name = name, synopsis = []}
		  | _ => {
		      kind = kind, name = name,
		      synopsis = if findSynopsis inS
			  then getSynopsis inS
			  else []
		    }
		(* end case *))
	  in
	    scanFile rootDir pagePath scanContents Fn.id
	  end

  (* extract the list of page files from a library document *)
    fun getPagesFromLib inS = let
	(* first we get the `xref` list items *)
	  fun getPages pages = (case findXRef inS
		 of SOME(MT.Match(_, [MT.Match(file, []), MT.Match(title, [])])) => (
		      case matchPageRef title
		       of SOME(MT.Match(_, [MT.Match(kw, []), MT.Match(name, [])])) => let
			    val kind = (case kw
				   of "signature" => FT.sigPage
				    | "structure" => FT.structPage
				    | "functor" => FT.functPage
				    | _ => error ["**bogus keyword \"", kw, "\""]
				  (* end case *))
			    val page = {
				    file = file,
				    info = {kind = kind, name = name}
				  }
			    in
			      getPages (page :: pages)
			    end
			| _ => let (* non-module page *)
			    val page = {
				    file = file,
				    info = {kind = FT.OtherPage, name = title}
				  }
			    in
			      getPages (page :: pages)
			    end
		      (* end case *))
		  | NONE => List.rev pages
		  | SOME(MT.Match(s, _)) => error [
			"**bogus xref \"", String.toString s, "\""
		      ]
		(* end case *))
	  in
	    {pages = getPages []}
	  end

  (* process a library file *)
    fun doLib rootDir libPath = let
	  val libDir = P.dir libPath
	  in
	    scanFile rootDir libPath
	      getPagesFromLib
		(fn {pages} => {pages = List.map (doPage rootDir libDir) pages})
	  end

  (* extract the list of library files from the root document *)
    fun getLibsFromRoot inS = let
	  fun getIncludes incs = (case findInclude inS
		 of SOME(MT.Match(_, [MT.Match(path, [])])) =>
		      getIncludes(path :: incs)
		  | NONE => List.rev incs
		  | SOME(MT.Match(s, _)) => error [
			"**bogus include \"", String.toString s, "\""
		      ]
		(* end case *))
	  in
	    getIncludes []
	  end

    fun extract rootDir = let
	  val rootDir = OS.FileSys.fullPath rootDir
	  in
	    scanFile rootDir "index.adoc"
	      getLibsFromRoot
		(fn libs => {libs = List.map (doLib rootDir) libs})
	  end

  end

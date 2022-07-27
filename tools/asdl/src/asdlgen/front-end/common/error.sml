(* error.sml
 *
 * COPYRIGHT (c) 2016 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Common infrastructure for error reporting.
 *)

structure Error :> sig

  (* an exception to raise when hitting an unrecoverable error condition.
   * This exception will be caught in the main program.
   *)
    exception ERROR

  (* logical positions in the input stream *)
    type pos = AntlrStreamPos.pos
    type span = AntlrStreamPos.span

    type err_stream

  (* make an error stream. *)
    val mkErrStream : string -> err_stream

    val anyErrors : err_stream -> bool
    val anyWarnings : err_stream -> bool
    val sourceFile : err_stream -> string
    val sourceMap : err_stream -> AntlrStreamPos.sourcemap

  (* add error messages to the error stream *)
    val error : err_stream * string list -> unit
    val errorAt : err_stream * span * string list -> unit

  (* add warning messages to the error stream *)
    val warning : err_stream * string list -> unit
    val warningAt : err_stream * span * string list -> unit

  (* add an ml-antlr parse error to the error stream *)
    val parseError : (AntlrRepair.add_or_delete -> 'tok -> string)
	  -> err_stream
	    -> (pos * 'tok AntlrRepair.repair_action)
	      -> unit

  (* print the errors to an output stream *)
    val report : TextIO.outstream * err_stream -> unit

  (* source-code locations *)
    datatype location
      = UNKNOWN
      | LOC of {file : string, l1 : int, c1 : int, l2 : int, c2 : int}

    val location : err_stream * span -> location
    val position : err_stream * pos -> location

  (* `fmt (spec, unknown) loc`
   *
   * formats the location loc as a string.  If loc is UNKNOWN, then the value of `unknown`
   * is returned.  Otherwise the format commands in `spec` are used to compose the string
   * (in the style of C's `printf`).  The supported format commands are as follows;
   *
   *	%f	-- the file name
   *	%l	-- the first line number
   *	%p	-- the start position of the span (line:col)
   *	%s	-- the span
   *	%%	-- the character %
   *)
    val fmt : (string * string) -> location -> string

  (* `locToString loc` is equivalent to `fmt ("[%f:%s] ", "<unknown>") loc` *)
    val locToString : location -> string

  (* a term marked with a source-map span *)
    type 'a mark = {span : span, tree : 'a}

  end = struct

    structure SP = AntlrStreamPos
    structure Repair = AntlrRepair
    structure F = Format

    type pos = SP.pos
    type span = SP.span

    datatype severity = WARN | ERR

    type error = {
	kind : severity,
	pos : span option,
	msg : string
      }

  (* an error stream collects the errors and warnings generated for
   * a compilation unit.
   *)
    datatype err_stream = ES of {
	srcFile		: string,
	sm		: SP.sourcemap,	(* the source map for mapping positions to *)
					(* source-file positions *)
	errors		: error list ref,
	numErrors	: int ref,
	numWarnings	: int ref
      }

    exception ERROR

  (* make an error stream. *)
    fun mkErrStream filename = ES{
	    srcFile = filename,
	    sm = SP.mkSourcemap' filename,
	    errors = ref [],
	    numErrors = ref 0,
	    numWarnings = ref 0
	  }

    fun anyErrors (ES{numErrors, ...}) = (!numErrors > 0)
    fun anyWarnings (ES{numWarnings, ...}) = (!numWarnings > 0)
    fun sourceFile (ES{srcFile, ...}) = srcFile
    fun sourceMap (ES{sm, ...}) = sm

    fun addErr (ES{errors, numErrors, ...}, pos, msg) = (
	  numErrors := !numErrors + 1;
	  errors := {kind=ERR, pos=pos, msg=msg} :: !errors)
	  
    fun addWarn (ES{errors, numWarnings, ...}, pos, msg) = (
	  numWarnings := !numWarnings + 1;
	  errors := {kind=WARN, pos=pos, msg=msg} :: !errors)

    fun parseError tok2str es (pos, repair) = let
	  val addToksToStr = String.concatWithMap " " (tok2str AntlrRepair.ADD)
	  val delToksToStr = String.concatWithMap " " (tok2str AntlrRepair.DEL)
	  val msg = (case repair
		 of Repair.Insert toks => ["syntax error; try inserting \"", addToksToStr toks, "\""]
		  | Repair.Delete toks => ["syntax error; try deleting \"", delToksToStr toks, "\""]
		  | Repair.Subst{old, new} => [
			"syntax error; try substituting \"", addToksToStr new, "\" for \"",
			delToksToStr old, "\""
		      ]
		  | Repair.FailureAt tok => ["syntax error at ", tok2str AntlrRepair.DEL tok]
		(* end case *))
	  in
	    addErr (es, SOME(pos, pos), String.concat msg)
	  end

  (* add error messages to the error stream *)
    fun error (es, msg) = addErr (es, NONE, String.concat msg)
    fun errorAt (es, span, msg) = addErr (es, SOME span, String.concat msg)

  (* add warning messages to the error stream *)
    fun warning (es, msg) = addWarn (es, NONE, String.concat msg)
    fun warningAt (es, span, msg) = addWarn (es, SOME span, String.concat msg)

  (* sort a list of errors by position in the source file *)
    val sort = let
	  fun gt (NONE, NONE) = false
	    | gt (NONE, _) = true
	    | gt (_, NONE) = false
	    | gt (SOME(l1, r1), SOME(l2, r2)) = (case Int.compare(l1, l2)
		 of LESS => false
		  | EQUAL => (Int.compare(r1, r2) = GREATER)
		  | GREATER => true
		(* end case *))
	  fun cmp (e1 : error, e2 : error) = gt(#pos e1, #pos e2)
	  in
	    ListMergeSort.sort cmp
	  end

  (* source-code locations *)
    datatype location
      = UNKNOWN
      | LOC of {file : string, l1 : int, c1 : int, l2 : int, c2 : int}

    fun location (ES{sm, ...}, (p1, p2) : span) =
	  if (p1 = p2)
	    then let
	      val {fileName=SOME f, lineNo, colNo} = SP.sourceLoc sm p1
	      in
		LOC{file=f, l1=lineNo, c1=colNo, l2=lineNo, c2=colNo}
	      end
	    else let
	      val {fileName=SOME f1, lineNo=l1, colNo=c1} = SP.sourceLoc sm p1
	      val {fileName=SOME f2, lineNo=l2, colNo=c2} = SP.sourceLoc sm p2
	      in
		if (f1 <> f2)
		  then LOC{file=f1, l1=l1, c1=c1, l2=l1, c2=c1}
		  else LOC{file=f1, l1=l1, c1=c1, l2=l2, c2=c2}
	      end

    fun position (ES{sm, ...}, p : pos) = let
	  val {fileName=SOME f, lineNo, colNo} = SP.sourceLoc sm p
	  in
	    LOC{file=f, l1=lineNo, c1=colNo, l2=lineNo, c2=colNo}
	  end

    local
      datatype fmt_items = FILE | LINE | POS | SPAN | PCT | STR of substring
    in

  (* format a location using the following formatting commands:
   *	%f	-- the file name
   *	%l	-- the first line number
   *	%p	-- the start position of the span (line:col)
   *	%s	-- the span
   *	%%	-- the character %
   *)
    fun fmt (msg, unkMsg) = let
	(* map msg to a list of format items *)
	  val items = let
		fun split (ss, start, n, items) = (case Substring.getc ss
		       of NONE => if (n > 0)
			    then STR start :: items
			    else items
			| SOME(#"%", ss') => let
			    fun continue (ss'', item) =
				  if (n > 0)
				    then split (ss'', ss'', 0,
				      item :: STR(Substring.slice(start, 0, SOME n)) :: items)
				    else split (ss'', ss'', 0, item :: items)
			    in
			      case Substring.getc ss'
			       of SOME(#"f", ss'') => continue (ss'', FILE)
				| SOME(#"l", ss'') => continue (ss'', LINE)
				| SOME(#"p", ss'') => continue (ss'', POS)
				| SOME(#"s", ss'') => continue (ss'', SPAN)
				| SOME(#"%", ss'') => continue (ss'', PCT)
				| _ => raise F.BadFormat
			      end
			| SOME(_, ss') => split (ss', start, n+1, items)
		      (* end case *))
		val msg = Substring.full msg
		in
		  List.rev (split (msg, msg, 0, []))
		end
	  fun fmt' UNKNOWN = unkMsg
	    | fmt' (LOC{file, l1, l2, c1, c2}) = let
		val i2s = Int.toString
		fun lc2s (l, c, items) = i2s l1 :: "." :: i2s c1 :: items
	      (* convert items to a string *)
		fun cvt (FILE, items) = file :: items
		  | cvt (LINE, items) = Int.toString l1 :: items
		  | cvt (POS, items) = i2s l1 :: "." :: i2s c1 :: items
		  | cvt (SPAN, items) = if (l1 = l2)
		      then if (c1 = c2)
			then lc2s(l1, c1, items)
			else lc2s(l1, c1, "-" :: i2s c2 :: items)
		      else lc2s(l1, c1, "-" :: lc2s(l2, c2, items))
		  | cvt (PCT, items) = "%" :: items
		  | cvt (STR ss, items) = Substring.string ss :: items
		in
		  String.concat (List.foldr cvt [] items)
		end
	  in
	    fmt'
	  end

    end (* local *)

    val locToString = fmt ("[%f:%s] ", "<unknown> ")

    fun printError (outStrm, ES{srcFile, sm, ...}) = let
	  fun pr {kind, pos, msg} = let
		val kind = (case kind of ERR => "Error" | Warn => "Warning")
		val pos = (case pos
		       of NONE => concat["[", srcFile, "] "]
			| SOME(p1, p2) => if (p1 = p2)
			    then let
			      val {fileName=SOME f, lineNo, colNo} = SP.sourceLoc sm p1
			      in
				F.format "[%s:%d.%d] " [
				    F.STR f, F.INT lineNo, F.INT colNo
				  ]
			      end
			    else let
			      val {fileName=SOME f1, lineNo=l1, colNo=c1} = SP.sourceLoc sm p1
			      val {fileName=SOME f2, lineNo=l2, colNo=c2} = SP.sourceLoc sm p2
			      in
				if (f1 <> f2)
				  then F.format "[%s:%d.%d-%s:%d.%d] " [
				      F.STR f1, F.INT l1, F.INT c1,
				      F.STR f2, F.INT l2, F.INT c2
				    ]
				else if (l1 <> l2)
				  then F.format "[%s:%d.%d-%d.%d] " [
				      F.STR f1, F.INT l1, F.INT c1,
				      F.INT l2, F.INT c2
				    ]
				  else F.format "[%s:%d.%d-%d] " [
				      F.STR f1, F.INT l1, F.INT c1, F.INT c2
				    ]
			      end
		      (* end case *))
		in
		  TextIO.output (outStrm, String.concat [pos, kind, ": ", msg, "\n"])
		end
	  in
	    pr
	  end

    fun report (outStrm, es as ES{errors, ...}) =
	  List.app (printError (outStrm, es)) (sort (!errors))

  (* a term marked with a source-map span *)
    type 'a mark = {span : span, tree : 'a}

  end

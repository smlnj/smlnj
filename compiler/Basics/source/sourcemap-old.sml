(* sourcemap.sml
 *
 * COPYRIGHT (c) 2012 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* Norman Ramsey: *)
(* I can imagine at least three implementations: one that doesn't
 * support resynchronization, one that supports resynchronization only at
 * column 1, and one that supports arbitrary resynchronization.
 *
 * This implementation supports arbitary resynchronization.
 *
 * Changed ErrorMsg to use SourceMap to get source locations; only the
 * formatting is done internally.
 *
 * Added SourceMap structure.
 *)

(* DBM: what is "resynchronization" and what is it used for?  Is there any
 * reason to continue to support it (and maintain the extra code complexity)?
 * If this was a feature used only by Ramsey's noweb utility, which is defunct,
 * then we could simplify the sourcemap code.  -- John claims that resynchronization
 * is still relevant (examples?). *)

(* DBM: "Resynchonization" supports a model where the input stream for a compilation
 * unit is made up of multiple source files.  These may be combined either by
 * concatenation or inserting one file in the middle of another (like #include
 * in cpp).  We'll call the pieces of source that are being combined "file segments"
 * or "segments" for short.
 *
 * We'll assume that minimal granularity for resynchonization is a
 * source line (i.e. no switching files in the middle of a line).
 * The boundaries between source file segments will be marked by #line
 * commands embedded in comments.  These have the form '(*#line nn "filename"*)'
 * or '(*#line nn*)' where nn is a new line number at which the next segment
 * starts, and filename is the name of the file for the next segment. It is
 * assumed that the #line command comment will appear on a line by itself
 * (presumably inserted by an external preprocessor that is responsible for
 * combining source segments to form the input stream).  These #line commands
 * are recognized and interpreted by the lexer, which calls resynch to change
 * the state of the current sourcemap.
 *
 * QUESTION: Can a source region, which designates a contiguous region in the
 * input stream, cross one or more file segments?  Or should all regions created
 * during parsing and used in elaboration be within a single segment (and therefore
 * be associated with a single source file)? The type and implementation of
 * function fileregion imply that a region can span multiple segments.
 *
 * QUESTION: Presumably, the original motivation for adding this feature was to support
 * Norman Ramsey's nw "literate programming" system, which we no longer support.
 * What new clients use this functionality?
 *
 * Obviously, the implementation of sourcemap could be made much simpler without
 * resynchronization.
 *
 * New functionality for mapping regions to source strings (for enhanced type error
 * messages) is currently incompatible with resynchronization.
 *
 *)

structure SourceMap :> SOURCE_MAP =
struct

  (* compiler bug errors *)
  exception SourceMap of string
  fun bug msg = raise SourceMap(msg)

  (* types ------------------------------- *)

  (* A character position is an integer.  A region is delimited by the
   * position of the start character and one beyond the end.
   * It might help to think of Icon-style positions, which fall between
   * characters.
   *)

  type charpos = int
    (* charpos is 1-based. I.e. the (default) position of the first character in the
     * input stream is 1 (????) *)

  type region = charpos * charpos
    (* INVARIANT: (lo,hi) : region ==> lo <= hi
     * If region /= (0,0), then lo < hi, i.e. all non-null regions are nonempty. *)

  type sourceloc = {fileName:string, line:int, column:int}
    (* lines and columns are 1-based (minimum value is 1) *)

(* The representation of a sourcemap is a pair of lists.
     lines: line numbers for newlines and resynchronizations,
            labeled by initial charpos of each lines.
     files: file name for resynchronization, labeled by
            initial position for resynchronization

   The representation satisfies these invariants:
     * The lists are never empty (initialization is treated as a resynchronization).
     * Initial positions strictly decrease as we traverse the line list.
     * The last element in the line list contains the smallest valid position (1).
     * For every element in files, there is a corresponding SYNC element in
       lines, and visa versa.
*)

  (* line -- elements of lines list *)
  datatype line
    = LINE of int         (* line number, simple line bump *)
    | SYNC of int * int * int
       (* resynch point with line, column, and the size of the #line directive gap;
        * there will be an associated entry in files list, which MAY change
        * the current file name, but may be the same as the previous file name
        * if the #line directive does not specify a file name. *)

  type sourcemap = {lines: (charpos * line) list ref,
		    files: string list ref}
  (* INVARIANTS for sourcemaps:
   * (1) length (!lines) > 0
   * (2) length (!files) > 0
   * (3) charpos components of lines are strictly decreasing (ending in 1)
   * (4) length (!files) = number of SYNC elements in lines
   * (5) last (initial) element of lines is the SYNC line: (1, SYNC(1,1,0))
   *)

  val nullRegion : region = (0,0)
  (* nullRegion is a conventional default region value.  It does not represent
   * a proper region, and does not have a location in the file. In particular, it
   * should not be viewed as an empty region at the beginning of the input. *)

  fun isNullRegion ((_,0): region) = true
    | isNullRegion _ = false

  (* newSourceMap: create a new sourcemap, given initial file name.
   * called only one place, in Source.newSource.  Initial position at the
   * start of the first line is 1, initial line number is 1. *)
  fun newSourceMap (fileName: string) : sourcemap =
      {files = ref [fileName],
       lines = ref [(1, SYNC(1,1,0))]}

  (* resynch: implements a #line directive, changing the current filename, line and column.
   * initpos is the position of the initial character of the #line comment
   * newpos is the character immediately following the end of the #line comment
   * ASSUMPTION: newpos > last line position in the sourcemap argument *)
  fun resynch ({files, lines}: sourcemap) (initpos, newpos, line, column, fileNameOp) =
      let val newFileName =
              case fileNameOp
                of SOME f => f
                 | NONE => hd (!files)   (* same as the current file name *)
       in files := newFileName :: !files;
	  lines := (newpos, SYNC(line,column,newpos-initpos)) :: !lines
      end

  fun lineNo (LINE l | SYNC(l,_,_)) = l

  (* Since pos is the position of the newline character, the next line doesn't
   * start until the succeeding position, pos+1. *)
  fun newline ({lines, ...}: sourcemap) pos =
      case !lines
        of (_,line) :: _ =>  lines := (pos+1, LINE(lineNo(line)+1)) :: !lines
         | nil => bug "newline"  (* invariant (1) violated *)

  fun lastLinePos ({lines, ...}: sourcemap) : charpos =
      case !lines
        of ((pos,line)::_) => pos
         | nil => bug "lastLineNumber" (* invariant (1) violated *)

  (* remove: remove from sourcemap lines those lines whose initial positions
   * exceed a target position, while maintaining the lines/files invariants.
   * The first line of the result will contain the target position.
   * ASSUMPTION: pos is >= initial pos of the sourcemap (normally 1). *)
  fun remove pos (lines: (charpos * line) list, files: string list) =
      let fun strip (lines as (pos', line)::lines', files as (_ :: files')) =
              if pos' > pos then
                 (case line
                    of LINE _ => strip (lines', files)
		     | SYNC _ => strip (lines', files'))
              else (lines, files)
	    | strip _ = bug "remove"
       in strip(lines, files)
      end

  (* ASSUMPTION: pos lies within the given line:
   *   lineStart <= pos < start of next line  *)
  fun column ((lineStart, line), pos) =
      let val col = case line
		      of LINE _  => 1
		       | SYNC(_,c,_) => c
       in pos - lineStart + col
      end

  fun filepos ({lines,files}:sourcemap) pos : sourceloc =
      case remove pos (!lines,!files)
        of ((linePos,line)::_, file::_) =>   (* pos is within top line *)
           {fileName = file, line = lineNo line, column = column((linePos,line), pos)}
         | _ => bug "filepos"

  (* Searching regions is a bit trickier, since we track file and line
   * simultaneously.  We exploit the invariant that every file entry has a
   * corresponding line entry.  We also exploit that only file entries
   * correspond to new regions. *)

  fun isNullRegion (0,0) = true
    | isNullRegion _ = false

  fun fileregion ({lines,files}: sourcemap) ((lo, hi): region) =
      if isNullRegion(lo,hi) then [] else
      let fun posToSourceLoc(pos, (linePos, line)::_,  file::_): sourceloc =
		 {fileName=file, line=lineNo(line), column=column((linePos,line), pos)}
	    | posToSourceLoc _ = bug "posToSourceLoc"

	  fun gather((linePos, line)::lines', files as file::files',
		     segment_end, answers) =
	       if linePos <= lo then (* last item *)
		 ({fileName=file, line=lineNo(line), column=column((linePos,line),lo)},
		  segment_end)
                 :: answers
	       else (case line
                       of LINE _ => gather(lines', files, segment_end, answers)
                        | SYNC(l,c,g) => (* crossing segment boundary *)
			  let val endpos =
				  (case lines'
				    of (linePos', _)::_ =>
                                       if linePos - g = linePos' then linePos' - 1
                                       else linePos - g)
                          in gather(lines', files',
				    posToSourceLoc (endpos, lines', files'),
				    ({fileName = file, line = l, column = c},
				     segment_end) :: answers)
			  end)
	    | gather _ = bug "fileregion"
	  val (lines0, files0) = remove hi (!lines,!files)
       in gather(lines0, files0, posToSourceLoc(hi,lines0,files0), [])
      end

   (* newlineCount : sourcemap -> region -> int
    * determines the (approximate) number of newlines occurring in a region,
    * which may be 0 for a region that lies within a single line. Any lines
    * containing #line directives (i.e. SYNC lines) are not counted. *)
   fun newlineCount ({lines,files}: sourcemap) ((lo, hi): region) =
       let val his as (hilines, hifiles) = remove hi (!lines,!files)
	   val (lolines, lofiles) = remove lo his
	in (length hilines - length lolines) - (length hifiles - length lofiles)
       end

   (* removeLines - remove lines, stoping when the next line contains pos *)
   fun removeLines (pos, (lines: (charpos * line) list)) =
       let fun strip (lines as (pos1, _)::lines') =
	       if pos1 > pos then strip (lines') else lines
	     | strip _ = bug "removeLines"
	in strip lines
       end

   (* removeABO - remove all-but-one.
    * removes lines until pos is in the next to last line *)
   fun removeABO (pos, (line::lines: (charpos * line) list)) =
       let fun strip (lines as (pos1, line1)::lines', (pos0, line0)) =
	       if pos1 > pos then strip (lines', (pos1, line1))
	       else (pos0,line0)::lines
	     | strip _ = bug "removeABO"
	in strip(lines, line)
       end

   (* widenToLines - take a region and expand it to the beginning, respectively end,
    * of the first and last lines intersecting the region. This works only
    * for nonsegmented inputs (no noninitial SYNCs). Also assumes that the
    * region hi limit comes before the last newline in the input, which
    * should be the case if the input ends with a newline. *)
   fun widenToLines ({lines,files}: sourcemap) ((lo, hi) : region) =
       if isNullRegion (lo,hi) then nullRegion
       else let val (lines1 as (pos1,_)::lines1') = removeABO(hi, !lines)
                val (pos2,_)::_ = removeLines(lo, lines1')
             in (pos2, pos1-1)
	    end

end (* structure SourceMap *)

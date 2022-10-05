(* sourcemap.sml
 *
 * COPYRIGHT (c) 2012 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* [DBM, 2022.09] Reversion to single-file (or interactive stream) model. Thus no
 * no "#line" directives, and no "resynchronization". *)

structure SourceMap :> SOURCE_MAP =
struct

  (* compiler bug errors *)
  exception SourceMap of string
  fun bug msg = raise SourceMap(msg)

  (* types ------------------------------- *)

  (* A character position (charpos) is an integer.  A region is delimited by the
   * position of the start character and one beyond the last character.
   * It might help to think of Icon-style positions, which fall between
   * characters.
   *)

  type charpos = int
    (* charpos is 1-based. I.e. the (default) position of the first character in the
     * input stream is 1 (not 0!) *)

  type region = charpos * charpos
    (* INVARIANT: (lo,hi) : region ==> lo <= hi
     * If region /= (0,0), then lo < hi, i.e. all non-null regions are nonempty. *)
    (* region should have been a datatype! (in-band representation of the null region!) *)

  (* line numbers, 1 based: INVARIANT lineno >= 1 *)
  type lineno = int

  type lines = (charpos * lineno) list

  type sourceloc = {fileName:string, line:int, column:int}
    (* lines and columns are both positive, 1-based (minimum value is 1) *)

(* The representation of a sourcemap is record with fields
     - filename : string  (the name of a single, fixed source file)
     - lines: lines ref
         initial charpos and line number of lines in decreasing order, so the last
         "line" in the list is the first line in the file, represented as (1,1).
	 Lines are numbered from 1.

   The representation satisfies these invariants:
     * The line list is never empty (initialized to [(1,1)], and thereafter lines are only added).
     * Initial positions are strictly decreasing accross the line list,
       even for "empty" lines, which have length at least 1 because we count the newline char.
     * The last element in the line list contains the smallest valid starting charpos (namely 1).
*)

  type sourcemap = {file: string, lines: lines ref}
		    
  (* INVARIANTS for sourcemaps:
   * (1) length (!lines) > 0 (initialized to [(1,1)] and only grows)
   * (2) charpos components of !lines are strictly decreasing,
       ending in initial charpos 1 for the last element, which represents the 1st line
   * (3) last (initial) element of lines is the initial line: (1, 1).
   *)

  val nullRegion : region = (0,0)
  (* nullRegion is a conventional default region value.  It does not represent
   * a proper region, and does not have a location in the file. In particular, it
   * should not be viewed as an empty region at the beginning of the input. *)

  (* isNullRegion : region -> bool *)
  fun isNullRegion ((_,0): region) = true
    | isNullRegion _ = false

  (* regionToString : region -> string *)
  fun regionToString ((lo, hi): region) =
      String.concat [Int.toString lo, "-", Int.toString hi]

  (* newSourceMap: string -> sourcemap
   * create a new sourcemap, given initial file name.
   * called only one place, in Source.newSource.  Initial position at the
   * start of the first line is 1, initial line number is 1. A source is
   * assumed to have at least one line, (i.e. to be nonempty). *)
  fun newSourceMap (fileName: string) : sourcemap =
      {file = fileName,
       lines = ref [(1, 1)]}  (* initial lines: line 1 starts at char 1 *)

  (* newline : sourcemap -> charpos -> unit
   * pos is the position of the newline character, so the next line doesn't
   * start until the succeeding character position, pos+1. *)
  fun newline ({lines, ...}: sourcemap) (pos: charpos) =
      case !lines
        of (_,line) :: _ =>  lines := (pos+1, line+1) :: !lines
         | nil => bug "newline"  (* sourcemap invariant (1) violated *)

  (* lastLinePos : sourcemap -> charpos *)
  fun lastLinePos ({lines, ...}: sourcemap) : charpos =
      case !lines
        of ((pos,line)::_) => pos
         | nil => bug "lastLineNumber" (* sourcemap invariant (1) violated *)

  (* remove : charpos -> lines -> lines *)
  (* remove: remove from a lines list those lines whose initial positions
   * exceed the target position pos, while maintaining the lines invariants.
   * The first line of the result will contain the target position.
   * ASSERT: pos >= 1. the initial charpos of any sourcemap *)
  fun remove (pos, (lines: lines)) =
      let fun strip (lines as (pos', line)::lines') =
              if pos' > pos then strip lines'
              else lines
	    | strip _ = bug "remove"
       in strip lines
      end

  (* column : charpos * charpos -> int
   * ASSERT: pos lies within a line starting at lineStartPos:
   *   lineStartPos <= pos < start of next line  *)
  fun column (lineStartPos: charpos, pos: charpos) =
      pos - lineStartPos + 1  (* each line starts at column 1 *)

  (* posToSourceloc : sourcemap -> charpos -> sourceloc *)
  fun posToSourceloc ({file, lines}:sourcemap) pos : sourceloc =
      case remove (pos, !lines)
        of (linePos,line) :: _ =>   (* pos is within top line *)
             {fileName = file, line = line, column = column (linePos, pos)}
         | _ => bug "posToSourceloc"

  fun posToLineColumn (pos: charpos, lines : lines) : (int * int) = 
      case remove (pos, lines)
        of (linePos,line) :: _ =>   (* pos is within top line *)
             (line, column (linePos, pos))
         | _ => bug "posToLineColumn"

  (* Searching regions is a bit trickier, since we track file and line
   * simultaneously.  We exploit the invariant that every file entry has a
   * corresponding line entry.  We also exploit that only file entries
   * correspond to new regions. *)

  (* isNullRegion : region -> bool *)
  fun isNullRegion (0,0) = true
    | isNullRegion _ = false

  (* filepos : sourcemap -> charpos -> sourceloc *)
  fun filepos ({file, lines} : sourcemap) (pos : charpos) : sourceloc =
      let val (lin, col) = posToLineColumn  (pos, !lines)
       in {fileName = file, line = lin, column = col}
      end

  (* fileregion : sourcemap -> region -> sourceloc * sourceloc *)
  (* result sourceloc pair have same file component, the file from the sourcemap *)
  fun fileregion ({file, lines}: sourcemap) ((lo, hi): region) : sourceloc * sourceloc =
      if isNullRegion (lo,hi) then bug "fileregion" else
      let (* ASSERT: pos is in the first line *)
          val lines' = !lines
	  val (lo_line, lo_column) = posToLineColumn (lo, lines')
	  val (hi_line, hi_column) = posToLineColumn (hi, lines')
       in ({fileName = file, line = lo_line, column = lo_column},
	   {fileName = file, line = hi_line, column = hi_column})
      end

   (* newlineCount : sourcemap -> region -> int
    * determines the number of newlines occurring within a region,
    * which may be 0 for a region that lies within a single line. *)
   fun newlineCount (sm: sourcemap) (region: region) =
       let val ({line=lo_line, ...}, {line=hi_line,...}) = fileregion sm region
	in hi_line - lo_line
       end

   (* endOfContainingLine: charpos * lines -> lines
    *  The first line of the result contains the charpos
    * ASSUME:
    *  (1) file ends in newline (no positions within empty "last" line)
    *  (2) pos is before the last newline (pos < posLast)
    * Note that the line number components of lines are irrelevant. *)
   fun endOfContainingLine (pos, lines: lines) =
       let fun strip (lines0 as ((pos0, _) :: (lines1 as ((pos1, _) :: _)))) =
	       let fun strip2 (lines0 as ((p1, _) :: (lines1 as ((p2, _) :: _)))) =
	           if p2 <= pos then lines0 else strip2 lines1
	        in if pos < pos0
		   then if pos1 <= pos  (* pos1 <= pos < pos0 *)
			then lines0
			else strip2 lines1 (* pos < pos1 *) 
		   else bug "endOfContainingLine1" (* pos >= pos0 -- too "late?" *)
	       end
	     | strip _ = bug "endOfContainingLine2" (* need at least two lines to bracket pos! *)
	in strip lines
       end

   (* widenToLines : sourcemap -> region -> region
    * expand a region to the beginning, respectively end,
    * of the first and last lines intersecting the region.
    * ASSUME: the region hi limit comes before the last newline in the input,
    * which should be the case if the input ends with a newline. *)
   fun widenToLines ({lines,...}: sourcemap) ((lo, hi) : region) : region =
       if isNullRegion (lo,hi) then nullRegion
       else let val (lines1 as ((after_hi,_) :: _)) = endOfContainingLine (hi, !lines)
                val (before_lo, _) :: _ = remove (lo, lines1)
             in (before_lo, after_hi - 1)
	    end

end (* structure SourceMap *)

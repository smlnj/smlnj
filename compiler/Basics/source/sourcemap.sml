(* Basics/source/sourcemap.sml 
 *
 * COPYRIGHT (c) 2012 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* [DBM, 2022.09] Reversion to single-file (or interactive stream) model. Thus no
 * no "#line" directives, and no "resynchronization". *)

structure SourceMap :> SOURCE_MAP =
struct

local

  structure SR = Source

  (* compiler bug errors; precedes ErrorMsg, so cannot use ErrorMsg.impossible *)
  exception SOURCEMAP of string
  fun bug msg = raise SOURCEMAP msg

in 

  (* types ------------------------------- *)

  (* A character position (charpos) is a positive integer.  A region is delimited
   * by the position of the first character the position one beyond the last character
   * of the region. Positions fall between characters, but "belong" to the "next"
   * character.
   *     a b c  ...   -- characters
   *    1 2 3  ...    -- character positions
   *)

  type charpos = SR.charpos
    (* INVARIANT : charpos >= 1:
     *  charpos is 1-based. I.e. the position of the first character in the
     *  input stream is 1 (not 0!) *)

  type lineno = SR.lineno
    (* line numbers, 1 based: INVARIANT lineno >= 1 *)

  type sourceMap = SR.sourceMap

  datatype region
    = NULLregion
    | REGION of charpos * charpos
      (* INVARIANT: REGION (lo,hi) ==> lo < hi.
       * All REGION regions are nonempty. *)

  type location = {line : lineno, column: int}
    (* Line and column are both positive, 1-based (minimum value is 1),
     * Thus the first line is line 1, and the first column is column 1. *)

  type sourceLoc = {source : SR.source, loc : location}

  type sourceRegion = {source : SR.source, start : location, finish : location}
    (* a "region" within a source, delineated by locations instead of charposes *)

  val nullRegion : region = NULLregion
    (* nullRegion is a conventional default region value.  It does not represent
     * a proper region, and does not have a location in the file. In particular, it
     * should not be interpretted as an empty region at the beginning of the input. *)

  (* isNullRegion : region -> bool *)
  fun isNullRegion (NULLregion: region) = true
    | isNullRegion _ = false


  (* initSourceMap: sourceMap *)
  val initSourceMap : sourceMap = [(1, 1)]

  (* UNUSED locally, NOT exported *)
  (* lastLineStartPos : sourceMap -> charpos *)
  fun lastLineStartPos (smap: sourceMap) : charpos =
      case smap
        of ((pos,line)::_) => pos
         | nil => bug "lastLineStartPos" (* sourceMap invariant (1) violated *)

  (* uptoPos : charpos * sourceMap -> sourceMap
   * REQUIRE: pos is "within" the sourceMap (no newlines since #1 (hd smap))
   * uptoPos lines from the sourceMap until the charpos occurs in the
   * first line of the remainder, i.e. the first line of the result will contain
   * the target position. *)
  fun uptoPos (pos: charpos, smap: sourceMap) =
      let fun strip (lines as (pos', _) :: lines') =
              if pos < pos' then strip lines' (* pos comes before this first line *)
              else lines (* pos is in the first line *)
	    | strip _ = bug "uptoPos"
       in strip smap
      end

  (* column : charpos * charpos -> int
   * REQUIRE: pos lies within the line starting at startPos:
   *   startPos <= pos < start of next line  *)
  fun column (startPos: charpos, pos: charpos) =
      pos - startPos + 1  (* each line starts at column 1 *)

  (* charposToLocation : sourceMap * charpos -> location
   * REQUIRE: pos is within top line (last line, current line?) of smap *)
  fun charposToLocation (smap: sourceMap, pos: charpos) : location =
      case uptoPos (pos, smap)
        of (startPos,lineno) :: _ => 
             {line = lineno, column = column (startPos, pos)}
         | _ => bug "charposToLocation"

  (* FROM Source.filepos => SM.sourceLocation (also replaces SM.filepos) *)
  (* charposToSourceLoc: source * SourceMap.charpos -> sourceLoc *)
  fun charposToSourceLoc (source as {sourceMap,...}: SR.source, pos: charpos) : sourceLoc =
      {source = source, loc = charposToLocation (!sourceMap, pos)}

  (* regionToLocations : sourceMap * region -> (location * location) option *)
  fun regionToLocations (smap: sourceMap, REGION (lo, hi)) : (location * location) option =
        SOME (charposToLocation (smap, lo), charposToLocation (smap, hi))
    | regionToLocations (_, NULLregion) = NONE

  (* sourceRegion : SR.source * region -> sourceRegion option *)
  fun sourceRegion (source as {sourceMap, ...}: SR.source, region: region)
                  : sourceRegion option =
      (case regionToLocations (!sourceMap, region)
	of SOME (start, finish) =>		
	     SOME {source = source, start = start, finish = finish}
	 | NONE => NONE)

  (* regionNewlineCount : sourceMap * region -> int
   * determines the number of newlines occurring within a region,
   * which may be 0 for a region that lies within a single line.
   * Also, by convention, returns 0 for NULLregion *)
  fun newlineCount (smap: sourceMap, region: region) =
      (case regionToLocations (smap, region)
	 of SOME ({line=lo_line, ...}, {line=hi_line,...}) =>
	      hi_line - lo_line  (* hi_line and lo_line may be equal *)
	  | NONE => 0)

  (* uptoPos': sourceMap -> charpos -> sourceMap
   * The next to last line of the result sourceMap contains the charpos.
   * REQUIRE: pos comes before the last line in the sourceMap argument.
   * NOTE. If a file ends with a newline, there will be no characters 
   * within the empty "last" line, so the target position can't be in the last line.
   * The line number components of lines are irrelevant here.
   * EDGE CASE. What if charpos is in the last line, possibly the first position
   *   in an empty last line? *)
  fun uptoPos' (pos: charpos, smap: sourceMap) =
      let fun strip (lines0 as ((pos0, _) :: (lines1 as ((pos1, _) :: _)))) =
	        let fun strip2 (lines0 as ((p1, _) :: (lines1 as ((p2, _) :: _)))) =
	                if p2 <= pos then lines0 else strip2 lines1
	         in if pos < pos0
		    then if pos1 <= pos  (* pos1 <= pos < pos0 *)
			 then lines0
			 else strip2 lines1 (* pos < pos1 *) 
		    else bug "uptoPos': too late" (* pos >= pos0 -- too "late?" *)
		end
	     | strip _ = bug "uptoPos': not enough lines"
			 (* need at least two lines to bracket pos! ?? *)
	in strip smap
       end

  (* LATENT BUG!? *)
  (* widenToLines : sourceMap -> region -> region
   * expand a region to the beginning, respectively end,
   * of the first and last lines intersecting the region.
   * ASSUME: the region hi limit comes before the last newline in the input,
   * which should be the case if the input ends with a newline.
   * EDGE CASE: What if "hi" is the position after a final newline in a file? FIXME! *)
  fun widenToLines (smap: sourceMap) (region: region) : region =
      (case region
	 of NULLregion => NULLregion
          | REGION (lo, hi) =>
	      let val (lines1 as ((after_hi, _) :: _)) = uptoPos' (hi, smap)
		  val (before_lo, _) :: _ = uptoPos (lo, lines1)
               in REGION (before_lo, after_hi - 1)
	      end)

  (* regionContent: source * region -> (string * region * int) option
   * returns NONE if the source's content is not available or if region = NULLregion,
   * content is widened to full lines, with region = widenedRegion and first line, line*)
  fun regionContent (source as {sourceMap,...}: SR.source, region) =
	case SR.getContent source
	  of NONE => NONE  (* interactive source without content history *)
	   | SOME content =>
	     (case widenToLines (!sourceMap) region
	       of (widenedRegion as REGION (lo, hi)) =>
		  let val content = substring(content, lo-1, hi-lo)
		      val {line,...} = charposToLocation (!sourceMap, lo)
		   in SOME (content, widenedRegion, line)
		  end
		| NULLregion => NONE)
 

  (* translating types to strings *)

  (* regionToString : region -> string *)
  fun regionToString (REGION (lo, hi): region) =
        String.concat [Int.toString lo, "-", Int.toString hi]
    | regionToString NULLregion = "<<>>"  (* conventional presentation of NULLregion *)

  (* locationToString : location -> string *)
  fun locationToString ({line, column}: location) =
      String.concat [Int.toString line, ".", Int.toString column]

  (* sourceLocToString : sourceLoc -> string *)
  fun sourceLocToString ({source = {fileOpened, ...}, loc}: sourceLoc) =
        String.concat [fileOpened, ":", locationToString loc]

  (* sourceRegionToString : sourceRegion -> string *)
  fun sourceRegionToString ({source = {fileOpened,...}, start, finish}: sourceRegion) =
        String.concat [fileOpened, ":", locationToString start, "-", locationToString finish]

  (* regionUnion : region * region -> region *)
  fun regionUnion (REGION (l1, r1), REGION (l2, r2)) =
        REGION (Int.min (l1, l2), Int.max (r1, r2))
    | regionUnion (NULLregion, reg2 as REGION _) = reg2
    | regionUnion (reg1 as REGION _, NULLregion) = reg1
    | regionUnion (NULLregion, NULLregion) = NULLregion

end (* top local *)
end (* structure SourceMap *)

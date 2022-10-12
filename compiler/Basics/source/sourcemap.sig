(* sourceMap.sig
 *
 * COPYRIGHT (c) 2012, 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* Character positions, source locations, source maps

The goal of this interface is to map character positions to locations
in source files, where a location is described in ``file-line-column''
format.  The major type exported by this interface is sourceMap,
which maintains the mapping.  This way, most of a compiler can work
with character positions, but we can use real source locations in
error messages.

A region represents a contiguous span of characters within a file,
starting with the character "at" the lower limit and ending at the
character immediately before the upper limit.

A source map is maintained as mutable state consisting of a list
of line numbers with their initial character positions, in reverse
order (first line comes last).

Column numbers are obtained by counting characters from the
beginning of the line; the first character on the line is in column 1.
Tabs are given no special treatment (they count as one character).

Character positions increase as the compiler moves through the source,
and the lexer mutates the source map any time something interesting
happens. The only interesting event is:

* The lexer encounters a newline, changing the line number in the source
  file.

Character positions are nonnegative (in fact, positive), and they increase
in successive lines added to a sourceMap, and the initial line of
the sourceMap starts at charpos 1.

The functions filepos and fileregion map character positions and
regions back to the source level. In the pair of sourcelocs returned by
fileregion, both sourcelocs contain the same file name. 
newlineCount returns the number of newlines that occurred
in the given region.

================================================================================
Change Log:
1. [Ramsey?]
  - changed ErrorMsg to use SourceMap to get source locations; only the
    formatting is done internally
  - added SourceMap structure

2. [DBM, 2022.09] Revised, along with ErrorMsg:
  - convert to the NewPP library
  - remove Ramsey's NoWeb(?) machinery supporting "#line" directives
    and "resynchronization" with multiple input files.
    A source is associated with a single file, or with an interactive
    input stream like StdIn.
*)

signature SOURCE_MAP =
sig

  (* types *)
  datatype region = NULLregion | REGION of Source.charpos * Source.charpos
  type location = {line : int, column: int}
  type sourceLoc = {source : Source.source, loc: location}
  type sourceRegion = {source: Source.source, start : location, finish: location}

  (* regions *)
  val nullRegion : region              (* == NULLregion *)
  val isNullRegion : region -> bool

  val charposToLocation : Source.sourceMap * Source.charpos -> location

  val regionToLocations : Source.sourceMap * region -> (location * location) option
      (* using sourceMaps to translate charpos to sourceLoc -- produces NONE for NULLregion *)

  val sourceRegion : Source.source * region -> sourceRegion option
      (* produces NONE for NULLregion *)

  val lastLineStartPos : Source.sourceMap -> Source.charpos
      (* the charpos of the last line (hd) of the sourceMap *)

  val newlineCount : Source.sourceMap * region -> int
      (* number of newlines within the region -- 0 for NULL region *)

  val widenToLines : Source.sourceMap -> region -> region
      (* widens a region to the containing region with full lines *)

  val regionContent : Source.source * region -> (string * region * int) option
      (* Contents of region in source widened to complete lines.
       * For region # REGION _, returns:
       * content of widened region, the widened region, and the starting line number
       * of the widened region, which is the same as the first line of the original
       * region.
       * For region # NULLregion, returns NONE. *)

  val regionToString : region -> string
  val locationToString : location -> string
  val sourceLocToString : sourceLoc -> string
  val sourceRegionToString : sourceRegion -> string

end (* signature SOURCE_MAP *)

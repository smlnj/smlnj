(* sourcemap.sig
 *
 * COPYRIGHT (c) 2012 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* Source locations

The goal of this interface is to map character positions to locations
in source files, where a location is described in ``file-line-column''
format.  The major type exported by this interface is sourcemap,
which maintains the mapping.  This way, most of a compiler can work
with character positions, but we can use real source locations in
error messages.

A region represents a contiguous span of source locations as seen by
the compiler.  Because of preprocessing, any region could be spread
out over multiple overlapping regions in the original source.

A source map is maintained as mutable state.  We create such a map by
giving the initial character position, file name, line, and column
number.  Column numbers are obtained by counting characters from the
beginning of the line; the first character on the line is deemed to be
in column 1.  Tabs are given no special treatment.

Character positions increase as the compiler moves through the source,
and the lexer mutates the source map any time something interesting
happens.  The two interesting events are:

* The lexer encounters a newline, changing the line number in the source
file.

* The lexer encounters "#line" or its equivalent, changing the
source coordinates.

By analogy with the lcc implementation, I call this event a
resynchronization. A resynchronization must change the line number.
It may change the file name and column number; if not specified they
default to the current file name and 1, respectively.  As suggested by
John Reppy, a resynchronization can specify a line number of 0 (in
order to make the numbering of the following line come out right).

Character positions must be nonnegative, and they must increase in
successive mutations of a single sourcemap (where the initialization
counts as a mutation).

forgetOldPositions causes the sourcemap to discard information about
positions already known to the source map.  Subsequent queries may
refer only to new positions (which must still be larger than the old
ones).  The only reason to call forgetOldPositions is to avoid space
leaks.

lastChange returns the position of the last mutation, or the initial
position if no mutations have taken place.

filepos and fileregion map character positions and regions back to the
source level.  If the null region is passed to fileregion, it returns
the empty list.  In any pair returned by fileregion, the two source
locations are guaranteed to have the same file name.  newlineCount
returns the number of newlines that occurred in the given region.
*)

(*
 * changed ErrorMsg to use SourceMap to get source locations; only the
 * formatting is done internally
 *
 * added SourceMap structure
*)

signature SOURCE_MAP =
sig

  type charpos = int
  type region = charpos * charpos

  type sourceloc = {fileName:string, line:int, column:int}

  type sourcemap (* = opaque mutable *)

  val nullRegion : region              (* left and right identity of span *)
  val isNullRegion : region -> bool

  (* creating and modifying sourcemaps *)
  val newSourceMap : string -> sourcemap
  val newline : sourcemap -> charpos -> unit
  val resynch : sourcemap ->
                charpos * charpos * int * int * string option -> unit

  (* using sourcemaps to translate charpos to sourceloc *)
  val filepos     : sourcemap -> charpos -> sourceloc
  val fileregion  : sourcemap -> region  -> (sourceloc * sourceloc) list

  val lastLinePos : sourcemap -> charpos
  val newlineCount : sourcemap -> region -> int

  val widenToLines : sourcemap -> region -> region

end (* signature SOURCE_MAP *)

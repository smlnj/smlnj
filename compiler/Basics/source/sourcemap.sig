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

A region represents a contiguous span of characters within a file.

A source map is maintained as mutable state consisting of a list
of line numbers with their initial character positions, in reverse
order (first line comes last).

Column numbers are obtained by counting characters from the
beginning of the line; the first character on the line is deemed to be
in column 1. Tabs are given no special treatment (they count as one 
character).

Character positions increase as the compiler moves through the source,
and the lexer mutates the source map any time something interesting
happens.  The only interesting event is:

* The lexer encounters a newline, changing the line number in the source
file.

Character positions must be nonnegative, and they must increase in
successive mutations of a single sourcemap (where the initialization
counts as a mutation).

The functions filepos and fileregion map character positions and
regions back to the source level. In the pair of sourcelocs returned by
fileregion, both sourcelocs contain the same file name. 
newlineCount returns the number of newlines that occurred
in the given region.
*)

(*
 * changed ErrorMsg to use SourceMap to get source locations; only the
 * formatting is done internally
 *
 * added SourceMap structure
*)

signature SOURCE_MAP =
sig

  type charpos = int  (* base 1 *)
  type region = charpos * charpos

  type sourceloc = {fileName : string, line : int, column : int}

  type sourcemap (* opaque mutable *)

  val nullRegion : region              (* (0,0), by convention *)
  val isNullRegion : region -> bool

  (* creating and modifying sourcemaps *)
  val newSourceMap : string -> sourcemap (* string is file name *)
  val newline : sourcemap -> charpos -> unit

  (* using sourcemaps to translate charpos to sourceloc *)
  val filepos     : sourcemap -> charpos -> sourceloc
  val fileregion  : sourcemap -> region  -> (sourceloc * sourceloc)

  val lastLinePos : sourcemap -> charpos
  val newlineCount : sourcemap -> region -> int

  val widenToLines : sourcemap -> region -> region

end (* signature SOURCE_MAP *)

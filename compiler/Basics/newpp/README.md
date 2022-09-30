File: newpp/README.md

# README for NewPP, A New Prettyprinter

Version 7.3.

This is a revision of Version 7 generation implmentation of the New
PrettyPrinter. It is a 2-phase prettyprinter where a value to be
prettyprinted is first mapped to a "format", which is then "rendered"
to printed text (or sometimes, to a "layout" type, such as string).

Version 7.1 Changes:

1. make the interface more consistent and orthogonal by introducing
"alignment" datatype and currying basic block constructing functions
with respect to alignment and indentation (bindent).

2. add "generic" version of formatList taking an alignment argument.

Version 7.2 Changes

1. BLOCK format constructor renamed ABLOCK (for aligned block).

2. Alignment replaces separator argument for several functions, such
as formatSeq and formatClosedSeq, and the alignment argument is
incorporated into the record argument as a field.

3. Added several functions to the interface: sequence, tupleFormats,
and listFormats, acting on format lists. alignedBlock, the general
constructor for aligned block, takes curried alignment and bindent
arguments.

4. Added functions for managing the line width: setLineWidthFun, 
resetLineWidthFun, getLineWidth. These are used to isolate the prettyprinter
library from compiler internals like Control.Print.lineWidth.

Version 7.3 Changes

1. Added functions to the interface: labeled, printFormat, etc.

2. Added a new alignment constructor (alignment mode) named "C", for
"compact".  This mode introduces no separators between format
elements.  So cblock is the same as concat, which is now depricated as
redundant and will be removed in the next version.

3. Added a new EMPTY constructor for blocks.  This means the EMPTY
block is now _first class_.  EMPTY will act as an _identity_ element
for binary format operators like ccat, hcat, vcat, pcat, and it will
"disappear" (be absorbed) when it occurs within a block.  So, for instance,
a format like hcat (text "abc", empty) when rendered will not produce a
spurious space character after the text "abc", because

  hcat (fmt,empty) == fmt.

4. New sequence operators specializing the general "sequence" function.: E.g.

    psequence sepfmt fmts == sequence {alignment = P, sep = sepfmt} fmts

5. New str-NewPP.adoc file to document the NEWPP signature in the style
   of the smlnj-lib documentation.

6. Created newpp/doc directory and put design-notes.txt and
   manual.adoc in the new doc directory.


## Features

- _flat_, _static_ measure of formats

- _memoized_ block measures

- _aligned_ and _special_ blocks

- **FLAT** format constructor (replaces **TRYFLAT** constructor from earlier versions)

- _indented_ and _nonindented_ blocks; indentation is associated with
  blocks, not with line breaks; indentation affects the complete
  content of a block -- an indented block always starts on a fresh
  line (after possible indentation).
  
  
## Files

- format.sml, the datatypes defining formats

- measure.{sig,sml}, computing the static, flat measure of a format

- render.{sig,sml}, rendering a format to printed characters

- newpp.{sig,sml}, the interface used for writing formatter functions
             Defines `NewPP : NEW_PP`

- newpp.cm, the CM file for compiling the prettyprinter

- doc/manual.{adoc, html}, the manual for the prettyprinter

- doc/design-notes.txt, extensive notes on the design of NewPP and
  prettyprinter library design in general.

Eventually the *.{sig,sml} files should be moved to a src directory.

Eventually, the NewPP library and documentation should be moved to
smlnj-lib.

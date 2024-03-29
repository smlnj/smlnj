# README for PrettyPrint, the New Prettyprinter Library

The implementation of the new pretty printer. This is a two-phase pretty printer
where a value to be prettyprinted is first mapped to a _format_, which is then
_rendered_ to printed text (or sometimes, to a "layout" type, such as string).

## Features

- _flat_, _static_ measure of formats

- _memoized_ block measures

- _aligned_ and _basic_ blocks

- **FLAT** format constructor (replaces **TRYFLAT** constructor from earlier versions)

- _indented_ and _nonindented_ blocks; indentation is associated with
  blocks, not with line breaks; indentation affects the complete
  content of a block -- an indented block always starts on a fresh
  line (after possible indentation).
  
  
## Files

- src/format.sml, the datatypes defining formats

- src/measure.{sig,sml}, computing the static, flat measure of a format

- src/render.{sig,sml}, rendering a format to printed characters

- src/prettyprint.{sig,sml}, the interface used for writing formatter functions
    Defines `PrettyPrint : PRETTYPRINT`

- src/source.cm, the CM file for compiling the prettyprinter, 

- prettyprint.cm, the CM file for compiling the prettyprinter, 
  referring to src/sources.cm

[Following files are located in compiler/Basics/prettyprint/doc:
- doc/str-PrettyPrint.{adoc, html}, the interface documentation

- doc/prettyprint-manual.{adoc, html}, the manual for the prettyprinter library

- doc/design-notes.txt, extensive notes on the design of PrettyPrint and
  prettyprinter library design in general.
  (now maintained in github/newpptr repo)
]

This copy of the PrettyPrint library [and documentation] is found in
smlnj-lib/PRETTYPRINT, and it should supercede the copy in compiler/Basics/prettyprint.


## Change Log:

**Version 7.1**

1. make the interface more consistent and orthogonal by introducing
"alignment" datatype and currying basic block constructing functions
with respect to alignment and indentation (bindent).

2. add "generic" version of formatList taking an alignment argument.


**Version 7.2**

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


**Version 7.3**

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


**Version 7.4**

1. Renamings:

  NewPP --> NewPrettyPrint    (newprettyprint.sml)
  NEW_PP --> NEW_PRETTYPRINT  (newprettyprint.sig)

  format.sml:
    separator --> break
	SEP --> BRK
	SBLOCK --> BLOCK
    
  newprettyprint.sml/sig
    specialBlock --> basicBlock
    sblock --> block
	siblock --> iblock

2. Added:

  format.sml (Format)
    NullBreak constructor of type break (was separator)
  
  newprettyprint.sig/sml (NewPrettyPrint: NEW_PRETTYPRINT)
	vHeaders, vHeaderFormats (from NewPPUtil)

3. Removed:

  newprettyprint.sig/sml
    tuple (use tupleFormat instead)


**Version 8.0**

0. Major change in the handling of indentation. Indentation is
   represented by two new format datacons: HINDENT, SINDENT. The
   bindent type is removed, as well as "indented block" functions.

1. Renamed:

2. Removed:

  format.sml, newprettyprint.{sig,sml}, measure.sml, render.sml
    bindent type removed

  NewPrettyPrint: NEW_PRETTYPRINT
    iblock, hiblock, piblock, viblock, ciblock

3. Added:

  format.sml
    format constructors: HINDENT, SINDENT (hard and soft indented formats)

4. Changed:

  newprettyprint.sig, sml (structure NewPrettyPrint)

	Implementation of hardIndent and softIndent have been changed. These
	functions are defined directly in terms of the HINDENT and SINDENT
	constructors, but are curried.


** Version 8.1 [2023.1.2]**

0. Merged HINDENT and SINDENT into a single format constructor INDENT
   that retains the behavior of SINDENT.  Function softIndent renamed
   to indent, and hardIndent replaced by breakIndent in NewPrettyPrint.
   
1. Renamed

   format.sml
     SINDENT --> INDENT  (* same semantics *)
	 
   newprettyprint.sig,sml
     softIndent --> indent

2. Removed

   format.sml
     HINDENT
	 
   newprettyprint.sig, sml
     hardIndent
   
3. Added

   newprettyprint.sig,sml
     breakIndent : int -> format -> format

   
** Version 8.2**

  NewPrettyPrint renamed PrettyPrint, and the corresponding change is made to file names,
  signatures, etc, and all former references to NewPrettyPrint throughout the compiler are
  replaced by references to PrettyPrint. The old PrettyPrint structure that was defined in
  Basics/print/prettyprint.sml is no longer used (or even compiled).
  
    NewPrettyPrint --> PrettyPrint
	NEW_PRETTYPRINT --> PRETTYPRINT
    smlnj-lib/NEWPP --> smlnj-lib/PRETTYPRINT
	src/newprettyprint.{sig,sml} --> src/prettyprint{sig,sml}
	smlnj-lib/NEWPP/newpp-lib.cm -> smlnj-lib/PRETTYPRINT/prettyprint-lib.cm


** Version 8.3**

  The PrettyPrint.breakIndent function is removed from PrettyPrint and PRETTYPRINT.
  It did not work correctly, because it was defined in terms of block, which resets
  the blm relative to which the indentation is taken.

  The smlnj-lib/PRETTYPRINT directory is renamed smlnj-lib/PrettyPrint.

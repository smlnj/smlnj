Admin/control/README.txt

This directory ($SMLNJ/Admin/control) contains the various "control" structures for the
compiler's major components. These used to be defined using the smlnj-lib/Controls
library, but they have been simplified to use ordinary references (thus no registries,
priorities, obscurities, help strings, etc.).

The control flags defined in these control structures have types bool, int (should/could
be word, since negative integer flag values usually are not appropriate), string, and, in
a couple cases, string lists. Control flags should not involve internal compiler types
(e.g. LambdaVar.var) since they are typically accessed (on the user side) from the
interactive top-level or from user programs, which do not normally have access to compiler
internal types. This means that the Control files don't depend on anything other than the
Basis library.


The contents of Admin/control (and what they will replace):

* Renamed and relocated control structures:
  (new versions are all now located in Admin/control/)

PrintControl: PRINT_CONTROL <-
  Control_Print: PRINTCONTROL (Basics/print/printcontrol.sml)

ParserControl: PARSER_CONTROL <-
  ParserControl: ? <- (Parse/main/parsercontrol.sml)

ElabDataControl : ELABDATA_CONTROL <-
  ElabDataControl (ElabData/main/edcontrol.s??)

ElaboratorControl: ELABORATOR_CONTROL <-
  ElabControl: ELAB_CONTROL (Elaborator/control/elabcontrol.s??)

MatchCompControl: MATCHCOMP_CONTROL <-
  MCControl (Elaborator/control/mccontrol.s??)

FLINTControl: FLINT_CONTROL <-
  FLINT_Control (FLINT/main/control.s??)

CodeGenControl : CODEGEN_CONTROL <-
  Control_CG: CGCONTROL (TopLevel/main/control.s??)

CompilerControl: COMPILER_CONTROL <-
  <- BasicControl [printWarnings] (Basics/control/basiccontrol.sml)
  plus items from the old version of Control: CONTROL (TopLevel/main/control.s??)

* New control structures

TopLevelControl: TOPLEVEL_CONTROL
  items from old Control: CONTROL (TopLevel/main/control.{sig,sml})

* Overall control structure that simply bundles all the above as structure
  elements. It has not control flags of its own, since all the control flags in
  the old version of Control have been moved to TopLevelControl or CompilerControl.

Control: CONTROL

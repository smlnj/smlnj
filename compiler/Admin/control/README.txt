Admin/control/README.txt

This directory ($SMLNJ/Admin/control) gathers the various "control" structures for the compiler's major components. These used to be defined
using the smlnj-lib/Controls library, but they have been simplified to
use ordinary references (thus no registries, priorities, obscurities, help
strings, etc.).


* Renamed and relocated control structures:
  (all now located in Admin/control/)

PrintControl: PRINT_CONTROL <-
  Control_Print: PRINTCONTROL (* Basics/print/printcontrol.sml *)

ElaboratorControl: ELABORATOR_CONTROL <-
  ElabControl: ELAB_CONTROL (Elaborator/control/elabcontrol.{sml,sig})

ElabDataControl : ELABDATA_CONTROL <- same
  (ElabData/main/edcontrol.{sml,sig})

MatchCompControl: MATCHCOMP_CONTROL <-
  MCControl (Elaborator/control/mccontrol.{sml,sig})

FLINTControl: FLINT_CONTROL <-
  FLINT_Control (FLINT/main/control.{sml,sig})

CodeGenControl : CODEGEN_CONTROL <-
  Control_CG: CGCONTROL (TopLevel/main/control.{sml,sig})

CompilerControl: COMPILER_CONTROL <-
  BasicControl: ? <- (Basics/control/basiccontrol.sml)
  plus items from Control: CONTROL (TopLevel/main/control.{sig,sml})

ParserControl: PARSER_CONTROL <-
  ParserControl: ? <- (Parse/main/parsercontrol.sml)

* New control structures

TopLevelControl: TOPLEVEL_CONTROL
  items from old Control: CONTROL (TopLevel/main/control.{sig,sml})

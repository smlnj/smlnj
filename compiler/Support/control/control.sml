(* Admin/control/control.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * The main Control structure, which bundles up all the "component" control structures.
 *)

structure Control : CONTROL =
struct

  structure Print : PRINT_CONTROL = PrintControl
     (* formerly found at Basics/print/printcontrol.sml *)

  (* ElabData controls *)
  structure ElabData : ELABDATA_CONTROL = ElabDataControl
     (* formerly found at ElabData/main/edcontrol.{sml,sig} *)

  (* elaborator controls *)
  structure Elaborator : ELABORATOR_CONTROL = ElaboratorControl
     (* formerly found at Elaborator/control/elabcontrol.{sml,sig} *)

  (* MatchComp (match compiler) controls *)
  structure MatchComp : MATCHCOMP_CONTROL = MatchCompControl
     (* formerly found at Elaborator/control/mccontrol.{sml,sig} *)

  (* FLINT controls *)
  structure FLINT = FLINTControl
     (* formerly found at FLINT/main/control.{sml,sig} *)

  structure CodeGen : CODEGEN_CONTROL = CodeGenControl
     (* formerly Control_CG : CONTROL_CG, found in TopLevel/main/control.{sig,sml} *)

  structure Compiler : COMPILER_CONTROL = CompilerControl
     (* formerly BasicControl, found in Basics/control/basiccontrol.sml *)

  structure Parser = ParserControl

  structure TopLevel = TopLevelControl

end (* structure Control *)

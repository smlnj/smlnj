(* Admin/control/codegen-control.sig
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure TopLevelControl =
struct

  val eldebugging = ref false (* "evalloop debugging" *)
  val pddebugging = ref false (* "PPDec debugging" *)
  val printAst = ref false (* "whether to print Ast representation" *)
  val printAbsyn = ElaboratorControl.printAbsyn

  val progressMsgs = ref false (* "whether to print a message after each phase is completed" *)

(* defunct --
  val interp = new ("interp", "?", false)
*)

end (* structure TopLevelControl *)

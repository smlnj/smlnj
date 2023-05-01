(* Admin/control/toplevel-control.sig
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TOPLEVEL_CONTROL =
sig

  val eldebugging : bool ref (* EvalLoopF debugging *)
  val pddebugging : bool ref (* PPDec debugging *)
  val printAst : bool ref
  val printAbsyn : bool ref

  val progressMsgs : bool ref
  (* turn on printing of progress messages at end of major stages in evalloop *)

  (* val interp: bool ref  -- defunct *)

end (* signature TOPLEVEL_CONTROL *)

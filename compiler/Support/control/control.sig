(* Admin/control/control.sig
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * The signature of the main Control structure, which bundles up all
 * the "component" control structures.
 *)

signature CONTROL =
sig

  structure Print : PRINT_CONTROL (* PrintControl *) 

  structure ElabData : ELABDATA_CONTROL  (* ElabDataControl *)

  structure Elaborator : ELABORATOR_CONTROL  (* ElaboratorControl *)

  structure MatchComp : MATCHCOMP_CONTROL  (* MatchCompControl *)

  structure FLINT : FLINT_CONTROL    (* FLINTControl *)

  structure CodeGen : CODEGEN_CONTROL (* CodeGenControl *)

  structure Compiler : COMPILER_CONTROL  (* CompilerControl *)

  structure Parser: PARSER_CONTROL  (* ParserControl *)

end (* signature CONTROL *)

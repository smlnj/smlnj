(* x86Rewrite.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature X86REWRITE =
  sig
    structure I  : X86INSTR
    structure CB : CELLS_BASIS = CellsBasis
    val rewriteUse : I.instruction * CB.cell * CB.cell -> I.instruction
    val rewriteDef : I.instruction * CB.cell * CB.cell -> I.instruction
    val frewriteUse : I.instruction * CB.cell * CB.cell -> I.instruction
    val frewriteDef : I.instruction * CB.cell * CB.cell -> I.instruction
  end

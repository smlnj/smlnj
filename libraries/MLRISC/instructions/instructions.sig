(* instructions.sig
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies.
 *
 *  This signature specifies the abstract view of an instruction.
 *)

signature INSTRUCTIONS =
sig
   structure C   : CELLS
   structure CB  :CELLS_BASIS = CellsBasis
   type operand             (* operands supported by architecture *)
   type addressing_mode                        (* addressing mode *)
   type ea              (* effective address for accessing memory *)
   type instr                       (* architecture instructions  *)
   
   datatype instruction =                   (* partially abstract *)
       LIVE of {regs: C.cellset, spilled: C.cellset}
     | KILL of {regs: C.cellset, spilled: C.cellset}
     | COPY of 
         {k: CB.cellkind, 
	  sz: int,		                       (* in bits *)
	  dst: CB.cell list, 
	  src: CB.cell list,
	  tmp: ea option      (* = NONE if |dst| = |src| = 1 *)
         }
     | ANNOTATION of {i: instruction, a: Annotations.annotation}
     | INSTR of instr
end

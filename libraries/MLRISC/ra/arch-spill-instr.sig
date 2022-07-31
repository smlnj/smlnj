(* arch-spill-instr.sig
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * Architecture specific instructions to emit when spilling an instruction.
 *)

(* TODO: Some day, all these interface functions will be sensitive to
 * the size being spilled or reloaded --- but today is not the day!
 *)
signature ARCH_SPILL_INSTR = sig
  structure I : INSTRUCTIONS
  structure CB : CELLS_BASIS = CellsBasis
  
  val spillToEA :
      CB.cellkind ->
         CB.cell * I.ea -> 
            {code:I.instruction list, proh:CB.cell list, newReg:CB.cell option}

  val reloadFromEA :
      CB.cellkind ->
         CB.cell * I.ea ->
            {code:I.instruction list, proh:CB.cell list, newReg:CB.cell option}	   

  val spill : 
      CB.cellkind -> 
         I.instruction * CB.cell * I.ea -> 
	    {code:I.instruction list, proh:CB.cell list, newReg:CB.cell option}
  val reload : 
      CB.cellkind ->
         I.instruction * CB.cell * I.ea -> 
		 {code:I.instruction list, proh:CB.cell list, newReg:CB.cell option}
end
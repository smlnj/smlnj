(* ppcSpillInstr.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * PPC instructions to emit when spilling an instruction.
 *)

functor PPCSpillInstr(Instr : PPCINSTR) : ARCH_SPILL_INSTR = struct
  structure I = Instr
  structure C = I.C
  structure Rewrite = PPCRewrite(I)
  structure CB = CellsBasis
  

  fun error msg = MLRiscErrorMsg.error ("PPCSpillInstr", msg)

  fun storeToEA CB.GP (reg, I.Displace{base, disp, mem}) = 
        I.st{st=I.STW, rs=reg, ra=base, d=I.LabelOp disp, mem=mem}
    | storeToEA CB.FP (freg, I.Displace{base, disp, mem}) = 
        I.stf{st=I.STFD, ra=base, d=I.LabelOp disp, fs=freg, mem=mem}
    | storeToEA _ _ = error "storeToEA"

  fun loadFromEA CB.GP (reg, I.Displace{base, disp, mem}) = 
        I.l{ld=I.LWZ, ra=base, d=I.LabelOp disp, rt=reg, mem=mem}
    | loadFromEA CB.FP (freg, I.Displace{base, disp, mem}) = 
        I.lf{ld=I.LFD, ra=base, d=I.LabelOp disp, ft=freg, mem=mem}
    | loadFromEA _ _ = error "loadFromEA"

  fun spillToEA ck reg_ea = 
      {code=[storeToEA ck reg_ea], proh=[], newReg=NONE}

  fun reloadFromEA ck reg_ea = 
      {code=[loadFromEA ck reg_ea], proh=[], newReg=NONE}

 (* spill a register to spillLoc *)
  fun spillR (instr, reg, ea) = let
        val newR = C.newReg()
        val instr' = Rewrite.rewriteDef(instr, reg, newR)
      in 
        {code=[instr', storeToEA CB.GP (newR, ea)], 
	 proh=[newR],
	 newReg=SOME newR}
      end

  fun spillF (instr, reg, ea) = let
	val newR = C.newFreg()
	val instr' = Rewrite.frewriteDef(instr, reg, newR)
      in 
	  {code=[instr', storeToEA CB.FP (newR, ea)],
	   proh=[newR],
	   newReg=SOME newR}
      end

 (* reload a register from spillLoc *)
  fun reloadR(instr, reg, ea) = let
    val newR = C.newReg()
    val instr' = Rewrite.rewriteUse(instr, reg, newR)
  in
      {code=[loadFromEA CB.GP (newR, ea), instr'],
       proh=[newR],
       newReg=SOME newR}
  end

  fun reloadF(instr, reg, ea) = let
    val newR = C.newFreg()
    val instr' = Rewrite.frewriteUse(instr, reg, newR)
  in
      {code=[loadFromEA CB.FP (newR, ea), instr'],
       proh=[newR],
       newReg=SOME newR}
  end

  fun spill CellsBasis.GP = spillR
    | spill CellsBasis.FP = spillF
    | spill _ = error "spill"

  fun reload CellsBasis.GP = reloadR
    | reload CellsBasis.FP = reloadF
    | reload _ = error "reload"
end


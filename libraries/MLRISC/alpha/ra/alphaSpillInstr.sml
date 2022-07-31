(* alphaSpillInstr.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * Alpha instructions to emit when spilling an instruction.
 *)

functor AlphaSpillInstr(Instr : ALPHAINSTR) : ARCH_SPILL_INSTR = struct
  structure I = Instr
  structure C = I.C
  structure CB = CellsBasis
 
  structure Rewrite = AlphaRewrite(I)

  fun error msg = MLRiscErrorMsg.error ("AlphaSpillInstr", msg)

  fun storeAtEA CB.GP (reg, I.Displace{base, disp, mem}) = 
       I.store{stOp=I.STL, b=base, d=I.LABop disp, r=reg, mem=mem}
    | storeAtEA CB.FP (reg, I.Displace{base, disp, mem}) =  
       I.fstore{stOp=I.STT, b=base, d=I.LABop disp, r=reg, mem=mem}
    | storeAtEA _ _ = error "storeAtEA"

  fun loadFromEA CB.GP (reg, I.Displace{base, disp, mem}) =
       I.load{ldOp=I.LDL, r=reg, b=base, d=I.LABop disp, mem=mem}
    | loadFromEA CB.FP (reg, I.Displace{base, disp, mem}) = 
       I.fload{ldOp=I.LDT, r=reg, b=base, d=I.LABop disp, mem=mem}
    | loadFromEA _ _ = error "loadFromEA"

  fun spillToEA ck reg_ea = 
      {code=[storeAtEA ck reg_ea], proh=[], newReg=NONE}

  fun reloadFromEA ck reg_ea =
      {code=[loadFromEA ck reg_ea], proh=[], newReg=NONE}

 (* spill a register to spillLoc *)
  fun spillR (instr, reg, spillLoc) = let
    val newR = C.newReg()
    val instr' = Rewrite.rewriteDef(instr, reg, newR)
  in 
      {code=[instr', storeAtEA CB.GP (newR, spillLoc)],
       proh=[newR],
       newReg=SOME newR}
  end

  fun spillF (instr, reg, spillLoc) = let
    val newR = C.newFreg()
    val instr' = Rewrite.frewriteDef(instr, reg, newR)
  in 
      {code=[instr', storeAtEA CB.FP (newR, spillLoc)],
       proh=[newR],
       newReg=SOME newR}
  end

 (* reload a register from spillLoc *)
  fun reloadR(instr, reg, spillLoc) = let
    val newR = C.newReg()
    val instr' = Rewrite.rewriteUse(instr, reg, newR)
  in
      {code=[loadFromEA CB.GP (newR, spillLoc), instr'],
       proh=[newR],
       newReg=SOME newR}
  end

  fun reloadF(instr, reg, spillLoc) = let
    val newR = C.newFreg()
    val instr' = Rewrite.frewriteUse(instr, reg, newR)
  in
      {code=[loadFromEA CB.FP (newR, spillLoc), instr'],
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


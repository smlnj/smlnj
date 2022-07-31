(* hppaSpillInstr.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * Hppa instructions to emit when spilling an instruction.
 *)

functor HppaSpillInstr(Instr : HPPAINSTR) : ARCH_SPILL_INSTR = struct
  structure I = Instr
  structure C = I.C
  structure Rewrite = HppaRewrite(I)
  structure CB=CellsBasis

  fun error msg = MLRiscErrorMsg.error ("HppaSpillInstr", msg)

  val tmpR = I.C.asmTmpR
  fun storeToEA CB.GP (reg, I.Displace{base, disp, mem}) = 
       [I.store{st=I.STW, b=base, d=I.LabExp(disp, I.F), r=reg, mem=mem}]
    | storeToEA CB.FP (reg, I.Displace{base, disp, mem}) = 
       [I.ldil{i=I.HILabExp(disp, I.F), t=tmpR},
	I.ldo{i=I.LOLabExp(disp, I.F), b=tmpR, t=tmpR},
	I.fstorex{fstx=I.FSTDX, b=base, x=tmpR, r=reg, mem=mem}]
    | storeToEA _ _ = error "storeToEA"

  fun loadFromEA CB.GP (reg, I.Displace{base, disp, mem}) = 
      [I.loadi{li=I.LDW, r=base, i=I.LabExp(disp,I.F), t=reg, mem=mem}]
    | loadFromEA CB.FP (reg, I.Displace{base, disp, mem}) =  
      [I.ldil{i=I.HILabExp(disp, I.F), t=tmpR},
       I.ldo{i=I.LOLabExp(disp, I.F), b=tmpR, t=tmpR},
       I.floadx{flx=I.FLDDX, b=base, x=tmpR, t=reg, mem=mem}]
    | loadFromEA _ _ = error "loadFromEA"

  fun spillToEA ck reg_ea = 
      {code=storeToEA ck reg_ea, proh=[], newReg=NONE}
  fun reloadFromEA ck reg_ea = 
      {code=loadFromEA ck reg_ea, proh=[], newReg=NONE}

 (* spill a register to spillLoc *)
  fun spillR (instr, reg, ea) = let
        val newR = C.newReg()
        val instr' = Rewrite.rewriteDef(instr, reg, newR)
      in 
        {code=instr' :: storeToEA CB.GP (newR, ea),
	 proh=[newR],
	 newReg=SOME newR}
      end

  fun spillF (instr, reg, ea) = let
	val newR = C.newFreg()
	val instr' = Rewrite.frewriteDef(instr, reg, newR)
      in 
	  {code=instr' :: storeToEA CB.FP (newR, ea),
	   proh=[newR],
	   newReg=SOME newR}
      end

 (* reload a register from spillLoc *)
  fun reloadR(instr, reg, ea) = let
    val newR = C.newReg()
    val instr' = Rewrite.rewriteUse(instr, reg, newR)
  in
      {code= loadFromEA CB.GP (newR, ea) @ [instr'],
       proh=[newR],
       newReg=SOME newR}
  end

  fun reloadF(instr, reg, ea) = let
    val newR = C.newFreg()
    val instr' = Rewrite.frewriteUse(instr, reg, newR)
  in
      {code=loadFromEA CB.FP (newR, ea) @ [instr'],
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


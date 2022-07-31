functor MIPSShuffle(I:MIPSINSTR) : MIPSSHUFFLE = struct
  structure I = I
  structure Shuffle = Shuffle(I)

  type t = {tmp:I.ea option, dst:I.C.cell list, src:I.C.cell list}

  val mem=I.Region.memory

  val zeroR = I.Reg I.C.r0 

  fun move{src=I.Direct rs, dst=I.Direct rt} = 
        [I.ARITH{oper=I.ADDU, rs=rs, i=zeroR, rt=rt}]
    | move{src=I.Direct rs, dst=I.Displace{base, disp}} = 
	[I.STORE{s=I.SW, rs=rs, b=base, d=I.Imm disp, mem=mem}]
    | move{src=I.Displace{base, disp}, dst=I.Direct rt} = 
	[I.LOAD{l=I.LW, rt=rt, b=base, d=I.Imm disp, mem=mem}]

  fun fmove{src=I.FDirect fs, dst=I.FDirect ft} =
        [I.FUNARY{oper=I.MOV_D, fs=fs, ft=ft}] 
    | fmove{src=I.FDirect fs, dst=I.Displace{base, disp}} = 
	[I.FSTORE{s=I.SDC1, fs=fs, b=base, d=I.Imm disp, mem=mem}]
    | fmove{src=I.Displace{base, disp}, dst=I.FDirect ft} =
	[I.FLOAD{l=I.LDC1, ft=ft, b=base, d=I.Imm disp, mem=mem}]

  val shuffle = Shuffle.shuffle {mvInstr=move, ea=I.Direct}

  val shufflefp = Shuffle.shuffle {mvInstr=fmove, ea=I.FDirect}
end


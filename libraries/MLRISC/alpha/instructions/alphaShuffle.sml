functor AlphaShuffle(I:ALPHAINSTR) : ALPHASHUFFLE = struct
  structure I = I
  structure Shuffle = Shuffle(I)

  type t = {tmp:I.ea option, dst:CellsBasis.cell list, src:CellsBasis.cell list}
  fun error msg = MLRiscErrorMsg.error("AlphaShuffle",msg)

  val zeroR = I.REGop(Option.valOf(I.C.zeroReg CellsBasis.GP))

  fun move{src=I.Direct rs, dst=I.Direct rd} = 
        [I.operate{oper=I.BIS, ra=rs, rb=zeroR, rc=rd}]
    | move{src=I.Direct rs, dst=I.Displace{base, disp, mem}} = 
	[I.store{stOp=I.STL, r=rs, b=base, d=I.LABop disp, mem=mem}]
    | move{src=I.Displace{base, disp, mem}, dst=I.Direct rt} = 
	[I.load{ldOp=I.LDL, r=rt, b=base, d=I.LABop disp, mem=mem}]
    | move _ = error "move"

  fun fmove{src=I.FDirect fs, dst=I.FDirect fd} = 
        [I.foperate{oper=I.CPYS, fa=fs, fb=fs, fc=fd}]
    | fmove{src=I.FDirect fs, dst=I.Displace{base, disp, mem}} = 
	[I.fstore{stOp=I.STT, r=fs, b=base, d=I.LABop disp, mem=mem}]
    | fmove{src=I.Displace{base, disp, mem}, dst=I.FDirect ft} =
	[I.fload{ldOp=I.LDT, r=ft, b=base, d=I.LABop disp, mem=mem}]
    | fmove _ = error "fmove"

  val shuffle = Shuffle.shuffle {mvInstr=move, ea=I.Direct}

  val shufflefp = Shuffle.shuffle {mvInstr=fmove, ea=I.FDirect}
end


functor SparcShuffle(I:SPARCINSTR) : SPARCSHUFFLE = 
struct
  structure I = I
  structure W = Word32
  structure Shuffle = Shuffle(I)
  structure CB = CellsBasis
  type t = {tmp:I.ea option, dst:CB.cell list, src:CB.cell list}

  fun error msg = MLRiscErrorMsg.error("SparcShuffle",msg)
  val zeroR = Option.valOf(I.C.zeroReg CB.GP)

  fun move{src=I.Direct rs, dst=I.Direct rt} = 
       [I.arith{a=I.OR, r=zeroR, i=I.REG rs, d=rt}]
    | move{src=I.Displace{base, disp, mem}, dst=I.Direct rt} =
       [I.load{l=I.LD, r=base, i=I.LAB disp, d=rt, mem=mem}] 
    | move{src=I.Direct rs, dst=I.Displace{base, disp, mem}} = 
       [I.store{s=I.ST, r=base, i=I.LAB disp, d=rs, mem=mem}] 
    | move _ = error "move"

  fun fmove{src=I.FDirect fs, dst=I.FDirect fd} = 
       [I.fpop1{a=I.FMOVd, r=fs, d=fd}] 
    | fmove{src=I.Displace{base, disp, mem}, dst=I.FDirect ft} = 
       [I.fload{l=I.LDDF, r=base, i=I.LAB disp, d=ft, mem=mem}] 
    | fmove{src=I.FDirect fs, dst=I.Displace{base, disp, mem}} = 
       [I.fstore{s=I.STDF, r=base, i=I.LAB disp, d=fs, mem=mem}] 
    | fmove _ = error "fmove"

  val shuffle = Shuffle.shuffle{mvInstr = move, ea=I.Direct}

  val shufflefp = Shuffle.shuffle {mvInstr=fmove, ea=I.FDirect}
end



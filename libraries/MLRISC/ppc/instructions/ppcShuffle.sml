functor PPCShuffle(I:PPCINSTR) = struct
  structure I = I
  structure Shuffle = Shuffle(I)

  type t = {tmp:I.ea option, dst:CellsBasis.cell list, src:CellsBasis.cell list}

  fun error msg = MLRiscErrorMsg.error("PPCShuffle",msg)

  (* WARNING: these move operators assume 32 bit addressing is used! 
   * Allen
   *)
  fun move{src=I.Direct rs, dst=I.Direct rd} = 
        [I.arith{oper=I.OR, rt=rd, ra=rs, rb=rs, Rc=false, OE=false}]
    | move{src=I.Direct rs, dst=I.Displace{base, disp, mem}} = 
	[I.st{st=I.STW, rs=rs, ra=base, d=I.LabelOp disp, mem=mem}]
    | move{src=I.Displace{base, disp, mem}, dst=I.Direct rt} = 
	[I.l{ld=I.LWZ, rt=rt, ra=base, d=I.LabelOp disp, mem=mem}]
    | move _ = error "move"

  fun fmove{src=I.FDirect fs, dst=I.FDirect fd} = 
        [I.funary{oper=I.FMR, fb=fs, ft=fd, Rc=false}]
    | fmove{src=I.FDirect fs, dst=I.Displace{base, disp, mem}} = 
	[I.stf{st=I.STFD, fs=fs, ra=base, d=I.LabelOp disp, mem=mem}]
    | fmove{src=I.Displace{base, disp, mem}, dst=I.FDirect ft} =
	[I.lf{ld=I.LFD, ft=ft, ra=base, d=I.LabelOp disp, mem=mem}]
    | fmove _ = error "fmove"

  val shuffle = Shuffle.shuffle {mvInstr=move, ea=I.Direct}

  val shufflefp = Shuffle.shuffle {mvInstr=fmove, ea=I.FDirect}
end

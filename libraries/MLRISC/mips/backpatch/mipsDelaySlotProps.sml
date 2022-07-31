functor MIPSDelaySlots
   (structure I : MIPSINSTR
    structure P : INSN_PROPERTIES where I = I
   ) : DELAY_SLOT_PROPERTIES =
struct
   structure I  = I
   structure C  = I.C
   structure SL = C.SortedCells

   fun error msg = MLRiscErrorMsg.error("MIPSDelaySlotProps",msg)

   datatype delay_slot = D_NONE | D_ERROR | D_ALWAYS | D_TAKEN | D_FALLTHRU

   val delaySlotSize = 4

   fun delaySlot{instr, backward} =
     case instr of
       I.J{nop, ...} => {n=false,nOn=D_ALWAYS,nOff=D_ERROR,nop=nop}
     | I.JR{nop, ...} => {n=false,nOn=D_ALWAYS,nOff=D_ERROR,nop=nop}
     | I.JAL{nop, ...} => {n=false,nOn=D_ALWAYS,nOff=D_ERROR,nop=nop}
     | I.JALR{nop, ...} => {n=false,nOn=D_ALWAYS,nOff=D_ERROR,nop=nop}
     | I.BRANCH{nop, ...} => {n=false,nOn=D_ALWAYS,nOff=D_ERROR,nop=nop}
     | I.FBRANCH{nop, ...} => {n=false,nOn=D_ALWAYS,nOff=D_ERROR,nop=nop}
     | I.ANNOTATION{i,...} => delaySlot{instr=i,backward=backward}
     | _ => {n=false,nOn=D_ERROR,nOff=D_NONE,nop=false}

   fun enableDelaySlot{instr, n=true, nop} = 
         error "enableDelaySlot: can't nullify"
     | enableDelaySlot{instr, n, nop} = 
       case instr of
         I.J{lab, ...} => I.J{lab=lab,nop=nop}
       | I.JR{rs, labels, ...} => I.JR{rs=rs,labels=labels,nop=nop}
       | I.JAL{lab, defs, uses, cutsTo, mem, ...} => 
         I.JAL{lab=lab, defs=defs, uses=uses, cutsTo=cutsTo, mem=mem, nop=nop}
       | I.JALR{rs, rt, defs, uses, cutsTo, mem, ...} => 
         I.JALR{rs=rs, rt=rt,
                defs=defs, uses=uses, cutsTo=cutsTo, mem=mem, nop=nop}
       | I.RET _ => I.RET{nop=nop}
       | I.BRANCH{likely, cond, rs, rt, lab, ...} =>
           I.BRANCH{likely=likely, cond=cond, rs=rs, rt=rt, lab=lab, nop=nop}
       | I.FBRANCH{likely, fbranch, cc, lab, ...} =>
           I.FBRANCH{likely=likely, fbranch=fbranch, cc=cc, lab=lab, nop=nop}
       | I.ANNOTATION{i,a} => 
           I.ANNOTATION{i=enableDelaySlot{instr=i,n=n,nop=nop},a=a}
       | _ => error "enableDelaySlot"

    val defUseI = P.defUse C.GP
    val defUseF = P.defUse C.FP
    val zeroR   = Option.valOf(C.zeroReg C.GP)
    fun conflict{src=i,dst=j} = error "conflict"

    fun delaySlotCandidate
          {jmp, delaySlot=(I.J _ | I.JR _ | I.JAL _ | I.JALR _ 
                          | I.RET _ | I.BRANCH _ | I.FBRANCH _)} = false
      | delaySlotCandidate{jmp=I.ANNOTATION{i,...},delaySlot} = 
           delaySlotCandidate{jmp=i,delaySlot=delaySlot}
      | delaySlotCandidate{jmp,delaySlot=I.ANNOTATION{i,...}} = 
           delaySlotCandidate{jmp=jmp,delaySlot=i}
      | delaySlotCandidate _ = true

   fun setTarget(I.ANNOTATION{i,a},lab) = I.ANNOTATION{i=setTarget(i,lab),a=a}
     | setTarget _ = error "setTarget"

end

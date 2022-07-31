functor SparcDelaySlots
   (structure I : SPARCINSTR
    structure P : INSN_PROPERTIES where I = I
    (* sharing/defn conflict:   sharing P.I = I*)
   ) : DELAY_SLOT_PROPERTIES =
struct
   structure I  = I
   structure C  = I.C
   structure SL = CellsBasis.SortedCells

   fun error msg = MLRiscErrorMsg.error("SparcDelaySlotProps",msg)

   datatype delay_slot = D_NONE | D_ERROR | D_ALWAYS | D_TAKEN | D_FALLTHRU

   val delaySlotSize = 4

   fun delaySlot{instr, backward} =
     case instr of
       I.INSTR(I.CALL{nop,...}) => {n=false,nOn=D_ERROR,nOff=D_ALWAYS,nop=nop}
     | I.INSTR(I.JMP{nop,...}) => {n=false,nOn=D_ERROR,nOff=D_ALWAYS,nop=nop}
     | I.INSTR(I.JMPL{nop,...}) => {n=false,nOn=D_ERROR,nOff=D_ALWAYS,nop=nop}
     | I.INSTR(I.RET{nop,...})  => {n=false,nOn=D_ERROR,nOff=D_ALWAYS,nop=nop}
     | I.INSTR(I.Bicc{b=I.BA,a,nop,...}) => {n=false,nOn=D_NONE,nOff=D_ALWAYS,nop=nop}
     | I.INSTR(I.Bicc{a,nop,...}) => {n=a,nOn=D_TAKEN,nOff=D_ALWAYS,nop=nop}
     | I.INSTR(I.FBfcc{a,nop,...}) => {n=a,nOn=D_TAKEN,nOff=D_ALWAYS,nop=nop}
     | I.INSTR(I.BR{a,nop,...}) => {n=a,nOn=D_TAKEN,nOff=D_ALWAYS,nop=nop}
     | I.INSTR(I.BP{a,nop,...}) => {n=a,nOn=D_TAKEN,nOff=D_ALWAYS,nop=nop}
     | I.INSTR(I.FCMP{nop,...}) => {n=false,nOn=D_ERROR,nOff=D_ALWAYS,nop=nop}
     | I.ANNOTATION{i,...} => delaySlot{instr=i,backward=backward}
     | _ => {n=false,nOn=D_ERROR,nOff=D_NONE,nop=false}

   fun enableDelaySlot{instr, n, nop} =
       case (instr,n) of
         (I.INSTR(I.CALL{defs,uses,label,cutsTo,mem,...}),false) => 
	    I.call{defs=defs,uses=uses,label=label,cutsTo=cutsTo,
                   nop=nop,mem=mem}
       | (I.INSTR(I.JMPL{r,i,d,defs,uses,mem,cutsTo,...}),false) => 
	    I.jmpl{r=r,i=i,d=d,defs=defs,uses=uses,cutsTo=cutsTo,
                   nop=nop,mem=mem}
       | (I.INSTR(I.JMP{r,i,labs,...}),false) => 
	    I.jmp{r=r,i=i,labs=labs,nop=nop}
       | (I.INSTR(I.RET{leaf,...}),false) => I.ret{leaf=leaf,nop=nop}
       | (I.INSTR(I.Bicc{b,a,label,...}),_) => I.bicc{b=b,a=n,nop=nop,label=label}
       | (I.INSTR(I.FBfcc{b,a,label,...}),_) => I.fbfcc{b=b,a=n,nop=nop,label=label}
       | (I.INSTR(I.BR{nop,label,p,r,rcond,...}),_) =>
            I.br{rcond=rcond,r=r,a=n,nop=nop,label=label,p=p}
       | (I.INSTR(I.BP{nop,label,p,cc,b,...}),_) =>
            I.bp{b=b,cc=cc,a=n,nop=nop,label=label,p=p}
       | (I.INSTR(I.FCMP{cmp,r1,r2,...}),false) => I.fcmp{cmp=cmp,r1=r1,r2=r2,nop=nop}
       | (I.ANNOTATION{i,a},n) => 
           I.ANNOTATION{i=enableDelaySlot{instr=i,n=n,nop=nop},a=a}
       | _ => error "enableDelaySlot"

    val defUseI = P.defUse CellsBasis.GP
    val defUseF = P.defUse CellsBasis.FP
    val psr     = [C.psr] 
    val fsr     = [C.fsr]
    val y       = [C.y]
    val zeroR   = Option.valOf(C.zeroReg CellsBasis.GP)
    val everything = [C.y,C.psr,C.fsr]
    fun conflict{src=i,dst=j} = 
        let fun cc I.ANDCC  = true
              | cc I.ANDNCC = true
              | cc I.ORCC   = true
              | cc I.ORNCC  = true
              | cc I.XORCC  = true
              | cc I.XNORCC = true
              | cc I.ADDCC  = true
              | cc I.TADDCC  = true
              | cc I.TADDTVCC = true
              | cc I.SUBCC = true
              | cc I.TSUBCC = true
              | cc I.TSUBTVCC= true
              | cc I.UMULCC = true
              | cc I.SMULCC = true
              | cc I.UDIVCC = true
              | cc I.SDIVCC = true 
              | cc _ = false
            fun defUseOther(I.INSTR(I.Ticc _)) = ([],psr)
              | defUseOther(I.INSTR(I.ARITH{a,...})) = 
                  if cc a then (psr,[]) else ([],[])
              | defUseOther(I.INSTR(I.WRY _)) = (y,[])
              | defUseOther(I.INSTR(I.RDY _)) = ([],y)
              | defUseOther(I.INSTR(I.FCMP _)) = (fsr,[])
              | defUseOther(I.INSTR(I.Bicc{b=I.BA,...})) = ([],[])
              | defUseOther(I.INSTR(I.Bicc _)) = ([],psr)
              | defUseOther(I.INSTR(I.FBfcc _)) = ([],fsr)
              | defUseOther(I.INSTR(I.MOVicc _)) = ([],psr)
              | defUseOther(I.INSTR(I.MOVfcc _)) = ([],fsr)
              | defUseOther(I.INSTR(I.FMOVicc _)) = ([],psr)
              | defUseOther(I.INSTR(I.FMOVfcc _)) = ([],fsr)
              | defUseOther(I.INSTR(I.CALL _)) = (everything,[])
              | defUseOther(I.INSTR(I.JMPL _)) = (everything,[])
              | defUseOther(I.ANNOTATION{i,...}) = defUseOther i
              | defUseOther _ = ([],[])
            fun clash(defUse) =
                let val (di,ui) = defUse i
                    val (dj,uj) = defUse j
                in  SL.nonEmptyIntersection(di,uj) orelse
                    SL.nonEmptyIntersection(di,dj) orelse
                    SL.nonEmptyIntersection(ui,dj) 
                end
            fun toSL f i = let val (d,u) = f i
                           in  (SL.uniq d, SL.uniq u) end
            fun defUseInt i = 
                let val (d,u) = defUseI i
                    val d     = SL.uniq d
                    val u     = SL.uniq u
                    (* no dependence on register 0! *) 
                in  (SL.rmv(zeroR,d), SL.rmv(zeroR,u)) end
        in  clash(defUseInt) orelse 
            clash(toSL defUseF) orelse
            clash(toSL defUseOther)
        end

    fun delaySlotCandidate{jmp,delaySlot=
              (  I.INSTR(I.CALL _) | I.INSTR(I.Bicc _) | I.INSTR(I.FBfcc _) 
               | I.INSTR(I.Ticc _) | I.INSTR(I.BR _) | I.INSTR(I.JMP _) | I.INSTR(I.JMPL _) 
	       | I.INSTR(I.RET _) | I.INSTR(I.BP _) | I.INSTR(I.FCMP _))} = false
      | delaySlotCandidate{jmp=I.ANNOTATION{i,...},delaySlot} = 
           delaySlotCandidate{jmp=i,delaySlot=delaySlot}
      | delaySlotCandidate{jmp,delaySlot=I.ANNOTATION{i,...}} = 
           delaySlotCandidate{jmp=jmp,delaySlot=i}
      | delaySlotCandidate _ = true 

   fun setTarget(I.INSTR(I.Bicc{b,a,nop,...}),lab) = I.bicc{b=b,a=a,nop=nop,label=lab}
     | setTarget(I.INSTR(I.FBfcc{b,a,nop,...}),lab) = I.fbfcc{b=b,a=a,nop=nop,label=lab}
     | setTarget(I.INSTR(I.BR{rcond,p,r,a,nop,...}),lab) = 
          I.br{rcond=rcond,p=p,r=r,a=a,nop=nop,label=lab}
     | setTarget(I.INSTR(I.BP{b,p,cc,a,nop,...}),lab) = 
          I.bp{b=b,p=p,cc=cc,a=a,nop=nop,label=lab}
     | setTarget(I.ANNOTATION{i,a},lab) = I.ANNOTATION{i=setTarget(i,lab),a=a}
     | setTarget _ = error "setTarget"

end

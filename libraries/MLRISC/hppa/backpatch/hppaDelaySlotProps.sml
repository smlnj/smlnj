(*
 * This module describes how to fill delay slots on the HPPA
 *)

functor HppaDelaySlots
   (structure I : HPPAINSTR
    structure P : INSN_PROPERTIES where I = I
   ) : DELAY_SLOT_PROPERTIES =
struct
   structure I  = I
   structure C  = I.C
   structure SL = CellsBasis.SortedCells

   fun error msg = MLRiscErrorMsg.error("HppaDelaySlotProps",msg)

   datatype delay_slot = D_NONE | D_ERROR | D_ALWAYS | D_TAKEN | D_FALLTHRU

   val delaySlotSize = 4

   (*
    * On the HP, things are quite complicated:
    *
    *  For conditional branches:
    *  -------------------------
    *                                 Branch direction         
    *                      Forward                    Backward
    *  Nullify bit on    Nullify if branch taken   Nullify if branch not-taken
    *  Nullify bit off   Delay slot active         Delay slot active
    *
    *  For unconditional branches:
    *  ---------------------------
    *       
    *  Nullify bit on    Delay slot nullified
    *  Nullify bit off   Delay slot active       
    *)

   fun delaySlot{instr, backward} =
       case instr of
         I.INSTR(I.BCOND{nop,n,...}) => 
             {nop=nop, n=n, nOn=if backward then D_TAKEN else D_FALLTHRU, 
              nOff=D_ALWAYS}
       | I.INSTR(I.BCONDI{nop,n,...}) => 
             {nop=nop, n=n, nOn=if backward then D_TAKEN else D_FALLTHRU, 
              nOff=D_ALWAYS}
       | I.INSTR(I.BB{nop,n,...}) =>
             {nop=nop, n=n, nOn=if backward then D_TAKEN else D_FALLTHRU, 
              nOff=D_ALWAYS}
       | I.INSTR(I.B{n,...}) => {nop=false, n=n, nOn=D_NONE, nOff=D_ALWAYS}
       | I.INSTR(I.LONGJUMP{n,...}) => {nop=false, n=n, nOn=D_NONE, nOff=D_ALWAYS}
       | I.INSTR(I.BV{n,...}) => {nop=false, n=n, nOn=D_NONE, nOff=D_ALWAYS}
       | I.INSTR(I.BE{n,...}) => {nop=false, n=n, nOn=D_NONE, nOff=D_ALWAYS}
       | I.INSTR(I.BLR{n,...}) => {nop=false, n=n, nOn=D_NONE, nOff=D_ALWAYS}
       | I.INSTR(I.BL{n,...}) => {nop=false, n=n, nOn=D_NONE, nOff=D_ALWAYS}
       | I.ANNOTATION{i,...} => delaySlot{instr=i,backward=backward}
       | _ => {n=false,nOn=D_ERROR,nOff=D_NONE,nop=false}

   fun enableDelaySlot{instr, n, nop} =
       case (instr,nop) of
         (I.INSTR(I.BCOND{cmp,bc,r1,r2,t,f,...}),_) => 
             I.bcond{cmp=cmp,bc=bc,nop=nop,n=n,r1=r1,r2=r2,t=t,f=f}
       | (I.INSTR(I.BCONDI{cmpi,bc,i,r2,t,f,...}),_) => 
             I.bcondi{cmpi=cmpi,bc=bc,nop=nop,n=n,i=i,r2=r2,t=t,f=f}
       | (I.INSTR(I.BB{bc,p,r,t,f,...}),_) => 
             I.bb{bc=bc,p=p,nop=nop,n=n,r=r,t=t,f=f}
       | (I.INSTR(I.B{lab,...}),false) => I.b{lab=lab,n=n}
       | (I.INSTR(I.BV{labs,b,x,...}),false) => I.bv{labs=labs,b=b,x=x,n=n}
       | (I.INSTR(I.BE{labs,b,d,sr,...}),false) => I.be{labs=labs,b=b,d=d,sr=sr,n=n}
       | (I.INSTR(I.BLR{x,t,labs,...}),false) => I.blr{x=x,t=t,labs=labs,n=n}
       | (I.INSTR(I.BL{lab,t,defs,uses,cutsTo,mem,...}),false) => 
            I.bl{lab=lab,t=t,defs=defs,uses=uses,cutsTo=cutsTo,mem=mem,n=n}
       | (I.INSTR(I.LONGJUMP{lab,tmp,tmpLab,...}),false) => 
            I.longjump{lab=lab,tmp=tmp,tmpLab=tmpLab,n=n}
       | (I.ANNOTATION{i,a},_) => 
           I.ANNOTATION{i=enableDelaySlot{instr=i,n=n,nop=nop},a=a}
       | _ => error "enableDelaySlot"

    val defUseI = P.defUse CellsBasis.GP
    val defUseF = P.defUse CellsBasis.FP
    val zeroR   = Option.valOf(C.zeroReg CellsBasis.GP) 

    fun conflict{src=i,dst=j} = 
        let fun clash(defUse) =
                let val (di,ui) = defUse i
                    val (dj,uj) = defUse j
                in  SL.nonEmptyIntersection(di,uj) orelse
                    SL.nonEmptyIntersection(di,dj) orelse
                    SL.nonEmptyIntersection(ui,dj) 
                end
            fun defUseInt i = 
                let val (d,u) = defUseI i
                    val d     = SL.uniq d
                    val u     = SL.uniq u
                    (* no dependence on register 0! *) 
                in  (SL.rmv(zeroR,d), SL.rmv(zeroR,u)) end
            fun defUseReal i = 
                let val (d,u) = defUseF i
                    val d     = SL.uniq d
                    val u     = SL.uniq u
                in  (d,u) end
        in  clash(defUseInt) orelse clash(defUseReal) 
        end

    fun delaySlotCandidate{jmp,delaySlot=
             (  I.INSTR(I.BCOND _) | I.INSTR(I.BCONDI _) | I.INSTR(I.BB _) | I.INSTR(I.FBRANCH _) 
              | I.INSTR(I.BV _) | I.INSTR(I.BE _) | I.INSTR(I.COMCLR_LDO _) | I.INSTR(I.COMICLR_LDO _) 
	      | I.INSTR(I.B _) | I.INSTR(I.LONGJUMP _) | I.INSTR(I.BLR _) | I.INSTR(I.BL _) 
	      | I.INSTR(I.BLE _))} = false
      | delaySlotCandidate{jmp=I.ANNOTATION{i,...},delaySlot} = 
           delaySlotCandidate{jmp=i,delaySlot=delaySlot}
      | delaySlotCandidate{jmp,delaySlot=I.ANNOTATION{i,...}} = 
           delaySlotCandidate{jmp=jmp,delaySlot=i}
      | delaySlotCandidate _ = true

   fun setTarget(I.INSTR(I.BCOND{n,nop,r1,r2,cmp,bc,t,f,...}), lab) = 
         I.bcond{cmp=cmp,bc=bc,nop=nop,n=n,r1=r1,r2=r2,t=lab,f=f}
     | setTarget(I.INSTR(I.BCONDI{n,nop,i,r2,cmpi,bc,t,f,...}), lab) = 
         I.bcondi{cmpi=cmpi,bc=bc,nop=nop,n=n,i=i,r2=r2,t=lab,f=f}
     | setTarget(I.INSTR(I.BB{bc,r,p,n,nop,t,f,...}), lab) = 
         I.bb{bc=bc,p=p,nop=nop,n=n,r=r,t=lab,f=f}
     | setTarget(I.INSTR(I.B{n,...}), lab) = I.b{n=n,lab=lab}
     | setTarget(I.INSTR(I.LONGJUMP{n, tmp, tmpLab, ...}), lab) = 
         I.longjump{n=n, tmp=tmp, tmpLab=tmpLab, lab=lab}
     | setTarget(I.ANNOTATION{i,a},lab) = I.ANNOTATION{i=setTarget(i,lab),a=a}
     | setTarget _ = error "setTarget"

end

(* sparcJumps.sml --- information to resolve jumps. 
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)
functor SparcJumps
  (structure Instr:SPARCINSTR
   structure Shuffle:SPARCSHUFFLE where I = Instr
   structure MLTreeEval : MLTREE_EVAL where T = Instr.T
  ) : SDI_JUMPS = 
struct
  structure I = Instr
  structure C = Instr.C
  structure Const = I.Constant
  structure CB = CellsBasis

  fun error msg = MLRiscErrorMsg.error("SparcJumps",msg)

  val branchDelayedArch = true


  fun isSdi(I.ANNOTATION{i,...}) = isSdi i
    | isSdi(I.LIVE _)		  = true
    | isSdi(I.KILL _)		  = true
    | isSdi(I.COPY _)		  = true
    | isSdi(I.INSTR instr) = let
	fun oper(I.IMMED n) = false
	      | oper(I.REG _) = false
	      | oper(I.HI _) = false
	      | oper(I.LO _) = false
	      | oper(I.LAB _) = true
	in  case instr of 
	      I.ARITH{i,...} => oper i
	    | I.SHIFT{i,...} => oper i
	    | I.LOAD{i,...} => oper i
	    | I.STORE{i,...} => oper i
	    | I.FLOAD{i,...} => oper i
	    | I.FSTORE{i,...} => oper i
	    | I.JMPL{i,...} => oper i
	    | I.JMP{i,...} => oper i
	    | I.MOVicc{i,...} => oper i
	    | I.MOVfcc{i,...} => oper i
	    | I.MOVR{i,...} => oper i
	    | I.CALL _ => true
	    | I.Bicc _ => true
	    | I.FBfcc _ => true
	    | I.BR _ => true
	    | I.BP _ => true
	    | I.Ticc{i,...} => oper i
	    | I.WRY{i,...} => oper i
	    | I.SAVE{i,...} => oper i
	    | I.RESTORE{i,...} => oper i
	    (* The following is only true of Version 8 *)
	    | I.FPop1{a=(I.FMOVd | I.FNEGd | I.FABSd), ...} => true
	    | _ => false
	end

  fun minSize(I.ANNOTATION{i,...}) = minSize i
    | minSize(I.LIVE _)  = 0
    | minSize(I.KILL _)  = 0
    | minSize (I.COPY _) = 0		(* ? *)
    | minSize(I.INSTR instr) = 
      (case instr
	of (I.Bicc{nop=true,...}) => 8
	 | (I.FBfcc{nop=true,...}) => 8
	 | (I.JMP{nop=true,...}) => 8
	 | (I.JMPL{nop=true,...}) => 8
	 | (I.CALL{nop=true,...}) => 8
	 | (I.BR{nop=true,...}) => 8
	 | (I.BP{nop=true,...}) => 8
	 | (I.RET{nop=true,...}) => 8
	 | (I.FCMP{nop=true,...}) => 8
	 | (I.FPop1{a=(I.FMOVd | I.FNEGd | I.FABSd),...}) => 8
	 |  _          => 4
      (*esac*))

  fun maxSize (I.INSTR(I.FPop1{a=(I.FMOVd | I.FNEGd | I.FABSd),...})) = 8
    | maxSize (I.ANNOTATION{i,...}) = maxSize i
    | maxSize _		   = 4

  fun immed13 n = ~4096 <= n andalso n < 4096
  fun immed22 n = ~0x200000 <= n andalso n < 0x1fffff
  fun immed16 n = ~0x8000 <= n andalso n < 0x8000
  fun immed19 n = ~0x40000 <= n andalso n < 0x40000
  fun immed30 n = ~0x4000000 <= n andalso n < 0x3ffffff

  fun instrLength([],n) = n
    | instrLength(I.INSTR(I.FPop1{a=(I.FMOVd | I.FNEGd | I.FABSd),...})::is,n) =
        instrLength(is,n+8)
    | instrLength(_::is,n) = instrLength(is,n+4)

  fun sdiSize(I.ANNOTATION{i, ...}, labmap, loc) = sdiSize(i, labmap, loc)
    | sdiSize(I.LIVE _, _, _) = 0
    | sdiSize(I.KILL _, _, _) = 0
    | sdiSize(I.COPY{k=CB.GP, src, dst, tmp, ...}, _, _) =
	      4 * length(Shuffle.shuffle{tmp=tmp, dst=dst, src=src})
    | sdiSize(I.COPY{k=CB.FP, src, dst, tmp, ...}, _, _) = let
		val instrs = Shuffle.shufflefp{src=src, dst=dst, tmp=tmp}
            in instrLength(instrs, 0)
            end
    | sdiSize(instr as I.INSTR i, labMap, loc) = let
	  fun oper(I.IMMED n,_) = 4
	    | oper(I.REG _,_) = 4
	    | oper(I.HI _,_) = 4
	    | oper(I.LO _,_) = 4
	    | oper(I.LAB lexp,hi) = if immed13(MLTreeEval.valueOf lexp) then 4 else hi
	  fun displacement lab = ((labMap lab) - loc) div 4
	  fun branch22 lab = if immed22(displacement lab) then 4 else 16
	  fun branch19 lab = if immed19(displacement lab) then 4 else 16
	  fun branch16 lab = if immed16(displacement lab) then 4 else 16
	  fun call lab = if immed30(displacement lab) then 4 else 20
	  fun delaySlot false = 0
	    | delaySlot true = 4
      in  
	 case i 
	 of  I.ARITH{a=I.OR,r,i,...} => 
	       if CellsBasis.cellId r = 0 then oper(i,8) else oper(i,12)
	  | I.ARITH{i,...} => oper(i,12)
	  | I.SHIFT{i,...} => oper(i,12)
	  | I.LOAD{i,...} => oper(i,12)
	  | I.STORE{i,...} => oper(i,12)
	  | I.FLOAD{i,...} => oper(i,12)
	  | I.FSTORE{i,...} => oper(i,12)
	  | I.Ticc{i,...} => oper(i,12)
	  | I.SAVE{i,...} => oper(i,12)
	  | I.RESTORE{i,...} => oper(i,12)
	  | I.MOVicc{i,...} => oper(i,12)
	  | I.MOVfcc{i,...} => oper(i,12)
	  | I.MOVR{i,...} => oper(i,12)
	  | I.JMPL{i,nop,...} => oper(i,12) + delaySlot nop
	  | I.JMP{i,nop,...} => oper(i,12) + delaySlot nop
	  | I.Bicc{label,nop,...} => branch22 label + delaySlot nop
	  | I.FBfcc{label,nop,...} => branch22 label + delaySlot nop
	  | I.BR{label,nop,...} => branch16 label + delaySlot nop
	  | I.BP{label,nop,...} => branch19 label + delaySlot nop
	  | I.CALL{label,...} => call label
	  | I.WRY{i,...} => oper(i,12)
 	  | I.FPop1{a=(I.FMOVd | I.FNEGd | I.FABSd),...} => 8	    
	  | _ => error "sdiSize"
      end
    | sdiSize _ = error "sdiSize"

  fun split22_10 n =
  let val w = Word32.fromInt n
  in  {hi=Word32.toInt(Word32.>>(w,0w10)),
       lo=Word32.toInt(Word32.andb(w,0wx3ff))
      }
  end

  fun split(I.LAB lexp) = split22_10(MLTreeEval.valueOf lexp)
    | split _ = error "split"

  (* Expand the immediate constant into two instructions *)
  fun expandImm(immed,instr) = 
      let val {lo,hi} = split immed
      in  
	  [I.sethi{i=hi,d=C.asmTmpR},
           I.arith{a=I.OR,r=C.asmTmpR,i=I.IMMED lo,d=C.asmTmpR},
           I.INSTR instr
          ]
      end

  (* Expand a span dependent instruction *)
  fun expand(I.ANNOTATION{i, ...}, size, pos) = expand(i, size, pos)
    | expand(I.LIVE _, _, _) = []
    | expand(I.KILL _, _, _) = []
    | expand(I.COPY{k=CB.GP, src, tmp, dst, ...}, _, _)  = 
       Shuffle.shuffle{src=src, dst=dst, tmp=tmp}
    | expand(I.COPY{k=CB.FP, src, tmp, dst, ...}, _, _)  = 
       Shuffle.shufflefp{src=src, dst=dst, tmp=tmp}
    | expand(instr as (I.INSTR i), size, pos) = 
      (case (i,size) 
       of (_,4) => [instr]
	| (I.ARITH{a=I.OR,r,i,d},8) =>
	    if CellsBasis.cellId r = 0 then
	    let val {lo,hi} = split i
	    in  [I.sethi{i=hi,d=C.asmTmpR},
		 I.arith{a=I.OR,r=C.asmTmpR,i=I.IMMED lo,d=d}
		]
	    end
	    else error "ARITH"
	| (I.ARITH{a,r,i,d},12) =>
	    expandImm(i,I.ARITH{a=a,r=r,i=I.REG C.asmTmpR,d=d})
	| (I.SHIFT{s,r,i,d},12) =>
	    expandImm(i,I.SHIFT{s=s,r=r,i=I.REG C.asmTmpR,d=d})
	| (I.SAVE{r,i,d},12) =>
	    expandImm(i,I.SAVE{r=r,i=I.REG C.asmTmpR,d=d})
	| (I.RESTORE{r,i,d},12) =>
	    expandImm(i,I.RESTORE{r=r,i=I.REG C.asmTmpR,d=d})
	| (I.LOAD{l,r,i,d,mem},12) =>  
	    expandImm(i,I.LOAD{l=l,r=r,i=I.REG C.asmTmpR,d=d,mem=mem})
	| (I.STORE{s,r,i,d,mem},12) =>
	    expandImm(i,I.STORE{s=s,r=r,i=I.REG C.asmTmpR,d=d,mem=mem})
	| (I.FLOAD{l,r,i,d,mem},12) =>
	    expandImm(i,I.FLOAD{l=l,r=r,i=I.REG C.asmTmpR,d=d,mem=mem})
	| (I.FSTORE{s,r,i,d,mem},12) =>
	    expandImm(i,I.FSTORE{s=s,r=r,i=I.REG C.asmTmpR,d=d,mem=mem})
	| (I.MOVicc{b,i,d},12) =>
	    expandImm(i,I.MOVicc{b=b,i=I.REG C.asmTmpR,d=d})
	| (I.MOVfcc{b,i,d},12) =>
	    expandImm(i,I.MOVfcc{b=b,i=I.REG C.asmTmpR,d=d})
	| (I.MOVR{rcond,r,i,d},12) =>
	    expandImm(i,I.MOVR{rcond=rcond,r=r,i=I.REG C.asmTmpR,d=d})
	| (I.JMPL _,8) => [instr]
	| (I.JMP _,8) => [instr]
	| (I.Bicc _,8) => [instr]
	| (I.FBfcc _,8) => [instr]
	| (I.BR _,8) => [instr]
	| (I.BP _,8) => [instr]
	| (I.JMPL{r,i,d,defs,uses,cutsTo,nop,mem},(12 | 16)) => 
	    expandImm(i,I.JMPL{r=r,i=I.REG C.asmTmpR,d=d,defs=defs,uses=uses,
			       cutsTo=cutsTo,nop=nop,mem=mem})
	| (I.JMP{r,i,labs,nop},(12 | 16)) => 
	    expandImm(i,I.JMP{r=r,i=I.REG C.asmTmpR,labs=labs,nop=nop})
	| (I.Ticc{t,cc,r,i},12) =>
	    expandImm(i,I.Ticc{t=t,cc=cc,r=r,i=I.REG C.asmTmpR})
	    (* 
	     * The sparc uses 22bits signed extended displacement offsets
	     * Let's hope it's enough
	     *)
	| (I.Bicc{b,a,label,nop},_) => error "Bicc"  
	| (I.FBfcc{b,a,label,nop},_) => error "FBfcc" 
	| (I.FPop1{a, r, d}, _) => let
	    fun nextRegNum c = C.FPReg(CellsBasis.registerNum c + 1)
	    (* Note: if r=d then the move is not required.
	     * This needs to be factored into the size before it
	     * can be done here.
	     *)
	    fun doDouble(oper) = 
	      [I.fpop1{a=oper, r=r, d=d},
	       I.fpop1{a=I.FMOVs, r=nextRegNum r, d=nextRegNum d}]
          in
	    case a 
             of I.FMOVd => doDouble(I.FMOVs)
	      | I.FNEGd => doDouble(I.FNEGs)
	      | I.FABSd => doDouble(I.FABSs)
	      | _ => error "expand: FPop1"
          end
	| (I.WRY{r,i},12) => expandImm(i,I.WRY{r=r,i=I.REG C.asmTmpR})
	|  _ => error "expand"
      (*esac*))
    | expand _ = error "expand"

end


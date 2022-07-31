(* hppaProps.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor HppaProps
   ( structure HppaInstr : HPPAINSTR
     structure MLTreeEval : MLTREE_EVAL where T = HppaInstr.T
     structure MLTreeHash : MLTREE_HASH where T = HppaInstr.T
    ) : INSN_PROPERTIES = 
struct
  structure I = HppaInstr
  structure C = HppaInstr.C
  structure CB = CellsBasis

  exception NegateConditional

  fun error msg = MLRiscErrorMsg.error("HppaProps",msg)

  datatype kind = IK_JUMP | IK_NOP | IK_INSTR | IK_COPY | IK_CALL 
                | IK_CALL_WITH_CUTS | IK_PHI | IK_SOURCE | IK_SINK
  datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

  val zeroR = Option.valOf(C.zeroReg CB.GP)
  val r31   = C.Reg CB.GP 31

   (*========================================================================
    *  Instruction Kinds
    *========================================================================*)
  (* Note: BLE and BL used to implement calls are not view as branches *)
  fun instrKind(I.ANNOTATION{i, ...}) = instrKind i
    | instrKind(I.COPY _) = IK_COPY
    | instrKind(I.INSTR instr) = 
      (case instr
	of (I.BCOND _) => IK_JUMP
	 | (I.BCONDI _) => IK_JUMP
	 | (I.BB _)     => IK_JUMP
	 | (I.B _)      => IK_JUMP
	 | (I.BE _)     => IK_JUMP
	 | (I.FBRANCH _)=> IK_JUMP
	 | (I.BV _)     => IK_JUMP
	 | (I.BLR _)    => IK_JUMP
	 | (I.BREAK _)  => IK_JUMP
	 | (I.NOP)      => IK_NOP
	 | (I.BL{cutsTo=_::_,...}) => IK_CALL_WITH_CUTS
	 | (I.BL  _)    => IK_CALL
	 | (I.BLE{cutsTo=_::_,...}) => IK_CALL_WITH_CUTS
	 | (I.BLE _)    => IK_CALL
	 | (I.PHI _)    => IK_PHI
	 | (I.SOURCE _) => IK_SOURCE
	 | (I.SINK _)   => IK_SINK
	 |  _	       => IK_INSTR)
    | instrKind _ = error "instrKind"

  fun moveInstr(I.COPY _) 	     = true
    | moveInstr(I.ANNOTATION{i,...}) = moveInstr i
    | moveInstr _ = false

  fun nop() = I.nop

   (*========================================================================
    *  Parallel Move
    *========================================================================*)
  fun moveTmpR(I.COPY{tmp, ...}) = 
      (case tmp
	of SOME(I.Direct r) => SOME r
	 | SOME(I.FDirect f) => SOME f
	 | _ => NONE
      (*esac*))
    | moveTmpR(I.ANNOTATION{i,...}) = moveTmpR i
    | moveTmpR _ = NONE

  fun moveDstSrc(I.COPY{dst, src, ...}) = (dst, src)
    | moveDstSrc(I.ANNOTATION{i,...}) = moveDstSrc i
    | moveDstSrc _ = error "moveDstSrc"

   (*========================================================================
    *  Branches and Calls/Returns
    *========================================================================*)
  fun branchTargets(I.ANNOTATION{i,...}) = branchTargets i
    | branchTargets(I.INSTR instr) = 
      (case instr
       of (I.BCOND{t, ...})    => [LABELLED t, FALLTHROUGH]
	| (I.BCONDI{t, ...})   => [LABELLED t, FALLTHROUGH]
	| (I.BB{t, ...})       => [LABELLED t, FALLTHROUGH]
	| (I.B{lab, ...})      => [LABELLED lab]
	| (I.FBRANCH{t,...})   => [LABELLED t, FALLTHROUGH]
	| (I.BE{labs=[],...})  => [ESCAPES]
	| (I.BE{labs,...})     => map LABELLED labs
	| (I.BV{labs=[],...})  => [ESCAPES]
	| (I.BV{labs,...})     => map LABELLED labs
	| (I.BLR{labs,...})    => map LABELLED labs
	| (I.BL{cutsTo,...})   => FALLTHROUGH::map LABELLED cutsTo
	| (I.BLE{cutsTo,...})  => FALLTHROUGH::map LABELLED cutsTo
	| (I.BREAK _)	       => [ESCAPES]
	| _ => error "branchTargets"
      (*easc*))
    | branchTargets  _ = error "branchTargets"

  fun jump label = I.b{lab=label,n=true}

  val immedRange = {lo= ~8192, hi=8191}
  fun loadImmed{immed,t} = 
      I.ldo{i=if #lo immedRange <= immed andalso immed <= #hi immedRange 
              then I.IMMED immed
              else I.LabExp(I.T.LI(I.T.I.fromInt(32,immed)),I.F),b=zeroR,t=t}
  fun loadOperand{opn,t} = I.ldo{i=opn,b=zeroR,t=t}

  fun setJumpTarget(I.ANNOTATION{a,i}, l) = I.ANNOTATION{a=a, i=setJumpTarget(i,l)}
    | setJumpTarget(I.INSTR(I.B{n,...}), L) = I.b{lab=L,n=n}
    | setJumpTarget _ = error "setJumpTarget"

  fun setBranchTargets{i=I.ANNOTATION{a,i}, t, f} = 
      I.ANNOTATION{a=a, i=setBranchTargets{i=i, t=t, f=f}}
    | setBranchTargets{i=I.INSTR(I.BCOND{cmp,bc,r1,r2,t,f,n,nop}), t=T, f=F} = 
        I.bcond{cmp=cmp,bc=bc,r1=r1,r2=r2,t=T,f=F,n=n,nop=nop}
    | setBranchTargets{i=I.INSTR(I.BCONDI{cmpi,bc,i,r2,t,f,n,nop=nop}),t=T, f=F} = 
        I.bcondi{cmpi=cmpi,bc=bc,i=i,r2=r2,t=T,f=F,n=n,nop=nop}
    | setBranchTargets{i=I.INSTR(I.BB{bc,r,p,t,f,n,nop}), t=T, f=F} = 
        I.bb{bc=bc,r=r,p=p,t=T,f=F,n=n,nop=nop}
    | setBranchTargets{i=I.INSTR(I.FBRANCH{cc,fmt,n,long,f1,f2,...}), t=T, f=F} = 
        I.fbranch{cc=cc,fmt=fmt,t=T,f=F,n=n,long=long,f1=f1,f2=f2}
    | setBranchTargets _ = error "setBranchTargets"


  (* negate the branch.  Since the HPPA instruction representation tracks both
   * the true and false target labels, we set the false label to be the
   * old true label and set the true label to be the argument label.
   *)
  fun negateConditional (br, lab) = let
    fun revFcond I.?    = I.!?
      | revFcond I.!<=> = I.<=>
      | revFcond I.==   = I.!=
      | revFcond I.?=   = I.!?=
      | revFcond I.!<>  = I.<>
      | revFcond I.!?>= = I.?>=
      | revFcond I.<    = I.!<
      | revFcond I.?<   = I.!?<
      | revFcond I.!>=  = I.>=
      | revFcond I.!?>  = I.?>
      | revFcond I.<=   = I.!<=
      | revFcond I.?<=  = I.!?<=
      | revFcond I.!>   = I.>
      | revFcond I.!?<= = I.?<=
      | revFcond I.>    = I.!>
      | revFcond I.?>   = I.!?>
      | revFcond I.!<=  = I.<=
      | revFcond I.!?<  = I.?<
      | revFcond I.>=   = I.!>=
      | revFcond I.?>=  = I.!?>=
      | revFcond I.!<   = I.<
      | revFcond I.!?=  = I.?=
      | revFcond I.<>   = I.!<>
      | revFcond I.!=   = I.==
      | revFcond I.!?   = I.?
      | revFcond I.<=>  = I.!<=>
      | revFcond _      = error "revFcond"
    fun negate (I.INSTR(I.BCOND{cmp,bc,r1,r2,t,f,n,nop})) = I.bcond{
	    bc=bc, r1=r1, r2=r2, t=lab, f=t, n=n, nop=nop,
	    cmp=case cmp of I.COMBT => I.COMBF | I.COMBF => I.COMBT
	  }
    | negate (I.INSTR(I.BCONDI{cmpi,bc,i,r2,t,f,n,nop})) = I.bcondi{
	    bc=bc, i=i, r2=r2, t=lab, f=t, n=n, nop=nop,
	    cmpi=case cmpi of I.COMIBT => I.COMIBF | I.COMIBF => I.COMIBT
	  }
    | negate (I.INSTR(I.BB{bc,r,p,t,f,n,nop})) = I.bb{
	    bc=case bc of I.BSET => I.BCLR | I.BCLR => I.BSET, 
            r=r,p=p,t=lab,f=t,n=n,nop=nop
	  }
    | negate (I.INSTR(I.FBRANCH{cc,fmt,f1,f2,t,f,n,long})) =
        I.fbranch{cc=revFcond cc,fmt=fmt,f1=f1,f2=f2,t=lab,f=t,n=n,long=long} 
    | negate (I.ANNOTATION{i,a}) = I.ANNOTATION{i=negate i,a=a}
    | negate _ = raise NegateConditional
    in
      negate br
    end

  (*========================================================================
   *  Equality and hashing for operands
   *========================================================================*)
   fun hashFieldSel I.F = 0w0
     | hashFieldSel I.S = 0w1
     | hashFieldSel I.D = 0w2
     | hashFieldSel I.R = 0w3
     | hashFieldSel I.T = 0w4
     | hashFieldSel I.P = 0w5
   fun hashOpn(I.IMMED i) = Word.fromInt i
     | hashOpn(I.LabExp(l,f)) = MLTreeHash.hash l + hashFieldSel f
     | hashOpn(I.HILabExp(l,f)) = MLTreeHash.hash l + hashFieldSel f + 0w10000
     | hashOpn(I.LOLabExp(l,f)) = MLTreeHash.hash l + hashFieldSel f + 0w20000
     | hashOpn(I.REG r) = CB.hashCell r
   fun eqOpn(I.IMMED i,I.IMMED j) = i = j
     | eqOpn(I.REG x,I.REG y) = CB.sameColor(x,y)
     | eqOpn(I.LabExp(a,b),I.LabExp(c,d)) = 
          b = d andalso MLTreeEval.==(a,c)
     | eqOpn(I.HILabExp(a,b),I.HILabExp(c,d)) = 
          b = d andalso MLTreeEval.==(a,c)
     | eqOpn(I.LOLabExp(a,b),I.LOLabExp(c,d)) = 
          b = d andalso MLTreeEval.==(a,c)
     | eqOpn _ = false
   

  (*========================================================================
   *  Definition and use (for register allocation mainly)
   *========================================================================*)
  fun defUseR instr = let
    fun hppaDU instr = let
      fun trap((I.ADDO | I.SUBO | I.SH1ADDO), d, u) = (d, u)
	| trap(_, d, u) = (d, u)
      fun trapi((I.ADDIO | I.SUBIO), d, u) = (d, u)
	| trapi(_, d, u) = (d, u)
    in
      case instr
       of I.STORE {b, r,...}          => ([],  [b,r])
	| I.LOAD {l, r1, r2, t, ...}  => ([t], [r1,r2])
	| I.LOADI {li, r, t, ...}     => ([t], [r])
	| I.ARITH {a, r1, r2, t, ...} => trap(a, [t], [r1,r2])
	| I.ARITHI {ai, r, t, ...}    => trapi(ai, [t], [r])
	| I.COMCLR_LDO{r1, r2, b, t1, t2, ...}=> 
	    if CB.sameColor(t1,t2) then ([t1], [b, r1, r2])
	    else ([t1, t2], [b, r1, r2, t2])
	| I.COMICLR_LDO{i1, r2, b, t1, t2, ...}=> 
	    if CB.sameColor(t1,t2) then ([t1], [b, r2])
	    else ([t1, t2], [b, r2, t2])
	| I.SHIFTV {r, t, ...}        => ([t], [r])
	| I.SHIFT {r, t, ...}         => ([t], [r])
	| I.BCOND {r1, r2, ...}       => ([],  [r1,r2])
	| I.BCONDI {r2, ...} 	    => ([],  [r2])
	| I.BB {r, ...} 	            => ([],  [r])
	| I.BV {x, b, ...}	    => ([],  [x,b])
	| I.BE {b, ...}	            => ([],  [b])
	| I.BLR{x, t, ...}            => ([t], [x])
	| I.BL{defs, uses, ...}       => (C.getReg defs, C.getReg uses)
	| I.BLE{t, b, defs, uses, ...}=>
	      (r31 :: t :: C.getReg defs, b :: C.getReg uses)
	| I.LDIL{i, t}		    => ([t], [])
	| I.LDO{b, t, ...}	    => ([t], [b])
	| I.MTCTL{r, t}		    => ([],  [r])
	| I.FSTORE {b, ...}	    => ([],  [b])
	| I.FSTOREX {b, x, ...}  	    => ([],  [b,x])
	| I.FLOAD {b, ...}	    => ([],  [b])
	| I.FLOADX{b, x, ...} 	    => ([],  [b,x])
	| _   => ([],[])
    end
  in
      case instr
       of I.ANNOTATION{i, ...} => defUseR i
	| I.LIVE{regs, ...} => ([], C.getReg regs)
	| I.KILL{regs, ...} => (C.getReg regs, [])
	| I.INSTR(i) => hppaDU(i)
	| I.COPY{k, dst, src, tmp, ...} => let
	    val (d,u) = case k of CB.GP => (dst, src) | _ => ([], [])
          in
	      case tmp 
	      of SOME(I.Direct r) => (r::d, u)
	       | SOME(I.Displace{base, ...}) => (* (d, base::u) *) (d, u)
	       | _ => (d,u)
          end
  end

  fun defUseF instr = let
    fun hppaDU instr = 
      case instr
	of I.FSTORE {r, ...}  	   => ([],  [r])
	 | I.FSTOREX{r, ...}	   => ([],  [r])
	 | I.FLOAD{t, ...}	   => ([t], [])
	 | I.FLOADX{t, ...}	   => ([t], [])
	 | I.FARITH {r1, r2, t, ...} => ([t], [r1,r2])
	 | I.FUNARY {f, t, ...}      => ([t], [f])
	 | I.FCNV {f, t, ...}        => ([t], [f])
	 | I.FBRANCH{f1, f2,...}	   => ([],  [f1, f2])
	 | I.BL{defs, uses, ...}     => (C.getFreg defs, C.getFreg uses)
	 | I.BLE{defs, uses, ...}    => (C.getFreg defs, C.getFreg uses)
	 | _ => ([],[])
  in 
      case instr
       of I.ANNOTATION{i, ...} => defUseF i
	| I.INSTR(i) => hppaDU(i)
	| I.LIVE{regs, ...} => ([], C.getFreg regs)
	| I.KILL{regs, ...} => (C.getFreg regs, [])
        | I.COPY{k, dst, src, tmp, ...} => let
	    val (d, u) = case k of CB.FP => (dst, src) | _ => ([],[])
          in
	     case tmp
	      of SOME(I.FDirect f) => (f::d, u)
	       | _ => (d, u)
          end
  end

  fun defUse CB.GP = defUseR
    | defUse CB.FP = defUseF
    | defUse _ = error "defUse"

  (*========================================================================
   *  Annotations 
   *========================================================================*)
  fun getAnnotations(I.ANNOTATION{i,a}) = 
       let val (i,an) = getAnnotations i in (i,a::an) end
    | getAnnotations i = (i,[])
  fun annotate(i,a) = I.ANNOTATION{i=i,a=a}

  (*========================================================================
   *  Replicate an instruction
   *========================================================================*)
  fun replicate(I.ANNOTATION{i,a}) = I.ANNOTATION{i=replicate i,a=a}
    | replicate(I.COPY{k, sz, tmp=SOME _, dst, src}) =  let
	  val tmp = case k of CB.GP => C.newReg()
			    | CB.FP => C.newFreg()
			    | _ => error "replicate: neither GP nor FP"
      in
        I.COPY{k=k, sz=sz, tmp=SOME(I.Direct(tmp)), dst=dst, src=src}
      end
    | replicate i = i
end




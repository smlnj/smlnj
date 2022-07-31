(* 
 * This is a revamping of the MIPS32 instruction selection module
 * using the new MLTREE and instruction representation.   I've dropped
 * the suffix 32 since we now support 64 bit datatypes.
 *
 * o How to simulate 32-bit in 64-mode
 *   All 32-bit values are sign extended to 64-bits. 
 *   The working is similar to the Alpha and the Sparc architecture.
 * o I'm using the native multiplication instructions for even simple
 *   multiply with a constant.  Too lazy to add the multiply module for now.
 * 
 * -- Allen
 *)

functor MIPS
   (structure MIPSInstr : MIPSINSTR
    structure PseudoInstrs : MIPS_PSEUDO_INSTR
    structure ExtensionComp : MLTREE_EXTENSION_COMP
       where I = MIPSInstr
       sharing PseudoInstrs.I = MIPSInstr

    (*
     * MIPS architecture version
     *)
    datatype mipsVersion = I | II | III | IV
    val mipsVersion : mipsVersion ref
   ) : MLTREECOMP =
struct

  structure I   = MIPSInstr
  structure T   = I.T
  structure S   = T.Stream
  structure R   = T.Region
  structure C   = MIPSInstr.C
  structure LE  = I.LabelExp
  structure P   = PseudoInstrs
  structure A   = MLRiscAnnotations

  fun error msg = MLRiscErrorMsg.error("MIPS",msg) 

  type instrStream = (I.instruction,C.cellset) T.stream
  type mltreeStream = (T.stm,T.mlrisc list) T.stream

  (*
   * This module is used to simulate operations of non-standard widths.
   *)
  structure Gen = MLTreeGen(structure T = T
                            val intTy = 64
                            val naturalWidths = [32,64]
                            datatype rep = SE | ZE | NEITHER
                            val rep = SE (* sign extended? XXX *)
                           )

  val zeroR = C.r0
  val zeroOpnd = I.Reg zeroR
  val zeroImm = I.Imm 0
  val zero = IntInf.fromInt 0
  fun toInt i = T.I.toInt(32, i)

  fun selectInstructions
        (instrStream as
         S.STREAM{emit,beginCluster,endCluster,getAnnotations,
                  defineLabel,entryLabel,pseudoOp,annotation,
                  exitBlock,comment,...}) =
  let
      (* jmp+label were a trap is generated -- one per cluster *)
      val trapLabel = ref (NONE : (I.instruction * Label.label) option)

      (* Add an overflow trap *)
      fun trap() = ()
 
      val newReg = C.newReg
      val newFreg = C.newFreg

      fun mark'(i,[]) = i
        | mark'(i,a::an) = mark'(I.ANNOTATION{i=i,a=a},an)
      fun mark(i,an) = emit(mark'(i,an))

      fun move(s,d,an) =
          if C.sameCell(s,d) orelse C.sameCell(d,zeroR) then () else
          mark(I.COPY{dst=[d],src=[s],impl=ref NONE,tmp=NONE},an)

      fun fmove(s,d,an) =
          if C.sameCell(s,d) then () else
          mark(I.FCOPY{dst=[d],src=[s],impl=ref NONE,tmp=NONE},an)

      (* emit a copy *)
      fun copy(dst,src,an) = 
          mark(I.COPY{dst=dst,src=src,impl=ref NONE,
                      tmp=case dst of
                           [_] => NONE | _ => SOME(I.Direct(newReg()))},an)

      (* emit a floating point copy *)
      fun fcopy(dst,src,an) = 
          mark(I.FCOPY{dst=dst,src=src,impl=ref NONE,
                      tmp=case dst of
                           [_] => NONE | _ => SOME(I.FDirect(newFreg()))},an)

      (* emit load label expression *)
      fun loadLabexp(le,d,an) =
          mark(I.ARITH{oper=I.ADDU,rt=d,rs=zeroR,i=I.Lab le},an)

      (* emit load immediate *)
      fun loadImmed(n,d,an) = error "loadImmed"

      (* generate an expression and return the register that holds the result *)
      fun expr(T.REG(_,r)) = r
        | expr(e as T.LI i) = if i = zero then zeroR else expr' e
        | expr e = expr' e
      and expr' e = let val r = newReg() in doExpr(e,r,[]); r end

      (* convert an operand into a register *)
      and reduceOpn(I.Reg r) = r
        | reduceOpn(I.Imm 0) = zeroR
        | reduceOpn opn =
          let val d = newReg()
          in  emit(I.ARITH{oper=I.ADDU,rt=d,rs=zeroR,i=opn}); d end (* XXX *)

      (* generate an operand *)
      and opn(T.REG(_,r)) = I.Reg r
        | opn(T.LI i) = if i = zero then zeroOpnd else error "opn"
        | opn(T.LABEXP le) = I.Lab le
        | opn e = I.Reg(expr e)

      (* compute base+displacement from an expression *)
      and addr exp =
          let fun toLexp(I.Imm i) = T.LI(IntInf.fromInt i)
                | toLexp(I.Lab le) = le
                | toLexp _ = error "addr.toLexp"

              fun add(t,n,I.Imm m)  =
                   I.Imm(toInt(T.I.ADD(t,n,IntInf.fromInt m)))
                | add(t,n,I.Lab le) = I.Lab(T.ADD(t,T.LI n,le))
                | add(t,n,_) = error "addr.add"

              fun addLe(ty,le,I.Imm 0) = I.Lab le
                | addLe(ty,le,disp) = I.Lab(T.ADD(ty,le,toLexp disp))

              fun sub(t,n,I.Imm m) =
                  I.Imm(toInt(T.I.SUB(t,IntInf.fromInt m,n)))
                | sub(t,n,I.Lab le) = I.Lab(T.SUB(t,le,T.LI n))
                | sub(t,n,_) = error "addr.sub"

              fun subLe(ty,le,I.Imm 0) = I.Lab le
                | subLe(ty,le,disp) = I.Lab(T.SUB(ty,le,toLexp disp))

              (* Should really take into account of the address width XXX *)
              fun fold(T.ADD(t,e,T.LI n),disp) = fold(e,add(t,n,disp))
                | fold(T.ADD(t,e,x as T.CONST _),disp) = fold(e,addLe(t,x,disp))                | fold(T.ADD(t,e,x as T.LABEL _),disp) = fold(e,addLe(t,x,disp))                | fold(T.ADD(t,e,T.LABEXP l),disp) = fold(e,addLe(t,l,disp))
                | fold(T.ADD(t,T.LI n,e),disp) = fold(e, add(t,n,disp))
                | fold(T.ADD(t,x as T.CONST _,e),disp) = fold(e,addLe(t,x,disp))                | fold(T.ADD(t,x as T.LABEL _,e),disp) = fold(e,addLe(t,x,disp))                | fold(T.ADD(t,T.LABEXP l,e),disp) = fold(e,addLe(t,l,disp))
                | fold(T.SUB(t,e,T.LI n),disp) = fold(e,sub(t,n,disp))
                | fold(T.SUB(t,e,x as T.CONST _),disp) = fold(e,subLe(t,x,disp))                | fold(T.SUB(t,e,x as T.LABEL _),disp) = fold(e,subLe(t,x,disp))                | fold(T.SUB(t,e,T.LABEXP l),disp) = fold(e,subLe(t,l,disp))
                | fold(e,disp) = (expr e,disp)

          in  fold(exp, zeroImm)
          end

      (* compute addressing mode for floating point.
       * In MIPS IV mode we also support register+register mode.
       *)
      and faddr exp =
          case !mipsVersion of
             IV =>
              (case exp of 
                 T.ADD(_,T.REG(_,b),T.REG(_,i)) => (b, I.Reg i)
               | _ => addr exp
              )
          | _ => addr exp

      (* generate an arithmetic operator *)
      and arith(oper,a,b,d,an) =
          mark(I.ARITH{oper=oper,rt=d,rs=expr a,i=I.Reg(expr b)},an)

      (* generate a commutative arithmetic operator 
       * that can take an immediate operand 
       *)
      and commarithi(oper,a,b,d,an) =
          let val (a, b) = 
                case b of
                  (T.LI _ | T.CONST _ | T.LABEXP _ | T.LABEL _) => (b, a)
                | _ => (a, b)
          in  mark(I.ARITH{oper=oper,rt=d,rs=expr a,i=opn b},an)
          end

      (* generate an unary arithmetic operator *)
      and unary(oper,a,d,an) =
          mark(I.UNARY{oper=oper,rt=d,rs=expr a},an)

      (* generate a load *)
      and load(ld,ea,rt,mem,an) =
          let val (base,offset) = addr ea
          in  mark(I.LOAD{l=ld,rt=rt,b=base,d=offset,mem=mem},an) end

      (* generate a store *)
      and store(st,ea,data,mem,an) =
          let val (base,offset) = addr ea
          in  mark(I.STORE{s=st,rs=expr data,b=base,d=offset,mem=mem},an) end

      (* generate multiply.
       * Note: low order result is in the LO register
       *)
      and multiply(oper,a,b,d,an) = 
          (mark(I.MULTIPLY{oper=oper,rs=expr a,rt=expr b},an);
           emit(I.MFLO d)
          )

      (* generate divide
       *  Note: quotient in LO; remainder is in HI
       *)
      and divide(oper,a,b,d,an) = 
          (mark(I.DIVIDE{oper=oper,rs=expr a,rt=expr b},an);
           emit(I.MFLO d)
          )

      and rem(oper,a,b,d,an) = 
          (mark(I.DIVIDE{oper=oper,rs=expr a,rt=expr b},an);
           emit(I.MFHI d)
          )

      (* generate an expression that targets register d *)
      and doExpr(exp,d,an) =
          case exp of
            T.REG(_,r) => move(r,d,an)
          | T.LI n     => loadImmed(n,d,an)
          | T.LABEL _  => loadLabexp(exp,d,an)
          | T.CONST _  => loadLabexp(exp,d,an)
          | T.LABEXP le => loadLabexp(le,d,an)

            (* 32 bit support *)
          | T.NEG(32, a) => unary(I.NEGU,a,d,an)
          | T.ADD(32, a, b) => commarithi(I.ADDU,a,b,d,an)
          | T.SUB(32, a, b) => arith(I.SUBU,a,b,d,an)
          | T.MULS(32, a, b) => multiply(I.MULT,a,b,d,an)
          | T.MULU(32, a, b) => multiply(I.MULTU,a,b,d,an)
          | T.DIVS(32, a, b) => divide(I.DIV,a,b,d,an)
          | T.DIVU(32, a, b) => divide(I.DIVU,a,b,d,an)
          | T.QUOTS(32, a, b) => error "quots"
          | T.REMS(32, a, b) => rem(I.DIV,a,b,d,an)
          | T.REMU(32, a, b) => rem(I.DIVU,a,b,d,an)

          | T.NEGT(32, a) => unary(I.NEG,a,d,an)
          | T.ADDT(32, a, b) => commarithi(I.ADD,a,b,d,an)
          | T.SUBT(32, a, b) => arith(I.SUB,a,b,d,an)
          | T.MULT(32, a, b) => error "mult"
          | T.DIVT(32, a, b) => error "divt"
          | T.QUOTT(32, a, b) => error "quott"
          | T.REMT(32, a, b) => error "remt"

          | T.SLL(32, a, b) => arith(I.SLL,a,b,d,an)
          | T.SRL(32, a, b) => arith(I.SRL,a,b,d,an)
          | T.SRA(32, a, b) => arith(I.SRA,a,b,d,an)

             (* 64 bit support *)
          | T.NEG(64, a) => unary(I.DNEGU,a,d,an)
          | T.ADD(64, a, b) => arith(I.DADDU,a,b,d,an)
          | T.SUB(64, a, b) => arith(I.DSUBU,a,b,d,an)
          | T.MULS(64, a, b) => multiply(I.DMULT,a,b,d,an)
          | T.MULU(64, a, b) => multiply(I.DMULTU,a,b,d,an)
          | T.DIVS(64, a, b) => divide(I.DDIV,a,b,d,an)
          | T.DIVU(64, a, b) => divide(I.DDIVU,a,b,d,an)
          | T.QUOTS(64, a, b) => error "quots"
          | T.REMS(64, a, b) => rem(I.DDIV,a,b,d,an)
          | T.REMU(64, a, b) => rem(I.DDIVU,a,b,d,an)

          | T.NEGT(64, a) => unary(I.DNEG,a,d,an)
          | T.ADDT(64, a, b) => commarithi(I.DADD,a,b,d,an)
          | T.SUBT(64, a, b) => arith(I.DSUB,a,b,d,an)
          | T.MULT(64, a, b) => error "mult"
          | T.DIVT(64, a, b) => error "divt"
          | T.QUOTT(64, a, b) => error "quott"
          | T.REMT(64, a, b) => error "remt"

          | T.SLL(64, a, b) => arith(I.DSLL,a,b,d,an)
          | T.SRL(64, a, b) => arith(I.DSRL,a,b,d,an)
          | T.SRA(64, a, b) => arith(I.DSRA,a,b,d,an)

            (* Bit ops *)
          | T.ANDB(_, a, b) => commarithi(I.AND,a,b,d,an)
          | T.ORB(_, a, b) => commarithi(I.OR,a,b,d,an)
          | T.XORB(_, a, b) => commarithi(I.XOR,a,b,d,an)

            (* Conditional move *)
          | T.COND(_, cc, yes, no) => error "cond"

            (* Loads *)
          | T.SX(_,_,T.LOAD(8,ea,mem)) => load(I.LB, ea, d, mem, an)
          | T.SX(_,_,T.LOAD(16,ea,mem)) => load(I.LH, ea, d, mem, an)
          | T.SX(_,_,T.LOAD(32,ea,mem)) => load(I.LW, ea, d, mem, an)
          | T.ZX(_,_,T.LOAD(8,ea,mem)) => load(I.LBU, ea, d, mem, an)
          | T.ZX(_,_,T.LOAD(16,ea,mem)) => load(I.LHU, ea, d, mem, an)
          | T.LOAD(8, ea, mem)  => load(I.LBU, ea, d, mem, an)
          | T.LOAD(16, ea, mem) => load(I.LHU, ea, d, mem, an)
          | T.LOAD(32, ea, mem) => load(I.LW, ea, d, mem, an)
          | T.LOAD(64, ea, mem) => load(I.LD, ea, d, mem, an)

            (* Annotations *)
          | T.MARK(e, A.MARKREG f) => (f d; doExpr(e, d, an))
          | T.MARK(e, a) => doExpr(e, d, a::an)

            (* Control dependence *)
          | T.PRED(e,c) => doExpr(e, d, A.CTRLUSE c::an)

            (* Extension *)
          | T.REXT e => ExtensionComp.compileRext (reducer()) {e=e, rd=d, an=an}

           (* Defaults *) 
          | e => doExpr(Gen.compileRexp e,d,an)

      (* generate a floating point expression
       * return the register that holds the result 
       *)
      and fexpr(T.FREG(_,r)) = r
        | fexpr e = let val d = newFreg() in doFexpr(e,d,[]); d end

      and farith(oper,a,b,d,an) =
          mark(I.FARITH{oper=oper,fs1=fexpr a,fs2=fexpr b,ft=d},an)
      and funary(oper,a,d,an) =
          mark(I.FUNARY{oper=oper,fs=fexpr a,ft=d},an)
      and farith3(oper,a,b,c,d,an) =
          mark(I.FARITH3{oper=oper,fs1=fexpr a,fs2=fexpr b,fs3=fexpr c,ft=d},an)
      and fload(ld,ea,fd,mem,an) =
          let val (base,offset) = faddr ea
          in  mark(I.FLOAD{l=ld,ft=fd,b=base,d=offset,mem=mem},an) end
      and fstore(st,ea,fs,mem,an) =
          let val (base,offset) = faddr ea
          in  mark(I.FSTORE{s=st,fs=fexpr fs,b=base,d=offset,mem=mem},an) end

      (* generate a floating point expression that targets register d *)
      and doFexpr(e,d,an) =
          case e of
            T.FREG(_,f)    => fmove(f,d,an)

            (* single precision support *)
          | T.FADD(32,a,b) => farith(I.ADD_S,a,b,d,an)
          | T.FSUB(32,a,b) => farith(I.SUB_S,a,b,d,an)
          | T.FMUL(32,a,b) => farith(I.MUL_S,a,b,d,an)
          | T.FDIV(32,a,b) => farith(I.DIV_S,a,b,d,an)
          | T.FABS(32,a)   => funary(I.ABS_S,a,d,an)
          | T.FNEG(32,a)   => funary(I.NEG_S,a,d,an)
          | T.FSQRT(32,a)  => funary(I.SQRT_S,a,d,an)

            (* double precision support *)
          | T.FADD(64,a,b) => farith(I.ADD_D,a,b,d,an)
          | T.FSUB(64,a,b) => farith(I.SUB_D,a,b,d,an)
          | T.FMUL(64,a,b) => farith(I.MUL_D,a,b,d,an)
          | T.FDIV(64,a,b) => farith(I.DIV_D,a,b,d,an)
          | T.FABS(64,a)   => funary(I.ABS_D,a,d,an)
          | T.FNEG(64,a)   => funary(I.NEG_D,a,d,an)
          | T.FSQRT(64,a)  => funary(I.SQRT_D,a,d,an)

            (* copy sign *)
          | T.FCOPYSIGN _ => error "fcopysign"

            (* loads *)
          | T.FLOAD(32,ea,mem) => fload(I.LWC1,ea,d,mem,an)
          | T.FLOAD(64,ea,mem) => fload(I.LDC1,ea,d,mem,an)
         
            (* floating/floating conversion 
             * Note: it is not necessary to convert single precision
             * to double on the alpha.
             *)
          | T.CVTF2F(to,from,e) => 
            if from = to then doFexpr(e, d, an)
            else
            (case (to,from) of
               (32,64) => funary(I.CVT_SD,e,d,an)
             | (64,32) => funary(I.CVT_DS,e,d,an) (* use normal rounding *)
             | _       => error "CVTF2F"
            )

            (* integer -> floating point conversion *)
          | T.CVTI2F(fty,ty,e) => error "cvti2f"

          | T.FMARK(e,A.MARKREG f) => (f d; doFexpr(e,d,an))
          | T.FMARK(e,a) => doFexpr(e,d,a::an)
          | T.FPRED(e,c) => doFexpr(e, d, A.CTRLUSE c::an)
          | T.FEXT e => ExtensionComp.compileFext (reducer()) {e=e, fd=d, an=an}
          | _ => error "doFexpr"

         (* generate an unconditional branch *)
      and goto(lab,an) = mark(I.J{lab=lab,nop=true},an)

         (* generate an unconditional jump *)
      and jmp(e,labels,an) = mark(I.JR{rs=expr e,labels=labels,nop=true},an)

         (* generate a call instruction *)
      and call(ea,flow,defs,uses,cutsTo,mem,an) = 
       let val defs=cellset defs
           val uses=cellset uses
           val instr = 
               case ea of
                 (T.LABEL lab) => 
                      I.JAL{lab=lab,defs=defs,uses=uses,cutsTo=cutsTo,
                            mem=mem,nop=true}
               | _ => I.JALR{rt=C.linkR, rs=expr ea, 
                            defs=defs,uses=uses,cutsTo=cutsTo,mem=mem,nop=true}
       in  mark(instr,an)
       end

         (* generate a return instruction *)
       and ret(an) = mark(I.RET{nop=true},an)

         (* generate an branch instruction *)
      and branch(e,label,an) = error "branch"

        (* generate a comparison *)
      and cmp(ty,cond,e1,e2,d,an) = error "cmp"

      and doCCexpr(T.CC(_,r),d,an) = move(r,d,an)
        | doCCexpr(T.FCC(_,r),d,an) = fmove(r,d,an)
        | doCCexpr(T.CMP(ty,cond,e1,e2),d,an)  = cmp(ty,cond,e1,e2,d,an) 
        | doCCexpr(T.FCMP(fty,cond,e1,e2),d,an) = error "doCCexpr.fcmp"
        | doCCexpr(T.CCMARK(e,A.MARKREG f),d,an) = (f d; doCCexpr(e,d,an))
        | doCCexpr(T.CCMARK(e,a),d,an) = doCCexpr(e,d,a::an)
        | doCCexpr(T.CCEXT e,d,an) = 
             ExtensionComp.compileCCext (reducer()) {e=e, ccd=d, an=an}
        | doCCexpr _ = error "doCCexpr"

      and ccExpr(T.CC(_,r)) = r
        | ccExpr(T.FCC(_,r)) = r
        | ccExpr e = let val d = newReg()
                     in  doCCexpr(e,d,[]); d end

      (* compile a statement *)
      and stmt(s,an) =
          case s of
            T.MV(ty,r,e) => doExpr(e,r,an)
          | T.FMV(ty,r,e) => doFexpr(e,r,an)
          | T.CCMV(r,e) => doCCexpr(e,r,an)
          | T.COPY(ty,dst,src) => copy(dst,src,an)
          | T.FCOPY(ty,dst,src) => fcopy(dst,src,an)
          | T.JMP(T.LABEL lab,_) => goto(lab,an)
          | T.JMP(e,labs) => jmp(e,labs,an)
          | T.BCC(cc,lab) => branch(cc,lab,an)
          | T.CALL{funct,targets,defs,uses,region} => 
              call(funct,targets,defs,uses,[],region,an)
          | T.FLOW_TO(T.CALL{funct,targets,defs,uses,region},cuts)=>
              call(funct,targets,defs,uses,cuts,region,an)
          | T.RET _ => ret(an)
          | T.STORE(8,ea,data,mem) => store(I.SB,ea,data,mem,an)
          | T.STORE(16,ea,data,mem) => store(I.SH,ea,data,mem,an)
          | T.STORE(32,ea,data,mem) => store(I.SW,ea,data,mem,an)
          | T.STORE(64,ea,data,mem) => store(I.SD,ea,data,mem,an)
          | T.FSTORE(32,ea,data,mem) => fstore(I.SWC1,ea,data,mem,an)
          | T.FSTORE(64,ea,data,mem) => fstore(I.SDC1,ea,data,mem,an)
          | T.DEFINE l => defineLabel l
          | T.LIVE S => mark'(I.LIVE{regs=cellset S,spilled=C.empty},an)
          | T.KILL S => mark'(I.KILL{regs=cellset S,spilled=C.empty},an)
          | T.ANNOTATION(s,a) => stmt(s,a::an)
          | T.EXT s => ExtensionComp.compileSext (reducer()) {stm=s,an=an}
          | s => doStmts (Gen.compileStm s)

      and reducer() =
          T.REDUCER{reduceRexp    = expr,
                    reduceFexp    = fexpr,
                    reduceCCexp   = ccExpr,
                    reduceStm     = stmt,
                    operand       = opn,
                    reduceOperand = reduceOpn,
                    addressOf     = addr,
                    emit          = mark,
                    instrStream   = instrStream,
                    mltreeStream  = self()
                   } 

      and doStmt s = stmt(s,[])
      and doStmts ss = app doStmt ss

       (* convert mlrisc to cellset:
        * condition code registers are mapped onto general registers
        *)
      and cellset mlrisc =
          let fun g([],acc) = acc
                | g(T.GPR(T.REG(_,r))::regs,acc)  = g(regs,C.addReg(r,acc))
                | g(T.FPR(T.FREG(_,f))::regs,acc) = g(regs,C.addFreg(f,acc))
                | g(T.CCR(T.CC(_,cc))::regs,acc)  = g(regs,C.addReg(cc,acc))
                | g(T.CCR(T.FCC(_,cc))::regs,acc) = g(regs,C.addReg(cc,acc))
                | g(_::regs, acc) = g(regs, acc)
          in  g(mlrisc, C.empty) end

      and beginCluster' n =
          (trapLabel := NONE;
           beginCluster n
          ) 

      and endCluster' a = 
          (case !trapLabel of
             NONE => ()
           | SOME(_, lab) => (defineLabel lab) (* XXX *)
           (*esac*);
           endCluster a
          )

      and self() = 
          S.STREAM
         { beginCluster   = beginCluster',
           endCluster     = endCluster',
           emit           = doStmt,
           pseudoOp       = pseudoOp,
           defineLabel    = defineLabel,
           entryLabel     = entryLabel,
           comment        = comment,
           annotation     = annotation,
           getAnnotations = getAnnotations,
           exitBlock      = fn regs => exitBlock(cellset regs)
         } 
   in  self()
   end
 
end


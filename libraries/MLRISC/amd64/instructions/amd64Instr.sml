(*
 * WARNING: This file was automatically generated by MDLGen (v3.1)
 * from the machine description file "amd64/amd64.mdl".
 * DO NOT EDIT this file directly
 *)


signature AMD64INSTR =
sig
   structure C : AMD64CELLS
   structure CB : CELLS_BASIS = CellsBasis
   structure T : MLTREE
   structure Constant: CONSTANT
   structure Region : REGION
      sharing Constant = T.Constant
      sharing Region = T.Region
   datatype operand =
     Immed of Int32.int
   | Immed64 of Int64.int
   | ImmedLabel of T.labexp
   | Relative of int
   | LabelEA of T.labexp
   | Direct of int * (CellsBasis.cell)
   | FDirect of CellsBasis.cell
   | Displace of {base:CellsBasis.cell, disp:operand, mem:Region.region}
   | Indexed of {base:(CellsBasis.cell) option, index:CellsBasis.cell, scale:int,
        disp:operand, mem:Region.region}
   type addressing_mode = operand
   type ea = operand
   type byte = Word8.word
   datatype cond =
     EQ
   | NE
   | LT
   | LE
   | GT
   | GE
   | B
   | BE
   | A
   | AE
   | C
   | NC
   | P
   | NP
   | O
   | NO
   datatype binaryOp =
     ADDQ
   | SUBQ
   | ANDQ
   | ORQ
   | XORQ
   | SHLQ
   | SARQ
   | SHRQ
   | IMULQ
   | ADCQ
   | SBBQ
   | ADDL
   | SUBL
   | ANDL
   | ORL
   | XORL
   | SHLL
   | SARL
   | SHRL
   | IMULL
   | ADCL
   | SBBL
   | ADDW
   | SUBW
   | ANDW
   | ORW
   | XORW
   | SHLW
   | SARW
   | SHRW
   | IMULW
   | ADDB
   | SUBB
   | ANDB
   | ORB
   | XORB
   | SHLB
   | SARB
   | SHRB
   | IMULB
   | BTSW
   | BTCW
   | BTRW
   | BTSL
   | BTCL
   | BTRL
   | ROLW
   | RORW
   | ROLL
   | RORL
   | XCHGB
   | XCHGW
   | XCHGL
   | LOCK_ADCW
   | LOCK_ADCL
   | LOCK_ADDW
   | LOCK_ADDL
   | LOCK_ANDW
   | LOCK_ANDL
   | LOCK_BTSW
   | LOCK_BTSL
   | LOCK_BTRW
   | LOCK_BTRL
   | LOCK_BTCW
   | LOCK_BTCL
   | LOCK_ORW
   | LOCK_ORL
   | LOCK_SBBW
   | LOCK_SBBL
   | LOCK_SUBW
   | LOCK_SUBL
   | LOCK_XORW
   | LOCK_XORL
   | LOCK_XADDB
   | LOCK_XADDW
   | LOCK_XADDL
   datatype multDivOp =
     IMULL1
   | MULL1
   | IDIVL1
   | DIVL1
   | IMULQ1
   | MULQ1
   | IDIVQ1
   | DIVQ1
   datatype unaryOp =
     DECQ
   | INCQ
   | NEGQ
   | NOTQ
   | DECL
   | INCL
   | NEGL
   | NOTL
   | DECW
   | INCW
   | NEGW
   | NOTW
   | DECB
   | INCB
   | NEGB
   | NOTB
   | LOCK_DECQ
   | LOCK_INCQ
   | LOCK_NEGQ
   | LOCK_NOTQ
   datatype shiftOp =
     SHLDL
   | SHRDL
   datatype bitOp =
     BTW
   | BTL
   | BTQ
   | LOCK_BTW
   | LOCK_BTL
   datatype move =
     MOVQ
   | MOVL
   | MOVB
   | MOVW
   | MOVABSQ
   | MOVSWQ
   | MOVZWQ
   | MOVSWL
   | MOVZWL
   | MOVSBQ
   | MOVZBQ
   | MOVSBL
   | MOVZBL
   | MOVSLQ
   | CVTSD2SI
   | CVTSS2SI
   | CVTSD2SIQ
   | CVTSS2SIQ
   datatype fbin_op =
     ADDSS
   | ADDSD
   | SUBSS
   | SUBSD
   | MULSS
   | MULSD
   | DIVSS
   | DIVSD
   | XORPS
   | XORPD
   | ANDPS
   | ANDPD
   | ORPS
   | ORPD
   datatype fcom_op =
     COMISS
   | COMISD
   | UCOMISS
   | UCOMISD
   datatype fmove_op =
     MOVSS
   | MOVSD
   | CVTSS2SD
   | CVTSD2SS
   | CVTSI2SS
   | CVTSI2SSQ
   | CVTSI2SD
   | CVTSI2SDQ
   datatype fsize =
     FP32
   | FP64
   datatype isize =
     I8
   | I16
   | I32
   | I64
   datatype instr =
     NOP
   | JMP of operand * Label.label list
   | JCC of {cond:cond, opnd:operand}
   | CALL of {opnd:operand, defs:C.cellset, uses:C.cellset, return:C.cellset,
        cutsTo:Label.label list, mem:Region.region, pops:Int32.int}
   | ENTER of {src1:operand, src2:operand}
   | LEAVE
   | RET of operand option
   | MOVE of {mvOp:move, src:operand, dst:operand}
   | LEAL of {r32:CellsBasis.cell, addr:operand}
   | LEAQ of {r64:CellsBasis.cell, addr:operand}
   | CMPQ of {lsrc:operand, rsrc:operand}
   | CMPL of {lsrc:operand, rsrc:operand}
   | CMPW of {lsrc:operand, rsrc:operand}
   | CMPB of {lsrc:operand, rsrc:operand}
   | TESTQ of {lsrc:operand, rsrc:operand}
   | TESTL of {lsrc:operand, rsrc:operand}
   | TESTW of {lsrc:operand, rsrc:operand}
   | TESTB of {lsrc:operand, rsrc:operand}
   | BITOP of {bitOp:bitOp, lsrc:operand, rsrc:operand}
   | BINARY of {binOp:binaryOp, src:operand, dst:operand}
   | SHIFT of {shiftOp:shiftOp, src:operand, dst:operand, count:operand}
   | MULTDIV of {multDivOp:multDivOp, src:operand}
   | MUL3 of {dst:CellsBasis.cell, src2:Int32.int, src1:operand}
   | MULQ3 of {dst:CellsBasis.cell, src2:Int32.int, src1:operand}
   | UNARY of {unOp:unaryOp, opnd:operand}
   | SET of {cond:cond, opnd:operand}
   | CMOV of {cond:cond, src:operand, dst:CellsBasis.cell}
   | PUSH of operand
   | PUSHFQ
   | POPFQ
   | POP of operand
   | CDQ
   | CDO
   | INT of byte
   | FMOVE of {fmvOp:fmove_op, dst:operand, src:operand}
   | FBINOP of {binOp:fbin_op, dst:CellsBasis.cell, src:operand}
   | FCOM of {comOp:fcom_op, dst:CellsBasis.cell, src:operand}
   | FSQRTS of {dst:operand, src:operand}
   | FSQRTD of {dst:operand, src:operand}
   | SAHF
   | LFENCE
   | MFENCE
   | SFENCE
   | PAUSE
   | XCHG of {lock:bool, sz:isize, src:operand, dst:operand}
   | CMPXCHG of {lock:bool, sz:isize, src:operand, dst:operand}
   | XADD of {lock:bool, sz:isize, src:operand, dst:operand}
   | RDTSC
   | RDTSCP
   | LAHF
   | SOURCE of {}
   | SINK of {}
   | PHI of {}
   and instruction =
     LIVE of {regs: C.cellset, spilled: C.cellset}
   | KILL of {regs: C.cellset, spilled: C.cellset}
   | COPY of {k: CellsBasis.cellkind,
              sz: int,          (* in bits *)
              dst: CellsBasis.cell list,
              src: CellsBasis.cell list,
              tmp: ea option (* NONE if |dst| = {src| = 1 *)}
   | ANNOTATION of {i:instruction, a:Annotations.annotation}
   | INSTR of instr
   val nop : instruction
   val jmp : operand * Label.label list -> instruction
   val jcc : {cond:cond, opnd:operand} -> instruction
   val call : {opnd:operand, defs:C.cellset, uses:C.cellset, return:C.cellset,
      cutsTo:Label.label list, mem:Region.region, pops:Int32.int} -> instruction
   val enter : {src1:operand, src2:operand} -> instruction
   val leave : instruction
   val ret : operand option -> instruction
   val move : {mvOp:move, src:operand, dst:operand} -> instruction
   val leal : {r32:CellsBasis.cell, addr:operand} -> instruction
   val leaq : {r64:CellsBasis.cell, addr:operand} -> instruction
   val cmpq : {lsrc:operand, rsrc:operand} -> instruction
   val cmpl : {lsrc:operand, rsrc:operand} -> instruction
   val cmpw : {lsrc:operand, rsrc:operand} -> instruction
   val cmpb : {lsrc:operand, rsrc:operand} -> instruction
   val testq : {lsrc:operand, rsrc:operand} -> instruction
   val testl : {lsrc:operand, rsrc:operand} -> instruction
   val testw : {lsrc:operand, rsrc:operand} -> instruction
   val testb : {lsrc:operand, rsrc:operand} -> instruction
   val bitop : {bitOp:bitOp, lsrc:operand, rsrc:operand} -> instruction
   val binary : {binOp:binaryOp, src:operand, dst:operand} -> instruction
   val shift : {shiftOp:shiftOp, src:operand, dst:operand, count:operand} -> instruction
   val multdiv : {multDivOp:multDivOp, src:operand} -> instruction
   val mul3 : {dst:CellsBasis.cell, src2:Int32.int, src1:operand} -> instruction
   val mulq3 : {dst:CellsBasis.cell, src2:Int32.int, src1:operand} -> instruction
   val unary : {unOp:unaryOp, opnd:operand} -> instruction
   val set : {cond:cond, opnd:operand} -> instruction
   val cmov : {cond:cond, src:operand, dst:CellsBasis.cell} -> instruction
   val push : operand -> instruction
   val pushfq : instruction
   val popfq : instruction
   val pop : operand -> instruction
   val cdq : instruction
   val cdo : instruction
   val int : byte -> instruction
   val fmove : {fmvOp:fmove_op, dst:operand, src:operand} -> instruction
   val fbinop : {binOp:fbin_op, dst:CellsBasis.cell, src:operand} -> instruction
   val fcom : {comOp:fcom_op, dst:CellsBasis.cell, src:operand} -> instruction
   val fsqrts : {dst:operand, src:operand} -> instruction
   val fsqrtd : {dst:operand, src:operand} -> instruction
   val sahf : instruction
   val lfence : instruction
   val mfence : instruction
   val sfence : instruction
   val pause : instruction
   val xchg : {lock:bool, sz:isize, src:operand, dst:operand} -> instruction
   val cmpxchg : {lock:bool, sz:isize, src:operand, dst:operand} -> instruction
   val xadd : {lock:bool, sz:isize, src:operand, dst:operand} -> instruction
   val rdtsc : instruction
   val rdtscp : instruction
   val lahf : instruction
   val source : {} -> instruction
   val sink : {} -> instruction
   val phi : {} -> instruction
end

functor AMD64Instr(T: MLTREE
                  ) : AMD64INSTR =
struct
   structure C = AMD64Cells
   structure CB = CellsBasis
   structure T = T
   structure Region = T.Region
   structure Constant = T.Constant
   datatype operand =
     Immed of Int32.int
   | Immed64 of Int64.int
   | ImmedLabel of T.labexp
   | Relative of int
   | LabelEA of T.labexp
   | Direct of int * (CellsBasis.cell)
   | FDirect of CellsBasis.cell
   | Displace of {base:CellsBasis.cell, disp:operand, mem:Region.region}
   | Indexed of {base:(CellsBasis.cell) option, index:CellsBasis.cell, scale:int,
        disp:operand, mem:Region.region}
   type addressing_mode = operand
   type ea = operand
   type byte = Word8.word
   datatype cond =
     EQ
   | NE
   | LT
   | LE
   | GT
   | GE
   | B
   | BE
   | A
   | AE
   | C
   | NC
   | P
   | NP
   | O
   | NO
   datatype binaryOp =
     ADDQ
   | SUBQ
   | ANDQ
   | ORQ
   | XORQ
   | SHLQ
   | SARQ
   | SHRQ
   | IMULQ
   | ADCQ
   | SBBQ
   | ADDL
   | SUBL
   | ANDL
   | ORL
   | XORL
   | SHLL
   | SARL
   | SHRL
   | IMULL
   | ADCL
   | SBBL
   | ADDW
   | SUBW
   | ANDW
   | ORW
   | XORW
   | SHLW
   | SARW
   | SHRW
   | IMULW
   | ADDB
   | SUBB
   | ANDB
   | ORB
   | XORB
   | SHLB
   | SARB
   | SHRB
   | IMULB
   | BTSW
   | BTCW
   | BTRW
   | BTSL
   | BTCL
   | BTRL
   | ROLW
   | RORW
   | ROLL
   | RORL
   | XCHGB
   | XCHGW
   | XCHGL
   | LOCK_ADCW
   | LOCK_ADCL
   | LOCK_ADDW
   | LOCK_ADDL
   | LOCK_ANDW
   | LOCK_ANDL
   | LOCK_BTSW
   | LOCK_BTSL
   | LOCK_BTRW
   | LOCK_BTRL
   | LOCK_BTCW
   | LOCK_BTCL
   | LOCK_ORW
   | LOCK_ORL
   | LOCK_SBBW
   | LOCK_SBBL
   | LOCK_SUBW
   | LOCK_SUBL
   | LOCK_XORW
   | LOCK_XORL
   | LOCK_XADDB
   | LOCK_XADDW
   | LOCK_XADDL
   datatype multDivOp =
     IMULL1
   | MULL1
   | IDIVL1
   | DIVL1
   | IMULQ1
   | MULQ1
   | IDIVQ1
   | DIVQ1
   datatype unaryOp =
     DECQ
   | INCQ
   | NEGQ
   | NOTQ
   | DECL
   | INCL
   | NEGL
   | NOTL
   | DECW
   | INCW
   | NEGW
   | NOTW
   | DECB
   | INCB
   | NEGB
   | NOTB
   | LOCK_DECQ
   | LOCK_INCQ
   | LOCK_NEGQ
   | LOCK_NOTQ
   datatype shiftOp =
     SHLDL
   | SHRDL
   datatype bitOp =
     BTW
   | BTL
   | BTQ
   | LOCK_BTW
   | LOCK_BTL
   datatype move =
     MOVQ
   | MOVL
   | MOVB
   | MOVW
   | MOVABSQ
   | MOVSWQ
   | MOVZWQ
   | MOVSWL
   | MOVZWL
   | MOVSBQ
   | MOVZBQ
   | MOVSBL
   | MOVZBL
   | MOVSLQ
   | CVTSD2SI
   | CVTSS2SI
   | CVTSD2SIQ
   | CVTSS2SIQ
   datatype fbin_op =
     ADDSS
   | ADDSD
   | SUBSS
   | SUBSD
   | MULSS
   | MULSD
   | DIVSS
   | DIVSD
   | XORPS
   | XORPD
   | ANDPS
   | ANDPD
   | ORPS
   | ORPD
   datatype fcom_op =
     COMISS
   | COMISD
   | UCOMISS
   | UCOMISD
   datatype fmove_op =
     MOVSS
   | MOVSD
   | CVTSS2SD
   | CVTSD2SS
   | CVTSI2SS
   | CVTSI2SSQ
   | CVTSI2SD
   | CVTSI2SDQ
   datatype fsize =
     FP32
   | FP64
   datatype isize =
     I8
   | I16
   | I32
   | I64
   datatype instr =
     NOP
   | JMP of operand * Label.label list
   | JCC of {cond:cond, opnd:operand}
   | CALL of {opnd:operand, defs:C.cellset, uses:C.cellset, return:C.cellset,
        cutsTo:Label.label list, mem:Region.region, pops:Int32.int}
   | ENTER of {src1:operand, src2:operand}
   | LEAVE
   | RET of operand option
   | MOVE of {mvOp:move, src:operand, dst:operand}
   | LEAL of {r32:CellsBasis.cell, addr:operand}
   | LEAQ of {r64:CellsBasis.cell, addr:operand}
   | CMPQ of {lsrc:operand, rsrc:operand}
   | CMPL of {lsrc:operand, rsrc:operand}
   | CMPW of {lsrc:operand, rsrc:operand}
   | CMPB of {lsrc:operand, rsrc:operand}
   | TESTQ of {lsrc:operand, rsrc:operand}
   | TESTL of {lsrc:operand, rsrc:operand}
   | TESTW of {lsrc:operand, rsrc:operand}
   | TESTB of {lsrc:operand, rsrc:operand}
   | BITOP of {bitOp:bitOp, lsrc:operand, rsrc:operand}
   | BINARY of {binOp:binaryOp, src:operand, dst:operand}
   | SHIFT of {shiftOp:shiftOp, src:operand, dst:operand, count:operand}
   | MULTDIV of {multDivOp:multDivOp, src:operand}
   | MUL3 of {dst:CellsBasis.cell, src2:Int32.int, src1:operand}
   | MULQ3 of {dst:CellsBasis.cell, src2:Int32.int, src1:operand}
   | UNARY of {unOp:unaryOp, opnd:operand}
   | SET of {cond:cond, opnd:operand}
   | CMOV of {cond:cond, src:operand, dst:CellsBasis.cell}
   | PUSH of operand
   | PUSHFQ
   | POPFQ
   | POP of operand
   | CDQ
   | CDO
   | INT of byte
   | FMOVE of {fmvOp:fmove_op, dst:operand, src:operand}
   | FBINOP of {binOp:fbin_op, dst:CellsBasis.cell, src:operand}
   | FCOM of {comOp:fcom_op, dst:CellsBasis.cell, src:operand}
   | FSQRTS of {dst:operand, src:operand}
   | FSQRTD of {dst:operand, src:operand}
   | SAHF
   | LFENCE
   | MFENCE
   | SFENCE
   | PAUSE
   | XCHG of {lock:bool, sz:isize, src:operand, dst:operand}
   | CMPXCHG of {lock:bool, sz:isize, src:operand, dst:operand}
   | XADD of {lock:bool, sz:isize, src:operand, dst:operand}
   | RDTSC
   | RDTSCP
   | LAHF
   | SOURCE of {}
   | SINK of {}
   | PHI of {}
   and instruction =
     LIVE of {regs: C.cellset, spilled: C.cellset}
   | KILL of {regs: C.cellset, spilled: C.cellset}
   | COPY of {k: CellsBasis.cellkind,
              sz: int,          (* in bits *)
              dst: CellsBasis.cell list,
              src: CellsBasis.cell list,
              tmp: ea option (* NONE if |dst| = {src| = 1 *)}
   | ANNOTATION of {i:instruction, a:Annotations.annotation}
   | INSTR of instr
   val nop = INSTR NOP
   and jmp = INSTR o JMP
   and jcc = INSTR o JCC
   and call = INSTR o CALL
   and enter = INSTR o ENTER
   and leave = INSTR LEAVE
   and ret = INSTR o RET
   and move = INSTR o MOVE
   and leal = INSTR o LEAL
   and leaq = INSTR o LEAQ
   and cmpq = INSTR o CMPQ
   and cmpl = INSTR o CMPL
   and cmpw = INSTR o CMPW
   and cmpb = INSTR o CMPB
   and testq = INSTR o TESTQ
   and testl = INSTR o TESTL
   and testw = INSTR o TESTW
   and testb = INSTR o TESTB
   and bitop = INSTR o BITOP
   and binary = INSTR o BINARY
   and shift = INSTR o SHIFT
   and multdiv = INSTR o MULTDIV
   and mul3 = INSTR o MUL3
   and mulq3 = INSTR o MULQ3
   and unary = INSTR o UNARY
   and set = INSTR o SET
   and cmov = INSTR o CMOV
   and push = INSTR o PUSH
   and pushfq = INSTR PUSHFQ
   and popfq = INSTR POPFQ
   and pop = INSTR o POP
   and cdq = INSTR CDQ
   and cdo = INSTR CDO
   and int = INSTR o INT
   and fmove = INSTR o FMOVE
   and fbinop = INSTR o FBINOP
   and fcom = INSTR o FCOM
   and fsqrts = INSTR o FSQRTS
   and fsqrtd = INSTR o FSQRTD
   and sahf = INSTR SAHF
   and lfence = INSTR LFENCE
   and mfence = INSTR MFENCE
   and sfence = INSTR SFENCE
   and pause = INSTR PAUSE
   and xchg = INSTR o XCHG
   and cmpxchg = INSTR o CMPXCHG
   and xadd = INSTR o XADD
   and rdtsc = INSTR RDTSC
   and rdtscp = INSTR RDTSCP
   and lahf = INSTR LAHF
   and source = INSTR o SOURCE
   and sink = INSTR o SINK
   and phi = INSTR o PHI
end


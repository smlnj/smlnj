(* amd64-opcodes.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This functor converts from types of instructions, e.g. addition, and  their
 * bit widths to AMD64 instructions.
 *)

functor AMD64Opcodes (structure I : AMD64INSTR) =
  struct

    type opcodes = {INC : I.unaryOp, DEC : I.unaryOp, NOT : I.unaryOp,
                    NEG : I.unaryOp,
                    ADD : I.binaryOp, SUB : I.binaryOp, IMUL : I.binaryOp,
                    SHL : I.binaryOp, SHR : I.binaryOp, SAR : I.binaryOp,
                    OR : I.binaryOp, AND : I.binaryOp, XOR : I.binaryOp,
                    CMP : {lsrc:I.operand, rsrc:I.operand} -> I.instr,
                    MOV : I.move}
    val opcodes8 = {INC=I.INCB,DEC=I.DECB,
                    NOT=I.NOTB,NEG=I.NEGB,
                    ADD=I.ADDB,SUB=I.SUBB,IMUL=I.IMULB,
                    SHR=I.SHRB,SAR=I.SARB,SHL=I.SHLB,
                    OR=I.ORB,AND=I.ANDB,XOR=I.XORB,
	 	    CMP=I.CMPB,MOV=I.MOVB}
    val opcodes16 = {INC=I.INCW,DEC=I.DECW,ADD=I.ADDW,SUB=I.SUBW,
                     NOT=I.NOTW,NEG=I.NEGW,
                     SHL=I.SHLW,SHR=I.SHRW,SAR=I.SARW,
                     OR=I.ORW,AND=I.ANDW,XOR=I.XORW,
	  	     IMUL=I.IMULW,
		     CMP=I.CMPW,MOV=I.MOVW}
    val opcodes32 = {INC=I.INCL,DEC=I.DECL,ADD=I.ADDL,SUB=I.SUBL,
                     NOT=I.NOTL,NEG=I.NEGL,
                     SHL=I.SHLL,SHR=I.SHRL,SAR=I.SARL,
                     OR=I.ORL,AND=I.ANDL,XOR=I.XORL,
		     IMUL=I.IMULL,
		     CMP=I.CMPL, MOV=I.MOVL}
    val opcodes64 = {INC=I.INCQ,DEC=I.DECQ,ADD=I.ADDQ,SUB=I.SUBQ,
                     NOT=I.NOTQ,NEG=I.NEGQ,
                     SHL=I.SHLQ,SHR=I.SHRQ,SAR=I.SARQ,
                     OR=I.ORQ,AND=I.ANDQ,XOR=I.XORQ,
		     IMUL=I.IMULQ,
		     CMP=I.CMPQ, MOV=I.MOVQ}

    fun opcodes ty = (case ty
	of 8 => opcodes8
         | 16 => opcodes16
         | 32 => opcodes32
	 | 64 => opcodes64
	 | _  => opcodes64
	(* end case *))

    fun opC opc ty = opc (opcodes ty)

    val notOp = opC #NOT val incOp = opC #INC val decOp = opC #DEC
    val addOp = opC #ADD val subOp = opC #SUB val notOp = opC #NOT
    val negOp = opC #NEG val shlOp = opC #SHL val shrOp = opC #SHR
    val sarOp = opC #SAR val orOp  = opC #OR  val andOp = opC #AND
    val xorOp = opC #XOR val movOp = opC #MOV val cmpOp = opC #CMP
    val imulOp = opC #IMUL
    fun div1Op 32 = I.DIVL1
      | div1Op 64 = I.DIVQ1
    fun idiv1Op 32 = I.IDIVL1
      | idiv1Op 64 = I.IDIVQ1
    fun mul1Op 32 = I.MULL1
      | mul1Op 64 = I.MULQ1
    fun imul1Op 32 = I.IMULL1
      | imul1Op 64 = I.IMULQ1
    val divOp = div1Op
    val idivOp = idiv1Op

   (* fromTy, toTy *)
    fun loadZXOp (8, 32) = I.MOVZBL
      | loadZXOp (16, 32) = I.MOVZWL
      | loadZXOp (8, 64) = I.MOVZBQ
      | loadZXOp (16, 64) = I.MOVZWQ
      | loadZXOp (fTy, tTy) = raise Fail ("incompatible "^Int.toString fTy^" "^Int.toString tTy)

   (* fromTy, toTy *)
    fun loadSXOp (8, 64) = I.MOVSBQ
      | loadSXOp (16, 64) = I.MOVSWQ
      | loadSXOp (8, 32) = I.MOVSBL
      | loadSXOp (16, 32) = I.MOVSWL
      | loadSXOp (32, 64) = I.MOVSLQ
      | loadSXOp (fTy, tTy) = raise Fail ("incompatible "^Int.toString fTy^" "^Int.toString tTy)

    val fopcodes32 = {MOV=I.MOVSS, ADD=I.ADDSS, SUB=I.SUBSS,
		      MUL=I.MULSS, DIV=I.DIVSS, UCOM=I.UCOMISS}
    val fopcodes64 = {MOV=I.MOVSD, ADD=I.ADDSD, SUB=I.SUBSD,
                      MUL=I.MULSD, DIV=I.DIVSD, UCOM=I.UCOMISD}

    fun fopC opc ty = let
	val opcodes = (case ty
	    of 32 => fopcodes32
	     | 64 => fopcodes64
	     | _  => fopcodes64
	    (* end case *))
      in
	opc opcodes
      end (* fopC *)

    val fmovOp = fopC #MOV val faddOp = fopC #ADD val fsubOp = fopC #SUB
    val fmulOp = fopC #MUL val fdivOp = fopC #DIV val ucomOp = fopC #UCOM

  end (* AMD64Opcodes *)

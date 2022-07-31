(* X86Jumps.sml --- information to resolve jumps for runtime code generation.
 *
 *  COPYRIGHT (c) 1997 Bell Laboratories.
 *)

functor X86Jumps
  (structure Instr : X86INSTR
   structure Eval : MLTREE_EVAL where T = Instr.T
   structure Shuffle : X86SHUFFLE where I = Instr
   structure MCEmitter : MC_EMIT where I = Instr) : SDI_JUMPS = 
struct
  structure I = Instr
  structure C = I.C
  structure Const = I.Constant

  fun error msg = MLRiscErrorMsg.error("X86Jumps",msg)

  val esp = 4
  val ebp = 5
  val branchDelayedArch = false

  fun imm8 i = ~128 <= i andalso i < 128

  fun isSdi(I.ANNOTATION{i, ...}) = isSdi i
    | isSdi(I.LIVE _)		  = true
    | isSdi(I.KILL _)		  = true
    | isSdi(I.COPY _)		  = false
    | isSdi(I.INSTR instr) = let
	  fun operand(I.ImmedLabel _) = true
	    | operand(I.LabelEA _) = true
	    | operand(I.Displace{disp, ...}) = operand disp
	    | operand(I.Indexed{disp, ...}) = operand disp
	    | operand _ = false
	  fun cmptest{lsrc, rsrc} = operand lsrc orelse operand rsrc
      in 
	  case instr
	  of I.JMP(opnd, _) => operand opnd
	   | I.JCC{opnd, ...} => operand opnd
	   | I.BINARY{src, dst, ...} => operand src orelse operand dst
	   | I.MOVE{src, dst, ...} => operand src orelse operand dst
	   | I.LEA{addr, ...} => operand addr
	   | ( I.CMPL arg | I.CMPW arg | I.CMPB arg 
	     | I.TESTL arg | I.TESTW arg | I.TESTB arg) => cmptest arg
	   | I.MULTDIV{src, ...} => operand src
	   | I.MUL3{src1, ...} => operand src1
	   | I.UNARY{opnd, ...} => operand opnd
	   | I.SET{opnd, ...} => operand opnd
	   | I.CMOV{src, dst, ...} => operand src 
	   | (I.PUSHL opnd | I.PUSHW opnd | I.PUSHB opnd) => operand opnd
	   | I.POP opnd =>  operand opnd
	   | I.FSTPT opnd => operand opnd
	   | I.FSTPL opnd => operand opnd
	   | I.FSTPS opnd => operand opnd
	   | I.FSTL opnd => operand opnd
	   | I.FSTS opnd => operand opnd
	   | I.FLDT opnd => operand opnd
	   | I.FLDL opnd => operand opnd
	   | I.FLDS opnd => operand opnd
	   | I.FBINARY{src, dst, ...} => operand src orelse operand dst
	   | I.FIBINARY{src, ...} => operand src 
	   | I.FILD opnd => operand opnd
	   | I.FILDL opnd => operand opnd
	   | I.FILDLL opnd => operand opnd
	   | _ => false
      end

  fun minSize(I.ANNOTATION{i, ...}) = minSize i
    | minSize(I.LIVE _)  = 0
    | minSize(I.KILL _)  = 0
    | minSize(I.INSTR i) = 
      (case i 
	of I.JMP _ => 2
	 | I.JCC _ => 2
	 | I.LEA _ => 2
	 |  _ => 1)
    | minSize _ = error"minSize"


  fun maxSize _ = 12

  (* value of span-dependent operand *)
  fun operand(I.ImmedLabel le) = Eval.valueOf le
    | operand(I.LabelEA le) = Eval.valueOf le
    | operand _ = error "operand"
  
  val encode = MCEmitter.emitInstr

  fun sdiSize(I.ANNOTATION{i, ...}, labmap, loc) = sdiSize(i, labmap, loc)
    | sdiSize(I.LIVE _, _, _) = 0
    | sdiSize(I.KILL _, _, _) = 0

    | sdiSize(I.INSTR instr, labmap, loc) = let
	fun branch(opnd, short, long) = let
	  val offset = operand opnd - loc
	in if imm8(offset - 2) then short else long
	end
      in
	case instr
	of I.JMP(opnd, _) => branch(opnd, 2, 5)
	 | I.JCC{opnd, ...} => branch(opnd, 2, 6)
	 | _ => Word8Vector.length(encode(I.INSTR instr))
      end  (*sdiSize*)
    | sdiSize _ = error "sdiSize"

  fun expand(I.ANNOTATION{i,...}, size, loc) = expand(i, size, loc)
    | expand(I.LIVE _, _, _) = []
    | expand(I.KILL _, _, _) = []
    | expand(I.INSTR instr, size, loc) = 
       (case instr 
	of I.JMP(opnd, labs)  => [I.jmp(I.Relative(operand opnd-loc), labs)]
	 | I.JCC{cond, opnd} => 
	    [I.jcc{cond=cond, opnd=I.Relative(operand opnd-loc)}]
	 | opnd => [I.INSTR opnd])
    | expand _ = error "expand"
end


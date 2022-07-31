(* mipsJumps.sml --- information to resolve jumps. 
 *
 *)
functor MIPSJumps
  (structure Instr : MIPSINSTR
   structure Shuffle : MIPSSHUFFLE
      sharing Shuffle.I = Instr) : SDI_JUMPS = 
struct
  structure I = Instr
  structure C = I.C
  structure Const = I.Constant
  structure LE = I.LabelExp

  fun error msg = MLRiscErrorMsg.error("MIPSJumps",msg)

  val branchDelayedArch = true

  fun isSdi instr =
  let fun opnd (I.Lab _) = true
        | opnd _ = false
  in  case instr of
        I.LUI{imm, ...} => opnd imm
      | I.ARITH{i, ...} => opnd i
      | I.LOAD{d, ...} => opnd d
      | I.STORE{d, ...} => opnd d
      | I.FLOAD{d, ...} => opnd d
      | I.FSTORE{d, ...} => opnd d
      | I.J _ => true
      | I.JR _ => true
      | I.JAL _ => true
      | I.JALR _ => true
      | I.RET _ => true
      | I.BRANCH _ => true
      | I.FBRANCH _ => true
      | I.ANNOTATION{i,...} => isSdi i
      | _ => false
  end

  fun minSize(I.COPY _)    = 0
    | minSize(I.FCOPY _)   = 0
    | minSize(I.ANNOTATION{i,...}) = minSize i
    | minSize _            = 4

  (* max Size is not used for the mips span dependency analysis. *)
  fun maxSize _ = error "maxSize"

  fun sdiSize(instr, labMap, loc) = 
  let fun branchOffset lab = (labMap lab - loc - 8) div 4
      fun delay nop = if nop then 8 else 4
      fun branch(nop, label) = delay nop (* XXX *)
  in  case instr of
        I.J{nop, lab, ...} => branch(nop,lab)
      | I.JAL{nop, lab, ...} => branch(nop,lab)
      | I.BRANCH{nop, lab, ...} => branch(nop,lab)
      | I.FBRANCH{nop, lab, ...} => branch(nop,lab)
      | I.JR{nop, ...} => delay nop
      | I.JALR{nop, ...} => delay nop
      | I.ANNOTATION{i, ...} => sdiSize(i, labMap, loc)
      | _ => error "sdiSize"
  end

  fun expand(instr, size, pos) = error "expand"

end

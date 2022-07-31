signature AMD64SHUFFLE = sig
  structure I : AMD64INSTR

  type t = {tmp:I.operand option, dst:CellsBasis.cell list, src:CellsBasis.cell list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end

functor AMD64Shuffle (I : AMD64INSTR) : AMD64SHUFFLE =
  struct

    structure I = I
    structure C = I.C
    structure CB = CellsBasis
    structure Shuffle = Shuffle (I)

    type t = {tmp:I.operand option, dst:CellsBasis.cell list, 
	      src:CellsBasis.cell list}

    exception foo
    val shuffle = Shuffle.shuffle
	   {mvInstr=fn{dst, src} => [I.move{mvOp=I.MOVQ, src=src, dst=dst}],
		    ea=fn r => I.Direct (64, r)}

    fun shufflefp x = raise Fail "todo"

  end (* AMD64Shuffle *)
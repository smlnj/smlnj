(* pseudo-ops.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * MLRISC pseudo-ops.
 * Ties together the assembler and client pseudo-ops
 *)


functor PseudoOps(structure Client : CLIENT_PSEUDO_OPS) : PSEUDO_OPS = struct
  structure Client = Client 
  structure Basis = Client.AsmPseudoOps
  structure T = Basis.T
  structure BT = PseudoOpsBasisTyp

  type pseudo_op = Client.pseudo_op Basis.pseudo_op

  fun toString(BT.EXT ext) = Client.toString ext
    | toString pOp = Basis.toString pOp

  fun sizeOf(BT.EXT ext, loc) = Client.sizeOf(ext, loc)
    | sizeOf(pOp, loc) = Basis.sizeOf(pOp, loc)

  fun emitValue(arg as {pOp, loc, emit}) = 
    (case pOp
      of BT.EXT ext => Client.emitValue{pOp=ext, loc=loc, emit=emit}
       | _ => Basis.emitValue arg
    (*esac*))

  fun adjustLabels(BT.EXT ext, loc) = Client.adjustLabels(ext, loc)
    | adjustLabels _ = false

end



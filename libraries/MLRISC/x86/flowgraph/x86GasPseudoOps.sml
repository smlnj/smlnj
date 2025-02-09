(* x86GasPseudoOps.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 *)
functor X86GasPseudoOps
   ( structure T : MLTREE
     structure MLTreeEval : MLTREE_EVAL  where T = T
    ) : PSEUDO_OPS_BASIS =

struct
  structure T = T
  structure PB = PseudoOpsBasisTyp
  structure Fmt = Format

  structure Endian =
     PseudoOpsLittle
        (structure T = T
         structure MLTreeEval=MLTreeEval
         val icache_alignment = 16
         val max_alignment = SOME 7
         val nop = {sz=1, en=0wx90: Word32.word})

  structure GasPseudoOps =
     GasPseudoOps(structure T = T
                  val labFmt = {gPrefix="", aPrefix="L"})

  type 'a pseudo_op = (T.labexp, 'a) PB.pseudo_op

  fun error msg = MLRiscErrorMsg.error ("GasPseudoOps.", msg)

  val sizeOf = Endian.sizeOf
  val emitValue = Endian.emitValue
  val lexpToString = GasPseudoOps.lexpToString
  val toString = GasPseudoOps.toString
  val defineLabel = GasPseudoOps.defineLabel
  val wordSize = 32
end

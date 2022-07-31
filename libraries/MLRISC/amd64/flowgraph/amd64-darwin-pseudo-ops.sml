(* amd64-darwin-pseudo-ops.sml
 *
 * COPYRIGHT (c) 2006 The SML/NJ Fellowship (www.smlnj.org)
 * All rights reserved.
 *)

functor AMD64DarwinPseudoOps (

    structure T : MLTREE
    structure MLTreeEval : MLTREE_EVAL  where T = T

  ) : PSEUDO_OPS_BASIS = struct

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
  
    structure POps = DarwinPseudoOps(T)
  
    type 'a pseudo_op = (T.labexp, 'a) PB.pseudo_op
    
    fun error msg = MLRiscErrorMsg.error ("AMD64DarwinPseudoOps.", msg)
  
    val sizeOf = Endian.sizeOf
    val emitValue = Endian.emitValue
    val lexpToString = POps.lexpToString
    val toString = POps.toString
    val defineLabel = POps.defineLabel
    val wordSize = 64

  end

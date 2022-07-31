(*
 * This module removes unnecessary overflow trapping arithmetic instructions.
 * 
 * There are two types of trapping arithmetic instructions generated in all the 
 * architectures.  The first type of architectures have arithmetic instructions
 * that traps for overflow in one single instruction (PA-RISC, Alpha).  
 * Other architectures have instructions that sets an overflow flag and
 * require explicit tests (Sparc, x86, PowerPC).  
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
functor SSAUntrap(SSAInstrGen : SSA_INSTRGEN) : SSA_OPTIMIZATION =
struct

   structure Gen  = SSAInstrGen
   structure SSA  = SSAInstrGen.SSA
   structure CFG  = SSA.CFG
   structure RTL  = SSA.RTL
   structure T    = RTL.T  
   structure T'   = SSA.MLTreeComp.T
   structure G    = Graph
   structure A    = Array

   type flowgraph = SSA.ssa

   val name = "Remove Trapping Arithmetic"

   val untrapped = MLRiscControl.getCounter "ssa-untrapped"

   datatype interval = BOT | TOP | RANGE of int * int
  
   fun meet(BOT,x) = x
     | meet(x,BOT) = x
     | meet(TOP,x) = TOP
     | meet(x,TOP) = TOP
     | meet(RANGE(a,b), RANGE(c,d)) = RANGE(Int.min(a,c),Int.max(b,c))

   fun meets [] = BOT
     | meets (a::b) = meet(a,meets b)

   fun run(SSA as G.GRAPH ssa) =
   let val Dom as G.GRAPH dom = SSA.dom SSA
       val rtlTbl  = SSA.rtlTbl SSA
       val defsTbl = SSA.defsTbl SSA
       val usesTbl = SSA.usesTbl SSA
       val ssaOpTbl= SSA.ssaOpTbl SSA
       val showOp  = SSA.showOp SSA
       val showVal = SSA.showVal SSA
       val {ops, ...} = SSA.nodes SSA

       fun untrap(i, rtl, defs, uses) = 
       let fun isConst x = List.nth(uses,x) < 0 
       in  case rtl of
             T.RTL{e,...} => untrap(i, e, defs, uses)
           | T.MV(ty, z, T.ADDT(_,T.REG(_,x), T.REG(_,y))) =>
                if isConst x orelse isConst y then
                  let val t = T.MV(ty, z, T.ADD(ty, T.REG(ty,x), T.REG(ty,y)))
                      val mltree = 
                           Gen.translate SSA {defs=defs, uses=uses, rtl=t}
                  in  Gen.replace SSA {id=i, mltree=mltree}
                  end
                else false
           | T.MV(ty, z, T.SUBT(_,T.REG(_,x), T.REG(_,y))) =>
                if isConst x orelse isConst y then
                  let val t = T.MV(ty, z, T.SUB(ty, T.REG(ty,x), T.REG(ty,y)))
                      val mltree = 
                           Gen.translate SSA {defs=defs, uses=uses, rtl=t}
                  in  Gen.replace SSA {id=i, mltree=mltree}
                  end
                else false
           | _ => false
       end

       fun process i = 
       let val rtl  = A.sub(rtlTbl, i) 
           val uses = A.sub(usesTbl, i)
           val defs = A.sub(defsTbl, i)
       in  if untrap(i, rtl, defs, uses) then
              (untrapped := !untrapped + 1; 
               print("TRAP "^showOp i^"\n")
              )
           else ()
       end

       fun walk X = 
       let val ops = A.sub(ops, X)
           fun scan [] = ()
             | scan(i::ops) = 
               let val rtl = A.sub(rtlTbl, i)
               in  scan ops
               end
       in  scan ops;
           app walk (#succ dom X)
       end

   in  SSA.forallNodes SSA process;
       walk (hd (#entries dom ()));
       SSA
   end

end

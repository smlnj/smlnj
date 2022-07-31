(*
 * This module is responsible for generating new instructions from 
 * MLTREE and inserting them into the SSA graph.  This is useful for
 * patching in new instructions as the SSA graph is being transformed.
 *
 * Special MLRISC Magic(tm) for invoking the instruction selection 
 * module and ssa-ifying the output code are all hidden here.
 * 
 * -- Allen (leunga@cs.nyu.edu)
 * 
 *)
functor SSAInstrGen(SSA : SSA) : SSA_INSTRGEN =
struct

   structure SSA = SSA
   structure MLTreeComp = SSA.MLTreeComp
   structure RTL = SSA.RTL
   structure T   = MLTreeComp.T
   structure T'  = RTL.T
   structure S   = T.Stream
   structure SP  = SSA.SP
   structure P   = SP.RTLProps
   structure G   = Graph
   structure R   = T.Region
   structure A   = Array
   structure W8A = Word8Array

   fun error msg = MLRiscErrorMsg.error("SSAInstrGen", msg)

   exception Illegal

   (* Translate RTL mltree into the normal SSA form *)
   fun translate SSA {defs,uses,rtl} = 
   let fun defOf(x) = List.nth(defs,x)
       val const = SSA.const SSA
       fun useOf(ty,x) = 
           let val v = List.nth(uses,x)
           in  if v < 0 then 
                  (case const v of
                     SP.OT.INT i => T.LI i
                   | SP.OT.INTINF i => T.LIInf i
                   | SP.OT.OPERAND opnd => error "useOf"
                  )
               else T.REG(ty,v) 
           end
       fun (* stm(T'.MV(ty,x,e)) = T.MV(ty,defOf x,rexp e)
         | stm(T'.STORE(ty,a,b,mem)) = T.STORE(ty,rexp a,rexp b,R.memory) 
         | *) stm(T'.RTL{e, ...}) = stm e 
         | stm s = error("stm: "^RTL.rtlToString s)
       and (* rexp(T'.REG(ty,x)) = useOf(ty, x)
         | rexp(T'.LI i) = T.LI i
         | rexp(T'.LI32 i) = T.LI32 i
         | rexp(T'.ADD(ty,a,b)) = T.ADD(ty,rexp a, rexp b)
         | rexp(T'.SUB(ty,a,b)) = T.SUB(ty,rexp a, rexp b)
         | rexp(T'.MULS(ty,a,b)) = T.MULS(ty,rexp a, rexp b)
         | rexp(T'.DIVS(ty,a,b)) = T.DIVS(ty,rexp a, rexp b)
         | rexp(T'.QUOTS(ty,a,b)) = T.QUOTS(ty,rexp a, rexp b)
         | rexp(T'.REMS(ty,a,b)) = T.REMS(ty,rexp a, rexp b)
         | rexp(T'.MULU(ty,a,b)) = T.MULU(ty,rexp a, rexp b)
         | rexp(T'.DIVU(ty,a,b)) = T.DIVU(ty,rexp a, rexp b)
         | rexp(T'.REMU(ty,a,b)) = T.REMU(ty,rexp a, rexp b)
         | rexp(T'.ADDT(ty,a,b)) = T.ADDT(ty,rexp a, rexp b)
         | rexp(T'.SUBT(ty,a,b)) = T.SUBT(ty,rexp a, rexp b)
         | rexp(T'.MULT(ty,a,b)) = T.MULT(ty,rexp a, rexp b)
         | rexp(T'.DIVT(ty,a,b)) = T.DIVT(ty,rexp a, rexp b)
         | rexp(T'.QUOTT(ty,a,b)) = T.QUOTT(ty,rexp a, rexp b)
         | rexp(T'.REMT(ty,a,b)) = T.REMT(ty,rexp a, rexp b)
         | rexp(T'.ANDB(ty,a,b)) = T.ANDB(ty,rexp a, rexp b)
         | rexp(T'.ORB(ty,a,b))  = T.ORB(ty,rexp a, rexp b)
         | rexp(T'.XORB(ty,a,b)) = T.XORB(ty,rexp a, rexp b)
         | rexp(T'.NOTB(ty,a)) = T.NOTB(ty,rexp a)
         | rexp(T'.SRA(ty,a,b)) = T.SRA(ty,rexp a,rexp b)
         | rexp(T'.SRL(ty,a,b)) = T.SRL(ty,rexp a,rexp b)
         | rexp(T'.SLL(ty,a,b)) = T.SLL(ty,rexp a,rexp b)
         | rexp(T'.CVTI2I(ty,ext,ty',a)) = T.CVTI2I(ty,ext,ty',rexp a)
         | rexp(T'.LOAD(ty,a,mem)) = T.LOAD(ty,rexp a,R.memory) 
         | *) rexp e = error("rexp: "^RTL.expToString e) 
   in  stm rtl end

   (* 
    * Translate mltree into instructions
    *)
   fun instrGen (SSA as G.GRAPH ssa) = 
   let val instrs = ref []
       fun emit i = instrs := i :: !instrs
       fun can'tUse _ = raise Illegal
       val instrStream = 
           S.STREAM
           { beginCluster = can'tUse,
             endCluster   = can'tUse,
             emit         = emit,
             pseudoOp     = can'tUse,
             defineLabel  = can'tUse,
             entryLabel   = can'tUse,
             comment      = can'tUse,
             annotation   = can'tUse,
             exitBlock    = can'tUse
           }

       val S.STREAM{emit, ...} = MLTreeComp.selectInstructions instrStream

       (* Translate instructions into SSA form *)
       fun translate instrs = ()

       (* Generate instructions *)
       fun gen mltree =
          (instrs := [];
           emit mltree;
           rev(!instrs)
          )

   in  gen
   end

   (*
    * Replace the instruction with a new mltree
    *)
   fun replace (SSA as G.GRAPH ssa) =
   let val instrGen = instrGen SSA
       val ssaOpTbl = SSA.ssaOpTbl SSA
       fun doit{id, mltree} = 
           case instrGen mltree of 
              [i] => (A.update(ssaOpTbl, id, i); true)
           | _ => false
   in  doit
   end

   (* 
    * Insert instructions into the SSA graph
    *)
   fun insert (SSA as G.GRAPH ssa) = 
   let val getOperands =
           P.defUse(SP.OT.makeNewValueNumbers(SSA.operandTbl SSA))
       val pinnedUseTbl = SSA.pinnedUseTbl
       val pinnedDefTbl = SSA.pinnedDefTbl
       fun isPinnedUse r = W8A.sub(pinnedUseTbl,r) <> 0w0 handle _ => false
       fun isPinnedDef r = W8A.sub(pinnedDefTbl,r) <> 0w0 handle _ => false
       fun hasPinnedUse [] = false
         | hasPinnedUse (r::rs) = isPinnedUse r orelse hasPinnedUse rs
       fun hasPinnedDef [] = false
         | hasPinnedDef (r::rs) = isPinnedDef r orelse hasPinnedDef rs

       fun isZero r = W8A.sub(SSA.zeroRegs,r) <> 0w0 handle _ => false

       val renameVar = SSA.newRenamedVar SSA

       exception Renaming
       val renameMap = IntHashTable.mkTable(32, Renaming)
       val lookupRenaming = IntHashTable.lookup renameMap
       val addRenaming = IntHashTable.insert renameMap

       fun addInstrs(block, instrs) = 
       let val n = length instrs
           val m = #capacity ssa ()
           val _ = SSA.reserve SSA (n+m)
           val newOp = SSA.newOp SSA

           fun renameUse v = if v < 0 then v else lookupRenaming v
           fun renameDef v = 
           let val v' = renameVar v
           in  if isZero v then v' 
               else (addRenaming(v,v'); v')
           end
 
           fun scan([], id, pos) = ()
             | scan(instr::rest, id, pos) = 
               let val (defs, uses) = getOperands instr
                   val rtl = P.rtl instr
                   val rtl = if hasPinnedUse uses orelse
                                hasPinnedDef defs then
                             RTL.pin rtl else rtl
                   val uses = map renameUse uses
                   val defs = map renameDef defs
               in  newOp{id=id, instr=instr, pos=pos, rtl=rtl, 
                         block=block, defs=defs, uses=uses};
                   scan(rest, id+1, pos+128)
               end
       in  scan(instrs, m, 0 (* XXX *))
       end
   in  ()
   end

end

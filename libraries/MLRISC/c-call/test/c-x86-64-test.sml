local 
(*
 * User defined constant type.  Dummy for now.
 * In practice, you'll want to use this type to implement constants with
 * values that cannot be determined until final code generation, e.g.
 * stack frame offset.
 *)
structure UserConst =
struct
   type const = unit
   fun toString() = ""  
   fun hash() = 0w0  
   fun valueOf _ = 0
   fun == _ = true  
end

(*
 * User defined datatype for representing aliasing.   Dummy for now.
 * You'll need this to represent aliasing information. 
 *)
structure UserRegion =
struct
   type region = unit
   fun toString () = "" 
   val memory = ()
   val stack = ()
   val readonly = ()
   val spill = ()
end

(*
 * User defined datatype for representing pseudo assembly operators.
 * Dummy for now.
 *
 * You'll need this to represent assembler directives. 
 *)
structure UserPseudoOps =
struct
   type pseudo_op = unit  
   fun toString () = ""
   fun emitValue _ = ()
   fun sizeOf _ = 0
   fun adjustLabels _ = true
end



(*
 * Client defined extensions.  None for now.
 * You'll need this only if you need to extend the set of MLTREE operators
 *)
structure UserExtension =
struct

   type ('s,'r,'f,'c) sx = ('s,'r,'f,'c) AMD64InstrExt.sext
   type ('s,'r,'f,'c) rx = unit
   type ('s,'r,'f,'c) fx = unit
   type ('s,'r,'f,'c) ccx = unit

end

(*
 * This module controls how we handle user extensions.  Since we don't
 * have any yet.  This is just a bunch of dummy routines.
 *)
functor UserMLTreeExtComp
	    (    structure I : AMD64INSTR where T.Extension = UserExtension
    structure TS : MLTREE_STREAM where T = I.T
    structure CFG : CONTROL_FLOW_GRAPH where I = I and P = TS.S.P
   ) : MLTREE_EXTENSION_COMP =
struct
    structure T = TS.T
    structure TS = TS
    structure I = I
    structure CFG = CFG
    structure C = I.C

    structure CompInstrExt = AMD64CompInstrExt (
      structure I = I
      structure TS = TS
      structure CFG = CFG)

    type reducer =
	  (I.instruction,C.cellset,I.operand,I.addressing_mode,CFG.cfg) TS.reducer

    val compileSext = CompInstrExt.compileSext

    fun compileRext _ = raise Fail "AMD64CompExtFn.compileRext"
    fun compileFext _ = raise Fail "AMD64CompExtFn.compileFext"
    fun compileCCext _ = raise Fail "AMD64CompExtFn.compileCCext"

end

val floats16ByteAligned = true

structure AMD64MLTree =
   MLTreeF (structure Constant  = UserConst
    structure Region    = UserRegion
    structure Extension = UserExtension)

structure AMD64MLTreeEval =
   MLTreeEval (structure T = AMD64MLTree
    fun eq _ _ = false
    val eqRext = eq val eqFext = eq
    val eqCCext = eq val eqSext = eq)


functor AMD64PseudoOpsFn (
    structure T : MLTREE
    structure MLTreeEval : MLTREE_EVAL where T = T
  ) : PSEUDO_OPS_BASIS = AMD64GasPseudoOps (
    structure T = T
    structure MLTreeEval = MLTreeEval)

(*
functor AMD64PseudoOpsFn (
    structure T : MLTREE
    structure MLTreeEval : MLTREE_EVAL where T = T
  ) : PSEUDO_OPS_BASIS = AMD64DarwinPseudoOps (
    structure T = T
    structure MLTreeEval = MLTreeEval)
*)

structure AMD64PseudoOps = AMD64PseudoOpsFn(
            structure T = AMD64MLTree
            structure MLTreeEval = AMD64MLTreeEval)

structure PseudoOps =
  struct

    structure Client =
      struct
	structure AsmPseudoOps = AMD64PseudoOps
	type pseudo_op = unit
			 
	fun toString () = ""
  
	fun emitValue _ = raise Fail "todo"
	fun sizeOf _ = raise Fail "todo"
	fun adjustLabels _ = raise Fail "todo"
      end (* Client *)
  
    structure PseudoOps = PseudoOps (structure Client = Client)
  end

structure AMD64Stream = InstructionStream(PseudoOps.PseudoOps)
structure AMD64Instr = AMD64Instr (AMD64MLTree)
structure AMD64Shuffle = AMD64Shuffle(AMD64Instr)

structure AMD64MLTreeHash =
   MLTreeHash (structure T = AMD64MLTree
    fun h _ _ = 0w0
    val hashRext = h val hashFext = h
    val hashCCext = h val hashSext = h)

structure AMD64Asm = AMD64AsmEmitter
   (structure Instr = AMD64Instr
    structure S = AMD64Stream
    structure MLTreeEval = AMD64MLTreeEval
    structure Shuffle = AMD64Shuffle
   )

structure AMD64InsnProps = AMD64Props 
			  (structure Instr = AMD64Instr
                           structure MLTreeHash = AMD64MLTreeHash
			   structure MLTreeEval = AMD64MLTreeEval)

structure AMD64CFG = ControlFlowGraph (
            structure I = AMD64Asm.I
	    structure GraphImpl = DirectedGraph
	    structure InsnProps = AMD64InsnProps
	    structure Asm = AMD64Asm)

structure AMD64MLTStream = MLTreeStream (
		      structure T = AMD64MLTree
		      structure S = AMD64Stream)

structure CompInstrExt = AMD64CompInstrExt (
      structure I = AMD64Instr
      structure TS = AMD64MLTStream
      structure CFG = AMD64CFG)

structure AMD64MTC = struct
  structure T = AMD64MLTree
  structure TS = AMD64MLTStream
  structure I = AMD64Instr
  structure CFG = AMD64CFG
  structure C = I.C
   type reducer =
     (I.instruction,C.cellset,I.operand,I.addressing_mode,AMD64CFG.cfg) TS.reducer
   fun unimplemented _ = MLRiscErrorMsg.impossible "UserMLTreeExtComp"
   val compileSext  = CompInstrExt.compileSext
   val compileRext  = unimplemented
   val compileFext  = unimplemented
   val compileCCext = unimplemented
		      
   structure AMD64MLTreeUtils : MLTREE_UTILS =
     struct
       structure T = AMD64MLTree
       structure IX = AMD64InstrExt
       structure U = MLTreeUtils (
       structure T = T
       fun hashSext _ _ = 0w0
       fun hashRext _ _ = 0w0
       fun hashFext _ _ = 0w0
       fun hashCCext _ _ = 0w0
       fun eqSext _ _ = raise Fail "eqSext"
       fun eqRext _ _ = raise Fail "eqRext"
       fun eqFext _ _ = raise Fail "eqFext"
       fun eqCCext _ _ = raise Fail "eqCCext"
       fun showSext (prt : T.printer) ext = raise Fail "todo"
       fun showRext _ _ = raise Fail "showRext"
       fun showFext _ _ = raise Fail "showFext"
       fun showCCext _ _ = raise Fail "showCCext")
       open U
     end
end

structure AMD64 = AMD64Gen (
		  structure I = AMD64Instr
		  structure MLTreeUtils = AMD64MTC.AMD64MLTreeUtils
		  structure ExtensionComp = AMD64MTC
		  val floats16ByteAligned = floats16ByteAligned
		  fun signBit _ = raise Fail "todo"
		  fun negateSignBit _ = raise Fail "todo"
		  )

structure AMD64Emit = CFGEmit (
             structure CFG = AMD64CFG
             structure E = AMD64Asm) 


structure AMD64FlowGraph = BuildFlowgraph 
	    (structure Props = AMD64InsnProps
             structure Stream = AMD64Stream
	     structure CFG = AMD64CFG)

structure AMD64Expand = CFGExpandCopies (structure CFG=AMD64CFG
                                         structure Shuffle = AMD64Shuffle)
structure AMD64BlockPlacement = DefaultBlockPlacement(AMD64CFG)

structure RASpill = RASpillWithRenaming (
    structure Asm = AMD64Asm
    structure InsnProps = AMD64InsnProps
    val max_dist = ref 4
    val keep_multiple_values = ref false)

structure C = AMD64Cells

datatype spill_operand_kind = SPILL_LOC 
                            | CONST_VAL

datatype ra_phase = SPILL_PROPAGATION 
                  | SPILL_COLORING

structure IntRA = 
  struct
    val dedicated = [C.rsp, C.rbp]
    val allRegs = C.Regs CellsBasis.GP {from=0, to=15, step=1}
    val allRegsSet = foldl C.addReg C.empty allRegs
    val avail = let
        val availSet = foldl C.rmvReg allRegsSet dedicated
        in
          C.getReg availSet
        end
    fun spillInit _ = ()
    fun spillLoc {info=frame, an, cell, id=loc} = 
raise Fail ""
(*        {opnd = AMD64Instr.Immed 0, kind = SPILL_LOC}*)
    val phases = [SPILL_PROPAGATION, SPILL_COLORING]
  end (* IntRA *)

structure FloatRA =
  struct
    val avail = C.Regs CellsBasis.FP {from=0, to=15, step=1}
    val dedicated = []
    fun spillInit _ = ()
    fun spillLoc (info, ans, id) = raise Fail ""
    val phases = [SPILL_PROPAGATION, SPILL_COLORING]
  end (* FloatRA *)

(* register allocation *)
structure AMD64RA = AMD64RegAlloc (
         structure I = AMD64Instr
         structure CFG = AMD64CFG
         structure Asm = AMD64Asm
         structure SpillHeur = ChowHennessySpillHeur
         structure Spill = RASpill
         structure Props = AMD64InsnProps
	 val floats16ByteAligned = floats16ByteAligned
         type spill_info = unit
         fun beforeRA (Graph.GRAPH graph) = ()
         datatype spill_operand_kind = datatype spill_operand_kind
         datatype ra_phase = datatype ra_phase
         structure Int = IntRA
         structure Float = FloatRA)

structure AMD64Expand = CFGExpandCopies (
    structure CFG=AMD64CFG
    structure Shuffle = AMD64Shuffle)

structure AMD64CCall = X86_64SVIDFn (
		    structure T = AMD64MLTree)

structure RA2 = 
    RISC_RA
    (structure I = AMD64Instr
     structure Asm = AMD64Asm
     structure CFG = AMD64CFG
     structure InsnProps = AMD64InsnProps
     structure Rewrite = 
       struct
         structure I = AMD64Instr
	 structure C=I.C
	 structure CB = CellsBasis
	 fun error msg = MLRiscErrorMsg.error("X86Rewrite", msg)
			 
	 fun operand (rs,rt) opnd =
	     (case opnd
	       of I.Direct (sz, r) => if CB.sameColor(r,rs) then I.Direct (sz, rt) else opnd
		| I.Displace{base, disp, mem} => 
		  if CB.sameColor(base,rs) then I.Displace{base=rt, disp=disp, mem=mem} 
		  else opnd
		| I.Indexed{base as SOME b, index, scale, disp, mem} => let
		      val base'= if CB.sameColor(b,rs) then SOME rt else base
		      val index'=if CB.sameColor(index,rs) then rt else index
		  in I.Indexed{base=base', index=index', scale=scale, disp=disp, mem=mem}
		  end
		| I.Indexed{base, index, scale, disp, mem=mem}  => 
		  if CB.sameColor(index,rs) then 
		      I.Indexed{base=base, index=rt, scale=scale, disp=disp, mem=mem}
		  else opnd
		| _ => opnd
              (*end case*))
	     

	 fun rewriteDef (instr, rs, rt) = let
	     fun operand(opnd as I.Direct (sz, r)) = 
		 if CB.sameColor(r,rs) then I.Direct (sz, rt) else opnd
	       | operand _ = error "operand: not I.Direct"
	     fun replace r = if CB.sameColor(r,rs) then rt else r
	     fun rewriteX86Def(instr) =
		 (case instr 
		   of I.CALL{opnd, defs, uses, return, cutsTo, mem, pops} => 
		      I.CALL{opnd=opnd, cutsTo=cutsTo, 
			     return=CB.CellSet.map {from=rs,to=rt} return, pops=pops,
			     defs=CB.CellSet.map {from=rs,to=rt} defs, uses=uses, mem=mem}
		    | I.MOVE{mvOp, src, dst} => I.MOVE{mvOp=mvOp, src=src, dst=operand dst}
		    | I.LEAL{r32, addr} => I.LEAL{r32=replace r32, addr=addr}
		    | I.LEAQ{r64, addr} => I.LEAQ{r64=replace r64, addr=addr}
		    | I.BINARY{binOp, src, dst} => 
		      I.BINARY{binOp=binOp, src=src, dst=operand dst}
		    | I.SHIFT{shiftOp, src, dst, count} => 
		      I.SHIFT{shiftOp=shiftOp, src=src, count=count, dst=operand dst}
		    | I.UNARY{unOp, opnd} => I.UNARY{unOp=unOp, opnd=operand opnd}
		    | I.SET{cond, opnd} => I.SET{cond=cond, opnd=operand opnd}
		    | _ => instr
	        (* end case *))

	     fun f (I.ANNOTATION{a,i}) =
		 I.ANNOTATION{i=rewriteDef(i,rs,rt),
			      a=(case a of
				     CB.DEF_USE{cellkind=CB.GP,defs,uses} =>
			             CB.DEF_USE{cellkind=CB.GP,uses=uses,
				 		defs=map replace defs}
				   | _ => a)}
	       | f (I.INSTR i) = I.INSTR(rewriteX86Def(i))
	       | f (I.COPY{k as CB.GP, sz, dst, src, tmp}) =
		 I.COPY{k=k, sz=sz, dst=map replace dst, src=src, tmp=tmp}
	 in 
	     f(instr)
	 end


	 fun rewriteUse (instr, rs, rt) = let
	     val operand = operand (rs, rt)
	     fun replace r = if CB.sameColor(r,rs) then rt else r
	     fun rewrite instr = (case instr
                 of I.JMP(opnd, labs) => I.JMP(operand opnd, labs)
		  | I.JCC{cond, opnd} => I.JCC{cond=cond, opnd = operand opnd}
		  | I.CALL{opnd, defs, uses, return, cutsTo, mem, pops} => 
		    I.CALL{opnd=operand opnd, defs=defs, return=return,
			   uses=CB.CellSet.map {from=rs,to=rt} uses, cutsTo=cutsTo,
			   mem=mem, pops=pops}
		  | I.MOVE{mvOp, src, dst as I.Direct _} => 
		    I.MOVE{mvOp=mvOp, src=operand src, dst=dst}
		  | I.MOVE{mvOp, src, dst} => 
		    I.MOVE{mvOp=mvOp, src=operand src, dst=operand dst}
		  | I.LEAL{r32, addr} => I.LEAL{r32=r32, addr=operand addr}
		  | I.LEAQ{r64, addr} => I.LEAQ{r64=r64, addr=operand addr}
		  | I.CMPL{lsrc, rsrc} => I.CMPL{lsrc=operand lsrc, rsrc=operand rsrc}
		  | I.CMPW{lsrc, rsrc} => I.CMPW{lsrc=operand lsrc, rsrc=operand rsrc}
		  | I.CMPB{lsrc, rsrc} => I.CMPB{lsrc=operand lsrc, rsrc=operand rsrc}
		  | I.TESTL{lsrc, rsrc} => I.TESTL{lsrc=operand lsrc, rsrc=operand rsrc}
		  | I.TESTW{lsrc, rsrc} => I.TESTW{lsrc=operand lsrc, rsrc=operand rsrc}
		  | I.TESTB{lsrc, rsrc} => I.TESTB{lsrc=operand lsrc, rsrc=operand rsrc}
		  | I.BITOP{bitOp, lsrc, rsrc} => 
		    I.BITOP{bitOp=bitOp, lsrc=operand lsrc, rsrc=operand rsrc}
		  | I.BINARY{binOp, src, dst} => 
		    I.BINARY{binOp=binOp, src=operand src, dst=operand dst}
		  | I.SHIFT{shiftOp, src, dst, count} => 
		    I.SHIFT{shiftOp=shiftOp, src=operand src, dst=operand dst, 
			    count=operand src}
                (* end case *))

             fun f(I.ANNOTATION{a,i}) = 
		 I.ANNOTATION{i=rewriteUse(i, rs, rt),
			      a = case a of
				      CB.DEF_USE{cellkind=CB.GP,defs,uses} =>
				      CB.DEF_USE{cellkind=CB.GP,uses=map replace uses,
						 defs=defs}
				    | _ => a}
	       | f(I.INSTR i) = I.INSTR(rewrite(i))
	       | f(I.COPY{k as CB.GP, sz, dst, src, tmp}) = 
		 I.COPY{k=k, sz=sz, dst=dst, src=List.map replace src, tmp=tmp}
	 in 
	     f (instr:I.instruction)
	 end

      
	 fun frewriteDef _ = raise Fail ""
	 fun frewriteUse _ = raise Fail ""
       end
     structure SpillInstr = AMD64SpillInstr (
               structure I = I
               structure Props = AMD64InsnProps
	       val floats16ByteAligned = true)
     structure SpillHeur = ChaitinSpillHeur
     structure Spill = RASpill (structure InsnProps = AMD64InsnProps
                                structure Asm = AMD64Asm)
     
     datatype spillOperandKind = SPILL_LOC | CONST_VAL
     type spill_info = unit
     fun beforeRA _ = ()

     val architecture = "amd64"
     fun pure _ = true

     structure Int =
	struct
	  val allRegs = C.Regs CellsBasis.GP {from=0, to=15, step=1}
	  val allRegsSet = List.foldl C.addReg C.empty allRegs
	  val dedicated = [C.rsp, C.rbp]
	  val avail = C.getReg (List.foldl C.rmvReg allRegsSet dedicated)
	  fun spillLoc _ = raise Fail ""
	  val mode = RACore.NO_OPTIMIZATION
	end
     structure Float =
	struct
	  val avail = C.Regs CellsBasis.FP {from=0, to=15, step=1}
	  val dedicated = []
	  fun spillLoc _ = raise Fail ""
	  val mode = Word.orb (RACore.HAS_PARALLEL_COPIES, RACore.DEAD_COPY_ELIM)
	end

    )
			   
structure RA = RA2
structure Cells = AMD64Instr.C
structure T = AMD64MLTree
structure CFG = AMD64CFG
structure FlowGraph = AMD64FlowGraph
structure ChkTy = MLTreeCheckTy(structure T = T val intTy = 64)
    val wordTy = 64

structure MC = AMD64MCFn(
	         structure Instr = AMD64Instr
		 structure Shuffle = AMD64Shuffle
		 structure MLTreeEval = AMD64MLTreeEval
	       )

    fun gen (functionName, stms, result) = let
           val insnStrm = FlowGraph.build()
	   val stream as AMD64Stream.STREAM
           { beginCluster,  (* start a cluster *)
             endCluster,    (* end a cluster *)
             emit,          (* emit MLTREE stm *)
             defineLabel,   (* define a local label *)
             entryLabel,    (* define an external entry *)
             exitBlock,     (* mark the end of a procedure *)
             pseudoOp,      (* emit a pseudo op *)
             annotation,    (* add an annotation *)
             ... } =
             AMD64.selectInstructions insnStrm
	fun doit () = (
	    beginCluster 0;      (* start a new cluster *)
            pseudoOp PseudoOpsBasisTyp.TEXT;		  
	    pseudoOp (PseudoOpsBasisTyp.EXPORT [functionName]);    
            entryLabel functionName; (* define the entry label *)
            List.app emit stms; (* emit all the statements *)
            exitBlock result;
            endCluster [])
	val cfg = doit ()
	val cfg = RA.run cfg
	val cfg = AMD64Expand.run cfg
        in  
         (cfg, stream)        (* end the cluster *)
       end

    fun dumpOutput (cfg, stream) = let
	val (cfg as Graph.GRAPH graph, blocks) = 
		AMD64BlockPlacement.blockPlacement cfg
	val CFG.INFO{annotations=an, data, decls, ...} = #graph_info graph
	in
	  AMD64Emit.asmEmit (cfg, blocks)
	end (* dumpOutput *)

   
    fun codegen (functionName, target, proto, initStms, args) = let 
        val _ = Label.reset()

	val [functionName, target] = List.map Label.global [functionName, target]

	(* construct the C call *)
	val {result, callseq} = AMD64CCall.genCall {
	           name=T.LABEL target,
	           paramAlloc=fn _ => false,
	           structRet=fn _ => T.REG (64, Cells.rax),
	           saveRestoreDedicated=fn _ => {save=[], restore=[]},
	           callComment=NONE,
	           proto=proto,
	           args=args}

	fun wordLit i = T.LI (T.I.fromInt (wordTy, i))

	val stms = List.concat [
		   [T.EXT(AMD64InstrExt.PUSHQ(T.REG(64, Cells.rbp))),
		    T.COPY (wordTy, [Cells.rbp], [Cells.rsp])],		   
		   initStms,
		   callseq, 
		   [T.EXT(AMD64InstrExt.LEAVE)],
		   [T.RET []]]

(*	val _ = List.all (fn stm => ChkTy.check stm 
				    orelse raise Fail ("typechecking error: "^AMD64MTC.AMD64MLTreeUtils.stmToString stm))
		stms
*)

        in
	   dumpOutput(gen (functionName, stms, result))
	end


    fun lit i = T.LI (T.I.fromInt (wordTy, i))
    (* machine-specific data *)
    val wordTy = 64
    val wordSzB = wordTy div 8
    val param0 = T.REG(wordTy, Cells.rdi)

    (* maximum argument size in machine words *)
    val maxArgSz = 16
    val maxArgSzB = maxArgSz * wordSzB

in
structure X86_64Test = GenTestFn (
		  structure T = AMD64MLTree
		  structure CCall = AMD64CCall
		  structure Cells = AMD64Cells
		  val codegen = codegen
		  val param0 = param0
		  val wordTy = 64)
end (* local *)

(*
(* unit testing code *)
structure Test = 
  struct

    open CCall

    fun li2k (_, k, _) = k

    val ty1 = CTy.C_STRUCT [CTy.C_STRUCT [CTy.C_unsigned CTy.I_char, CTy.C_unsigned CTy.I_int]]
    val ty2 = CTy.C_STRUCT [CTy.C_signed CTy.I_short]
    val ty3 = CTy.C_STRUCT [CTy.C_signed CTy.I_short, CTy.C_PTR]
    val ty4 = CTy.C_STRUCT [CTy.C_PTR, CTy.C_PTR]
    val ty4 = CTy.C_STRUCT [CTy.C_STRUCT[CTy.C_unsigned CTy.I_int], CTy.C_PTR]
    val ty5 = CTy.C_STRUCT [CTy.C_STRUCT[CTy.C_float]]
    val ty6 = CTy.C_STRUCT [CTy.C_STRUCT[CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float]]
    val ty7 = CTy.C_STRUCT [CTy.C_STRUCT[CTy.C_STRUCT[CTy.C_float,CTy.C_float],CTy.C_float,CTy.C_float]]
    val ty8 = CTy.C_STRUCT [CTy.C_STRUCT[CTy.C_STRUCT[CTy.C_float,CTy.C_unsigned CTy.I_int],CTy.C_float,CTy.C_float]]
    val ty9 = CTy.C_STRUCT [CTy.C_STRUCT[CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float,CTy.C_float]]
    val ty10 = CTy.C_STRUCT [CTy.C_STRUCT[CTy.C_float,CTy.C_float, CTy.C_STRUCT[CTy.C_float,CTy.C_unsigned CTy.I_int]]]
    val ty11 = CTy.C_STRUCT [CTy.C_PTR, CTy.C_float, CTy.C_float, CTy.C_float]
	       
    fun kindOfEB () = let
	fun test (eb, k) = (kindOfEightByte eb = k) orelse raise Fail "failed test"
	fun eb1 ty = hd (eightBytesOfCTy ty)
	fun eb2 ty = hd(tl (eightBytesOfCTy ty))
        in
	   List.all test [(eb1 ty1, K_GPR), (eb1 ty2, K_GPR), (eb2 ty3, K_GPR),
			  (eb1 ty5, K_FPR), (eb1 ty6, K_FPR), (eb2 ty6, K_FPR),
			  (eb1 ty7, K_FPR), (eb2 ty7, K_FPR),
			  (eb1 ty8, K_GPR), (eb2 ty8, K_FPR)]
        end

    fun slots () = let
	fun test (lis : SA.slot list, ks2 : location_kind list) = let
	    val ks1 = List.map li2k lis
            in
	        (List.length ks1 = List.length ks2) andalso (ListPair.all (op =) (ks1, ks2))
	    end
	    val tests = [
	               (ty2, [K_GPR]), 
		       (ty1, [K_GPR]), 
		       (ty3, [K_GPR, K_GPR]), 
		       (ty4, [K_GPR, K_GPR]), 
		       (ty5, [K_FPR]), 
		       (ty6, [K_FPR, K_FPR]),
		       (ty7, [K_FPR, K_FPR]),
		       (ty8, [K_GPR, K_FPR]),
		       (ty11, [K_MEM, K_MEM, K_MEM])
				       ]
	    val (ts, anss) = ListPair.unzip tests
            in
	       ListPair.all test (List.map slotsOfCTy ts, anss) orelse raise Fail "failed test"
            end
  end
*)


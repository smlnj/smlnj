structure AMD64Demo =
  struct
    structure C = AMD64Instr.C
    structure T = AMD64MLTree
    structure CFG = AMD64CFG

    fun codegen (functionName, mltreeStms, ls) = let 
        val _ = Label.reset()
        val insnStrm = AMD64FlowGraph.build()
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
            entryLabel functionName; (* define the entry label *)
            app emit mltreeStms; (* emit all the statements *)
            exitBlock ls;
            endCluster [])
	val cfg = doit ()
	val cfg = AMD64RA.run cfg
	val cfg = AMD64Expand.run cfg
        in  
         (cfg, stream)        (* end the cluster *)
       end (* codegen *)

    fun li i = T.LI (T.I.fromInt (32, i))

    fun dumpOutput (cfg, stream) = let
	val (cfg as Graph.GRAPH graph, blocks) = 
		AMD64BlockPlacement.blockPlacement cfg
	val CFG.INFO{annotations=an, data, decls, ...} = #graph_info graph
	in
	  AMD64Emit.asmEmit (cfg, blocks)
	end (* dumpOutput *)

    structure CB = CellsBasis
    val [rax, rbx, rdi, rsi, rdx, rcx, r8, r9, r10, r11, r12, r13, r14, r15] = 
	   ([C.rax, C.rbx, C.rdi, C.rsi, C.rdx, C.rcx] @
	     C.Regs CB.GP {from=8, to=15, step=1})

    val [xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7, xmm8, xmm9, xmm10, 
         xmm11, xmm12, xmm13, xmm14, xmm15] =
           C.Regs CB.FP {from=0, to=15, step=1}

    val callerSaves = [rax, rcx, rdx, rsi, rdi, r8, r9, r10, r11]

    structure CTy = CTypes
    val intTy = CTy.C_signed CTy.I_int
    fun reg r = T.REG (64, r)
    fun gpr r = T.GPR (reg r)
    fun fpr r = T.FPR (T.FREG (32, r))

    fun eg () = let
	val l = Label.label "eg" ()
	val raxR = T.GPR (T.REG (32, C.rax))
	val f1 = C.newFreg ()
	val f2 = C.newFreg ()
	val f3 = C.newFreg ()
	val r1 = C.newReg ()
	val r2 = C.newReg ()
	val deds as [rax, rdi, rsi, rbx, rsp, r10] = 
	            [C.rax, C.rdi, C.rsi, C.rbx, C.rsp, C.GPReg 10]
	val tmps = map (fn _ => C.newReg ()) deds
	(* c-calls stuff *)
	val ccL = Label.label "cFun" ()
	val numArgs = 0
	fun genArgs 0 = []
          | genArgs i = CCalls.ARG (li (numArgs-i+1)) :: genArgs (i-1)
        val args = genArgs numArgs
        val fpArgs = [CCalls.FARG (T.FREG (32, f1)), CCalls.FARG (T.FREG (32, f2))]
        val fpTys = [CTy.C_float, CTy.C_float]
        val paramTys = map (fn _ => intTy) args @ fpTys
	val proto = {conv="", retTy=CTy.C_void, paramTys=paramTys}
(*	val {result, callseq} = CCalls.genCall {
	           name=T.LABEL ccL,
	           paramAlloc=fn _ => false,
	           structRet=fn _ => T.REG (64, C.rax),
	           saveRestoreDedicated=fn _ => {save=[], restore=[]},
	           callComment=NONE,
	           proto=proto,
	           args=args @ fpArgs
				}
*)
	val stms = [
	    T.FMV (64, f3, T.FADD (64, T.FLOAD (64, T.ADD (64, li 128, T.REG (64, r2)), ()), T.FLOAD (64, T.ADD (64, li 128, T.REG (64, r1)), ()))),
	    T.FSTORE (64, T.REG (64, r1), T.FREG (64, f3), ())
	]
(*	val stms' = [
	    T.FMV (32, f1, T.FLOAD (32, T.ADD (64, li 128, T.REG (64, r1)), ())),
	    T.FMV (32, f2, T.FADD (32, T.FREG (32, f1), T.FREG (32, f1))),
	    T.FCOPY (32, [xmm0, xmm1], [f1, f2]),
	    T.CALL {funct=T.LABEL ccL, targets=[], defs=[], uses=[fpr xmm0, fpr xmm1],
	            region=UserRegion.memory, pops=0}
	] 
	val stms = 
	   [ T.FMV (32, f1, T.FLOAD (32, T.ADD (64, li 128, T.REG (64, r1)), ())),
	     T.COPY (64, tmps, deds)
	      ]
val stms1 = [
    T.MV (64, r1, li 999),
    T.MV (64, rax, T.COND (64, T.CMP (64, T.EQ, reg r1, li 123), li 11, li 22))
(*    T.MV (64, r1, T.LABEL ccL)
    T.STORE (64, T.REG (64, r1), T.LABEL ccL, ())*)
] *)
	   (*@ [
	    T.COPY (64, deds, tmps),
	    T.MV (32, r2, T.MULS (32, li 5, T.ADD (32, T.REG (32, C.rax), T.REG (32, r1)))),
	    T.BCC (T.FCMP (32, T.<, T.FREG (32, f2), T.FLOAD (32, T.REG (64, r1), ())), ccL),
	    T.FSTORE (32, T.REG (64, r1), T.FREG (32, f1), ()),
	    T.FMV (32, f1, T.CVTF2F (64, 32, T.FLOAD (64, T.REG (64, C.rbx), ()))),
	    T.FMV (64, f2, T.CVTI2F (64, 64, T.LOAD (64, T.REG (64, C.rbx), ()))),
	    T.FMV (64, f3, T.FADD (64, T.FREG (64, f1), T.FREG (64, f2))),
	    T.FMV (32, f1, T.CVTI2F (32, 32, T.REG (32, C.rax))),
 	    T.JMP (T.LABEL ccL, []) 
	] *)
	in
	  dumpOutput (codegen (l, stms, [raxR]))
	end

  end (* AMD64Demo *)

structure D = AMD64Demo

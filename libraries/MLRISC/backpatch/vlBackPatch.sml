(* vlBackPatch.sml -- variable length back patching. 
 *
 * Copyright 1999 by Bell Laboratories 
 *)

(* NOTE on maxIter:
 * 
 * maxIter -- is the number of times a span-dependent instruction
 *	is allowed to expand in a non-monotonic way. 
 *
 * This table shows the number of span-dependent instructions
 * whose size was over-estimated as a function of maxIter, for the
 * file Parse/parse/ml.grm.sml:
 *
 *    maxIter		# of instructions:
 *	10			687
 *	20			438
 *	30			198
 *      40			  0
 *
 * In compiling the compiler, there is no significant difference in
 * compilation speed between maxIter=10 and maxIter=40. 
 * Indeed 96% of the  files in the compiler reach a fix point within
 * 13 iterations.
 *)
functor BackPatch
  (structure CodeString : CODE_STRING
   structure Jumps      : SDI_JUMPS 
   structure Props      : INSN_PROPERTIES 
                        where I = Jumps.I
   structure Emitter    : MC_EMIT
                        where I = Props.I
   structure CFG        : CONTROL_FLOW_GRAPH
                        where I = Emitter.I
   structure Asm        : INSTRUCTION_EMITTER
                        where I = CFG.I) =
struct 
  structure I   = Jumps.I
  structure C   = I.C
  structure CFG = CFG
  structure P   = CFG.P
  structure G   = Graph
  structure W8V = Word8Vector

  datatype desc =
      BYTES of W8V.vector * desc 
    | PSEUDO of P.pseudo_op  * desc
    | SDI of I.instruction * int ref * desc
    | LABEL of Label.label * desc
    | NIL

  datatype cluster = CLUSTER of {cluster: desc}

  val maxIter = MLRiscControl.mkInt
		    ("variable-length-backpatch-iterations",
		     "number of variable-length backpath iterations")

  val _ = maxIter := 40
 
  fun error msg = MLRiscErrorMsg.error("vlBackPatch",msg)

  val clusterList : cluster list ref = ref []
  val dataList : P.pseudo_op list ref = ref []
  fun cleanUp() = (clusterList := []; dataList := [])

  fun bbsched(G.GRAPH{graph_info=CFG.INFO{data, ...}, ...}, blocks) = let
    fun bytes([], p) = p
      | bytes([s], p) = BYTES(s, p)
      | bytes(s, p) = BYTES(W8V.concat s, p)

    fun f((_, CFG.BLOCK{align, labels, insns, ...})::rest) = let
         fun instrs([], b) = bytes(rev b, f rest)
           | instrs(i::rest, b) = 
             if Jumps.isSdi i then 
               bytes(rev b, SDI(i, ref(Jumps.minSize i), instrs(rest, [])))
             else
               instrs(rest, Emitter.emitInstr(i)::b)
         fun doLabels(lab::rest) = LABEL(lab, doLabels rest)
           | doLabels [] = instrs(rev(!insns), [])
         fun alignIt(NONE) = doLabels(!labels)
	   | alignIt(SOME p) = PSEUDO(p, alignIt(NONE))
        in
          alignIt(!align)
        end 
      | f [] = NIL
  in 
    clusterList := 
      CLUSTER{cluster=f blocks}:: !clusterList;
    dataList := !data @ !dataList
  end

  
  fun finish () = let
    val iter = ref 0 (* iteration count *)
    fun labels (BYTES(s,rest), pos, chgd) = labels(rest, pos+W8V.length s, chgd)
      | labels (SDI(instr, r as ref size, rest), pos, chgd) = 
          let val s = Jumps.sdiSize(instr, Label.addrOf, pos)
              (* Allow contraction in the first two passes;
               * after which only allows expansion to ensure termination.
               *)
          in 
	      if (!iter <= !maxIter andalso s <> size) orelse s > size then
		  (r := s; labels(rest, pos + s, true))
	      else labels(rest, pos + size, chgd)
          end
      | labels (LABEL(l,rest), pos, changed) = 
        if Label.addrOf(l) = pos then labels(rest, pos, changed)
        else (Label.setAddr(l, pos); labels(rest, pos, true))
      | labels (PSEUDO(pOp, rest), pos, chgd) = let
          val oldSz = P.sizeOf(pOp, pos)
          val newSz = (P.adjustLabels(pOp, pos); P.sizeOf(pOp, pos))
        in labels(rest, pos + newSz, chgd orelse newSz<>oldSz) (* XXXX???? *)
        end
      | labels (NIL, pos, chgd) = (pos, chgd)

    fun clusterLabels clusters = let
      fun f (CLUSTER{cluster, ...}, (pos,chgd)) = labels(cluster, pos, chgd) 
    in List.foldl f (0, false) clusters
    end
      
    val nop = Props.nop()

    val loc = ref 0

    fun output v = 
      W8V.app (fn v => (CodeString.update(!loc, v); loc:= !loc+1)) v


    fun chunk(pos, []) = ()
      | chunk(pos, CLUSTER{cluster}::rest) = let
          fun outputInstr i = output (Emitter.emitInstr(nop))
          fun nops 0 = ()
            | nops n = (outputInstr(nop); nops(n-1))
          fun f(pos, BYTES(s,r)) = (output s; f(pos+W8V.length s,r))
            | f(pos, SDI(instr, ref size, r)) = let
                val emitInstrs = map (fn i => Emitter.emitInstr(i))
                val instrs = emitInstrs (Jumps.expand(instr, size, pos))
                val sum = List.foldl (fn (a,b) => (W8V.length a + b)) 0
                val n = size - sum instrs
              in
                (*
                 if n > 0 then 
                   (print ("\t\t\t Inserting " ^ Int.toString n ^ "nops\n");
                    emit instr)
                 else (); 
                *)
                app output instrs;
		nops(n);
                f(pos+size, r)
              end
            | f(pos, LABEL(lab, rest)) = 
                if pos = Label.addrOf lab then f(pos,rest)
                else error "chunk: LABEL"
            | f(pos, PSEUDO(pOp, rest)) = let
                val s : Word8.word list ref = ref []
              in
                P.emitValue{pOp=pOp, loc=pos, 
                            emit=(fn w => s :=  w :: (!s))};
                output(W8V.fromList(rev(!s)));
                f(pos + P.sizeOf(pOp, pos), rest)
              end
            | f(pos, NIL) = chunk(pos, rest)

        in f(pos, cluster)
        end

    fun fix clusters = let
      val (pos, changed) = clusterLabels clusters
    in 
      if changed then (iter := !iter + 1; fix clusters) else pos
    end

    (* 
     * Initialize labels so that they have 
     * some reasonable value to begin with.
     *)
    fun initLabels (clusters) = let
      fun init(BYTES(bytes, rest), loc) = init(rest, loc + W8V.length bytes)
	| init(PSEUDO(pOp, rest), loc) = 
	    (P.adjustLabels(pOp, loc); init(rest, loc + P.sizeOf(pOp, loc)))
	| init(SDI(sdi, size, rest), loc) = init(rest, loc + !size)
	| init(LABEL(lab, rest), loc) = 
	    (Label.setAddr(lab, loc); init(rest, loc))
	| init(NIL, loc) = loc
    in 
      List.foldl
	(fn (CLUSTER{cluster, ...}, loc) => init(cluster, loc)) 0 clusters
    end

    (* The dataList is in reverse order, and the entries in each
     * are also in reverse 
     *)
    fun compUnit(d::dl, cl, acc) = compUnit(dl, cl, PSEUDO(d, acc))
      | compUnit([], cl, acc) = let
	  fun revCl(c::cl, acc) = revCl(cl, c::acc)
	    | revCl([], acc) = acc
        in revCl(cl, [CLUSTER{cluster=acc}])
        end
      
    val compressed =  compUnit(!dataList, !clusterList, NIL) before cleanUp()
  in
    initLabels(compressed);
    CodeString.init(fix compressed);
    loc := 0; chunk(0, compressed) 
  end (* finish *)

end (* functor BackPatch *)

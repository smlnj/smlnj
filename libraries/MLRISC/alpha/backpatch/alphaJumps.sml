(* alphaJumps.sml --- information to resolve jumps. 
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)
functor AlphaJumps
  (structure Instr : ALPHAINSTR
   structure Shuffle : ALPHASHUFFLE 
			   where I = Instr
   structure MLTreeEval : MLTREE_EVAL 
			   where T = Instr.T
  ) : SDI_JUMPS = 
struct
  structure I = Instr
  structure C = I.C
  structure Const = I.Constant
  structure Eval = MLTreeEval
  structure CB   = CellsBasis

  fun error msg = MLRiscErrorMsg.error("AlphaJumps",msg)

  val branchDelayedArch = false

  fun isSdi(I.ANNOTATION{i,...}) = isSdi i
    | isSdi(I.LIVE _)		 = true
    | isSdi(I.KILL _)		 = true
    | isSdi(I.COPY _)		 = true
    | isSdi(I.INSTR i) = 
      (case i
        of (I.LDA{d=I.LABop _, ...})      => true
	 | (I.LOAD{d=I.LABop _, ...})     => true
	 | (I.STORE{d=I.LABop _, ...})    => true
	 | (I.FSTORE{d=I.LABop _, ...})   => true
	 | (I.FLOAD{d=I.LABop _, ...})    => true
	 | (I.OPERATE{rb=I.LABop _, ...}) => true
	 | (I.OPERATEV{rb=I.LABop _, ...})=> true
	 | (I.CMOVE{rb=I.LABop _, ...})   => true
	 |  _ => false
      (*esac*))

  fun minSize(I.LIVE _)		    = 0
    | minSize(I.KILL _)		    = 0
    | minSize(I.COPY _)		    = 0
    | minSize(I.ANNOTATION{i,...}) = minSize i
    | minSize _            = 4

  (* max Size is not used for the alpha span dependency analysis. *)
  fun maxSize _ = error "maxSize"

  fun immed16 n =  ~32768 <= n andalso n < 32768 
  fun im16load n = if immed16 n then 4 else 8
  fun im16Oper le = if immed16 (Eval.valueOf le) then 4 else 12

  fun immed8 n = n >= 0 andalso n < 256
  fun im8Oper le = if immed8 (Eval.valueOf le) then 4 else 12
  fun sdiSize(I.ANNOTATION{i,...},x,y) = sdiSize(i,x,y)
    | sdiSize(I.LIVE _, _, _) = 0
    | sdiSize(I.KILL _, _, _) = 0
    | sdiSize(I.COPY{k=CB.GP, src, dst, tmp, ...}, _, _) =
        4 * length(Shuffle.shuffle{tmp=tmp, dst=dst, src=src})
    | sdiSize(I.COPY{k=CB.FP, src, dst, tmp, ...}, _, _) = 
	4 * length(Shuffle.shufflefp{src=src, dst=dst, tmp=tmp})
    | sdiSize(I.INSTR instr, _, _) = 
      (case instr
	of (I.LDA{d=I.LABop le, ...}) => im16load(Eval.valueOf le)
	| (I.LOAD{d=I.LABop le, ...}) => im16Oper le
	| (I.STORE{d=I.LABop le, ...}) => im16Oper le
	| (I.FLOAD{d=I.LABop le, ...}) => im16Oper le
	| (I.FSTORE{d=I.LABop le, ...}) => im16Oper le
	| (I.OPERATE{rb=I.LABop le, ...}) => im8Oper le
	| (I.OPERATEV{rb=I.LABop le, ...}) => im8Oper le
	| (I.CMOVE{rb=I.LABop le, ...}) => im8Oper le
	|  _ => error "sdiSize"
       (*easc*))
    | sdiSize _ = error "sdiSize"

 (* NOTE: All sdis must use a dedicated physical register as a 
  * temporaries, since sdi expansion is performed after register 
  * allocation.
  *)
  val zeroR = Option.valOf(C.zeroReg CellsBasis.GP)

  fun caseSize 4 { four, twelf } = four ()
    | caseSize 12 { four, twelf } = twelf ()
    | caseSize sz _ = error ("caseSize " ^ Int.toString sz)

  fun expand(I.ANNOTATION{i,...}, size, pos) = expand(i,size,pos)
    | expand(I.LIVE _, _, _) = []
    | expand(I.KILL _, _, _) = []
    | expand(I.COPY{k=CB.GP, src, tmp, dst, ...}, _, _)  = 
       Shuffle.shuffle{src=src, dst=dst, tmp=tmp}
    | expand(I.COPY{k=CB.FP, src, tmp, dst, ...}, _, _)  = 
       Shuffle.shufflefp{src=src, dst=dst, tmp=tmp}
    | expand(I.INSTR instr, size, pos) = let
	fun load(ldClass, ldOp, r, b, d as I.LABop le, mem) = 
	    caseSize size
		{ four = fn () =>
	             [ldClass{ldOp=ldOp, r=r, b=b,
			      d=I.IMMop(Eval.valueOf le), mem=mem}],
		  twelf = fn () => let
		     val instrs = expand(I.lda{r=r, b=b, d=d}, 8, pos)
	          in instrs @ [ldClass{ldOp=ldOp, r=r, b=r,
				       d=I.IMMop 0, mem=mem}]
		  end }
	  | load _ = error "store"

	fun store(stClass, stOp, r, b, d as I.LABop le, mem) =
	    caseSize size
	        { four = fn () =>
		     [stClass{stOp=stOp, r=r, b=b,
			      d=I.IMMop(Eval.valueOf le), mem=mem}],
		  twelf = fn () => let
		     val instrs = expand(I.lda{r=C.asmTmpR, b=b, d=d}, 8, pos)
		  in instrs @ [stClass{stOp=stOp, r=r, b=C.asmTmpR,
				       d=I.IMMop 0, mem=mem}]
		  end }
	  | store _ = error "store"

	fun operate(opClass, oper, ra, rb as I.LABop le, rc) =
	    caseSize size
	        { four = fn () =>
		     [opClass{oper=oper, ra=ra,
			      rb=I.IMMop(Eval.valueOf le), rc=rc}],
		  twelf = fn () => let
		     val instrs = expand(I.lda{r=C.asmTmpR, b=zeroR, d=rb}, 8, pos)
		  in instrs @ [opClass{oper=oper, ra=ra,
				       rb=I.REGop C.asmTmpR, rc=rc}]
		  end }
	  | operate _ = error "operate"
      in

	case instr
	of I.LDA{r=rd, b=rs, d=I.LABop le} =>
	   (case size of
	      4  => [I.lda{r=rd, b=rs, d=I.LOLABop le}]
	    | 8  => [I.lda{r=rd, b=rs, d=I.LOLABop le},
		     I.ldah{r=rd, b=rd, d=I.HILABop le}]
	    | _  => error "expand:LDA"
	   )
	| I.LOAD{ldOp, r, b, d, mem} => load(I.load, ldOp, r, b, d, mem)
	| I.FLOAD{ldOp, r, b, d, mem} => load(I.fload, ldOp, r, b, d, mem)
	| I.STORE{stOp, r, b, d, mem} => store(I.store, stOp, r, b, d, mem)
	| I.FSTORE{stOp, r, b, d, mem} => store(I.fstore, stOp, r, b, d, mem)
	| I.OPERATE{oper, ra, rb, rc} => operate(I.operate, oper, ra, rb, rc)
	| I.OPERATEV{oper, ra, rb, rc} => operate(I.operatev, oper, ra, rb, rc)
	| I.CMOVE{oper, ra, rb, rc} =>
	    caseSize size
		{ four = fn () => [I.INSTR instr],
		  twelf = fn () => let
		      val instrs = expand(I.lda{r=C.asmTmpR, b=zeroR, d=rb},
					  8, pos)
		  in  instrs @ [I.cmove{oper=oper, ra=ra,
					rb=I.REGop C.asmTmpR, rc=rc}]
		  end }
	| _ => error "expand"
      end
    | expand _ = error "expand"
end



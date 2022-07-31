(* hppaJumps.sml --- information to resolve jumps. 
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)
functor HppaJumps
  ( structure Instr:HPPAINSTR
    structure Shuffle:HPPASHUFFLE 
			 where I = Instr
    structure MLTreeEval : MLTREE_EVAL 
		         where T = Instr.T
  ) : SDI_JUMPS =				
struct
  structure I = Instr
  structure C = Instr.C
  structure Const = I.Constant
  structure CB = CellsBasis

  fun error msg = MLRiscErrorMsg.error("HppaJumps",msg)

  val branchDelayedArch = false

  fun minSize(I.INSTR(I.FBRANCH _)) = 12 (* FCMP/FTEST/B *)
    | minSize(I.INSTR(I.BLR{labs,...})) = 8 + 8 * length labs (* FCMP/FTEST/B *)
    | minSize(I.ANNOTATION{i,...}) = minSize i
    | minSize(I.LIVE _)  = 0
    | minSize(I.KILL _)  = 0
    | minSize(I.COPY _)  = 0
    | minSize(I.INSTR(I.COMCLR_LDO _)) = 8
    | minSize(I.INSTR(I.COMICLR_LDO _)) = 8
    | minSize _          = 4
  
  fun maxSize (I.INSTR(I.BCOND _))  = 16  (* BCOND+LONGJUMP *)
    | maxSize (I.INSTR(I.BCONDI _)) = 16  (* BCONDI+LONGJUMP *)
    | maxSize (I.INSTR(I.BB _))     = 16  (* BB+LONGJUMP *)
    | maxSize (I.INSTR(I.B _))	    = 12  (* LONGJUMP *)
    | maxSize (I.INSTR(I.FBRANCH _))= 20
    | maxSize (I.ANNOTATION{i,...}) = maxSize i
    | maxSize _			    = 4

  fun isSdi(I.ANNOTATION{i,...})  = isSdi i
    | isSdi(I.LIVE _)		  = true
    | isSdi(I.KILL _)		  = true
    | isSdi(I.COPY _)		  = true  
    | isSdi(I.INSTR instr) = let

	fun opnd (I.LabExp _) = true
	  | opnd _ = false
      in
	case instr
	of I.BCOND _		=> true
	 | I.BCONDI _		=> true
	 | I.BB _		=> true
	 | I.B _		=> true
	 | I.FBRANCH _		=> true
	 | I.BLR _              => true
	 | I.LDO{i, ...}	=> opnd i
	 | I.STORE{d, ...}	=> opnd d
	 | I.ARITHI{i, ...}	=> opnd i
	 | I.LOADI{i, ...}      => opnd i
	 | I.COMICLR_LDO{i1, ...} => opnd i1
	 | _			=> false
      end

  fun im11 n = ~1024 <= n andalso n < 1024
  fun im12 n = ~2048 <= n andalso n < 2048
  fun im14 n = ~8192 <= n andalso n < 8192
  fun im17 n = ~65536 <= n andalso n < 65536

  fun sdiSize(I.ANNOTATION{i, ...}, labMap, loc) = sdiSize(i, labMap, loc)
    | sdiSize(I.LIVE _, _, _) = 0
    | sdiSize(I.KILL _, _, _) = 0
    | sdiSize(I.COPY{k=CB.GP, dst, src, tmp, ...}, _, _) = 
        4 * length(Shuffle.shuffle{tmp=tmp, dst=dst, src=src})
    | sdiSize(I.COPY{k=CB.FP, dst, src, tmp, ...}, _, _) = 
        4 * length(Shuffle.shufflefp{tmp=tmp, dst=dst, src=src})
    | sdiSize(I.INSTR(instr), labMap, loc) = let
	fun branchOffset lab = ((labMap lab) - loc - 8) div 4
	fun branch(lab,nop) = let
	  val offset = branchOffset lab
	in
	   if im12 offset then 
	      if nop then 8 else 4 
	   else if im17 offset then 8 else 16
	end
	fun memDisp(c, short, long) = if im14(c) then short else long
      in
	case instr 
	 of I.LDO{i=I.LabExp(lexp, _), ...} => memDisp(MLTreeEval.valueOf lexp, 4, 12)
	  | I.LOADI{i=I.LabExp(lexp, _), ...} => memDisp(MLTreeEval.valueOf lexp, 4, 12)
	  | I.STORE{d=I.LabExp(lexp, _), ...} => memDisp(MLTreeEval.valueOf lexp, 4, 12)
	  | I.COMICLR_LDO{i1=I.LabExp(lexp,_), ...} =>
	      if im11(MLTreeEval.valueOf lexp) then 8 else 16
	  | I.ARITHI{ai, i=I.LabExp(lexp,_), ...} => let
	      fun arithImmed() = if im11(MLTreeEval.valueOf lexp) then 4 else 12
	    in
	      case ai
	      of I.ADDI => arithImmed()
	       | I.ADDIO => arithImmed()
	       | I.SUBI => arithImmed()
	       | I.SUBIO => arithImmed()
	       | _ => error "sdiSize: ARITHI LabelExp"
	     (*esac*)
	    end
	  | I.BCOND{t, nop, ...}   => branch(t,nop)
	  | I.BCONDI{t, nop, ...}  => branch(t,nop)
	  | I.BB{t, nop, ...}      => branch(t,nop)
	  | I.B{lab, ...}          => if im17 (branchOffset lab) then 4 else 12
	  | I.FBRANCH{t, ...}      => if im17 (branchOffset t) then 12 else 20
	  | I.BLR{labs,...} => let
	      val l = length labs * 8
	      fun badOffsets(t::ts,n) =
		    not(im17(branchOffset t + n)) orelse badOffsets(ts,n+2)
		| badOffsets([],n) = false
	    in l + (if badOffsets(labs,2) then 20 else 8) 
	    end
	  | _  => error "sdiSize"
      end
    | sdiSize _ = error "SdiSize"

   (* Note: A better sequence would be to use ADDIL, however
    * the expansion is done after register allocation and
    * ADDIL defines %r1. 
    *)

   (*
  fun longJump{lab, n} = let
    val baseDisp =  LE.MINUS(LE.LABEL lab, LE.INT 8192)
    val labOpnd = (baseDisp, I.T)
    val baseptrR = 8
  in
    [I.LDIL{i=I.HILabExp labOpnd, t=C.asmTmpR},
     I.LDO{i=I.LOLabExp labOpnd, b=C.asmTmpR, t=C.asmTmpR},
     I.ARITH{a=I.ADD, r1=baseptrR, r2=C.asmTmpR, t=C.asmTmpR},
     I.BV{x=0, labs=[lab], b=C.asmTmpR, n=n}]
  end
    *)

  fun longJump{lab, n} =
    (print "longJump used\n";
     [I.longjump{lab=lab, tmpLab=Label.anon(), n=n, tmp=C.asmTmpR}]
    )

  fun split11 n = let
    val w = Word.fromInt(n)
  in (Word.toIntX(Word.~>>(w, 0w11)), Word.toIntX(Word.andb(w, 0wx7ff)))
  end

  fun split11X n = let
    val w = Word.fromInt(n)
    val hi' = Word.~>>(w, 0w11)
    val lo' = Word.andb(w, 0wx7ff)
    val (hi,lo) = 
      if Word.<=(lo', 0wx3ff) then (hi', lo') else (hi'+0w1, lo'-0wx800)
  in (Word.toIntX hi, Word.toIntX lo)
  end

  fun loadIndexed I.LDW = I.LDWX
    | loadIndexed I.LDH = I.LDHX
    | loadIndexed I.LDB = I.LDBX

  fun expand(I.ANNOTATION{i,...},size,pos) = expand(i,size,pos)
    | expand(I.LIVE _, _, _) = []
    | expand(I.KILL _, _, _) = []
    | expand(I.COPY{k=CB.GP, dst, src, tmp, ...}, _, _) = 
        Shuffle.shuffle{tmp=tmp, dst=dst, src=src}
    | expand(I.COPY{k=CB.FP, dst, src, tmp, ...}, _, _) = 
        Shuffle.shufflefp{tmp=tmp, dst=dst, src=src}
    | expand(instr as I.INSTR(i), size, pos) =
      (case i
        of I.LDO{i=I.LabExp lexp, t, b} =>
           (case size 
	    of 4 => [instr]
	     | 12 => [I.ldil{i=I.HILabExp lexp, t=C.asmTmpR},
		      I.ldo{i=I.LOLabExp lexp, b=C.asmTmpR, t=C.asmTmpR},
		      I.arith{a=I.ADD, r1=C.asmTmpR, r2=b, t=t}]
	     | _ => error "LDO"
	  (*esac*))
	 | I.COMICLR_LDO{cc, i1=I.LabExp lexp, r2, t1, b, i2, t2} =>
	   (case size 
	    of 8 => [instr]
	     | 16 => 
		[I.ldil{i=I.HILabExp lexp, t=C.asmTmpR},
		 I.ldo{i=I.LOLabExp lexp, b=C.asmTmpR, t=C.asmTmpR},
		 I.comclr_ldo{cc=cc, r1=C.asmTmpR, r2=r2, t1=t1, 
			      b=b, i=i2, t2=t2} 
		]
	     | _ => error "COMICLR_LDO"
	  (*esac*))
	 | I.STORE{st, d as I.LabExp lexp, b, r, mem} =>
	  (case size 
	   of 4 => [instr]
	    | 12 =>
		[I.ldil{i=I.HILabExp lexp, t=C.asmTmpR},
		 I.arith{a=I.ADD, r1=C.asmTmpR, r2=b, t=C.asmTmpR},
		 I.store{st=st, b=C.asmTmpR, d=I.LOLabExp lexp, r=r, mem=mem}]
	    | _ => error "STORE"
	  (*esac*))
	 | I.STORE _ => error "expand:STORE" 
	 | I.ARITHI{ai, r, i=I.LabExp lexp, t} =>
	   (case size
	    of 4 => [instr]
	     | 12 => 
	      (* Note: A better sequence would be to use ADDIL, however
	       * the expansion is done after register allocation and
	       * ADDIL defines %r1. 
	       *)
		 [I.ldil{i=I.HILabExp lexp, t=C.asmTmpR},
		  I.ldo{i=I.LOLabExp lexp, b=C.asmTmpR, t=C.asmTmpR},
		  I.arith{
		     a = case ai of I.ADDI => I.ADD | I.ADDIO => I.ADDO
				  | I.SUBI => I.SUB | I.SUBIO => I.SUBO
				  | _ => error "expand: I.ARITHI LabExp",
		     t=t,
		     r1=C.asmTmpR,
		     r2=r}]
	     | _ => error "ARITHI"
	   (*esac*))
	 | I.LOADI{li, r, i=I.LabExp lexp, t, mem} =>
	   (case size
	    of 4  => [instr]
	     | 12 => [I.ldil{i=I.HILabExp lexp, t=C.asmTmpR},
		      I.arith{a=I.ADD, r1=C.asmTmpR, r2=r, t=C.asmTmpR},
		      I.loadi{li=li, r=C.asmTmpR, i=I.LOLabExp lexp, t=t, mem=mem}]
	     | _ => error "LOADI"
	   (*esac*))
	 | I.BCOND{cmp,bc, t, f, r1, r2, n, nop} => let
	     fun rev I.COMBT=I.bcond{cmp=I.COMBF,bc=bc,t=f,f=f,r1=r1,r2=r2,n=true,nop=false}
	       | rev I.COMBF=I.bcond{cmp=I.COMBT,bc=bc,t=f,f=f,r1=r1,r2=r2,n=true,nop=false}
	   in
	     case (size,nop)
	     of (4,false) => [instr]
	      | (8,true)  => [instr]
	      | (8,_)     => [rev cmp, I.b{lab=t, n=n}]
	      | (16,_)    => rev cmp :: longJump{lab=t, n=n}
	      | _ => error "BCOND"
	     (*esac*)
	   end
	 | I.BCONDI{cmpi, bc, t, f, i, r2, n, nop} => let
	     fun rev I.COMIBT=I.bcondi{cmpi=I.COMIBF,bc=bc,i=i,r2=r2,t=f,f=f,n=true,nop=false}
	       | rev I.COMIBF=I.bcondi{cmpi=I.COMIBT,bc=bc,i=i,r2=r2,t=f,f=f,n=true,nop=false}
	   in
	     case (size,nop)
	       of (4,false) => [instr]
		| (8,true) => [instr]
		| (8,_) => [rev cmpi, I.b{lab=t, n=n}]
		| (16,_) => rev cmpi :: longJump{lab=t, n=n}
		| _ => error "BCONDI"
	     (*esac*)
	   end
	 | I.BB{bc, r, p, t, f, n, nop} => let
	     fun rev I.BSET = I.bb{bc=I.BCLR,r=r,p=p,t=f,f=f,n=true,nop=false}
	       | rev I.BCLR = I.bb{bc=I.BSET,r=r,p=p,t=f,f=f,n=true,nop=false}
	   in case (size,nop) of
		(4,false) => [instr] 
	      | (8,true) => [instr] 
	      | (8,_) => [rev bc, I.b{lab=t,n=n}] 
	      | (16,_) => rev bc :: longJump{lab=t, n=n}
	      | _ => error "BB"
	   end    
	 |I.B{lab=lab, n=n} =>
	   (case size 
	     of 4 => [instr]
	      | 12 => longJump{lab=lab, n=n}
	      | _ => error "B"
	   (*esac*))
	 | I.FBRANCH{t, f, n, ...} =>
	   (case size 
	     of 12 => [instr]
	      | 20 => 
		  (* lets hope this sequence never gets generated sequence:
			     FTEST
			     allways trapping instruction
			     B (f)
			     longJmp
		   *)
		     error "FBRANCH(20)"
	      | _ => error "FBRANCH"
	   (*esac*))
	 | I.BLR{labs,n,t,x,...} =>
	    (if size = 8 + 8 * length labs then
		I.blr{labs=[],n=n,t=t,x=x}::
		I.nop::
		foldr (fn (l,is) => I.b{lab=l,n=true}::I.nop::is) [] labs
	     else error "BLR"
	    )
	 | _ => error "expand")
    | expand _ = error "expand"
end


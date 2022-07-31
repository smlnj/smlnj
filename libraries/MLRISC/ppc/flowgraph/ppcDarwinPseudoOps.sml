(* ppcDarwinPseudoOps.sml
 *
 * COPYRIGHT (c) 2002 Bell labs, Lucent Technologies.
 *
 * PPC/Darwin (aka MacOS X) pseudo operations.
 *)

functor PPCDarwinPseudoOps (
    structure T : MLTREE
    structure MLTreeEval : MLTREE_EVAL  where T = T
  ) : PSEUDO_OPS_BASIS = struct

    structure T = T
    structure PB = PseudoOpsBasisTyp
    structure Fmt = Format

    structure Endian = PseudoOpsBig (
      structure T = T
      structure MLTreeEval=MLTreeEval
      val icache_alignment = 16
      val max_alignment = SOME 7
      val nop = {sz=4, en=0wx60000000: Word32.word})  (* FIX:: ori 0, 0, 0 *)

(* EXPAND
    structure GasPseudoOps = 
     GasPseudoOps(structure T = T
		  val labFmt = {gPrefix="", aPrefix="L"})
*)

    type 'a pseudo_op = (T.labexp, 'a) PB.pseudo_op
  
    fun error msg = MLRiscErrorMsg.error ("PPCDarwinPseudoOps.", msg)

    val sizeOf = Endian.sizeOf
    val emitValue = Endian.emitValue

    val labelToString = Label.fmt {gPrefix="", aPrefix="L"}

    fun prIntInf i =
	  if IntInf.sign i < 0 then "-"^IntInf.toString(IntInf.~ i) 
	  else IntInf.toString i

    fun prInt i = if i < 0 then "-"^Int.toString(~i) else Int.toString i

  (* operator precedences:
     Note: these differ from C's precedences
	  2 MULT, DIV, LSHIFT, RSHIFT
	  1 AND, OR
	  0 PLUS, MINUS
  *)

    fun parens (str, prec, op_prec) = 
	  if prec > op_prec then "(" ^ str ^ ")" else str

    fun lexpToString le = toStr(le, 0)

    and toStr(T.LABEL lab, _) = labelToString lab 
      | toStr(T.LABEXP le, p) = toStr(le, p)
      | toStr(T.CONST c, _) = 
          (prInt(T.Constant.valueOf c) handle _ => T.Constant.toString c)
      | toStr(T.LI i, _) = prIntInf i
      | toStr(T.MULS(_,lexp1, lexp2), _) = toStr(lexp1, 2) ^ "*" ^ toStr(lexp2,2)
      | toStr(T.DIVS(T.DIV_TO_ZERO,_,lexp1, lexp2), _) =
	                    toStr(lexp1, 2) ^ "/" ^ toStr(lexp2,2)
      | toStr(T.SLL(_,lexp, cnt), prec) = toStr(lexp,2) ^ "<<" ^ toStr(cnt,2)
      | toStr(T.SRL(_,lexp, cnt), prec) = toStr(lexp,2) ^ ">>" ^ toStr(cnt,2)
      | toStr(T.ANDB(_,lexp, mask), prec) = 
          parens(toStr(lexp,1) ^ "&" ^ toStr(mask, 1), prec, 1)
      | toStr(T.ORB(_,lexp, mask), prec) = 
          parens(toStr(lexp, 1) ^ "|" ^ toStr(mask, 1), prec, 1)
      | toStr(T.ADD(_,lexp1, lexp2), prec) = 
          parens(toStr(lexp1, 0) ^ "+" ^ toStr(lexp2, 0), prec, 0)
      | toStr(T.SUB(_,lexp1, lexp2), prec) = 
          parens(toStr(lexp1, 0) ^ "-" ^ toStr(lexp2, 0), prec, 0)
      | toStr _ = error "toStr"

    fun decls (fmt, labs) =
      String.concat 
        (map (fn lab => (Fmt.format fmt [Fmt.STR (lexpToString(T.LABEL lab))])) labs)

    fun toString(PB.ALIGN_SZ n)     = Fmt.format "\t.align\t%d" [Fmt.INT n]
      | toString(PB.ALIGN_ENTRY)    = "\t.align\t4"	(* 16 byte boundary *)
      | toString(PB.ALIGN_LABEL)    = "\t.align\t2"
  
      | toString(PB.DATA_LABEL lab) = labelToString lab ^ ":"
      | toString(PB.DATA_READ_ONLY) = "\t.const_data"
      | toString(PB.DATA)           = "\t.data"
      | toString(PB.BSS)	    = "\t.section\t__DATA,__BSS"
      | toString(PB.TEXT)           = "\t.text"
      | toString(PB.SECTION at)     = "\t.section\t" ^ Atom.toString at
      | toString(PB.REORDER)        = ""
      | toString(PB.NOREORDER)      = ""
      | toString(PB.INT{sz, i})     = let
	  fun join [] = []
	    | join [lexp] = [lexpToString lexp]
	    | join (lexp::r) = lexpToString lexp :: "," :: join r
	  val pop = (case sz
	         of 8 => "\t.byte\t"
		  | 16 => "\t.short\t"
		  | 32 => "\t.long\t"
		  | 64 => error "INT64"
		  | _ => error ("pop: INT sz = " ^ Int.toString sz)
	        (* end case *))
	  in
	    String.concat (pop :: join i)
	  end
      | toString(PB.ASCII s)        =
	  Fmt.format "\t.ascii\t\"%s\"" [Fmt.STR(String.toCString s)]
      | toString(PB.ASCIIZ s)       = 
          Fmt.format "\t.asciz \"%s\"" [Fmt.STR(String.toCString s)]
      | toString(PB.SPACE sz)	  = Fmt.format "\t.space\t%d" [Fmt.INT sz]
      | toString(PB.FLOAT{sz, f})   = let
	  fun join [] = []
	    | join [f] = [f]
	    | join (f::r) = f :: "," :: join r
	  val pop = (case sz
	         of 32 => "\t.single "
		  | 64 => "\t.double "
		  | _ => error ("pop: FLOAT sz = " ^ Int.toString sz)
	        (* end case *))
	  in
	    String.concat (pop :: join f)
	  end
      | toString(PB.IMPORT labs) = decls("\t.extern\t%s", labs)
      | toString(PB.EXPORT labs) = decls("\t.globl\t%s", labs)
      | toString(PB.COMMENT txt) = Fmt.format "; %s" [Fmt.STR txt]
      | toString(PB.EXT _) = error "EXT"

    fun defineLabel lab = labelToString lab ^ ":"

    val wordSize = 32
  end

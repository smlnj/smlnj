(* gasPseudoOps.sml
 *
 * COPYRIGHT (c) 2006 The Fellowship of SML/NJ (www.smlnj.org)
 * All rights reserved.
 *
 * Implements the string related functions to emit pseudo-ops
 * in the standard GAS syntax (see http://www.gnu.org/software/binutils/manual/gas-2.9.1/)
 *)

functor GasPseudoOps (
    structure T : MLTREE
    val labFmt : {gPrefix: string, aPrefix: string}
  ) : AS_PSEUDO_OPS =
  struct
    structure T = T
    structure PB = PseudoOpsBasisTyp
    structure Fmt = Format

    fun error msg = MLRiscErrorMsg.error ("GasPseudoOps.", msg)

    fun prIntInf i = if IntInf.sign i < 0 then "-"^IntInf.toString(IntInf.~ i) 
                     else IntInf.toString i

    fun prInt i = if i < 0 then "-"^Int.toString(~i) else Int.toString i

    (* operator precedences:
       Note: these differ from C's precedences
	    3 NEG, NOTB
	    2 MULS, DIVS, SLL, SRA
	    1 ANDB, ORB, XORB
	    0 PLUS, MINUS
    *)

  (* NOTE: we use ">=" here instead of ">" so that we don't have to worry about associativity *)
    fun parens (str, prec, op_prec) = 
	if prec >= op_prec then "(" ^ str ^ ")" else str

    fun lexpToString le = toStr(le, 0)

    and toStr(T.LABEL lab, _) = Label.fmt labFmt lab 
      | toStr(T.LABEXP le, p) = toStr(le, p)
      | toStr(T.NEG(_, T.CONST c), _) =
          (prInt(~(T.Constant.valueOf c)) handle _ => "-"^T.Constant.toString c)
      | toStr(T.NEG(_, T.LI i), _) = prIntInf(~i)
      | toStr(T.NEG(_, lexp), prec) = "-" ^ parens(toStr(lexp, 3), prec, 3)
      | toStr(T.NOTB(_, lexp), prec) = "~" ^ parens(toStr(lexp, 3), prec, 3)
      | toStr(T.CONST c, _) = 
          (prInt(T.Constant.valueOf c) handle _ => T.Constant.toString c)
      | toStr(T.LI i, _) = prIntInf i
      | toStr(T.MULS(_,lexp1, lexp2), prec) =
	  parens(toStr(lexp1, 2) ^ "*" ^ toStr(lexp2,2), prec, 2)
      | toStr(T.DIVS(T.DIV_TO_ZERO, _, lexp1, lexp2), prec) =
	  parens(toStr(lexp1, 2) ^ "/" ^ toStr(lexp2,2), prec, 2)
      | toStr(T.SLL(_,lexp, cnt), prec) =
	  parens(toStr(lexp,2) ^ "<<" ^ toStr(cnt,2), prec, 2)
      | toStr(T.SRA(_,lexp, cnt), prec) =
	  parens(toStr(lexp,2) ^ ">>" ^ toStr(cnt,2), prec, 2)
      | toStr(T.ANDB(_,lexp, mask), prec) = 
          parens(toStr(lexp,1) ^ "&" ^ toStr(mask, 1), prec, 1)
      | toStr(T.ORB(_,lexp, mask), prec) = 
          parens(toStr(lexp, 1) ^ "|" ^ toStr(mask, 1), prec, 1)
      | toStr(T.XORB(_,lexp, mask), prec) = 
          parens(toStr(lexp, 1) ^ "^" ^ toStr(mask, 1), prec, 1)
      | toStr(T.ADD(_,lexp1, lexp2), prec) = 
          parens(toStr(lexp1, 0) ^ "+" ^ toStr(lexp2, 0), prec, 0)
      | toStr(T.SUB(_,lexp1, lexp2), prec) = 
          parens(toStr(lexp1, 0) ^ "-" ^ toStr(lexp2, 0), prec, 0)
      | toStr _ = error "toStr"

    fun defineLabel lab = lexpToString (T.LABEL lab) ^ ":"

    fun decls (fmt, labs) =
	  String.concat 
	    (map (fn lab => (Fmt.format fmt [Fmt.STR (lexpToString(T.LABEL lab))])) labs)

    (* if i is a power of two, return the lg of i, otherwise return nothing *)
    fun lg i = let
        fun loop (0, k) = SOME k
	  | loop (i, k) = if i mod 2 = 0
            then loop (i div 2, k+1)
	    else NONE
        in
	   loop (i, 0)
        end

    fun toString(PB.ALIGN_SZ n)     = 
        (* always favor p2align because it is guaranteed to be consistent across assemblers *)
        (case lg n
         of NONE => Fmt.format "\t.align\t%d" [Fmt.INT n]
	  | SOME k => Fmt.format "\t.p2align\t%d" [Fmt.INT k]
        (* end case *))
      | toString(PB.ALIGN_ENTRY)    = "\t.align\t4"	(* 16 byte boundary *)
      | toString(PB.ALIGN_LABEL)    = "\t.p2align\t4,,7"

      | toString(PB.DATA_LABEL lab) = Label.fmt labFmt lab ^ ":"
      | toString(PB.DATA_READ_ONLY) = "\t.section\t.rodata"
      | toString(PB.DATA)	    = "\t.data"
      | toString(PB.BSS)	    = "\t.section\t.bss"
      | toString(PB.TEXT)	    = "\t.text"
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
		  | 64 => "\t.quad\t"
		  | n => error ("unexpected INT size: " ^ Int.toString n)
		(* end case *))
	  in
	    String.concat (pop :: join i)
	  end

      | toString(PB.ASCII s)        =
	  Fmt.format "\t.ascii\t\"%s\"" [Fmt.STR(String.toCString s)]
      | toString(PB.ASCIIZ s)       = 
          Fmt.format "\t.asciz\t\"%s\"" [Fmt.STR(String.toCString s)]

      | toString(PB.SPACE sz)	  = Fmt.format "\t.space\t%d" [Fmt.INT sz]

      | toString(PB.FLOAT{sz, f})   = let
	  fun join [] = []
	    | join [f] = [f]
	    | join (f::r) = f :: "," :: join r
	  val pop = (case sz
		 of 32 => "\t.single "
		  | 64 => "\t.double "
		  | 128 => "\t.extended "
		  | n => error ("unexpected FLOAT size: " ^ Int.toString n)
		(* end case *))
	  in
	    String.concat (pop :: join f)
	  end

      | toString(PB.IMPORT labs) = decls("\t.extern\t%s", labs)
      | toString(PB.EXPORT labs) = decls("\t.global\t%s", labs)
      | toString(PB.COMMENT txt) = Fmt.format "/* %s */" [Fmt.STR txt]
      | toString(PB.EXT _) = error "EXT"

  end

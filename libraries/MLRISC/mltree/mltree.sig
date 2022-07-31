(* mltree.sig
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

signature MLTREE = sig
  structure Constant    : CONSTANT
  structure Region      : REGION
(*  structure Stream      : INSTRUCTION_STREAM *)
  structure Basis       : MLTREE_BASIS
  structure Extension   : MLTREE_EXTENSION
  structure I           : MACHINE_INT

  type ty  = Basis.ty
  type fty = Basis.fty
  type var = CellsBasis.cell (* variable *)
  type src = var (* source variable *)
  type dst = var (* destination variable *)
  type reg = var (* physical register *)
  type an  = Annotations.annotation

  datatype cond = datatype Basis.cond
  datatype fcond = datatype Basis.fcond
  datatype rounding_mode = datatype Basis.rounding_mode
  datatype div_rounding_mode = datatype Basis.div_rounding_mode
  datatype ext = datatype Basis.ext

  (* Statements/effects.  These types are parameterized by the statement
   * extension type.  Unfortunately, this has to be made polymorphic to make
   * it possible for recursive type definitions to work.
   *)
  datatype stm =
      (* assignment *)
      MV      of ty * dst * rexp
    | CCMV    of dst * ccexp
    | FMV     of fty * dst * fexp

      (* parallel copies *)
    | COPY    of ty * dst list * src list
    | FCOPY   of fty * dst list * src list

      (* control flow *)
    | JMP     of rexp * controlflow
(*
    | SWITCH  of
        {tblLab: Label.label,		 (* label associated with table *)
	 base : rexp option,		 (* base pointer -- if any *)
	 table : fn Label.label -> rexp, (* get table address *)
	 index : rexp,			 (* index into table  *)
	 targets : controlflow}		 (* targets of switch *)
*)
    | BCC     of ccexp * Label.label
    | CALL    of {funct:rexp, targets:controlflow,
                  defs:mlrisc list, uses:mlrisc list,
                  region: Region.region,
		  pops: Int32.int}
    | FLOW_TO of stm * controlflow
    | RET     of controlflow
    | IF      of ccexp * stm * stm

      (* memory update: ea, data *)
    | STORE  of ty * rexp * rexp * Region.region
    | FSTORE of fty * rexp * fexp * Region.region

      (* control dependence *)
    | REGION of stm * ctrl

    | SEQ    of stm list   (* sequencing *)
    | DEFINE of Label.label   (* define local label *)

    | ANNOTATION of stm * an
    | EXT of sext  (* extension *)

      (* synthetic instructions to indicated that the regs are live or
       * killed at this program point. The spilled list must always
       * start out as the empty list.
       *)
    | LIVE of mlrisc list
    | KILL of mlrisc list

      (* RTL operators:
       * The following are used internally
       * for describing instruction semantics.
       * The frontend must not use these.
       *)
    | PHI    of {preds:int list,block:int}
    | ASSIGN of ty * rexp * rexp
    | SOURCE
    | SINK
    | RTL    of {hash:word, attribs:Basis.attribs ref, e:stm}

  and rexp =
      REG    of ty * reg

      (* sizes of constants are inferred by context *)
    | LI     of I.machine_int
    | LABEL  of Label.label
    | CONST  of Constant.const
    | LABEXP of rexp

    | NEG    of ty * rexp
    | ADD    of ty * rexp * rexp
    | SUB    of ty * rexp * rexp

      (* signed multiplication etc. *)
    | MULS   of ty * rexp * rexp
    | DIVS   of div_rounding_mode * ty * rexp * rexp
    | REMS   of div_rounding_mode * ty * rexp * rexp

      (* unsigned multiplication etc. *)
    | MULU   of ty * rexp * rexp
    | DIVU   of ty * rexp * rexp
    | REMU   of ty * rexp * rexp

      (* overflow-trapping versions of above. These are all signed *)
    | NEGT   of ty * rexp
    | ADDT   of ty * rexp * rexp
    | SUBT   of ty * rexp * rexp
    | MULT   of ty * rexp * rexp
    | DIVT   of div_rounding_mode * ty * rexp * rexp
    (* there is no REMT because remainder never overflows *)

      (* bit operations *)
    | ANDB   of ty * rexp * rexp
    | ORB    of ty * rexp * rexp
    | XORB   of ty * rexp * rexp
    | EQVB   of ty * rexp * rexp
    | NOTB   of ty * rexp

    | SRA    of ty * rexp * rexp    (* value, shift *)
    | SRL    of ty * rexp * rexp
    | SLL    of ty * rexp * rexp

      (* type promotion/conversion *)
    | SX     of ty * ty * rexp  (* toTy, fromTy *)
    | ZX     of ty * ty * rexp  (* toTy, fromTy *)
    | CVTF2I of ty * rounding_mode * fty * fexp

      (*
       * COND(ty,cc,e1,e2):
       * Evaluate into either e1 or e2, depending on cc.
       * Both e1 and e2 are allowed to be evaluated eagerly.
       *)
    | COND of ty * ccexp * rexp * rexp

      (* integer load *)
    | LOAD of ty * rexp * Region.region

      (* predication *)
    | PRED of rexp * ctrl

    | LET of stm * rexp

    | REXT of ty * rext

    | MARK of rexp * an

    | OP    of ty * oper * rexp list
    | ARG   of ty * rep ref * string
    | $     of ty * CellsBasis.cellkind * rexp
    | PARAM of int
    | BITSLICE of ty * (int * int) list * rexp
    | ???

  and rep  = REP of string

  and oper = OPER of Basis.misc_op

  and fexp =
      FREG   of fty * src
    | FLOAD  of fty * rexp * Region.region

    | FADD   of fty * fexp * fexp
    | FMUL   of fty * fexp * fexp
    | FSUB   of fty * fexp * fexp
    | FDIV   of fty * fexp * fexp
    | FABS   of fty * fexp
    | FNEG   of fty * fexp
    | FSQRT  of fty * fexp
    | FCOND  of fty * ccexp *
                fexp * fexp
    | FCOPYSIGN of fty * fexp (*sign*) * fexp (*magnitude*)

    | CVTI2F of fty * ty * rexp  (* from signed integer *)
    | CVTF2F of fty * fty * fexp (* float to float conversion *)

    | FPRED of fexp * ctrl

    | FEXT of fty * fext

    | FMARK of fexp * an

  and ccexp =
      CC     of Basis.cond * src
    | FCC    of Basis.fcond * src
    | TRUE
    | FALSE
    | NOT    of ccexp
    | AND    of ccexp * ccexp
    | OR     of ccexp * ccexp
    | XOR    of ccexp * ccexp
    | EQV    of ccexp * ccexp
    | CMP    of ty * Basis.cond * rexp * rexp
    | FCMP   of fty * Basis.fcond * fexp * fexp
    | CCMARK of ccexp * an
    | CCEXT  of ty * ccext

  and mlrisc =
      CCR of ccexp
    | GPR of rexp
    | FPR of fexp

  withtype controlflow = Label.label list (* control flow info *)
       and ctrl   = var                   (* control dependence info *)
       and ctrls  = ctrl list
       and sext   = (stm, rexp, fexp, ccexp) Extension.sx
       and rext   = (stm, rexp, fexp, ccexp) Extension.rx
       and fext   = (stm, rexp, fexp, ccexp) Extension.fx
       and ccext  = (stm, rexp, fexp, ccexp) Extension.ccx
       and labexp = rexp
  (*
   * Useful type abbreviations for working for MLTree.
   *)
  type rewriter =  (* rewriting functions *)
    {stm:stm->stm, rexp:rexp->rexp, fexp:fexp->fexp, ccexp:ccexp->ccexp}
  type 'a folder = (* aggregation functions *)
    {stm:stm*'a->'a, rexp:rexp*'a->'a, fexp:fexp*'a->'a, ccexp:ccexp*'a->'a}
  type hasher =    (* hashing functions *)
    {stm:stm->word, rexp:rexp->word, fexp:fexp->word, ccexp:ccexp->word}
  type equality =  (* comparison functions *)
    {stm:stm * stm->bool, rexp:rexp * rexp->bool,
     fexp:fexp * fexp->bool, ccexp:ccexp * ccexp->bool}
  type printer =   (* pretty printing functions *)
    {stm:stm->string, rexp:rexp->string, fexp:fexp->string, ccexp:ccexp->string,
     dstReg : ty * var -> string, srcReg : ty * var -> string}

end (* MLTREE *)

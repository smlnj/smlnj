(* primop-util.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Various utility definitions and functions for primops
 *)

structure PrimopUtil : sig

    val IADD : Primop.primop  (* default integer addition *)
    val ISUB : Primop.primop  (* default integer subtraction *)
    val IMUL : Primop.primop
    val IDIV : Primop.primop
    val INEG : Primop.primop

    val FEQLd : Primop.primop
    val IEQL : Primop.primop
    val INEQ : Primop.primop
    val IGT : Primop.primop
    val ILT : Primop.primop
    val ILE : Primop.primop
    val IGE : Primop.primop

  (** default word arithmetic and comparison operators *)
    val UADD : Primop.primop
    val UIEQL : Primop.primop  (* for UINT kind, may not matter *)

    val mkIEQL : int -> Primop.primop   (* make equality primop for other sizes *)
    val mkUIEQL : int -> Primop.primop  (* and for unsigned (kind = UINT) *)

    val prNumkind : Primop.numkind -> string
    val toString: Primop.primop -> string

  (* This should return more than just a boolean.
   * True means "can not be dead-code eliminated" *)
    val effect : Primop.primop -> bool

  end = struct

    structure P = Primop

    val defaultIntKind = P.INT Target.defaultIntSz  (* 31 or 63, depending on Target.is64 *)
    val defaultUIntKind = P.UINT Target.defaultIntSz  (* 31 or 63, depending on Target.is64 *)

  (** default integer arithmetic and comparison operators *)
    val IADD = P.IARITH{oper=P.IADD, sz=Target.defaultIntSz}
    val ISUB = P.IARITH{oper=P.ISUB, sz=Target.defaultIntSz}
    val IMUL = P.IARITH{oper=P.IMUL, sz=Target.defaultIntSz}
    val IDIV = P.INLDIV defaultIntKind
    val INEG = P.IARITH{oper=P.INEG, sz=Target.defaultIntSz}

    val IEQL = P.CMP{oper=P.EQL, kind=defaultIntKind}
    val INEQ = P.CMP{oper=P.NEQ, kind=defaultIntKind}
    val IGT  = P.CMP{oper=P.GT,  kind=defaultIntKind}
    val ILT  = P.CMP{oper=P.LT,  kind=defaultIntKind}
    val IGE  = P.CMP{oper=P.GTE, kind=defaultIntKind}
    val ILE  = P.CMP{oper=P.LTE, kind=defaultIntKind}

  (** default word arithmetic and comparison operators *)
    val UADD = P.PURE_ARITH{oper=P.ADD, kind=defaultUIntKind}
    val UIEQL = P.CMP{oper=P.EQL, kind=defaultUIntKind}

    fun mkIEQL size = P.CMP{oper=P.EQL, kind=P.INT size}
    fun mkUIEQL size = P.CMP{oper=P.EQL, kind=P.UINT size}

  (** default floating-point equality operator *)
    val FEQLd = P.CMP{oper=P.EQL, kind=P.FLOAT 64}

(**************************************************************************
 *               OTHER PRIMOP-RELATED UTILITY FUNCTIONS                   *
 **************************************************************************)

    fun prNumkind (P.INT bits) = "i" ^ Int.toString bits
      | prNumkind (P.UINT bits) = "u" ^ Int.toString bits
      | prNumkind (P.FLOAT bits) = "f" ^ Int.toString bits

    val cvtParam = Int.toString
    fun cvtParams (from, to) = concat [cvtParam from, "_", cvtParam to]

    fun toString (P.IARITH{oper, sz}) =
	  concat [ArithOps.arithopToString oper, "_", Int.toString sz]
      | toString (P.PURE_ARITH{oper, kind}) =
	  concat [ArithOps.pureopToString oper, "_", prNumkind kind]
      | toString (P.INLDIV kind) =  "inldiv_"  ^ prNumkind kind
      | toString (P.INLMOD kind) =  "inlmod_"  ^ prNumkind kind
      | toString (P.INLQUOT kind) =  "inlquot_"  ^ prNumkind kind
      | toString (P.INLREM kind) =  "inlrem_"  ^ prNumkind kind
      | toString (P.INLLSHIFT kind) =  "inllshift_"  ^ prNumkind kind
      | toString (P.INLRSHIFT kind) =  "inlrshift_"  ^ prNumkind kind
      | toString (P.INLRSHIFTL kind) = "inlrshiftl_" ^ prNumkind kind
      | toString (P.CMP{oper, kind}) = ArithOps.cmpopToString oper ^ prNumkind kind
      | toString (P.FSGN sz) = "fsgn_" ^ cvtParam sz
      | toString P.INLCHR = "inlchr"
      | toString (P.TESTU arg) = "testu_" ^ cvtParams arg
      | toString (P.TEST arg) = "test_" ^ cvtParams arg
      | toString (P.TRUNC arg) = "trunc_" ^ cvtParams arg
      | toString (P.EXTEND arg) = "extend_" ^ cvtParams arg
      | toString (P.COPY arg) = "copy_" ^ cvtParams arg
      | toString (P.TEST_INF i) = "test_inf_" ^ cvtParam i
      | toString (P.TRUNC_INF i) = "trunc_inf_" ^ cvtParam i
      | toString (P.EXTEND_INF i) = concat ["extend_", cvtParam i, "_inf"]
      | toString (P.COPY_INF i) =  concat ["copy_", cvtParam i, "_inf"]
      | toString (P.REAL_TO_INT{floor,from,to}) = concat [
	    if floor then "floor_real" else "round_real",
	    Int.toString from, "_to_int", Int.toString to
	  ]
      | toString (P.INT_TO_REAL{from,to}) = concat [
	    "int", Int.toString from, "_to_real", Int.toString to
	  ]
      | toString (P.NUMSUBSCRIPT kind) = "numsubscript_" ^ prNumkind kind
      | toString (P.NUMSUBSCRIPTV kind) = "numsubscriptv_" ^ prNumkind kind
      | toString (P.NUMUPDATE kind) = "numupdate_" ^ prNumkind kind
      | toString (P.INLNUMSUBSCRIPT kind) = "inlnumsubscript_" ^ prNumkind kind
      | toString (P.INLNUMSUBSCRIPTV kind) = "inlnumsubscriptv_" ^ prNumkind kind
      | toString (P.INLNUMUPDATE kind) = "inlnumupdate_" ^ prNumkind kind
      | toString P.SUBSCRIPT = "subscript"
      | toString P.SUBSCRIPTV = "subscriptv"
      | toString P.INLSUBSCRIPT = "inlsubscript"
      | toString P.INLSUBSCRIPTV = "inlsubscriptv"
      | toString P.INLMKARRAY = "inlmkarray"
      | toString P.PTREQL = "ptreql"
      | toString P.PTRNEQ = "ptrneq"
      | toString P.POLYEQL = "polyeql"
      | toString P.POLYNEQ = "polyneq"
      | toString P.BOXED = "boxed"
      | toString P.UNBOXED = "unboxed"
      | toString P.LENGTH = "length"
      | toString P.OBJLENGTH = "objlength"
      | toString P.CAST = "cast"
      | toString P.GETHDLR = "gethdlr"
      | toString P.SETHDLR = "sethdlr"
      | toString P.GETVAR = "getvar"
      | toString P.SETVAR = "setvar"
      | toString P.MAKEREF = "makeref"
      | toString P.CALLCC = "callcc"
      | toString P.CAPTURE = "capture"
      | toString P.THROW = "throw"
      | toString P.ISOLATE = "isolate"
      | toString P.DEREF = "!"
      | toString P.ASSIGN = ":="
      | toString P.UPDATE = "update"
      | toString P.INLUPDATE = "inlupdate"
      | toString P.UNBOXEDUPDATE = "unboxedupdate"
      | toString P.GETTAG = "gettag"
      | toString P.MKSPECIAL = "mkspecial"
      | toString P.SETSPECIAL = "setspecial"
      | toString P.GETSPECIAL = "getspecial"
      | toString (P.INLMIN nk) = "inlmin_" ^ prNumkind nk
      | toString (P.INLMAX nk) = "inlmax_" ^ prNumkind nk
      | toString (P.INLABS nk) = "inlabs_" ^ prNumkind nk
      | toString P.INLNOT = "inlnot"
      | toString P.INLCOMPOSE = "inlcompose"
      | toString P.INLBEFORE = "inlbefore"
      | toString P.INLIGNORE = "inlignore"
      | toString P.INLIDENTITY = "inlidentity"
    (* Primops to support new array representations *)
      | toString P.NEW_ARRAY0 = "newarray0"
      | toString P.GET_SEQ_DATA = "getseqdata"
      | toString P.SUBSCRIPT_REC = "subscriptrec"
      | toString P.SUBSCRIPT_RAW64 = "subscriptraw64"
      | toString P.INTERN64 = "intern64"
      | toString P.EXTERN64 = "extern64"
    (* Primops to support new experimental C FFI. *)
      | toString (P.RAW_LOAD nk) = concat ["raw_load(", prNumkind nk, ")"]
      | toString (P.RAW_STORE nk) = concat ["raw_store(", prNumkind nk, ")"]
      | toString (P.RAW_CCALL _) = "raw_ccall"
      | toString (P.RAW_RECORD{ align64 }) =
	  if align64 then "raw64_record" else "raw_record"
      | toString P.UNBOXEDASSIGN = "(unboxed):="
      | toString P.WCAST = "wcast"
      | toString P.MARKEXN = "markexn"
      | toString P.INL_ARRAY = "inl_array"
      | toString P.INL_VECTOR = "inl_vector"
      | toString (P.INL_MONOARRAY kind) = "inl_monoarray_" ^ prNumkind kind
      | toString (P.INL_MONOVECTOR kind) = "inl_monovector_" ^ prNumkind kind
      | toString P.MKETAG = "mketag"
      | toString P.WRAP = "wrap"
      | toString P.UNWRAP = "unwrap"
      | toString P.PTR_TO_WORD = "cptr_to_word"
      | toString P.WORD_TO_PTR = "word_to_cptr"
      | toString (P.REAL_TO_BITS sz) = "real_to_bits_" ^ cvtParam sz

  (* should return more than just a boolean:
   * {Store,Continuation}-{read,write}
   *)
    fun effect p = (case p
	   of P.PURE_ARITH _ => false
	    | (P.INLRSHIFT _ | P.INLRSHIFTL _) => false
	    | P.CMP _ => false
	    | P.FSGN _ => false
	    | (P.EXTEND _ | P.TRUNC _ | P.COPY _) => false
	    | (P.PTREQL | P.PTRNEQ | P.POLYEQL | P.POLYNEQ) => false
	    | (P.BOXED | P.UNBOXED) => false
	    | (P.LENGTH | P.OBJLENGTH) => false
	    | (P.CAST | P.WCAST) => false
	    | (P.INLMIN _ | P.INLMAX _ | P.INLNOT | P.INLCOMPOSE | P.INLIGNORE) => false
	    | (P.WRAP | P.UNWRAP) => false
	    | P.INLIDENTITY => false
	    | (P.INTERN64 | P.EXTERN64) => false
	    | (P.PTR_TO_WORD | P.WORD_TO_PTR | P.REAL_TO_BITS _) => false
	    | _ => true
	  (* end case *))

  end

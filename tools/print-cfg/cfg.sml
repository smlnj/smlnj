(* cfg.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * CFG IR for SML/NJ code generation.
 *
 * This file is a modification of the `CodeGen/cfg/cfg.sml` file, where we have
 * replaced datatype renamings with datatype declarations.
 *)

structure CFG_Prim =
  struct
    datatype numkind = INT | FLT

  (* type signature for a field in a `RAW_RECORD`; the `sz` is in bits *)
    type raw_ty = {kind : numkind, sz : int}

  (* allocation operations *)
    datatype alloc
      = SPECIAL
      | RECORD of {desc : IntInf.int, mut : bool}
      | RAW_RECORD of {desc : IntInf.int, align : int, fields : raw_ty list}
      | RAW_ALLOC of {desc : IntInf.int option, align : int, len : int}

  (* arithmetic operations that may overflow; for the division operators,
   * we assume that the second argument is never zero (i.e., an explicit
   * test for zero is done before the operation).
   *)
    datatype arithop
      = IADD | ISUB | IMUL
(* Question: perhaps the division operators should be moved out of `arith`, since
 * the div-by-zero and overflow tests will have to be explicit?
 *)
      | IDIV | IREM

  (* rounding modes for float conversions *)
    datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO

    datatype arith
      = ARITH of {oper : arithop, sz : int}
      | FLOAT_TO_INT of {mode : rounding_mode, from : int, to : int}

  (* arithmetic operations that do not overflow; for the division operators,
   * we distinguish between signed and unsigned operations, and assume that
   * the second argument is never zero (i.e., an explicit test for zero is
   * done before the operation).
   *)
    datatype pureop
      = ADD | SUB | MUL
      | SDIV | SREM
      | UDIV | UREM
      | SHL | ASHR | LSHR
      | ORB | XORB | ANDB
      | CNTPOP | CNTLZ | CNTTZ
      | ROTL | ROTR
      | FADD | FSUB | FMUL | FDIV | FREM
      | FMADD
      | FNEG | FABS | FCOPYSIGN
      | FSQRT

    datatype pure
      = PURE_ARITH of {oper : pureop, sz : int}
      | EXTEND of {signed : bool, from : int, to : int}
      | TRUNC of {from : int, to : int}
      | INT_TO_FLOAT of {from : int, to : int}
      | FLOAT_TO_BITS of {sz : int}
      | BITS_TO_FLOAT of {sz : int}
      | PURE_SUBSCRIPT
      | PURE_RAW_SUBSCRIPT of {kind : numkind, sz : int}
      | RAW_SELECT of {kind : numkind, sz : int, offset : int}

    datatype looker
      = DEREF
      | SUBSCRIPT
      | RAW_SUBSCRIPT of {kind : numkind, sz : int}
      | RAW_LOAD of {kind : numkind, sz : int}
      | GET_HDLR
      | GET_VAR

    datatype setter
      = UNBOXED_UPDATE | UPDATE				(* array update *)
      | UNBOXED_ASSIGN | ASSIGN				(* reference assignment *)
      | RAW_UPDATE of {kind : numkind, sz : int}	(* raw array update *)
      | RAW_STORE of {kind : numkind, sz : int}		(* raw store to base+offset *)
      | SET_HDLR
      | SET_VAR

  (* fcmpop conforms to the IEEE std 754 predicates. *)
    datatype fcmpop
      = F_EQ (* = *)  | F_ULG (* ?<> *) | F_UN (* ? *)   | F_LEG (* <=> *)
      | F_GT (* > *)  | F_GE  (* >= *)  | F_UGT (* ?> *) | F_UGE (* ?>= *)
      | F_LT (* < *)  | F_LE  (* <= *)  | F_ULT (* ?< *) | F_ULE (* ?<= *)
      | F_LG (* <> *) | F_UE  (* ?= *)

  (* comparison operators
   * NOTE: this type is defined in the ArithOps structure (ElabData/prim/arithops.sml)
   *)
    datatype cmpop
      = GT | GTE | LT | LTE | EQL | NEQ

  (* These are two-way branches dependent on pure inputs *)
    datatype branch
      = CMP of {oper: cmpop, signed: bool, sz : int}
      | FCMP of {oper: fcmpop, sz: int}
      | FSGN of int
      | PEQL | PNEQ
      | LIMIT of word

  end

structure CFG =
  struct

    datatype ty
      = LABt
      | PTRt
      | TAGt
      | NUMt of {sz : int}
      | FLTt of {sz : int}

  (* fragment/function parameters *)
    type param = {name : LambdaVar.lvar, ty : ty}

  (* branch probabilities are measured in thousandths (1..999).  We use 0 to
   * represent the absence of probability information.
   *)
    type probability = int

    datatype exp
      = VAR of {name : LambdaVar.lvar}
      | LABEL of {name : LambdaVar.lvar}
      | NUM of {iv : IntInf.int, sz : int}
      | LOOKER of {oper : CFG_Prim.looker, args : exp list}
      | PURE of {oper : CFG_Prim.pure, args : exp list}
      | SELECT of {idx : int, arg : exp}

    datatype stm
      = LET of exp * param * stm
      | ALLOC of CFG_Prim.alloc * exp list * LambdaVar.lvar * stm
      | APPLY of exp * exp list * ty list
      | THROW of exp * exp list * ty list
      | GOTO of LambdaVar.lvar * exp list
      | SWITCH of exp * stm list
      | BRANCH of CFG_Prim.branch * exp list * probability * stm * stm
      | ARITH of CFG_Prim.arith * exp list * param * stm
      | SETTER of CFG_Prim.setter * exp list * stm
      | CALLGC of exp list * LambdaVar.lvar list * stm
      | RCC of {
	    reentrant : bool,		(* true for reentrant functions *)
	    linkage : string,		(*  *)
	    proto : CTypes.c_proto,	(* function prototype *)
	    args : exp list,		(* arguments; first arg is function pointer *)
	    results : param list,	(* result bindings *)
	    live : param list,		(* variables that are live across the call;
					 * this list is [] for non-reentrant functions.
					 *)
	    k : stm			(* the continuation *)
	  }

  (* the different kinds of fragments *)
    datatype frag_kind
      = STD_FUN				(* entry fragment for escaping function *)
      | STD_CONT			(* entry fragment for escaping continuation *)
      | KNOWN_FUN			(* entry fragment for known function (introduced
					 * during clustering)
					 *)
      | INTERNAL			(* internal to a cluster *)

  (* an extended basic block *)
    datatype frag = Frag of {
	kind : frag_kind,		(* the fragment's kind *)
	lab : LambdaVar.lvar,		(* the fragment's label *)
	params : param list,		(* the parameters (with types) *)
	body : stm			(* the fragment's body *)
      }

  (* per-cluster attributes *)
    type attrs = {
	alignHP : int,		(* alignment requirement in bytes for heap pointer *)
	needsBasePtr : bool,	(* true if cluster does PC-relative addressing *)
	hasTrapArith : bool,	(* true if cluster contains `ARITH` operations *)
	hasRCC : bool		(* true if cluster contains raw C Calls *)
      }

  (* a cluster is a maximal flow graph where every known call is to a
   * fragment in the the cluster.
   *)
    datatype cluster = Cluster of {attrs : attrs, frags : frag list}

  (* a compilation unit *)
    type comp_unit = {srcFile : string, entry : cluster, fns : cluster list}

  end

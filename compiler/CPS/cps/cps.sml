(* cps.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CPS : CPS =
  struct

    datatype record_kind
      = RK_VECTOR	(* vector *)
      | RK_RECORD	(* SML record/tuple *)
      | RK_ESCAPE	(* closure record for escaping function *)
      | RK_CONT		(* closure record for continuation *)
      | RK_FCONT	(* closure record for unboxed 64-bit aligned data *)
      | RK_KNOWN	(* closure record for known function *)
      | RK_RAW64BLOCK	(* 64-bit aligned raw data record *)
      | RK_RAWBLOCK	(* word-aligned raw data record *)

    datatype pkind = VPT | RPT of int | FPT of int

  (* kinds of integers: size in bits and tagged vs boxed *)
    type intty = {sz : int, tag : bool}

    datatype cty
      = NUMt of intty	(* integers of the given type *)
      | PTRt of pkind	(* pointer *)
      | FUNt		(* function? *)
      | FLTt of int 	(* float of given size *)
      | CNTt		(* continuation *)

    structure P =
      struct
      (* numkind includes kind and size *)
	datatype numkind = INT of int | UINT of int | FLOAT of int

      (* integer arithmetic operations that may overflow *)
	datatype arithop = datatype ArithOps.arithop

      (* pure arithmetic operations that cannot overflow *)
	datatype pureop = datatype ArithOps.pureop

      (* generic comparison operations *)
	datatype cmpop = datatype ArithOps.cmpop

      (* fcmpop conforms to the IEEE std 754 predicates. *)
	datatype fcmpop
	  = F_EQ (* = *)  | F_ULG (* ?<> *) | F_UN (* ? *)   | F_LEG (* <=> *)
	  | F_GT (* > *)  | F_GE  (* >= *)  | F_UGT (* ?> *) | F_UGE (* ?>= *)
	  | F_LT (* < *)  | F_LE  (* <= *)  | F_ULT (* ?< *) | F_ULE (* ?<= *)
	  | F_LG (* <> *) | F_UE  (* ?= *)

      (* These are two-way branches dependent on pure inputs *)
	datatype branch
	  = CMP of {oper: cmpop, kind: numkind}
	  | FCMP of {oper: fcmpop, size: int}
	  | FSGN of int
	  | BOXED | UNBOXED | PEQL | PNEQ
	(* `STREQL s` tests if a string is equal to `s`, where the tested string must have
	 * the same length as `s` and `s` is not the empty string.
	 *)
	  | STREQL of string

      (* These all update the store *)
	datatype setter
	  = NUMUPDATE of {kind: numkind}
	  | UNBOXEDUPDATE | UPDATE
	  | UNBOXEDASSIGN | ASSIGN
	  | SETHDLR | SETVAR | SETSPECIAL
	  | RAWSTORE of {kind: numkind}
	  | RAWUPDATE of cty

      (* These fetch from the store, never have functions as arguments. *)
	datatype looker
	  = DEREF | SUBSCRIPT | NUMSUBSCRIPT of {kind: numkind}
	  | GETSPECIAL | GETHDLR | GETVAR
	  | RAWLOAD of {kind: numkind}

      (* These might raise exceptions, never have functions as arguments.*)
	datatype arith
	  = IARITH of {oper: arithop, sz: int}
	  | TEST of {from: int, to: int}
	  | TESTU of {from: int, to: int}
	  | TEST_INF of int
	  | REAL_TO_INT of {floor: bool, from: int, to: int}

      (* These don't raise exceptions and don't access the store. *)
	datatype pure
	  = PURE_ARITH of {oper: pureop, kind: numkind}
	  | PURE_NUMSUBSCRIPT of {kind: numkind}
	  | LENGTH | OBJLENGTH | MAKEREF
	  | COPY of {from: int, to: int}
	  | EXTEND of {from: int, to: int}
	  | TRUNC of {from: int, to: int}
	  | COPY_INF of int
	  | EXTEND_INF of int
	  | TRUNC_INF of int
	  | INT_TO_REAL of {from: int, to: int}
	  | SUBSCRIPTV
	  | GETTAG | MKSPECIAL | CAST | GETCON | GETEXN
	  | BOX | UNBOX
	(* tagging/boxing of numbers; numkind should be either `INT` or `FLOAT` *)
	  | WRAP of numkind | UNWRAP of numkind
          | GETSEQDATA | RECSUBSCRIPT | RAW64SUBSCRIPT | NEWARRAY0
	(* allocate uninitialized words from the heap; optionally
	 * initialize the tag.
	 *)
 	  | RAWRECORD of record_kind option

      end (* P *)

    type lvar = LambdaVar.lvar

    datatype value
      = VAR of lvar
      | LABEL of lvar
      | NUM of intty IntConst.t
      | REAL of int RealConst.t
      | STRING of string
      | VOID

    datatype accesspath
      = OFFp of int
      | SELp of int * accesspath

    datatype fun_kind
      = CONT           (* continuation functions *)
      | KNOWN          (* general known functions *)
      | KNOWN_REC      (* known recursive functions *)
      | KNOWN_CHECK    (* known functions that need a heap limit check *)
      | KNOWN_TAIL     (* tail-recursive kernal *)
      | KNOWN_CONT     (* known continuation functions *)
      | ESCAPE         (* before the closure phase, any user function;
			  after the closure phase, escaping user function *)
      | NO_INLINE_INTO (* before the closure phase,
			  a user function inside of which no in-line expansions
			  should be performed;
			  does not occur after the closure phase *)

    datatype cexp
      = RECORD of record_kind * (value * accesspath) list * lvar * cexp
      | SELECT of int * value * lvar * cty * cexp
      | OFFSET of int * value * lvar * cexp
      | APP of value * value list
      | FIX of function list * cexp
      | SWITCH of value * lvar * cexp list
      | BRANCH of P.branch * value list * lvar * cexp * cexp
      | SETTER of P.setter * value list * cexp
      | LOOKER of P.looker * value list * lvar * cty * cexp
      | ARITH of P.arith * value list * lvar * cty * cexp
      | PURE of P.pure * value list * lvar * cty * cexp
      (* experimental "raw C call" (Blume, 1/2001) *)
      | RCC of bool * string * CTypes.c_proto * value list * (lvar * cty) list * cexp

    withtype function = fun_kind * lvar * lvar list * cty list * cexp

  end (* structure CPS *)

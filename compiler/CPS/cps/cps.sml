(* cps.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure CPS : CPS =
  struct

(* TODO: the `record_kind` type mixes the issue of what the purpose of a record is
 * with the representation decision.  I propose defining three types:
 *
 *    datatype record_kind
 *      = RK_RECORD       (* records *)
 *      | RK_VECTOR       (* vector literal *)
 *      | RK_FUN          (* user-function-closure record *)
 *      | RK_CONT         (* continuation-closure record *)
 *      | RK_KNOWN        (* known function/continuation closure record *)
 *
 *    type record_rep = {ptrLen : int, rawLen : int}
 *    type intty = {sz : int, tag : bool}
 *
 *    datatype cty
 *      = VALt                  (* an ML value (a super type of ENUMt,
 *                               * NUMt{tag=true, ...}, PTRt, and FUNt).
 *                               *)
 *      | ENUMt                 (* datatype-constructor tag *)
 *      | NUMt of intty	        (* integers of the given type *)
 *      | FLTt of int 	        (* float of given size *)
 *      | RPTRt of record_rep	(* pointers to records/mixed records/raw records *)
 *      | PTRt                  (* pointers to heap data (a super type of RPTRt) *)
 *      | FUNt		        (* user function *)
 *      | CNTt of cty list	(* continuation *)
 *
 * Then the mapping from the current record_kind to the new rep would be something like
 *
 *    RK_VECTOR         ==>     (RK_VECTOR, {ptrLen = <n>, rawLen = 0})
 *    RK_RECORD         ==>     (RK_RECORD, {ptrLen = <n>, rawLen = 0})
 *    RK_ESCAPE         ==>     (RK_FUN, {ptrLen = <n>, rawLen = 0})
 *    RK_CONT           ==>     (RK_CONT, {ptrLen = <n>, rawLen = 0})
 *    RK_FCONT          ==>     (RK_CONT, {ptrLen = 0, rawLen = <n>})
 *    RK_KNOWN          ==>     (RK_KNOWN, {ptrLen = <n>, rawLen = 0})
 *    RK_MIXED          ==>     (<kind>, {ptrLen = <n>, rawLen = <m>})
 *    RK_RAWBLOCK       ==>     (RK_RECORD, {ptrLen = 0, rawLen = <n>})
 *
 * Note that this change also requires a change to the `RECORD` form.  It either needs
 * a type annotation or the `SELECT`/`RECORD` change described below.
 *)

    (* record representation; regular records have `rawLen = 0` and raw records
     * have `ptrLen = 0`.
     *)
    type record_rep = {ptrLen : int, rawLen : int}

    datatype record_kind
      = RK_VECTOR	(* vector *)
      | RK_RECORD	(* SML record/tuple *)
      | RK_ESCAPE	(* closure record for escaping function *)
      | RK_CONT		(* closure record for continuation *)
      | RK_FCONT	(* closure record for unboxed 64-bit aligned data *)
      | RK_KNOWN	(* closure record for known function *)
      | RK_MIXED of record_rep	(* mixed record *)
      | RK_RAWBLOCK	(* raw data record *)

    datatype pkind = VPT | RPT of record_rep

  (* type info for integers: size in bits and tagged vs boxed *)
    type intty = {sz : int, tag : bool}

    datatype cty
      = NUMt of intty	        (* integers of the given type *)
      | ENUMt			(* datatype-constructor tag *)
      | PTRt of pkind	        (* pointer *)
      | FUNt		        (* function? *)
      | FLTt of int	        (* float of given size *)
      | CNTt of cty list	(* continuation *)

    val ptrTy = PTRt
    fun rPtrTy n = PTRt(RPT{ptrLen = n, rawLen = 0})
    fun fPtrTy n = PTRt(RPT{ptrLen = 0, rawLen = n})

    structure P =
      struct
      (* numkind includes kind and size *)
	datatype numkind = datatype NumKind.t

      (* integer arithmetic operations that may overflow *)
	datatype arithop = datatype ArithOps.t

      (* pure arithmetic operations that cannot overflow *)
	datatype pureop = datatype PureOps.t

      (* generic comparison operations *)
	datatype cmpop = datatype CompareOps.t

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
          | IS_POW2 of int
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
          | BITS_TO_REAL of int                 (* bitcast from word to real *)
          | REAL_TO_BITS of int                 (* bitcast from real to word *)
	  | SUBSCRIPTV
	  | GETTAG | MKSPECIAL | CAST | GETCON | GETEXN
(* TODO: the BOX/UNBOX primops are removed during the `CPSTransFn` pass; we
 * should just not generate them in `ConvertFn`
 *)
	  | BOX | UNBOX
	(* tagging/boxing of numbers; numkind should be either `INT` or `FLOAT` *)
	  | WRAP of numkind | UNWRAP of numkind
          | GETSEQDATA | RECSUBSCRIPT | RAW64SUBSCRIPT | NEWARRAY0
	(* allocate uninitialized words from the heap; optionally
	 * initialize the tag.
	 *)
 	  | RAWRECORD of record_kind option

(* TODO: if we make the change below to RECORD and SELECT, then we could
 * make RECORD a primop and split out all of the allocating primops into
 * their own datatype.
        datatype alloc
          (* user records + arguments, closure, and spill records *)
          = RECORD of record_kind
          (* zero-length array *)
          | ARRAY0
          (* ref cells *)
          | REF
          (* special objects: weak pointers and suspensions *)
          | SPECIAL
          (* heap-allocated native numbers *)
          | WRAP of numkind
          (* uninitialized words with an optional header *)
          | ALLOC of int * record_kind option
*)
      end (* P *)

    type lvar = LambdaVar.lvar

    datatype value
      = VAR of lvar
      | LABEL of lvar
      | NUM of intty IntConst.t
      | ENUM of int
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
			* after the closure phase, escaping user function *)
      | NO_INLINE_INTO (* before the closure phase,
			* a user function inside of which no in-line expansions
			* should be performed;
			* does not occur after the closure phase
                        *)

    datatype cexp
      = RECORD of record_kind * (value * accesspath) list * lvar * cexp
(* TODO: move access paths into the `SELECT` expression and simplify the `RECORD`
 * expression form.
      = RECORD of record_kind * value list * lvar * cexp
      | SELECT of accesspath * lvar * lvar * cty * cexp
*)
      | SELECT of int * value * lvar * cty * cexp
(* TODO: remove `OFFSET` *)
      | OFFSET of int * value * lvar * cexp     (* DEPRECATED *)
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

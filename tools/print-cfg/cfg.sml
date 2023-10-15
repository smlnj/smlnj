(* cfg.sml
 *
 * Generated from cfg.asdl by asdlgen.
 *)

structure CTypes = struct
    type calling_convention = string
    datatype c_int
      = I_char
      | I_short
      | I_int
      | I_long
      | I_long_long
    datatype c_type
      = C_void
      | C_float
      | C_double
      | C_long_double
      | C_unsigned of c_int
      | C_signed of c_int
      | C_PTR
      | C_ARRAY of c_type * int
      | C_STRUCT of c_type list
      | C_UNION of c_type list
    type c_proto = {conv : calling_convention, retTy : c_type, paramTys : c_type list}
  end

structure CFG_Prim = struct
    datatype fcmpop
      = F_EQ
      | F_ULG
      | F_UN
      | F_LEG
      | F_GT
      | F_GE
      | F_UGT
      | F_UGE
      | F_LT
      | F_LE
      | F_ULT
      | F_ULE
      | F_LG
      | F_UE
    datatype cmpop
      = GT
      | GTE
      | LT
      | LTE
      | EQL
      | NEQ
    datatype branch
      = CMP of {oper : cmpop, signed : bool, sz : int}
      | FCMP of {oper : fcmpop, sz : int}
      | FSGN of int
      | PEQL
      | PNEQ
      | LIMIT of word
    datatype numkind
      = INT
      | FLT
    datatype setter
      = UNBOXED_UPDATE
      | UPDATE
      | UNBOXED_ASSIGN
      | ASSIGN
      | RAW_UPDATE of {kind : numkind, sz : int}
      | RAW_STORE of {kind : numkind, sz : int}
      | SET_HDLR
      | SET_VAR
    datatype looker
      = DEREF
      | SUBSCRIPT
      | RAW_SUBSCRIPT of {kind : numkind, sz : int}
      | RAW_LOAD of {kind : numkind, sz : int}
      | GET_HDLR
      | GET_VAR
    datatype pureop
      = ADD
      | SUB
      | SMUL
      | SDIV
      | SREM
      | UMUL
      | UDIV
      | UREM
      | LSHIFT
      | RSHIFT
      | RSHIFTL
      | ORB
      | XORB
      | ANDB
      | FADD
      | FSUB
      | FMUL
      | FDIV
      | FNEG
      | FABS
      | FSQRT
      | FCOPYSIGN
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
    datatype arithop
      = IADD
      | ISUB
      | IMUL
      | IDIV
      | IREM
    datatype rounding_mode
      = TO_NEAREST
      | TO_NEGINF
      | TO_POSINF
      | TO_ZERO
    datatype arith
      = ARITH of {oper : arithop, sz : int}
      | FLOAT_TO_INT of {mode : rounding_mode, from : int, to : int}
    type raw_ty = {kind : numkind, sz : int}
    datatype alloc
      = SPECIAL
      | RECORD of {desc : IntInf.int, mut : bool}
      | RAW_RECORD of {desc : IntInf.int, align : int, fields : raw_ty list}
      | RAW_ALLOC of {desc : IntInf.int option, align : int, len : int}
  end

structure CFG = struct
    type attrs = {alignHP : int, needsBasePtr : bool, hasTrapArith : bool, hasRCC : bool}
    datatype frag_kind
      = STD_FUN
      | STD_CONT
      | KNOWN_FUN
      | INTERNAL
    type probability = int
    datatype ty
      = LABt
      | PTRt
      | TAGt
      | NUMt of {sz : int}
      | FLTt of {sz : int}
    type param = {name : LambdaVar.lvar, ty : ty}
    datatype exp
      = VAR of {name : LambdaVar.lvar}
      | LABEL of {name : LambdaVar.lvar}
      | NUM of {iv : IntInf.int, sz : int}
      | LOOKER of {oper : CFG_Prim.looker, args : exp list}
      | PURE of {oper : CFG_Prim.pure, args : exp list}
      | SELECT of {idx : int, arg : exp}
      | OFFSET of {idx : int, arg : exp}
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
      | RCC of {reentrant : bool, linkage : string, proto : CTypes.c_proto, args : exp list, results : param list, live : param list, k : stm}
    datatype frag = Frag of {kind : frag_kind, lab : LambdaVar.lvar, params : param list, body : stm}
    datatype cluster = Cluster of {attrs : attrs, frags : frag list}
    type comp_unit = {srcFile : string, entry : cluster, fns : cluster list}
  end


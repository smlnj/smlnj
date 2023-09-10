(* cfg-pickle.sig
 *
 * Generated from cfg.asdl by asdlgen.
 *)

signature LAMBDA_VAR_PICKLE = sig
    type instream
    type outstream
    val write_lvar : outstream * LambdaVar.lvar -> unit
    val read_lvar : instream -> LambdaVar.lvar
  end

signature CTYPES_PICKLE = sig
    type instream
    type outstream
    val write_c_type : outstream * CTypes.c_type -> unit
    val read_c_type : instream -> CTypes.c_type
    val write_c_int : outstream * CTypes.c_int -> unit
    val read_c_int : instream -> CTypes.c_int
    val write_calling_convention : outstream * CTypes.calling_convention -> unit
    val read_calling_convention : instream -> CTypes.calling_convention
    val write_c_proto : outstream * CTypes.c_proto -> unit
    val read_c_proto : instream -> CTypes.c_proto
  end

signature CFG__PRIM_PICKLE = sig
    type instream
    type outstream
    val write_numkind : outstream * CFG_Prim.numkind -> unit
    val read_numkind : instream -> CFG_Prim.numkind
    val write_rounding_mode : outstream * CFG_Prim.rounding_mode -> unit
    val read_rounding_mode : instream -> CFG_Prim.rounding_mode
    val write_raw_ty : outstream * CFG_Prim.raw_ty -> unit
    val read_raw_ty : instream -> CFG_Prim.raw_ty
    val write_alloc : outstream * CFG_Prim.alloc -> unit
    val read_alloc : instream -> CFG_Prim.alloc
    val write_arithop : outstream * CFG_Prim.arithop -> unit
    val read_arithop : instream -> CFG_Prim.arithop
    val write_arith : outstream * CFG_Prim.arith -> unit
    val read_arith : instream -> CFG_Prim.arith
    val write_pureop : outstream * CFG_Prim.pureop -> unit
    val read_pureop : instream -> CFG_Prim.pureop
    val write_pure : outstream * CFG_Prim.pure -> unit
    val read_pure : instream -> CFG_Prim.pure
    val write_looker : outstream * CFG_Prim.looker -> unit
    val read_looker : instream -> CFG_Prim.looker
    val write_setter : outstream * CFG_Prim.setter -> unit
    val read_setter : instream -> CFG_Prim.setter
    val write_cmpop : outstream * CFG_Prim.cmpop -> unit
    val read_cmpop : instream -> CFG_Prim.cmpop
    val write_fcmpop : outstream * CFG_Prim.fcmpop -> unit
    val read_fcmpop : instream -> CFG_Prim.fcmpop
    val write_branch : outstream * CFG_Prim.branch -> unit
    val read_branch : instream -> CFG_Prim.branch
  end

signature CFGPICKLE = sig
    type instream
    type outstream
    val write_ty : outstream * CFG.ty -> unit
    val read_ty : instream -> CFG.ty
    val write_exp : outstream * CFG.exp -> unit
    val read_exp : instream -> CFG.exp
    val write_param : outstream * CFG.param -> unit
    val read_param : instream -> CFG.param
    val write_probability : outstream * CFG.probability -> unit
    val read_probability : instream -> CFG.probability
    val write_stm : outstream * CFG.stm -> unit
    val read_stm : instream -> CFG.stm
    val write_frag_kind : outstream * CFG.frag_kind -> unit
    val read_frag_kind : instream -> CFG.frag_kind
    val write_frag : outstream * CFG.frag -> unit
    val read_frag : instream -> CFG.frag
    val write_attrs : outstream * CFG.attrs -> unit
    val read_attrs : instream -> CFG.attrs
    val write_cluster : outstream * CFG.cluster -> unit
    val read_cluster : instream -> CFG.cluster
    val write_comp_unit : outstream * CFG.comp_unit -> unit
    val read_comp_unit : instream -> CFG.comp_unit
  end


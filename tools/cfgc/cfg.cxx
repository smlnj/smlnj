// cfg.cxx
//
// Generated from cfg.asdl by asdlgen.
//

#include "cfg.hxx"

namespace CTypes {
    c_type * c_type::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_C_void:
            return new C_void;
          case _con_C_float:
            return new C_float;
          case _con_C_double:
            return new C_double;
          case _con_C_long_double:
            return new C_long_double;
          case _con_C_unsigned:
            {
                auto f0 = read_c_int(is);
                return new C_unsigned(f0);
            }
          case _con_C_signed:
            {
                auto f0 = read_c_int(is);
                return new C_signed(f0);
            }
          case _con_C_PTR:
            return new C_PTR;
          case _con_C_ARRAY:
            {
                auto f0 = c_type::read(is);
                auto f1 = asdl::read_int(is);
                return new C_ARRAY(f0, f1);
            }
          case _con_C_STRUCT:
            {
                auto f0 = read_c_type_seq(is);
                return new C_STRUCT(f0);
            }
          case _con_C_UNION:
            {
                auto f0 = read_c_type_seq(is);
                return new C_UNION(f0);
            }
        }
    }
    c_type::~c_type () { }
    C_void::~C_void () { }
    C_float::~C_float () { }
    C_double::~C_double () { }
    C_long_double::~C_long_double () { }
    C_unsigned::~C_unsigned () { }
    C_signed::~C_signed () { }
    C_PTR::~C_PTR () { }
    C_ARRAY::~C_ARRAY ()
    {
        delete this->_v0;
    }
    C_STRUCT::~C_STRUCT () { }
    C_UNION::~C_UNION () { }
    // c_type_seq pickler suppressed
    std::vector<c_type *> read_c_type_seq (asdl::instream & is)
    {
        return asdl::read_seq<c_type>(is);
    }
    // pickler suppressed for c_int
    c_int read_c_int (asdl::instream & is)
    {
        return static_cast<c_int>(asdl::read_tag8(is));
    }
    // pickler suppressed for calling_convention
    calling_convention read_calling_convention (asdl::instream & is)
    {
        auto v = asdl::read_string(is);
        return v;
    }
    c_proto * c_proto::read (asdl::instream & is)
    {
        auto fconv = read_calling_convention(is);
        auto fretTy = c_type::read(is);
        auto fparamTys = read_c_type_seq(is);
        return new c_proto(fconv, fretTy, fparamTys);
    }
    c_proto::~c_proto ()
    {
        delete this->_v_retTy;
    }
} // namespace CTypes
namespace CFG_Prim {
    // pickler suppressed for numkind
    numkind read_numkind (asdl::instream & is)
    {
        return static_cast<numkind>(asdl::read_tag8(is));
    }
    // pickler suppressed for rounding_mode
    rounding_mode read_rounding_mode (asdl::instream & is)
    {
        return static_cast<rounding_mode>(asdl::read_tag8(is));
    }
    raw_ty * raw_ty::read (asdl::instream & is)
    {
        auto fkind = read_numkind(is);
        auto fsz = asdl::read_int(is);
        return new raw_ty(fkind, fsz);
    }
    raw_ty::~raw_ty () { }
    // raw_ty_seq pickler suppressed
    std::vector<raw_ty *> read_raw_ty_seq (asdl::instream & is)
    {
        return asdl::read_seq<raw_ty>(is);
    }
    alloc * alloc::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_SPECIAL:
            return new SPECIAL;
          case _con_RECORD:
            {
                auto fdesc = asdl::read_integer(is);
                auto fmut = asdl::read_bool(is);
                return new RECORD(fdesc, fmut);
            }
          case _con_RAW_RECORD:
            {
                auto fdesc = asdl::read_integer(is);
                auto falign = asdl::read_int(is);
                auto ffields = read_raw_ty_seq(is);
                return new RAW_RECORD(fdesc, falign, ffields);
            }
          case _con_RAW_ALLOC:
            {
                auto fdesc = asdl::read_integer_option(is);
                auto falign = asdl::read_int(is);
                auto flen = asdl::read_int(is);
                return new RAW_ALLOC(fdesc, falign, flen);
            }
        }
    }
    alloc::~alloc () { }
    SPECIAL::~SPECIAL () { }
    RECORD::~RECORD () { }
    RAW_RECORD::~RAW_RECORD () { }
    RAW_ALLOC::~RAW_ALLOC () { }
    // pickler suppressed for arithop
    arithop read_arithop (asdl::instream & is)
    {
        return static_cast<arithop>(asdl::read_tag8(is));
    }
    arith * arith::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_ARITH:
            {
                auto foper = read_arithop(is);
                auto fsz = asdl::read_int(is);
                return new ARITH(foper, fsz);
            }
          case _con_FLOAT_TO_INT:
            {
                auto fmode = read_rounding_mode(is);
                auto ffrom = asdl::read_int(is);
                auto fto = asdl::read_int(is);
                return new FLOAT_TO_INT(fmode, ffrom, fto);
            }
        }
    }
    arith::~arith () { }
    ARITH::~ARITH () { }
    FLOAT_TO_INT::~FLOAT_TO_INT () { }
    // pickler suppressed for pureop
    pureop read_pureop (asdl::instream & is)
    {
        return static_cast<pureop>(asdl::read_tag8(is));
    }
    pure * pure::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_PURE_ARITH:
            {
                auto foper = read_pureop(is);
                auto fsz = asdl::read_int(is);
                return new PURE_ARITH(foper, fsz);
            }
          case _con_EXTEND:
            {
                auto fsigned = asdl::read_bool(is);
                auto ffrom = asdl::read_int(is);
                auto fto = asdl::read_int(is);
                return new EXTEND(fsigned, ffrom, fto);
            }
          case _con_TRUNC:
            {
                auto ffrom = asdl::read_int(is);
                auto fto = asdl::read_int(is);
                return new TRUNC(ffrom, fto);
            }
          case _con_INT_TO_FLOAT:
            {
                auto ffrom = asdl::read_int(is);
                auto fto = asdl::read_int(is);
                return new INT_TO_FLOAT(ffrom, fto);
            }
          case _con_FLOAT_TO_BITS:
            {
                auto fsz = asdl::read_int(is);
                return new FLOAT_TO_BITS(fsz);
            }
          case _con_BITS_TO_FLOAT:
            {
                auto fsz = asdl::read_int(is);
                return new BITS_TO_FLOAT(fsz);
            }
          case _con_PURE_SUBSCRIPT:
            return new PURE_SUBSCRIPT;
          case _con_PURE_RAW_SUBSCRIPT:
            {
                auto fkind = read_numkind(is);
                auto fsz = asdl::read_int(is);
                return new PURE_RAW_SUBSCRIPT(fkind, fsz);
            }
          case _con_RAW_SELECT:
            {
                auto fkind = read_numkind(is);
                auto fsz = asdl::read_int(is);
                auto foffset = asdl::read_int(is);
                return new RAW_SELECT(fkind, fsz, foffset);
            }
        }
    }
    pure::~pure () { }
    PURE_ARITH::~PURE_ARITH () { }
    EXTEND::~EXTEND () { }
    TRUNC::~TRUNC () { }
    INT_TO_FLOAT::~INT_TO_FLOAT () { }
    FLOAT_TO_BITS::~FLOAT_TO_BITS () { }
    BITS_TO_FLOAT::~BITS_TO_FLOAT () { }
    PURE_SUBSCRIPT::~PURE_SUBSCRIPT () { }
    PURE_RAW_SUBSCRIPT::~PURE_RAW_SUBSCRIPT () { }
    RAW_SELECT::~RAW_SELECT () { }
    looker * looker::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_DEREF:
            return new DEREF;
          case _con_SUBSCRIPT:
            return new SUBSCRIPT;
          case _con_RAW_SUBSCRIPT:
            {
                auto fkind = read_numkind(is);
                auto fsz = asdl::read_int(is);
                return new RAW_SUBSCRIPT(fkind, fsz);
            }
          case _con_RAW_LOAD:
            {
                auto fkind = read_numkind(is);
                auto fsz = asdl::read_int(is);
                return new RAW_LOAD(fkind, fsz);
            }
          case _con_GET_HDLR:
            return new GET_HDLR;
          case _con_GET_VAR:
            return new GET_VAR;
        }
    }
    looker::~looker () { }
    DEREF::~DEREF () { }
    SUBSCRIPT::~SUBSCRIPT () { }
    RAW_SUBSCRIPT::~RAW_SUBSCRIPT () { }
    RAW_LOAD::~RAW_LOAD () { }
    GET_HDLR::~GET_HDLR () { }
    GET_VAR::~GET_VAR () { }
    setter * setter::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_UNBOXED_UPDATE:
            return new UNBOXED_UPDATE;
          case _con_UPDATE:
            return new UPDATE;
          case _con_UNBOXED_ASSIGN:
            return new UNBOXED_ASSIGN;
          case _con_ASSIGN:
            return new ASSIGN;
          case _con_RAW_UPDATE:
            {
                auto fkind = read_numkind(is);
                auto fsz = asdl::read_int(is);
                return new RAW_UPDATE(fkind, fsz);
            }
          case _con_RAW_STORE:
            {
                auto fkind = read_numkind(is);
                auto fsz = asdl::read_int(is);
                return new RAW_STORE(fkind, fsz);
            }
          case _con_SET_HDLR:
            return new SET_HDLR;
          case _con_SET_VAR:
            return new SET_VAR;
        }
    }
    setter::~setter () { }
    UNBOXED_UPDATE::~UNBOXED_UPDATE () { }
    UPDATE::~UPDATE () { }
    UNBOXED_ASSIGN::~UNBOXED_ASSIGN () { }
    ASSIGN::~ASSIGN () { }
    RAW_UPDATE::~RAW_UPDATE () { }
    RAW_STORE::~RAW_STORE () { }
    SET_HDLR::~SET_HDLR () { }
    SET_VAR::~SET_VAR () { }
    // pickler suppressed for cmpop
    cmpop read_cmpop (asdl::instream & is)
    {
        return static_cast<cmpop>(asdl::read_tag8(is));
    }
    // pickler suppressed for fcmpop
    fcmpop read_fcmpop (asdl::instream & is)
    {
        return static_cast<fcmpop>(asdl::read_tag8(is));
    }
    branch * branch::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_CMP:
            {
                auto foper = read_cmpop(is);
                auto fsigned = asdl::read_bool(is);
                auto fsz = asdl::read_int(is);
                return new CMP(foper, fsigned, fsz);
            }
          case _con_FCMP:
            {
                auto foper = read_fcmpop(is);
                auto fsz = asdl::read_int(is);
                return new FCMP(foper, fsz);
            }
          case _con_FSGN:
            {
                auto f0 = asdl::read_int(is);
                return new FSGN(f0);
            }
          case _con_PEQL:
            return new PEQL;
          case _con_PNEQ:
            return new PNEQ;
          case _con_LIMIT:
            {
                auto f0 = asdl::read_uint(is);
                return new LIMIT(f0);
            }
        }
    }
    branch::~branch () { }
    CMP::~CMP () { }
    FCMP::~FCMP () { }
    FSGN::~FSGN () { }
    PEQL::~PEQL () { }
    PNEQ::~PNEQ () { }
    LIMIT::~LIMIT () { }
} // namespace CFG_Prim
namespace CFG {
    ty * ty::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_LABt:
            return new LABt;
          case _con_PTRt:
            return new PTRt;
          case _con_TAGt:
            return new TAGt;
          case _con_NUMt:
            {
                auto fsz = asdl::read_int(is);
                return new NUMt(fsz);
            }
          case _con_FLTt:
            {
                auto fsz = asdl::read_int(is);
                return new FLTt(fsz);
            }
        }
    }
    ty::~ty () { }
    LABt::~LABt () { }
    PTRt::~PTRt () { }
    TAGt::~TAGt () { }
    NUMt::~NUMt () { }
    FLTt::~FLTt () { }
    // ty_seq pickler suppressed
    std::vector<ty *> read_ty_seq (asdl::instream & is)
    {
        return asdl::read_seq<ty>(is);
    }
    exp * exp::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_VAR:
            {
                auto fname = LambdaVar::read_lvar(is);
                return new VAR(fname);
            }
          case _con_LABEL:
            {
                auto fname = LambdaVar::read_lvar(is);
                return new LABEL(fname);
            }
          case _con_NUM:
            {
                auto fiv = asdl::read_integer(is);
                auto fsz = asdl::read_int(is);
                return new NUM(fiv, fsz);
            }
          case _con_LOOKER:
            {
                auto foper = CFG_Prim::looker::read(is);
                auto fargs = read_exp_seq(is);
                return new LOOKER(foper, fargs);
            }
          case _con_PURE:
            {
                auto foper = CFG_Prim::pure::read(is);
                auto fargs = read_exp_seq(is);
                return new PURE(foper, fargs);
            }
          case _con_SELECT:
            {
                auto fidx = asdl::read_int(is);
                auto farg = exp::read(is);
                return new SELECT(fidx, farg);
            }
          case _con_OFFSET:
            {
                auto fidx = asdl::read_int(is);
                auto farg = exp::read(is);
                return new OFFSET(fidx, farg);
            }
        }
    }
    exp::~exp () { }
    VAR::~VAR () { }
    LABEL::~LABEL () { }
    NUM::~NUM () { }
    LOOKER::~LOOKER ()
    {
        delete this->_v_oper;
    }
    PURE::~PURE ()
    {
        delete this->_v_oper;
    }
    SELECT::~SELECT ()
    {
        delete this->_v_arg;
    }
    OFFSET::~OFFSET ()
    {
        delete this->_v_arg;
    }
    // exp_seq pickler suppressed
    std::vector<exp *> read_exp_seq (asdl::instream & is)
    {
        return asdl::read_seq<exp>(is);
    }
    param * param::read (asdl::instream & is)
    {
        auto fname = LambdaVar::read_lvar(is);
        auto fty = ty::read(is);
        return new param(fname, fty);
    }
    param::~param ()
    {
        delete this->_v_ty;
    }
    // param_seq pickler suppressed
    std::vector<param *> read_param_seq (asdl::instream & is)
    {
        return asdl::read_seq<param>(is);
    }
    // pickler suppressed for probability
    probability read_probability (asdl::instream & is)
    {
        auto v = asdl::read_int(is);
        return v;
    }
    stm * stm::read (asdl::instream & is)
    {
        _tag_t tag = static_cast<_tag_t>(asdl::read_tag8(is));
        switch (tag) {
          case _con_LET:
            {
                auto f0 = exp::read(is);
                auto f1 = param::read(is);
                auto f2 = stm::read(is);
                return new LET(f0, f1, f2);
            }
          case _con_ALLOC:
            {
                auto f0 = CFG_Prim::alloc::read(is);
                auto f1 = read_exp_seq(is);
                auto f2 = LambdaVar::read_lvar(is);
                auto f3 = stm::read(is);
                return new ALLOC(f0, f1, f2, f3);
            }
          case _con_APPLY:
            {
                auto f0 = exp::read(is);
                auto f1 = read_exp_seq(is);
                auto f2 = read_ty_seq(is);
                return new APPLY(f0, f1, f2);
            }
          case _con_THROW:
            {
                auto f0 = exp::read(is);
                auto f1 = read_exp_seq(is);
                auto f2 = read_ty_seq(is);
                return new THROW(f0, f1, f2);
            }
          case _con_GOTO:
            {
                auto f0 = LambdaVar::read_lvar(is);
                auto f1 = read_exp_seq(is);
                return new GOTO(f0, f1);
            }
          case _con_SWITCH:
            {
                auto f0 = exp::read(is);
                auto f1 = read_stm_seq(is);
                return new SWITCH(f0, f1);
            }
          case _con_BRANCH:
            {
                auto f0 = CFG_Prim::branch::read(is);
                auto f1 = read_exp_seq(is);
                auto f2 = read_probability(is);
                auto f3 = stm::read(is);
                auto f4 = stm::read(is);
                return new BRANCH(f0, f1, f2, f3, f4);
            }
          case _con_ARITH:
            {
                auto f0 = CFG_Prim::arith::read(is);
                auto f1 = read_exp_seq(is);
                auto f2 = param::read(is);
                auto f3 = stm::read(is);
                return new ARITH(f0, f1, f2, f3);
            }
          case _con_SETTER:
            {
                auto f0 = CFG_Prim::setter::read(is);
                auto f1 = read_exp_seq(is);
                auto f2 = stm::read(is);
                return new SETTER(f0, f1, f2);
            }
          case _con_CALLGC:
            {
                auto f0 = read_exp_seq(is);
                auto f1 = LambdaVar::read_lvar_seq(is);
                auto f2 = stm::read(is);
                return new CALLGC(f0, f1, f2);
            }
          case _con_RCC:
            {
                auto freentrant = asdl::read_bool(is);
                auto flinkage = asdl::read_string(is);
                auto fproto = CTypes::c_proto::read(is);
                auto fargs = read_exp_seq(is);
                auto fresults = read_param_seq(is);
                auto flive = read_param_seq(is);
                auto fk = stm::read(is);
                return new RCC(freentrant, flinkage, fproto, fargs, fresults, flive, fk);
            }
        }
    }
    stm::~stm () { }
    LET::~LET ()
    {
        delete this->_v0;
        delete this->_v1;
        delete this->_v2;
    }
    ALLOC::~ALLOC ()
    {
        delete this->_v0;
        delete this->_v3;
    }
    APPLY::~APPLY ()
    {
        delete this->_v0;
    }
    THROW::~THROW ()
    {
        delete this->_v0;
    }
    GOTO::~GOTO () { }
    SWITCH::~SWITCH ()
    {
        delete this->_v0;
    }
    BRANCH::~BRANCH ()
    {
        delete this->_v0;
        delete this->_v3;
        delete this->_v4;
    }
    ARITH::~ARITH ()
    {
        delete this->_v0;
        delete this->_v2;
        delete this->_v3;
    }
    SETTER::~SETTER ()
    {
        delete this->_v0;
        delete this->_v2;
    }
    CALLGC::~CALLGC ()
    {
        delete this->_v2;
    }
    RCC::~RCC ()
    {
        delete this->_v_proto;
        delete this->_v_k;
    }
    // stm_seq pickler suppressed
    std::vector<stm *> read_stm_seq (asdl::instream & is)
    {
        return asdl::read_seq<stm>(is);
    }
    // pickler suppressed for frag_kind
    frag_kind read_frag_kind (asdl::instream & is)
    {
        return static_cast<frag_kind>(asdl::read_tag8(is));
    }
    frag * frag::read (asdl::instream & is)
    {
        auto fkind = read_frag_kind(is);
        auto flab = LambdaVar::read_lvar(is);
        auto fparams = read_param_seq(is);
        auto fbody = stm::read(is);
        return new frag(fkind, flab, fparams, fbody);
    }
    frag::~frag ()
    {
        delete this->_v_body;
    }
    // frag_seq pickler suppressed
    std::vector<frag *> read_frag_seq (asdl::instream & is)
    {
        return asdl::read_seq<frag>(is);
    }
    attrs * attrs::read (asdl::instream & is)
    {
        auto falignHP = asdl::read_int(is);
        auto fneedsBasePtr = asdl::read_bool(is);
        auto fhasTrapArith = asdl::read_bool(is);
        auto fhasRCC = asdl::read_bool(is);
        return new attrs(falignHP, fneedsBasePtr, fhasTrapArith, fhasRCC);
    }
    attrs::~attrs () { }
    cluster * cluster::read (asdl::instream & is)
    {
        auto fattrs = attrs::read(is);
        auto ffrags = read_frag_seq(is);
        return new cluster(fattrs, ffrags);
    }
    cluster::~cluster ()
    {
        delete this->_v_attrs;
    }
    // cluster_seq pickler suppressed
    std::vector<cluster *> read_cluster_seq (asdl::instream & is)
    {
        return asdl::read_seq<cluster>(is);
    }
    comp_unit * comp_unit::read (asdl::instream & is)
    {
        auto fsrcFile = asdl::read_string(is);
        auto fentry = cluster::read(is);
        auto ffns = read_cluster_seq(is);
        return new comp_unit(fsrcFile, fentry, ffns);
    }
    comp_unit::~comp_unit ()
    {
        delete this->_v_entry;
    }
} // namespace CFG

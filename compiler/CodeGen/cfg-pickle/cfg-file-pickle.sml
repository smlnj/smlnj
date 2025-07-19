(* cfg-file-pickle.sml
 *
 * Generated from cfg.asdl by asdlgen.
 *)

structure LambdaVarFilePickle : LAMBDA_VAR_PICKLE
  where type instream = ASDLFilePickle.instream
  where type outstream = ASDLFilePickle.outstream = struct
    type instream = ASDLFilePickle.instream
    type outstream = ASDLFilePickle.outstream

    val read_lvar = LambdaVarPickle.read_lvar ASDLFilePickle.input1
    val write_lvar = LambdaVarPickle.write_lvar ASDLFilePickle.output1
  end

structure CTypesFilePickle : CTYPES_PICKLE
  where type instream = ASDLFilePickle.instream
  where type outstream = ASDLFilePickle.outstream = struct
    type instream = ASDLFilePickle.instream
    type outstream = ASDLFilePickle.outstream
    (*---------- begin rd-util.in ----------*)
    (* read an option *)
    fun readOption rdFn inS = (case ASDLFilePickle.readTag8 inS
           of 0w0 => NONE
            | 0w1 => let val obj = rdFn inS in SOME obj end
            | _ => raise ASDL.DecodeError
          (* end case *))
    (* read a sequence into a sequence of values *)
    fun readSeq rdFn inS = let
          val len = ASDLFilePickle.readUInt inS
          fun read (0w0, xs) = reverse (xs, [])
            | read (n, xs) = let
                val x = rdFn inS
                in
                  read (n-0w1, x::xs)
                end
          and reverse ([], xs) = xs
            | reverse (x::xr, xs) = reverse (xr, x::xs)
          in
            read (len, [])
          end
(*---------- end rd-util.in ----------*)
(*---------- begin wr-util.in ----------*)
    (* write an option *)
    fun writeOption wrFn (outS, NONE) = ASDLFilePickle.writeTag8(outS, 0w0)
      | writeOption wrFn (outS, SOME obj) = (
          ASDLFilePickle.writeTag8(outS, 0w1);
          wrFn(outS, obj))
    (* write a list of values as a sequence *)
    fun writeSeq wrFn (outS, xs) = let
          fun write [] = ()
            | write (x::xr) = (wrFn(outS, x); write xr)
          in
            ASDLFilePickle.writeUInt(outS, Word.fromInt(length xs));
            write xs
          end
(*---------- end wr-util.in ----------*)

    fun read_calling_convention (inS : instream) = ASDLFilePickle.readString inS
    fun read_c_int (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => CTypes.I_char
            | 0w2 => CTypes.I_short
            | 0w3 => CTypes.I_int
            | 0w4 => CTypes.I_long
            | 0w5 => CTypes.I_long_long
            | _ => raise ASDL.DecodeError)
    fun read_c_type (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => CTypes.C_void
            | 0w2 => CTypes.C_float
            | 0w3 => CTypes.C_double
            | 0w4 => CTypes.C_long_double
            | 0w5 => let val x0 = read_c_int inS in CTypes.C_unsigned (x0) end
            | 0w6 => let val x0 = read_c_int inS in CTypes.C_signed (x0) end
            | 0w7 => CTypes.C_PTR
            | 0w8 => let
              val x0 = read_c_type inS
              val x1 = ASDLFilePickle.readInt inS
              in
                CTypes.C_ARRAY (x0, x1)
              end
            | 0w9 => let val x0 = readSeq read_c_type inS in CTypes.C_STRUCT (x0) end
            | 0w10 => let val x0 = readSeq read_c_type inS in CTypes.C_UNION (x0) end
            | _ => raise ASDL.DecodeError)
    fun read_c_proto (inS : instream) = let
          val conv = read_calling_convention inS
          val retTy = read_c_type inS
          val paramTys = readSeq read_c_type inS
          in
            {conv = conv, retTy = retTy, paramTys = paramTys}
          end
    fun write_calling_convention (outS : outstream, obj) = ASDLFilePickle.writeString
            (outS, obj)
    fun write_c_int (outS : outstream, obj) = (case obj
           of CTypes.I_char => ASDLFilePickle.writeTag8 (outS, 0w1)
            | CTypes.I_short => ASDLFilePickle.writeTag8 (outS, 0w2)
            | CTypes.I_int => ASDLFilePickle.writeTag8 (outS, 0w3)
            | CTypes.I_long => ASDLFilePickle.writeTag8 (outS, 0w4)
            | CTypes.I_long_long => ASDLFilePickle.writeTag8 (outS, 0w5))
    fun write_c_type (outS : outstream, obj) = (case obj
           of CTypes.C_void => ASDLFilePickle.writeTag8 (outS, 0w1)
            | CTypes.C_float => ASDLFilePickle.writeTag8 (outS, 0w2)
            | CTypes.C_double => ASDLFilePickle.writeTag8 (outS, 0w3)
            | CTypes.C_long_double => ASDLFilePickle.writeTag8 (outS, 0w4)
            | CTypes.C_unsigned x0 => (
              ASDLFilePickle.writeTag8 (outS, 0w5);
              write_c_int (outS, x0))
            | CTypes.C_signed x0 => (
              ASDLFilePickle.writeTag8 (outS, 0w6);
              write_c_int (outS, x0))
            | CTypes.C_PTR => ASDLFilePickle.writeTag8 (outS, 0w7)
            | CTypes.C_ARRAY(x0, x1) => (
              ASDLFilePickle.writeTag8 (outS, 0w8);
              write_c_type (outS, x0);
              ASDLFilePickle.writeInt (outS, x1))
            | CTypes.C_STRUCT x0 => (
              ASDLFilePickle.writeTag8 (outS, 0w9);
              writeSeq write_c_type (outS, x0))
            | CTypes.C_UNION x0 => (
              ASDLFilePickle.writeTag8 (outS, 0w10);
              writeSeq write_c_type (outS, x0)))
    fun write_c_proto (outS : outstream, obj) = let
          val {conv, retTy, paramTys} = obj
          in
            write_calling_convention (outS, conv);
            write_c_type (outS, retTy);
            writeSeq write_c_type (outS, paramTys)
          end
  end

structure CFG_PrimFilePickle : CFG__PRIM_PICKLE
  where type instream = ASDLFilePickle.instream
  where type outstream = ASDLFilePickle.outstream = struct
    type instream = ASDLFilePickle.instream
    type outstream = ASDLFilePickle.outstream
    (*---------- begin rd-util.in ----------*)
    (* read an option *)
    fun readOption rdFn inS = (case ASDLFilePickle.readTag8 inS
           of 0w0 => NONE
            | 0w1 => let val obj = rdFn inS in SOME obj end
            | _ => raise ASDL.DecodeError
          (* end case *))
    (* read a sequence into a sequence of values *)
    fun readSeq rdFn inS = let
          val len = ASDLFilePickle.readUInt inS
          fun read (0w0, xs) = reverse (xs, [])
            | read (n, xs) = let
                val x = rdFn inS
                in
                  read (n-0w1, x::xs)
                end
          and reverse ([], xs) = xs
            | reverse (x::xr, xs) = reverse (xr, x::xs)
          in
            read (len, [])
          end
(*---------- end rd-util.in ----------*)
(*---------- begin wr-util.in ----------*)
    (* write an option *)
    fun writeOption wrFn (outS, NONE) = ASDLFilePickle.writeTag8(outS, 0w0)
      | writeOption wrFn (outS, SOME obj) = (
          ASDLFilePickle.writeTag8(outS, 0w1);
          wrFn(outS, obj))
    (* write a list of values as a sequence *)
    fun writeSeq wrFn (outS, xs) = let
          fun write [] = ()
            | write (x::xr) = (wrFn(outS, x); write xr)
          in
            ASDLFilePickle.writeUInt(outS, Word.fromInt(length xs));
            write xs
          end
(*---------- end wr-util.in ----------*)

    fun read_fcmpop (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => CFG_Prim.F_EQ
            | 0w2 => CFG_Prim.F_ULG
            | 0w3 => CFG_Prim.F_UN
            | 0w4 => CFG_Prim.F_LEG
            | 0w5 => CFG_Prim.F_GT
            | 0w6 => CFG_Prim.F_GE
            | 0w7 => CFG_Prim.F_UGT
            | 0w8 => CFG_Prim.F_UGE
            | 0w9 => CFG_Prim.F_LT
            | 0w10 => CFG_Prim.F_LE
            | 0w11 => CFG_Prim.F_ULT
            | 0w12 => CFG_Prim.F_ULE
            | 0w13 => CFG_Prim.F_LG
            | 0w14 => CFG_Prim.F_UE
            | _ => raise ASDL.DecodeError)
    fun read_cmpop (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => CFG_Prim.GT
            | 0w2 => CFG_Prim.GTE
            | 0w3 => CFG_Prim.LT
            | 0w4 => CFG_Prim.LTE
            | 0w5 => CFG_Prim.EQL
            | 0w6 => CFG_Prim.NEQ
            | _ => raise ASDL.DecodeError)
    fun read_branch (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => let
              val oper = read_cmpop inS
              val signed = ASDLFilePickle.readBool inS
              val sz = ASDLFilePickle.readInt inS
              in
                CFG_Prim.CMP {oper = oper, signed = signed, sz = sz}
              end
            | 0w2 => let
              val oper = read_fcmpop inS
              val sz = ASDLFilePickle.readInt inS
              in
                CFG_Prim.FCMP {oper = oper, sz = sz}
              end
            | 0w3 => let val x0 = ASDLFilePickle.readInt inS in CFG_Prim.FSGN (x0) end
            | 0w4 => CFG_Prim.PEQL
            | 0w5 => CFG_Prim.PNEQ
            | 0w6 => let val x0 = ASDLFilePickle.readUInt inS in CFG_Prim.LIMIT (x0) end
            | _ => raise ASDL.DecodeError)
    fun read_numkind (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => CFG_Prim.INT
            | 0w2 => CFG_Prim.FLT
            | _ => raise ASDL.DecodeError)
    fun read_setter (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => CFG_Prim.UNBOXED_UPDATE
            | 0w2 => CFG_Prim.UPDATE
            | 0w3 => CFG_Prim.UNBOXED_ASSIGN
            | 0w4 => CFG_Prim.ASSIGN
            | 0w5 => let
              val kind = read_numkind inS
              val sz = ASDLFilePickle.readInt inS
              in
                CFG_Prim.RAW_UPDATE {kind = kind, sz = sz}
              end
            | 0w6 => let
              val kind = read_numkind inS
              val sz = ASDLFilePickle.readInt inS
              in
                CFG_Prim.RAW_STORE {kind = kind, sz = sz}
              end
            | 0w7 => CFG_Prim.SET_HDLR
            | 0w8 => CFG_Prim.SET_VAR
            | _ => raise ASDL.DecodeError)
    fun read_looker (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => CFG_Prim.DEREF
            | 0w2 => CFG_Prim.SUBSCRIPT
            | 0w3 => let
              val kind = read_numkind inS
              val sz = ASDLFilePickle.readInt inS
              in
                CFG_Prim.RAW_SUBSCRIPT {kind = kind, sz = sz}
              end
            | 0w4 => let
              val kind = read_numkind inS
              val sz = ASDLFilePickle.readInt inS
              in
                CFG_Prim.RAW_LOAD {kind = kind, sz = sz}
              end
            | 0w5 => CFG_Prim.GET_HDLR
            | 0w6 => CFG_Prim.GET_VAR
            | _ => raise ASDL.DecodeError)
    fun read_pureop (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => CFG_Prim.ADD
            | 0w2 => CFG_Prim.SUB
            | 0w3 => CFG_Prim.MUL
            | 0w4 => CFG_Prim.SDIV
            | 0w5 => CFG_Prim.SREM
            | 0w6 => CFG_Prim.UDIV
            | 0w7 => CFG_Prim.UREM
            | 0w8 => CFG_Prim.SHL
            | 0w9 => CFG_Prim.ASHR
            | 0w10 => CFG_Prim.LSHR
            | 0w11 => CFG_Prim.ORB
            | 0w12 => CFG_Prim.XORB
            | 0w13 => CFG_Prim.ANDB
            | 0w14 => CFG_Prim.CNTPOP
            | 0w15 => CFG_Prim.CNTLZ
            | 0w16 => CFG_Prim.CNTTZ
            | 0w17 => CFG_Prim.ROTL
            | 0w18 => CFG_Prim.ROTR
            | 0w19 => CFG_Prim.FADD
            | 0w20 => CFG_Prim.FSUB
            | 0w21 => CFG_Prim.FMUL
            | 0w22 => CFG_Prim.FDIV
            | 0w23 => CFG_Prim.FREM
            | 0w24 => CFG_Prim.FMADD
            | 0w25 => CFG_Prim.FNEG
            | 0w26 => CFG_Prim.FABS
            | 0w27 => CFG_Prim.FCOPYSIGN
            | 0w28 => CFG_Prim.FSQRT
            | _ => raise ASDL.DecodeError)
    fun read_pure (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => let
              val oper = read_pureop inS
              val sz = ASDLFilePickle.readInt inS
              in
                CFG_Prim.PURE_ARITH {oper = oper, sz = sz}
              end
            | 0w2 => let
              val signed = ASDLFilePickle.readBool inS
              val from = ASDLFilePickle.readInt inS
              val to = ASDLFilePickle.readInt inS
              in
                CFG_Prim.EXTEND {signed = signed, from = from, to = to}
              end
            | 0w3 => let
              val from = ASDLFilePickle.readInt inS
              val to = ASDLFilePickle.readInt inS
              in
                CFG_Prim.TRUNC {from = from, to = to}
              end
            | 0w4 => let
              val from = ASDLFilePickle.readInt inS
              val to = ASDLFilePickle.readInt inS
              in
                CFG_Prim.INT_TO_FLOAT {from = from, to = to}
              end
            | 0w5 => let
              val sz = ASDLFilePickle.readInt inS
              in CFG_Prim.FLOAT_TO_BITS {sz = sz}
              end
            | 0w6 => let
              val sz = ASDLFilePickle.readInt inS
              in CFG_Prim.BITS_TO_FLOAT {sz = sz}
              end
            | 0w7 => CFG_Prim.PURE_SUBSCRIPT
            | 0w8 => let
              val kind = read_numkind inS
              val sz = ASDLFilePickle.readInt inS
              in
                CFG_Prim.PURE_RAW_SUBSCRIPT {kind = kind, sz = sz}
              end
            | 0w9 => let
              val kind = read_numkind inS
              val sz = ASDLFilePickle.readInt inS
              val offset = ASDLFilePickle.readInt inS
              in
                CFG_Prim.RAW_SELECT {kind = kind, sz = sz, offset = offset}
              end
            | _ => raise ASDL.DecodeError)
    fun read_arithop (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => CFG_Prim.IADD
            | 0w2 => CFG_Prim.ISUB
            | 0w3 => CFG_Prim.IMUL
            | 0w4 => CFG_Prim.IDIV
            | 0w5 => CFG_Prim.IREM
            | _ => raise ASDL.DecodeError)
    fun read_rounding_mode (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => CFG_Prim.TO_NEAREST
            | 0w2 => CFG_Prim.TO_NEGINF
            | 0w3 => CFG_Prim.TO_POSINF
            | 0w4 => CFG_Prim.TO_ZERO
            | _ => raise ASDL.DecodeError)
    fun read_arith (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => let
              val oper = read_arithop inS
              val sz = ASDLFilePickle.readInt inS
              in
                CFG_Prim.ARITH {oper = oper, sz = sz}
              end
            | 0w2 => let
              val mode = read_rounding_mode inS
              val from = ASDLFilePickle.readInt inS
              val to = ASDLFilePickle.readInt inS
              in
                CFG_Prim.FLOAT_TO_INT {mode = mode, from = from, to = to}
              end
            | _ => raise ASDL.DecodeError)
    fun read_raw_ty (inS : instream) = let
          val kind = read_numkind inS
          val sz = ASDLFilePickle.readInt inS
          in
            {kind = kind, sz = sz}
          end
    fun read_alloc (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => CFG_Prim.SPECIAL
            | 0w2 => let
              val desc = ASDLFilePickle.readInteger inS
              val mut = ASDLFilePickle.readBool inS
              in
                CFG_Prim.RECORD {desc = desc, mut = mut}
              end
            | 0w3 => let
              val desc = ASDLFilePickle.readInteger inS
              val align = ASDLFilePickle.readInt inS
              val fields = readSeq read_raw_ty inS
              in
                CFG_Prim.RAW_RECORD {desc = desc, align = align, fields = fields}
              end
            | 0w4 => let
              val desc = readOption ASDLFilePickle.readInteger inS
              val align = ASDLFilePickle.readInt inS
              val len = ASDLFilePickle.readInt inS
              in
                CFG_Prim.RAW_ALLOC {desc = desc, align = align, len = len}
              end
            | _ => raise ASDL.DecodeError)
    fun write_fcmpop (outS : outstream, obj) = (case obj
           of CFG_Prim.F_EQ => ASDLFilePickle.writeTag8 (outS, 0w1)
            | CFG_Prim.F_ULG => ASDLFilePickle.writeTag8 (outS, 0w2)
            | CFG_Prim.F_UN => ASDLFilePickle.writeTag8 (outS, 0w3)
            | CFG_Prim.F_LEG => ASDLFilePickle.writeTag8 (outS, 0w4)
            | CFG_Prim.F_GT => ASDLFilePickle.writeTag8 (outS, 0w5)
            | CFG_Prim.F_GE => ASDLFilePickle.writeTag8 (outS, 0w6)
            | CFG_Prim.F_UGT => ASDLFilePickle.writeTag8 (outS, 0w7)
            | CFG_Prim.F_UGE => ASDLFilePickle.writeTag8 (outS, 0w8)
            | CFG_Prim.F_LT => ASDLFilePickle.writeTag8 (outS, 0w9)
            | CFG_Prim.F_LE => ASDLFilePickle.writeTag8 (outS, 0w10)
            | CFG_Prim.F_ULT => ASDLFilePickle.writeTag8 (outS, 0w11)
            | CFG_Prim.F_ULE => ASDLFilePickle.writeTag8 (outS, 0w12)
            | CFG_Prim.F_LG => ASDLFilePickle.writeTag8 (outS, 0w13)
            | CFG_Prim.F_UE => ASDLFilePickle.writeTag8 (outS, 0w14))
    fun write_cmpop (outS : outstream, obj) = (case obj
           of CFG_Prim.GT => ASDLFilePickle.writeTag8 (outS, 0w1)
            | CFG_Prim.GTE => ASDLFilePickle.writeTag8 (outS, 0w2)
            | CFG_Prim.LT => ASDLFilePickle.writeTag8 (outS, 0w3)
            | CFG_Prim.LTE => ASDLFilePickle.writeTag8 (outS, 0w4)
            | CFG_Prim.EQL => ASDLFilePickle.writeTag8 (outS, 0w5)
            | CFG_Prim.NEQ => ASDLFilePickle.writeTag8 (outS, 0w6))
    fun write_branch (outS : outstream, obj) = (case obj
           of CFG_Prim.CMP{oper, signed, sz} => (
              ASDLFilePickle.writeTag8 (outS, 0w1);
              write_cmpop (outS, oper);
              ASDLFilePickle.writeBool (outS, signed);
              ASDLFilePickle.writeInt (outS, sz))
            | CFG_Prim.FCMP{oper, sz} => (
              ASDLFilePickle.writeTag8 (outS, 0w2);
              write_fcmpop (outS, oper);
              ASDLFilePickle.writeInt (outS, sz))
            | CFG_Prim.FSGN x0 => (
              ASDLFilePickle.writeTag8 (outS, 0w3);
              ASDLFilePickle.writeInt (outS, x0))
            | CFG_Prim.PEQL => ASDLFilePickle.writeTag8 (outS, 0w4)
            | CFG_Prim.PNEQ => ASDLFilePickle.writeTag8 (outS, 0w5)
            | CFG_Prim.LIMIT x0 => (
              ASDLFilePickle.writeTag8 (outS, 0w6);
              ASDLFilePickle.writeUInt (outS, x0)))
    fun write_numkind (outS : outstream, obj) = (case obj
           of CFG_Prim.INT => ASDLFilePickle.writeTag8 (outS, 0w1)
            | CFG_Prim.FLT => ASDLFilePickle.writeTag8 (outS, 0w2))
    fun write_setter (outS : outstream, obj) = (case obj
           of CFG_Prim.UNBOXED_UPDATE => ASDLFilePickle.writeTag8 (outS, 0w1)
            | CFG_Prim.UPDATE => ASDLFilePickle.writeTag8 (outS, 0w2)
            | CFG_Prim.UNBOXED_ASSIGN => ASDLFilePickle.writeTag8 (outS, 0w3)
            | CFG_Prim.ASSIGN => ASDLFilePickle.writeTag8 (outS, 0w4)
            | CFG_Prim.RAW_UPDATE{kind, sz} => (
              ASDLFilePickle.writeTag8 (outS, 0w5);
              write_numkind (outS, kind);
              ASDLFilePickle.writeInt (outS, sz))
            | CFG_Prim.RAW_STORE{kind, sz} => (
              ASDLFilePickle.writeTag8 (outS, 0w6);
              write_numkind (outS, kind);
              ASDLFilePickle.writeInt (outS, sz))
            | CFG_Prim.SET_HDLR => ASDLFilePickle.writeTag8 (outS, 0w7)
            | CFG_Prim.SET_VAR => ASDLFilePickle.writeTag8 (outS, 0w8))
    fun write_looker (outS : outstream, obj) = (case obj
           of CFG_Prim.DEREF => ASDLFilePickle.writeTag8 (outS, 0w1)
            | CFG_Prim.SUBSCRIPT => ASDLFilePickle.writeTag8 (outS, 0w2)
            | CFG_Prim.RAW_SUBSCRIPT{kind, sz} => (
              ASDLFilePickle.writeTag8 (outS, 0w3);
              write_numkind (outS, kind);
              ASDLFilePickle.writeInt (outS, sz))
            | CFG_Prim.RAW_LOAD{kind, sz} => (
              ASDLFilePickle.writeTag8 (outS, 0w4);
              write_numkind (outS, kind);
              ASDLFilePickle.writeInt (outS, sz))
            | CFG_Prim.GET_HDLR => ASDLFilePickle.writeTag8 (outS, 0w5)
            | CFG_Prim.GET_VAR => ASDLFilePickle.writeTag8 (outS, 0w6))
    fun write_pureop (outS : outstream, obj) = (case obj
           of CFG_Prim.ADD => ASDLFilePickle.writeTag8 (outS, 0w1)
            | CFG_Prim.SUB => ASDLFilePickle.writeTag8 (outS, 0w2)
            | CFG_Prim.MUL => ASDLFilePickle.writeTag8 (outS, 0w3)
            | CFG_Prim.SDIV => ASDLFilePickle.writeTag8 (outS, 0w4)
            | CFG_Prim.SREM => ASDLFilePickle.writeTag8 (outS, 0w5)
            | CFG_Prim.UDIV => ASDLFilePickle.writeTag8 (outS, 0w6)
            | CFG_Prim.UREM => ASDLFilePickle.writeTag8 (outS, 0w7)
            | CFG_Prim.SHL => ASDLFilePickle.writeTag8 (outS, 0w8)
            | CFG_Prim.ASHR => ASDLFilePickle.writeTag8 (outS, 0w9)
            | CFG_Prim.LSHR => ASDLFilePickle.writeTag8 (outS, 0w10)
            | CFG_Prim.ORB => ASDLFilePickle.writeTag8 (outS, 0w11)
            | CFG_Prim.XORB => ASDLFilePickle.writeTag8 (outS, 0w12)
            | CFG_Prim.ANDB => ASDLFilePickle.writeTag8 (outS, 0w13)
            | CFG_Prim.CNTPOP => ASDLFilePickle.writeTag8 (outS, 0w14)
            | CFG_Prim.CNTLZ => ASDLFilePickle.writeTag8 (outS, 0w15)
            | CFG_Prim.CNTTZ => ASDLFilePickle.writeTag8 (outS, 0w16)
            | CFG_Prim.ROTL => ASDLFilePickle.writeTag8 (outS, 0w17)
            | CFG_Prim.ROTR => ASDLFilePickle.writeTag8 (outS, 0w18)
            | CFG_Prim.FADD => ASDLFilePickle.writeTag8 (outS, 0w19)
            | CFG_Prim.FSUB => ASDLFilePickle.writeTag8 (outS, 0w20)
            | CFG_Prim.FMUL => ASDLFilePickle.writeTag8 (outS, 0w21)
            | CFG_Prim.FDIV => ASDLFilePickle.writeTag8 (outS, 0w22)
            | CFG_Prim.FREM => ASDLFilePickle.writeTag8 (outS, 0w23)
            | CFG_Prim.FMADD => ASDLFilePickle.writeTag8 (outS, 0w24)
            | CFG_Prim.FNEG => ASDLFilePickle.writeTag8 (outS, 0w25)
            | CFG_Prim.FABS => ASDLFilePickle.writeTag8 (outS, 0w26)
            | CFG_Prim.FCOPYSIGN => ASDLFilePickle.writeTag8 (outS, 0w27)
            | CFG_Prim.FSQRT => ASDLFilePickle.writeTag8 (outS, 0w28))
    fun write_pure (outS : outstream, obj) = (case obj
           of CFG_Prim.PURE_ARITH{oper, sz} => (
              ASDLFilePickle.writeTag8 (outS, 0w1);
              write_pureop (outS, oper);
              ASDLFilePickle.writeInt (outS, sz))
            | CFG_Prim.EXTEND{signed, from, to} => (
              ASDLFilePickle.writeTag8 (outS, 0w2);
              ASDLFilePickle.writeBool (outS, signed);
              ASDLFilePickle.writeInt (outS, from);
              ASDLFilePickle.writeInt (outS, to))
            | CFG_Prim.TRUNC{from, to} => (
              ASDLFilePickle.writeTag8 (outS, 0w3);
              ASDLFilePickle.writeInt (outS, from);
              ASDLFilePickle.writeInt (outS, to))
            | CFG_Prim.INT_TO_FLOAT{from, to} => (
              ASDLFilePickle.writeTag8 (outS, 0w4);
              ASDLFilePickle.writeInt (outS, from);
              ASDLFilePickle.writeInt (outS, to))
            | CFG_Prim.FLOAT_TO_BITS{sz} => (
              ASDLFilePickle.writeTag8 (outS, 0w5);
              ASDLFilePickle.writeInt (outS, sz))
            | CFG_Prim.BITS_TO_FLOAT{sz} => (
              ASDLFilePickle.writeTag8 (outS, 0w6);
              ASDLFilePickle.writeInt (outS, sz))
            | CFG_Prim.PURE_SUBSCRIPT => ASDLFilePickle.writeTag8 (outS, 0w7)
            | CFG_Prim.PURE_RAW_SUBSCRIPT{kind, sz} => (
              ASDLFilePickle.writeTag8 (outS, 0w8);
              write_numkind (outS, kind);
              ASDLFilePickle.writeInt (outS, sz))
            | CFG_Prim.RAW_SELECT{kind, sz, offset} => (
              ASDLFilePickle.writeTag8 (outS, 0w9);
              write_numkind (outS, kind);
              ASDLFilePickle.writeInt (outS, sz);
              ASDLFilePickle.writeInt (outS, offset)))
    fun write_arithop (outS : outstream, obj) = (case obj
           of CFG_Prim.IADD => ASDLFilePickle.writeTag8 (outS, 0w1)
            | CFG_Prim.ISUB => ASDLFilePickle.writeTag8 (outS, 0w2)
            | CFG_Prim.IMUL => ASDLFilePickle.writeTag8 (outS, 0w3)
            | CFG_Prim.IDIV => ASDLFilePickle.writeTag8 (outS, 0w4)
            | CFG_Prim.IREM => ASDLFilePickle.writeTag8 (outS, 0w5))
    fun write_rounding_mode (outS : outstream, obj) = (case obj
           of CFG_Prim.TO_NEAREST => ASDLFilePickle.writeTag8 (outS, 0w1)
            | CFG_Prim.TO_NEGINF => ASDLFilePickle.writeTag8 (outS, 0w2)
            | CFG_Prim.TO_POSINF => ASDLFilePickle.writeTag8 (outS, 0w3)
            | CFG_Prim.TO_ZERO => ASDLFilePickle.writeTag8 (outS, 0w4))
    fun write_arith (outS : outstream, obj) = (case obj
           of CFG_Prim.ARITH{oper, sz} => (
              ASDLFilePickle.writeTag8 (outS, 0w1);
              write_arithop (outS, oper);
              ASDLFilePickle.writeInt (outS, sz))
            | CFG_Prim.FLOAT_TO_INT{mode, from, to} => (
              ASDLFilePickle.writeTag8 (outS, 0w2);
              write_rounding_mode (outS, mode);
              ASDLFilePickle.writeInt (outS, from);
              ASDLFilePickle.writeInt (outS, to)))
    fun write_raw_ty (outS : outstream, obj) = let
          val {kind, sz} = obj
          in
            write_numkind (outS, kind);
            ASDLFilePickle.writeInt (outS, sz)
          end
    fun write_alloc (outS : outstream, obj) = (case obj
           of CFG_Prim.SPECIAL => ASDLFilePickle.writeTag8 (outS, 0w1)
            | CFG_Prim.RECORD{desc, mut} => (
              ASDLFilePickle.writeTag8 (outS, 0w2);
              ASDLFilePickle.writeInteger (outS, desc);
              ASDLFilePickle.writeBool (outS, mut))
            | CFG_Prim.RAW_RECORD{desc, align, fields} => (
              ASDLFilePickle.writeTag8 (outS, 0w3);
              ASDLFilePickle.writeInteger (outS, desc);
              ASDLFilePickle.writeInt (outS, align);
              writeSeq write_raw_ty (outS, fields))
            | CFG_Prim.RAW_ALLOC{desc, align, len} => (
              ASDLFilePickle.writeTag8 (outS, 0w4);
              writeOption ASDLFilePickle.writeInteger (outS, desc);
              ASDLFilePickle.writeInt (outS, align);
              ASDLFilePickle.writeInt (outS, len)))
  end

structure CFGFilePickle : CFGPICKLE
  where type instream = ASDLFilePickle.instream
  where type outstream = ASDLFilePickle.outstream = struct
    type instream = ASDLFilePickle.instream
    type outstream = ASDLFilePickle.outstream
    (*---------- begin rd-util.in ----------*)
    (* read an option *)
    fun readOption rdFn inS = (case ASDLFilePickle.readTag8 inS
           of 0w0 => NONE
            | 0w1 => let val obj = rdFn inS in SOME obj end
            | _ => raise ASDL.DecodeError
          (* end case *))
    (* read a sequence into a sequence of values *)
    fun readSeq rdFn inS = let
          val len = ASDLFilePickle.readUInt inS
          fun read (0w0, xs) = reverse (xs, [])
            | read (n, xs) = let
                val x = rdFn inS
                in
                  read (n-0w1, x::xs)
                end
          and reverse ([], xs) = xs
            | reverse (x::xr, xs) = reverse (xr, x::xs)
          in
            read (len, [])
          end
(*---------- end rd-util.in ----------*)
(*---------- begin wr-util.in ----------*)
    (* write an option *)
    fun writeOption wrFn (outS, NONE) = ASDLFilePickle.writeTag8(outS, 0w0)
      | writeOption wrFn (outS, SOME obj) = (
          ASDLFilePickle.writeTag8(outS, 0w1);
          wrFn(outS, obj))
    (* write a list of values as a sequence *)
    fun writeSeq wrFn (outS, xs) = let
          fun write [] = ()
            | write (x::xr) = (wrFn(outS, x); write xr)
          in
            ASDLFilePickle.writeUInt(outS, Word.fromInt(length xs));
            write xs
          end
(*---------- end wr-util.in ----------*)

    fun read_attrs (inS : instream) = let
          val alignHP = ASDLFilePickle.readInt inS
          val needsBasePtr = ASDLFilePickle.readBool inS
          val hasTrapArith = ASDLFilePickle.readBool inS
          val hasRCC = ASDLFilePickle.readBool inS
          in
            {
              alignHP = alignHP,
              needsBasePtr = needsBasePtr,
              hasTrapArith = hasTrapArith,
              hasRCC = hasRCC}
          end
    fun read_frag_kind (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => CFG.STD_FUN
            | 0w2 => CFG.STD_CONT
            | 0w3 => CFG.KNOWN_FUN
            | 0w4 => CFG.INTERNAL
            | _ => raise ASDL.DecodeError)
    fun read_probability (inS : instream) = ASDLFilePickle.readInt inS
    fun read_ty (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => CFG.LABt
            | 0w2 => CFG.PTRt
            | 0w3 => CFG.TAGt
            | 0w4 => let val sz = ASDLFilePickle.readInt inS in CFG.NUMt {sz = sz} end
            | 0w5 => let val sz = ASDLFilePickle.readInt inS in CFG.FLTt {sz = sz} end
            | _ => raise ASDL.DecodeError)
    fun read_param (inS : instream) = let
          val name = LambdaVarFilePickle.read_lvar inS
          val ty = read_ty inS
          in
            {name = name, ty = ty}
          end
    fun read_exp (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => let
              val name = LambdaVarFilePickle.read_lvar inS
              in CFG.VAR {name = name}
              end
            | 0w2 => let
              val name = LambdaVarFilePickle.read_lvar inS
              in CFG.LABEL {name = name}
              end
            | 0w3 => let
              val iv = ASDLFilePickle.readInteger inS
              val sz = ASDLFilePickle.readInt inS
              in
                CFG.NUM {iv = iv, sz = sz}
              end
            | 0w4 => let
              val oper = CFG_PrimFilePickle.read_looker inS
              val args = readSeq read_exp inS
              in
                CFG.LOOKER {oper = oper, args = args}
              end
            | 0w5 => let
              val oper = CFG_PrimFilePickle.read_pure inS
              val args = readSeq read_exp inS
              in
                CFG.PURE {oper = oper, args = args}
              end
            | 0w6 => let
              val idx = ASDLFilePickle.readInt inS
              val arg = read_exp inS
              in
                CFG.SELECT {idx = idx, arg = arg}
              end
            | _ => raise ASDL.DecodeError)
    fun read_stm (inS : instream) = (case ASDLFilePickle.readTag8 inS
           of 0w1 => let
              val x0 = read_exp inS
              val x1 = read_param inS
              val x2 = read_stm inS
              in
                CFG.LET (x0, x1, x2)
              end
            | 0w2 => let
              val x0 = CFG_PrimFilePickle.read_alloc inS
              val x1 = readSeq read_exp inS
              val x2 = LambdaVarFilePickle.read_lvar inS
              val x3 = read_stm inS
              in
                CFG.ALLOC (x0, x1, x2, x3)
              end
            | 0w3 => let
              val x0 = read_exp inS
              val x1 = readSeq read_exp inS
              val x2 = readSeq read_ty inS
              in
                CFG.APPLY (x0, x1, x2)
              end
            | 0w4 => let
              val x0 = read_exp inS
              val x1 = readSeq read_exp inS
              val x2 = readSeq read_ty inS
              in
                CFG.THROW (x0, x1, x2)
              end
            | 0w5 => let
              val x0 = LambdaVarFilePickle.read_lvar inS
              val x1 = readSeq read_exp inS
              in
                CFG.GOTO (x0, x1)
              end
            | 0w6 => let
              val x0 = read_exp inS
              val x1 = readSeq read_stm inS
              in
                CFG.SWITCH (x0, x1)
              end
            | 0w7 => let
              val x0 = CFG_PrimFilePickle.read_branch inS
              val x1 = readSeq read_exp inS
              val x2 = read_probability inS
              val x3 = read_stm inS
              val x4 = read_stm inS
              in
                CFG.BRANCH (x0, x1, x2, x3, x4)
              end
            | 0w8 => let
              val x0 = CFG_PrimFilePickle.read_arith inS
              val x1 = readSeq read_exp inS
              val x2 = read_param inS
              val x3 = read_stm inS
              in
                CFG.ARITH (x0, x1, x2, x3)
              end
            | 0w9 => let
              val x0 = CFG_PrimFilePickle.read_setter inS
              val x1 = readSeq read_exp inS
              val x2 = read_stm inS
              in
                CFG.SETTER (x0, x1, x2)
              end
            | 0w10 => let
              val x0 = readSeq read_exp inS
              val x1 = readSeq LambdaVarFilePickle.read_lvar inS
              val x2 = read_stm inS
              in
                CFG.CALLGC (x0, x1, x2)
              end
            | 0w11 => let
              val reentrant = ASDLFilePickle.readBool inS
              val linkage = ASDLFilePickle.readString inS
              val proto = CTypesFilePickle.read_c_proto inS
              val args = readSeq read_exp inS
              val results = readSeq read_param inS
              val live = readSeq read_param inS
              val k = read_stm inS
              in
                CFG.RCC
                  {
                    reentrant = reentrant,
                    linkage = linkage,
                    proto = proto,
                    args = args,
                    results = results,
                    live = live,
                    k = k}
              end
            | _ => raise ASDL.DecodeError)
    fun read_frag (inS : instream) = let
          val kind = read_frag_kind inS
          val lab = LambdaVarFilePickle.read_lvar inS
          val params = readSeq read_param inS
          val body = read_stm inS
          in
            CFG.Frag {kind = kind, lab = lab, params = params, body = body}
          end
    fun read_cluster (inS : instream) = let
          val attrs = read_attrs inS
          val frags = readSeq read_frag inS
          in
            CFG.Cluster {attrs = attrs, frags = frags}
          end
    fun read_comp_unit (inS : instream) = let
          val srcFile = ASDLFilePickle.readString inS
          val entry = read_cluster inS
          val fns = readSeq read_cluster inS
          in
            {srcFile = srcFile, entry = entry, fns = fns}
          end
    fun write_attrs (outS : outstream, obj) = let
          val {alignHP, needsBasePtr, hasTrapArith, hasRCC} = obj
          in
            ASDLFilePickle.writeInt (outS, alignHP);
            ASDLFilePickle.writeBool (outS, needsBasePtr);
            ASDLFilePickle.writeBool (outS, hasTrapArith);
            ASDLFilePickle.writeBool (outS, hasRCC)
          end
    fun write_frag_kind (outS : outstream, obj) = (case obj
           of CFG.STD_FUN => ASDLFilePickle.writeTag8 (outS, 0w1)
            | CFG.STD_CONT => ASDLFilePickle.writeTag8 (outS, 0w2)
            | CFG.KNOWN_FUN => ASDLFilePickle.writeTag8 (outS, 0w3)
            | CFG.INTERNAL => ASDLFilePickle.writeTag8 (outS, 0w4))
    fun write_probability (outS : outstream, obj) = ASDLFilePickle.writeInt (outS, obj)
    fun write_ty (outS : outstream, obj) = (case obj
           of CFG.LABt => ASDLFilePickle.writeTag8 (outS, 0w1)
            | CFG.PTRt => ASDLFilePickle.writeTag8 (outS, 0w2)
            | CFG.TAGt => ASDLFilePickle.writeTag8 (outS, 0w3)
            | CFG.NUMt{sz} => (
              ASDLFilePickle.writeTag8 (outS, 0w4);
              ASDLFilePickle.writeInt (outS, sz))
            | CFG.FLTt{sz} => (
              ASDLFilePickle.writeTag8 (outS, 0w5);
              ASDLFilePickle.writeInt (outS, sz)))
    fun write_param (outS : outstream, obj) = let
          val {name, ty} = obj
          in
            LambdaVarFilePickle.write_lvar (outS, name);
            write_ty (outS, ty)
          end
    fun write_exp (outS : outstream, obj) = (case obj
           of CFG.VAR{name} => (
              ASDLFilePickle.writeTag8 (outS, 0w1);
              LambdaVarFilePickle.write_lvar (outS, name))
            | CFG.LABEL{name} => (
              ASDLFilePickle.writeTag8 (outS, 0w2);
              LambdaVarFilePickle.write_lvar (outS, name))
            | CFG.NUM{iv, sz} => (
              ASDLFilePickle.writeTag8 (outS, 0w3);
              ASDLFilePickle.writeInteger (outS, iv);
              ASDLFilePickle.writeInt (outS, sz))
            | CFG.LOOKER{oper, args} => (
              ASDLFilePickle.writeTag8 (outS, 0w4);
              CFG_PrimFilePickle.write_looker (outS, oper);
              writeSeq write_exp (outS, args))
            | CFG.PURE{oper, args} => (
              ASDLFilePickle.writeTag8 (outS, 0w5);
              CFG_PrimFilePickle.write_pure (outS, oper);
              writeSeq write_exp (outS, args))
            | CFG.SELECT{idx, arg} => (
              ASDLFilePickle.writeTag8 (outS, 0w6);
              ASDLFilePickle.writeInt (outS, idx);
              write_exp (outS, arg)))
    fun write_stm (outS : outstream, obj) = (case obj
           of CFG.LET(x0, x1, x2) => (
              ASDLFilePickle.writeTag8 (outS, 0w1);
              write_exp (outS, x0);
              write_param (outS, x1);
              write_stm (outS, x2))
            | CFG.ALLOC(x0, x1, x2, x3) => (
              ASDLFilePickle.writeTag8 (outS, 0w2);
              CFG_PrimFilePickle.write_alloc (outS, x0);
              writeSeq write_exp (outS, x1);
              LambdaVarFilePickle.write_lvar (outS, x2);
              write_stm (outS, x3))
            | CFG.APPLY(x0, x1, x2) => (
              ASDLFilePickle.writeTag8 (outS, 0w3);
              write_exp (outS, x0);
              writeSeq write_exp (outS, x1);
              writeSeq write_ty (outS, x2))
            | CFG.THROW(x0, x1, x2) => (
              ASDLFilePickle.writeTag8 (outS, 0w4);
              write_exp (outS, x0);
              writeSeq write_exp (outS, x1);
              writeSeq write_ty (outS, x2))
            | CFG.GOTO(x0, x1) => (
              ASDLFilePickle.writeTag8 (outS, 0w5);
              LambdaVarFilePickle.write_lvar (outS, x0);
              writeSeq write_exp (outS, x1))
            | CFG.SWITCH(x0, x1) => (
              ASDLFilePickle.writeTag8 (outS, 0w6);
              write_exp (outS, x0);
              writeSeq write_stm (outS, x1))
            | CFG.BRANCH(x0, x1, x2, x3, x4) => (
              ASDLFilePickle.writeTag8 (outS, 0w7);
              CFG_PrimFilePickle.write_branch (outS, x0);
              writeSeq write_exp (outS, x1);
              write_probability (outS, x2);
              write_stm (outS, x3);
              write_stm (outS, x4))
            | CFG.ARITH(x0, x1, x2, x3) => (
              ASDLFilePickle.writeTag8 (outS, 0w8);
              CFG_PrimFilePickle.write_arith (outS, x0);
              writeSeq write_exp (outS, x1);
              write_param (outS, x2);
              write_stm (outS, x3))
            | CFG.SETTER(x0, x1, x2) => (
              ASDLFilePickle.writeTag8 (outS, 0w9);
              CFG_PrimFilePickle.write_setter (outS, x0);
              writeSeq write_exp (outS, x1);
              write_stm (outS, x2))
            | CFG.CALLGC(x0, x1, x2) => (
              ASDLFilePickle.writeTag8 (outS, 0w10);
              writeSeq write_exp (outS, x0);
              writeSeq LambdaVarFilePickle.write_lvar (outS, x1);
              write_stm (outS, x2))
            | CFG.RCC{reentrant, linkage, proto, args, results, live, k} => (
              ASDLFilePickle.writeTag8 (outS, 0w11);
              ASDLFilePickle.writeBool (outS, reentrant);
              ASDLFilePickle.writeString (outS, linkage);
              CTypesFilePickle.write_c_proto (outS, proto);
              writeSeq write_exp (outS, args);
              writeSeq write_param (outS, results);
              writeSeq write_param (outS, live);
              write_stm (outS, k)))
    fun write_frag (outS : outstream, obj) = let
          val CFG.Frag{kind, lab, params, body} = obj
          in
            write_frag_kind (outS, kind);
            LambdaVarFilePickle.write_lvar (outS, lab);
            writeSeq write_param (outS, params);
            write_stm (outS, body)
          end
    fun write_cluster (outS : outstream, obj) = let
          val CFG.Cluster{attrs, frags} = obj
          in
            write_attrs (outS, attrs);
            writeSeq write_frag (outS, frags)
          end
    fun write_comp_unit (outS : outstream, obj) = let
          val {srcFile, entry, fns} = obj
          in
            ASDLFilePickle.writeString (outS, srcFile);
            write_cluster (outS, entry);
            writeSeq write_cluster (outS, fns)
          end
  end


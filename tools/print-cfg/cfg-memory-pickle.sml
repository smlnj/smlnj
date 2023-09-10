(* cfg-memory-pickle.sml
 *
 * Generated from cfg.asdl by asdlgen.
 *)

structure LambdaVarMemoryPickle : LAMBDA_VAR_PICKLE
  where type instream = ASDLMemoryPickle.instream
  where type outstream = ASDLMemoryPickle.outstream = struct
    (*---------- begin streams.in ----------*)
    type instream = ASDLMemoryPickle.instream
    type outstream = ASDLMemoryPickle.outstream
(*---------- end streams.in ----------*)

    val write_lvar = LambdaVarPickle.write_lvar ASDLMemoryPickle.output1
    val read_lvar = LambdaVarPickle.read_lvar ASDLMemoryPickle.input1
  end

structure CTypesMemoryPickle : CTYPES_PICKLE
  where type instream = ASDLMemoryPickle.instream
  where type outstream = ASDLMemoryPickle.outstream = struct
    (*---------- begin pickle-util.in ----------*)
    type instream = ASDLMemoryPickle.instream
    type outstream = ASDLMemoryPickle.outstream
  (* write an option *)
    fun writeOption wrFn (outS, NONE) = ASDLMemoryPickle.writeTag8(outS, 0w0)
      | writeOption wrFn (outS, SOME obj) = (
          ASDLMemoryPickle.writeTag8(outS, 0w1);
          wrFn(outS, obj))
  (* read an option *)
    fun readOption rdFn inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w0 => NONE
            | 0w1 => let val obj = rdFn inS in SOME obj end
            | _ => raise ASDL.DecodeError
          (* end case *))
  (* write a list of values as a sequence *)
    fun writeSeq wrFn (outS, xs) = let
          fun write [] = ()
            | write (x::xr) = (wrFn(outS, x); write xr)
          in
            ASDLMemoryPickle.writeUInt(outS, Word.fromInt(length xs));
            write xs
          end
  (* read a sequence into a sequence of values *)
    fun readSeq rdFn inS = let
          val len = ASDLMemoryPickle.readUInt inS
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
(*---------- end pickle-util.in ----------*)

    fun write_calling_convention (outS, obj) = ASDLMemoryPickle.writeString (outS, obj)
    fun read_calling_convention inS = ASDLMemoryPickle.readString inS
    fun write_c_int (outS, obj) = (case obj
           of CTypes.I_char => ASDLMemoryPickle.writeTag8 (outS, 0w1)
            | CTypes.I_short => ASDLMemoryPickle.writeTag8 (outS, 0w2)
            | CTypes.I_int => ASDLMemoryPickle.writeTag8 (outS, 0w3)
            | CTypes.I_long => ASDLMemoryPickle.writeTag8 (outS, 0w4)
            | CTypes.I_long_long => ASDLMemoryPickle.writeTag8 (outS, 0w5))
    fun read_c_int inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => CTypes.I_char
            | 0w2 => CTypes.I_short
            | 0w3 => CTypes.I_int
            | 0w4 => CTypes.I_long
            | 0w5 => CTypes.I_long_long
            | _ => raise ASDL.DecodeError)
    fun write_c_type (outS, obj) = (case obj
           of CTypes.C_void => ASDLMemoryPickle.writeTag8 (outS, 0w1)
            | CTypes.C_float => ASDLMemoryPickle.writeTag8 (outS, 0w2)
            | CTypes.C_double => ASDLMemoryPickle.writeTag8 (outS, 0w3)
            | CTypes.C_long_double => ASDLMemoryPickle.writeTag8 (outS, 0w4)
            | CTypes.C_unsigned x0 => (
              ASDLMemoryPickle.writeTag8 (outS, 0w5);
              write_c_int (outS, x0))
            | CTypes.C_signed x0 => (
              ASDLMemoryPickle.writeTag8 (outS, 0w6);
              write_c_int (outS, x0))
            | CTypes.C_PTR => ASDLMemoryPickle.writeTag8 (outS, 0w7)
            | CTypes.C_ARRAY(x0, x1) => (
              ASDLMemoryPickle.writeTag8 (outS, 0w8);
              write_c_type (outS, x0);
              ASDLMemoryPickle.writeInt (outS, x1))
            | CTypes.C_STRUCT x0 => (
              ASDLMemoryPickle.writeTag8 (outS, 0w9);
              writeSeq write_c_type (outS, x0))
            | CTypes.C_UNION x0 => (
              ASDLMemoryPickle.writeTag8 (outS, 0w10);
              writeSeq write_c_type (outS, x0)))
    fun read_c_type inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => CTypes.C_void
            | 0w2 => CTypes.C_float
            | 0w3 => CTypes.C_double
            | 0w4 => CTypes.C_long_double
            | 0w5 => let val x0 = read_c_int inS in CTypes.C_unsigned (x0) end
            | 0w6 => let val x0 = read_c_int inS in CTypes.C_signed (x0) end
            | 0w7 => CTypes.C_PTR
            | 0w8 => let
              val x0 = read_c_type inS
              val x1 = ASDLMemoryPickle.readInt inS
              in
                  CTypes.C_ARRAY (x0, x1)
              end
            | 0w9 => let val x0 = readSeq read_c_type inS in CTypes.C_STRUCT (x0) end
            | 0w10 => let val x0 = readSeq read_c_type inS in CTypes.C_UNION (x0) end
            | _ => raise ASDL.DecodeError)
    fun write_c_proto (outS, obj) = let
          val {conv, retTy, paramTys} = obj
          in
            write_calling_convention (outS, conv);
            write_c_type (outS, retTy);
            writeSeq write_c_type (outS, paramTys)
          end
    fun read_c_proto inS = let
          val conv = read_calling_convention inS
          val retTy = read_c_type inS
          val paramTys = readSeq read_c_type inS
          in
              {conv = conv, retTy = retTy, paramTys = paramTys}
          end
  end

structure CFG_PrimMemoryPickle : CFG__PRIM_PICKLE
  where type instream = ASDLMemoryPickle.instream
  where type outstream = ASDLMemoryPickle.outstream = struct
    (*---------- begin pickle-util.in ----------*)
    type instream = ASDLMemoryPickle.instream
    type outstream = ASDLMemoryPickle.outstream
  (* write an option *)
    fun writeOption wrFn (outS, NONE) = ASDLMemoryPickle.writeTag8(outS, 0w0)
      | writeOption wrFn (outS, SOME obj) = (
          ASDLMemoryPickle.writeTag8(outS, 0w1);
          wrFn(outS, obj))
  (* read an option *)
    fun readOption rdFn inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w0 => NONE
            | 0w1 => let val obj = rdFn inS in SOME obj end
            | _ => raise ASDL.DecodeError
          (* end case *))
  (* write a list of values as a sequence *)
    fun writeSeq wrFn (outS, xs) = let
          fun write [] = ()
            | write (x::xr) = (wrFn(outS, x); write xr)
          in
            ASDLMemoryPickle.writeUInt(outS, Word.fromInt(length xs));
            write xs
          end
  (* read a sequence into a sequence of values *)
    fun readSeq rdFn inS = let
          val len = ASDLMemoryPickle.readUInt inS
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
(*---------- end pickle-util.in ----------*)

    fun write_fcmpop (outS, obj) = (case obj
           of CFG_Prim.F_EQ => ASDLMemoryPickle.writeTag8 (outS, 0w1)
            | CFG_Prim.F_ULG => ASDLMemoryPickle.writeTag8 (outS, 0w2)
            | CFG_Prim.F_UN => ASDLMemoryPickle.writeTag8 (outS, 0w3)
            | CFG_Prim.F_LEG => ASDLMemoryPickle.writeTag8 (outS, 0w4)
            | CFG_Prim.F_GT => ASDLMemoryPickle.writeTag8 (outS, 0w5)
            | CFG_Prim.F_GE => ASDLMemoryPickle.writeTag8 (outS, 0w6)
            | CFG_Prim.F_UGT => ASDLMemoryPickle.writeTag8 (outS, 0w7)
            | CFG_Prim.F_UGE => ASDLMemoryPickle.writeTag8 (outS, 0w8)
            | CFG_Prim.F_LT => ASDLMemoryPickle.writeTag8 (outS, 0w9)
            | CFG_Prim.F_LE => ASDLMemoryPickle.writeTag8 (outS, 0w10)
            | CFG_Prim.F_ULT => ASDLMemoryPickle.writeTag8 (outS, 0w11)
            | CFG_Prim.F_ULE => ASDLMemoryPickle.writeTag8 (outS, 0w12)
            | CFG_Prim.F_LG => ASDLMemoryPickle.writeTag8 (outS, 0w13)
            | CFG_Prim.F_UE => ASDLMemoryPickle.writeTag8 (outS, 0w14))
    fun read_fcmpop inS = (case ASDLMemoryPickle.readTag8 inS
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
    fun write_cmpop (outS, obj) = (case obj
           of CFG_Prim.GT => ASDLMemoryPickle.writeTag8 (outS, 0w1)
            | CFG_Prim.GTE => ASDLMemoryPickle.writeTag8 (outS, 0w2)
            | CFG_Prim.LT => ASDLMemoryPickle.writeTag8 (outS, 0w3)
            | CFG_Prim.LTE => ASDLMemoryPickle.writeTag8 (outS, 0w4)
            | CFG_Prim.EQL => ASDLMemoryPickle.writeTag8 (outS, 0w5)
            | CFG_Prim.NEQ => ASDLMemoryPickle.writeTag8 (outS, 0w6))
    fun read_cmpop inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => CFG_Prim.GT
            | 0w2 => CFG_Prim.GTE
            | 0w3 => CFG_Prim.LT
            | 0w4 => CFG_Prim.LTE
            | 0w5 => CFG_Prim.EQL
            | 0w6 => CFG_Prim.NEQ
            | _ => raise ASDL.DecodeError)
    fun write_branch (outS, obj) = (case obj
           of CFG_Prim.CMP{oper, signed, sz} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w1);
              write_cmpop (outS, oper);
              ASDLMemoryPickle.writeBool (outS, signed);
              ASDLMemoryPickle.writeInt (outS, sz))
            | CFG_Prim.FCMP{oper, sz} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w2);
              write_fcmpop (outS, oper);
              ASDLMemoryPickle.writeInt (outS, sz))
            | CFG_Prim.FSGN x0 => (
              ASDLMemoryPickle.writeTag8 (outS, 0w3);
              ASDLMemoryPickle.writeInt (outS, x0))
            | CFG_Prim.PEQL => ASDLMemoryPickle.writeTag8 (outS, 0w4)
            | CFG_Prim.PNEQ => ASDLMemoryPickle.writeTag8 (outS, 0w5)
            | CFG_Prim.LIMIT x0 => (
              ASDLMemoryPickle.writeTag8 (outS, 0w6);
              ASDLMemoryPickle.writeUInt (outS, x0)))
    fun read_branch inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => let
              val oper = read_cmpop inS
              val signed = ASDLMemoryPickle.readBool inS
              val sz = ASDLMemoryPickle.readInt inS
              in
                  CFG_Prim.CMP {oper = oper, signed = signed, sz = sz}
              end
            | 0w2 => let
              val oper = read_fcmpop inS
              val sz = ASDLMemoryPickle.readInt inS
              in
                  CFG_Prim.FCMP {oper = oper, sz = sz}
              end
            | 0w3 => let val x0 = ASDLMemoryPickle.readInt inS in CFG_Prim.FSGN (x0) end
            | 0w4 => CFG_Prim.PEQL
            | 0w5 => CFG_Prim.PNEQ
            | 0w6 => let
              val x0 = ASDLMemoryPickle.readUInt inS
              in CFG_Prim.LIMIT (x0)
              end
            | _ => raise ASDL.DecodeError)
    fun write_numkind (outS, obj) = (case obj
           of CFG_Prim.INT => ASDLMemoryPickle.writeTag8 (outS, 0w1)
            | CFG_Prim.FLT => ASDLMemoryPickle.writeTag8 (outS, 0w2))
    fun read_numkind inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => CFG_Prim.INT
            | 0w2 => CFG_Prim.FLT
            | _ => raise ASDL.DecodeError)
    fun write_setter (outS, obj) = (case obj
           of CFG_Prim.UNBOXED_UPDATE => ASDLMemoryPickle.writeTag8 (outS, 0w1)
            | CFG_Prim.UPDATE => ASDLMemoryPickle.writeTag8 (outS, 0w2)
            | CFG_Prim.UNBOXED_ASSIGN => ASDLMemoryPickle.writeTag8 (outS, 0w3)
            | CFG_Prim.ASSIGN => ASDLMemoryPickle.writeTag8 (outS, 0w4)
            | CFG_Prim.RAW_UPDATE{kind, sz} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w5);
              write_numkind (outS, kind);
              ASDLMemoryPickle.writeInt (outS, sz))
            | CFG_Prim.RAW_STORE{kind, sz} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w6);
              write_numkind (outS, kind);
              ASDLMemoryPickle.writeInt (outS, sz))
            | CFG_Prim.SET_HDLR => ASDLMemoryPickle.writeTag8 (outS, 0w7)
            | CFG_Prim.SET_VAR => ASDLMemoryPickle.writeTag8 (outS, 0w8))
    fun read_setter inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => CFG_Prim.UNBOXED_UPDATE
            | 0w2 => CFG_Prim.UPDATE
            | 0w3 => CFG_Prim.UNBOXED_ASSIGN
            | 0w4 => CFG_Prim.ASSIGN
            | 0w5 => let
              val kind = read_numkind inS
              val sz = ASDLMemoryPickle.readInt inS
              in
                  CFG_Prim.RAW_UPDATE {kind = kind, sz = sz}
              end
            | 0w6 => let
              val kind = read_numkind inS
              val sz = ASDLMemoryPickle.readInt inS
              in
                  CFG_Prim.RAW_STORE {kind = kind, sz = sz}
              end
            | 0w7 => CFG_Prim.SET_HDLR
            | 0w8 => CFG_Prim.SET_VAR
            | _ => raise ASDL.DecodeError)
    fun write_looker (outS, obj) = (case obj
           of CFG_Prim.DEREF => ASDLMemoryPickle.writeTag8 (outS, 0w1)
            | CFG_Prim.SUBSCRIPT => ASDLMemoryPickle.writeTag8 (outS, 0w2)
            | CFG_Prim.RAW_SUBSCRIPT{kind, sz} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w3);
              write_numkind (outS, kind);
              ASDLMemoryPickle.writeInt (outS, sz))
            | CFG_Prim.RAW_LOAD{kind, sz} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w4);
              write_numkind (outS, kind);
              ASDLMemoryPickle.writeInt (outS, sz))
            | CFG_Prim.GET_HDLR => ASDLMemoryPickle.writeTag8 (outS, 0w5)
            | CFG_Prim.GET_VAR => ASDLMemoryPickle.writeTag8 (outS, 0w6))
    fun read_looker inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => CFG_Prim.DEREF
            | 0w2 => CFG_Prim.SUBSCRIPT
            | 0w3 => let
              val kind = read_numkind inS
              val sz = ASDLMemoryPickle.readInt inS
              in
                  CFG_Prim.RAW_SUBSCRIPT {kind = kind, sz = sz}
              end
            | 0w4 => let
              val kind = read_numkind inS
              val sz = ASDLMemoryPickle.readInt inS
              in
                  CFG_Prim.RAW_LOAD {kind = kind, sz = sz}
              end
            | 0w5 => CFG_Prim.GET_HDLR
            | 0w6 => CFG_Prim.GET_VAR
            | _ => raise ASDL.DecodeError)
    fun write_pureop (outS, obj) = (case obj
           of CFG_Prim.ADD => ASDLMemoryPickle.writeTag8 (outS, 0w1)
            | CFG_Prim.SUB => ASDLMemoryPickle.writeTag8 (outS, 0w2)
            | CFG_Prim.SMUL => ASDLMemoryPickle.writeTag8 (outS, 0w3)
            | CFG_Prim.SDIV => ASDLMemoryPickle.writeTag8 (outS, 0w4)
            | CFG_Prim.SREM => ASDLMemoryPickle.writeTag8 (outS, 0w5)
            | CFG_Prim.UMUL => ASDLMemoryPickle.writeTag8 (outS, 0w6)
            | CFG_Prim.UDIV => ASDLMemoryPickle.writeTag8 (outS, 0w7)
            | CFG_Prim.UREM => ASDLMemoryPickle.writeTag8 (outS, 0w8)
            | CFG_Prim.LSHIFT => ASDLMemoryPickle.writeTag8 (outS, 0w9)
            | CFG_Prim.RSHIFT => ASDLMemoryPickle.writeTag8 (outS, 0w10)
            | CFG_Prim.RSHIFTL => ASDLMemoryPickle.writeTag8 (outS, 0w11)
            | CFG_Prim.ORB => ASDLMemoryPickle.writeTag8 (outS, 0w12)
            | CFG_Prim.XORB => ASDLMemoryPickle.writeTag8 (outS, 0w13)
            | CFG_Prim.ANDB => ASDLMemoryPickle.writeTag8 (outS, 0w14)
            | CFG_Prim.FADD => ASDLMemoryPickle.writeTag8 (outS, 0w15)
            | CFG_Prim.FSUB => ASDLMemoryPickle.writeTag8 (outS, 0w16)
            | CFG_Prim.FMUL => ASDLMemoryPickle.writeTag8 (outS, 0w17)
            | CFG_Prim.FDIV => ASDLMemoryPickle.writeTag8 (outS, 0w18)
            | CFG_Prim.FNEG => ASDLMemoryPickle.writeTag8 (outS, 0w19)
            | CFG_Prim.FABS => ASDLMemoryPickle.writeTag8 (outS, 0w20)
            | CFG_Prim.FSQRT => ASDLMemoryPickle.writeTag8 (outS, 0w21)
            | CFG_Prim.FCOPYSIGN => ASDLMemoryPickle.writeTag8 (outS, 0w22))
    fun read_pureop inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => CFG_Prim.ADD
            | 0w2 => CFG_Prim.SUB
            | 0w3 => CFG_Prim.SMUL
            | 0w4 => CFG_Prim.SDIV
            | 0w5 => CFG_Prim.SREM
            | 0w6 => CFG_Prim.UMUL
            | 0w7 => CFG_Prim.UDIV
            | 0w8 => CFG_Prim.UREM
            | 0w9 => CFG_Prim.LSHIFT
            | 0w10 => CFG_Prim.RSHIFT
            | 0w11 => CFG_Prim.RSHIFTL
            | 0w12 => CFG_Prim.ORB
            | 0w13 => CFG_Prim.XORB
            | 0w14 => CFG_Prim.ANDB
            | 0w15 => CFG_Prim.FADD
            | 0w16 => CFG_Prim.FSUB
            | 0w17 => CFG_Prim.FMUL
            | 0w18 => CFG_Prim.FDIV
            | 0w19 => CFG_Prim.FNEG
            | 0w20 => CFG_Prim.FABS
            | 0w21 => CFG_Prim.FSQRT
            | 0w22 => CFG_Prim.FCOPYSIGN
            | _ => raise ASDL.DecodeError)
    fun write_pure (outS, obj) = (case obj
           of CFG_Prim.PURE_ARITH{oper, sz} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w1);
              write_pureop (outS, oper);
              ASDLMemoryPickle.writeInt (outS, sz))
            | CFG_Prim.EXTEND{signed, from, to} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w2);
              ASDLMemoryPickle.writeBool (outS, signed);
              ASDLMemoryPickle.writeInt (outS, from);
              ASDLMemoryPickle.writeInt (outS, to))
            | CFG_Prim.TRUNC{from, to} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w3);
              ASDLMemoryPickle.writeInt (outS, from);
              ASDLMemoryPickle.writeInt (outS, to))
            | CFG_Prim.INT_TO_FLOAT{from, to} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w4);
              ASDLMemoryPickle.writeInt (outS, from);
              ASDLMemoryPickle.writeInt (outS, to))
            | CFG_Prim.FLOAT_TO_BITS{sz} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w5);
              ASDLMemoryPickle.writeInt (outS, sz))
            | CFG_Prim.BITS_TO_FLOAT{sz} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w6);
              ASDLMemoryPickle.writeInt (outS, sz))
            | CFG_Prim.PURE_SUBSCRIPT => ASDLMemoryPickle.writeTag8 (outS, 0w7)
            | CFG_Prim.PURE_RAW_SUBSCRIPT{kind, sz} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w8);
              write_numkind (outS, kind);
              ASDLMemoryPickle.writeInt (outS, sz))
            | CFG_Prim.RAW_SELECT{kind, sz, offset} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w9);
              write_numkind (outS, kind);
              ASDLMemoryPickle.writeInt (outS, sz);
              ASDLMemoryPickle.writeInt (outS, offset)))
    fun read_pure inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => let
              val oper = read_pureop inS
              val sz = ASDLMemoryPickle.readInt inS
              in
                  CFG_Prim.PURE_ARITH {oper = oper, sz = sz}
              end
            | 0w2 => let
              val signed = ASDLMemoryPickle.readBool inS
              val from = ASDLMemoryPickle.readInt inS
              val to = ASDLMemoryPickle.readInt inS
              in
                  CFG_Prim.EXTEND {signed = signed, from = from, to = to}
              end
            | 0w3 => let
              val from = ASDLMemoryPickle.readInt inS
              val to = ASDLMemoryPickle.readInt inS
              in
                  CFG_Prim.TRUNC {from = from, to = to}
              end
            | 0w4 => let
              val from = ASDLMemoryPickle.readInt inS
              val to = ASDLMemoryPickle.readInt inS
              in
                  CFG_Prim.INT_TO_FLOAT {from = from, to = to}
              end
            | 0w5 => let
              val sz = ASDLMemoryPickle.readInt inS
              in CFG_Prim.FLOAT_TO_BITS {sz = sz}
              end
            | 0w6 => let
              val sz = ASDLMemoryPickle.readInt inS
              in CFG_Prim.BITS_TO_FLOAT {sz = sz}
              end
            | 0w7 => CFG_Prim.PURE_SUBSCRIPT
            | 0w8 => let
              val kind = read_numkind inS
              val sz = ASDLMemoryPickle.readInt inS
              in
                  CFG_Prim.PURE_RAW_SUBSCRIPT {kind = kind, sz = sz}
              end
            | 0w9 => let
              val kind = read_numkind inS
              val sz = ASDLMemoryPickle.readInt inS
              val offset = ASDLMemoryPickle.readInt inS
              in
                  CFG_Prim.RAW_SELECT {kind = kind, sz = sz, offset = offset}
              end
            | _ => raise ASDL.DecodeError)
    fun write_arithop (outS, obj) = (case obj
           of CFG_Prim.IADD => ASDLMemoryPickle.writeTag8 (outS, 0w1)
            | CFG_Prim.ISUB => ASDLMemoryPickle.writeTag8 (outS, 0w2)
            | CFG_Prim.IMUL => ASDLMemoryPickle.writeTag8 (outS, 0w3)
            | CFG_Prim.IDIV => ASDLMemoryPickle.writeTag8 (outS, 0w4)
            | CFG_Prim.IREM => ASDLMemoryPickle.writeTag8 (outS, 0w5))
    fun read_arithop inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => CFG_Prim.IADD
            | 0w2 => CFG_Prim.ISUB
            | 0w3 => CFG_Prim.IMUL
            | 0w4 => CFG_Prim.IDIV
            | 0w5 => CFG_Prim.IREM
            | _ => raise ASDL.DecodeError)
    fun write_rounding_mode (outS, obj) = (case obj
           of CFG_Prim.TO_NEAREST => ASDLMemoryPickle.writeTag8 (outS, 0w1)
            | CFG_Prim.TO_NEGINF => ASDLMemoryPickle.writeTag8 (outS, 0w2)
            | CFG_Prim.TO_POSINF => ASDLMemoryPickle.writeTag8 (outS, 0w3)
            | CFG_Prim.TO_ZERO => ASDLMemoryPickle.writeTag8 (outS, 0w4))
    fun read_rounding_mode inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => CFG_Prim.TO_NEAREST
            | 0w2 => CFG_Prim.TO_NEGINF
            | 0w3 => CFG_Prim.TO_POSINF
            | 0w4 => CFG_Prim.TO_ZERO
            | _ => raise ASDL.DecodeError)
    fun write_arith (outS, obj) = (case obj
           of CFG_Prim.ARITH{oper, sz} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w1);
              write_arithop (outS, oper);
              ASDLMemoryPickle.writeInt (outS, sz))
            | CFG_Prim.FLOAT_TO_INT{mode, from, to} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w2);
              write_rounding_mode (outS, mode);
              ASDLMemoryPickle.writeInt (outS, from);
              ASDLMemoryPickle.writeInt (outS, to)))
    fun read_arith inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => let
              val oper = read_arithop inS
              val sz = ASDLMemoryPickle.readInt inS
              in
                  CFG_Prim.ARITH {oper = oper, sz = sz}
              end
            | 0w2 => let
              val mode = read_rounding_mode inS
              val from = ASDLMemoryPickle.readInt inS
              val to = ASDLMemoryPickle.readInt inS
              in
                  CFG_Prim.FLOAT_TO_INT {mode = mode, from = from, to = to}
              end
            | _ => raise ASDL.DecodeError)
    fun write_raw_ty (outS, obj) = let
          val {kind, sz} = obj
          in
            write_numkind (outS, kind);
            ASDLMemoryPickle.writeInt (outS, sz)
          end
    fun read_raw_ty inS = let
          val kind = read_numkind inS
          val sz = ASDLMemoryPickle.readInt inS
          in
              {kind = kind, sz = sz}
          end
    fun write_alloc (outS, obj) = (case obj
           of CFG_Prim.SPECIAL => ASDLMemoryPickle.writeTag8 (outS, 0w1)
            | CFG_Prim.RECORD{desc, mut} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w2);
              ASDLMemoryPickle.writeInteger (outS, desc);
              ASDLMemoryPickle.writeBool (outS, mut))
            | CFG_Prim.RAW_RECORD{desc, align, fields} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w3);
              ASDLMemoryPickle.writeInteger (outS, desc);
              ASDLMemoryPickle.writeInt (outS, align);
              writeSeq write_raw_ty (outS, fields))
            | CFG_Prim.RAW_ALLOC{desc, align, len} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w4);
              writeOption ASDLMemoryPickle.writeInteger (outS, desc);
              ASDLMemoryPickle.writeInt (outS, align);
              ASDLMemoryPickle.writeInt (outS, len)))
    fun read_alloc inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => CFG_Prim.SPECIAL
            | 0w2 => let
              val desc = ASDLMemoryPickle.readInteger inS
              val mut = ASDLMemoryPickle.readBool inS
              in
                  CFG_Prim.RECORD {desc = desc, mut = mut}
              end
            | 0w3 => let
              val desc = ASDLMemoryPickle.readInteger inS
              val align = ASDLMemoryPickle.readInt inS
              val fields = readSeq read_raw_ty inS
              in
                  CFG_Prim.RAW_RECORD {desc = desc, align = align, fields = fields}
              end
            | 0w4 => let
              val desc = readOption ASDLMemoryPickle.readInteger inS
              val align = ASDLMemoryPickle.readInt inS
              val len = ASDLMemoryPickle.readInt inS
              in
                  CFG_Prim.RAW_ALLOC {desc = desc, align = align, len = len}
              end
            | _ => raise ASDL.DecodeError)
  end

structure CFGMemoryPickle : CFGPICKLE
  where type instream = ASDLMemoryPickle.instream
  where type outstream = ASDLMemoryPickle.outstream = struct
    (*---------- begin pickle-util.in ----------*)
    type instream = ASDLMemoryPickle.instream
    type outstream = ASDLMemoryPickle.outstream
  (* write an option *)
    fun writeOption wrFn (outS, NONE) = ASDLMemoryPickle.writeTag8(outS, 0w0)
      | writeOption wrFn (outS, SOME obj) = (
          ASDLMemoryPickle.writeTag8(outS, 0w1);
          wrFn(outS, obj))
  (* read an option *)
    fun readOption rdFn inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w0 => NONE
            | 0w1 => let val obj = rdFn inS in SOME obj end
            | _ => raise ASDL.DecodeError
          (* end case *))
  (* write a list of values as a sequence *)
    fun writeSeq wrFn (outS, xs) = let
          fun write [] = ()
            | write (x::xr) = (wrFn(outS, x); write xr)
          in
            ASDLMemoryPickle.writeUInt(outS, Word.fromInt(length xs));
            write xs
          end
  (* read a sequence into a sequence of values *)
    fun readSeq rdFn inS = let
          val len = ASDLMemoryPickle.readUInt inS
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
(*---------- end pickle-util.in ----------*)

    fun write_attrs (outS, obj) = let
          val {alignHP, needsBasePtr, hasTrapArith, hasRCC} = obj
          in
            ASDLMemoryPickle.writeInt (outS, alignHP);
            ASDLMemoryPickle.writeBool (outS, needsBasePtr);
            ASDLMemoryPickle.writeBool (outS, hasTrapArith);
            ASDLMemoryPickle.writeBool (outS, hasRCC)
          end
    fun read_attrs inS = let
          val alignHP = ASDLMemoryPickle.readInt inS
          val needsBasePtr = ASDLMemoryPickle.readBool inS
          val hasTrapArith = ASDLMemoryPickle.readBool inS
          val hasRCC = ASDLMemoryPickle.readBool inS
          in
              {
              alignHP = alignHP,
              needsBasePtr = needsBasePtr,
              hasTrapArith = hasTrapArith,
              hasRCC = hasRCC}
          end
    fun write_frag_kind (outS, obj) = (case obj
           of CFG.STD_FUN => ASDLMemoryPickle.writeTag8 (outS, 0w1)
            | CFG.STD_CONT => ASDLMemoryPickle.writeTag8 (outS, 0w2)
            | CFG.KNOWN_FUN => ASDLMemoryPickle.writeTag8 (outS, 0w3)
            | CFG.INTERNAL => ASDLMemoryPickle.writeTag8 (outS, 0w4))
    fun read_frag_kind inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => CFG.STD_FUN
            | 0w2 => CFG.STD_CONT
            | 0w3 => CFG.KNOWN_FUN
            | 0w4 => CFG.INTERNAL
            | _ => raise ASDL.DecodeError)
    fun write_probability (outS, obj) = ASDLMemoryPickle.writeInt (outS, obj)
    fun read_probability inS = ASDLMemoryPickle.readInt inS
    fun write_ty (outS, obj) = (case obj
           of CFG.LABt => ASDLMemoryPickle.writeTag8 (outS, 0w1)
            | CFG.PTRt => ASDLMemoryPickle.writeTag8 (outS, 0w2)
            | CFG.TAGt => ASDLMemoryPickle.writeTag8 (outS, 0w3)
            | CFG.NUMt{sz} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w4);
              ASDLMemoryPickle.writeInt (outS, sz))
            | CFG.FLTt{sz} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w5);
              ASDLMemoryPickle.writeInt (outS, sz)))
    fun read_ty inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => CFG.LABt
            | 0w2 => CFG.PTRt
            | 0w3 => CFG.TAGt
            | 0w4 => let val sz = ASDLMemoryPickle.readInt inS in CFG.NUMt {sz = sz} end
            | 0w5 => let val sz = ASDLMemoryPickle.readInt inS in CFG.FLTt {sz = sz} end
            | _ => raise ASDL.DecodeError)
    fun write_param (outS, obj) = let
          val {name, ty} = obj
          in
            LambdaVarMemoryPickle.write_lvar (outS, name);
            write_ty (outS, ty)
          end
    fun read_param inS = let
          val name = LambdaVarMemoryPickle.read_lvar inS
          val ty = read_ty inS
          in
              {name = name, ty = ty}
          end
    fun write_exp (outS, obj) = (case obj
           of CFG.VAR{name} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w1);
              LambdaVarMemoryPickle.write_lvar (outS, name))
            | CFG.LABEL{name} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w2);
              LambdaVarMemoryPickle.write_lvar (outS, name))
            | CFG.NUM{iv, sz} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w3);
              ASDLMemoryPickle.writeInteger (outS, iv);
              ASDLMemoryPickle.writeInt (outS, sz))
            | CFG.LOOKER{oper, args} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w4);
              CFG_PrimMemoryPickle.write_looker (outS, oper);
              writeSeq write_exp (outS, args))
            | CFG.PURE{oper, args} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w5);
              CFG_PrimMemoryPickle.write_pure (outS, oper);
              writeSeq write_exp (outS, args))
            | CFG.SELECT{idx, arg} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w6);
              ASDLMemoryPickle.writeInt (outS, idx);
              write_exp (outS, arg))
            | CFG.OFFSET{idx, arg} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w7);
              ASDLMemoryPickle.writeInt (outS, idx);
              write_exp (outS, arg)))
    fun read_exp inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => let
              val name = LambdaVarMemoryPickle.read_lvar inS
              in CFG.VAR {name = name}
              end
            | 0w2 => let
              val name = LambdaVarMemoryPickle.read_lvar inS
              in CFG.LABEL {name = name}
              end
            | 0w3 => let
              val iv = ASDLMemoryPickle.readInteger inS
              val sz = ASDLMemoryPickle.readInt inS
              in
                  CFG.NUM {iv = iv, sz = sz}
              end
            | 0w4 => let
              val oper = CFG_PrimMemoryPickle.read_looker inS
              val args = readSeq read_exp inS
              in
                  CFG.LOOKER {oper = oper, args = args}
              end
            | 0w5 => let
              val oper = CFG_PrimMemoryPickle.read_pure inS
              val args = readSeq read_exp inS
              in
                  CFG.PURE {oper = oper, args = args}
              end
            | 0w6 => let
              val idx = ASDLMemoryPickle.readInt inS
              val arg = read_exp inS
              in
                  CFG.SELECT {idx = idx, arg = arg}
              end
            | 0w7 => let
              val idx = ASDLMemoryPickle.readInt inS
              val arg = read_exp inS
              in
                  CFG.OFFSET {idx = idx, arg = arg}
              end
            | _ => raise ASDL.DecodeError)
    fun write_stm (outS, obj) = (case obj
           of CFG.LET(x0, x1, x2) => (
              ASDLMemoryPickle.writeTag8 (outS, 0w1);
              write_exp (outS, x0);
              write_param (outS, x1);
              write_stm (outS, x2))
            | CFG.ALLOC(x0, x1, x2, x3) => (
              ASDLMemoryPickle.writeTag8 (outS, 0w2);
              CFG_PrimMemoryPickle.write_alloc (outS, x0);
              writeSeq write_exp (outS, x1);
              LambdaVarMemoryPickle.write_lvar (outS, x2);
              write_stm (outS, x3))
            | CFG.APPLY(x0, x1, x2) => (
              ASDLMemoryPickle.writeTag8 (outS, 0w3);
              write_exp (outS, x0);
              writeSeq write_exp (outS, x1);
              writeSeq write_ty (outS, x2))
            | CFG.THROW(x0, x1, x2) => (
              ASDLMemoryPickle.writeTag8 (outS, 0w4);
              write_exp (outS, x0);
              writeSeq write_exp (outS, x1);
              writeSeq write_ty (outS, x2))
            | CFG.GOTO(x0, x1) => (
              ASDLMemoryPickle.writeTag8 (outS, 0w5);
              LambdaVarMemoryPickle.write_lvar (outS, x0);
              writeSeq write_exp (outS, x1))
            | CFG.SWITCH(x0, x1) => (
              ASDLMemoryPickle.writeTag8 (outS, 0w6);
              write_exp (outS, x0);
              writeSeq write_stm (outS, x1))
            | CFG.BRANCH(x0, x1, x2, x3, x4) => (
              ASDLMemoryPickle.writeTag8 (outS, 0w7);
              CFG_PrimMemoryPickle.write_branch (outS, x0);
              writeSeq write_exp (outS, x1);
              write_probability (outS, x2);
              write_stm (outS, x3);
              write_stm (outS, x4))
            | CFG.ARITH(x0, x1, x2, x3) => (
              ASDLMemoryPickle.writeTag8 (outS, 0w8);
              CFG_PrimMemoryPickle.write_arith (outS, x0);
              writeSeq write_exp (outS, x1);
              write_param (outS, x2);
              write_stm (outS, x3))
            | CFG.SETTER(x0, x1, x2) => (
              ASDLMemoryPickle.writeTag8 (outS, 0w9);
              CFG_PrimMemoryPickle.write_setter (outS, x0);
              writeSeq write_exp (outS, x1);
              write_stm (outS, x2))
            | CFG.CALLGC(x0, x1, x2) => (
              ASDLMemoryPickle.writeTag8 (outS, 0w10);
              writeSeq write_exp (outS, x0);
              writeSeq LambdaVarMemoryPickle.write_lvar (outS, x1);
              write_stm (outS, x2))
            | CFG.RCC{reentrant, linkage, proto, args, results, live, k} => (
              ASDLMemoryPickle.writeTag8 (outS, 0w11);
              ASDLMemoryPickle.writeBool (outS, reentrant);
              ASDLMemoryPickle.writeString (outS, linkage);
              CTypesMemoryPickle.write_c_proto (outS, proto);
              writeSeq write_exp (outS, args);
              writeSeq write_param (outS, results);
              writeSeq write_param (outS, live);
              write_stm (outS, k)))
    fun read_stm inS = (case ASDLMemoryPickle.readTag8 inS
           of 0w1 => let
              val x0 = read_exp inS
              val x1 = read_param inS
              val x2 = read_stm inS
              in
                  CFG.LET (x0, x1, x2)
              end
            | 0w2 => let
              val x0 = CFG_PrimMemoryPickle.read_alloc inS
              val x1 = readSeq read_exp inS
              val x2 = LambdaVarMemoryPickle.read_lvar inS
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
              val x0 = LambdaVarMemoryPickle.read_lvar inS
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
              val x0 = CFG_PrimMemoryPickle.read_branch inS
              val x1 = readSeq read_exp inS
              val x2 = read_probability inS
              val x3 = read_stm inS
              val x4 = read_stm inS
              in
                  CFG.BRANCH (x0, x1, x2, x3, x4)
              end
            | 0w8 => let
              val x0 = CFG_PrimMemoryPickle.read_arith inS
              val x1 = readSeq read_exp inS
              val x2 = read_param inS
              val x3 = read_stm inS
              in
                  CFG.ARITH (x0, x1, x2, x3)
              end
            | 0w9 => let
              val x0 = CFG_PrimMemoryPickle.read_setter inS
              val x1 = readSeq read_exp inS
              val x2 = read_stm inS
              in
                  CFG.SETTER (x0, x1, x2)
              end
            | 0w10 => let
              val x0 = readSeq read_exp inS
              val x1 = readSeq LambdaVarMemoryPickle.read_lvar inS
              val x2 = read_stm inS
              in
                  CFG.CALLGC (x0, x1, x2)
              end
            | 0w11 => let
              val reentrant = ASDLMemoryPickle.readBool inS
              val linkage = ASDLMemoryPickle.readString inS
              val proto = CTypesMemoryPickle.read_c_proto inS
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
    fun write_frag (outS, obj) = let
          val CFG.Frag{kind, lab, params, body} = obj
          in
            write_frag_kind (outS, kind);
            LambdaVarMemoryPickle.write_lvar (outS, lab);
            writeSeq write_param (outS, params);
            write_stm (outS, body)
          end
    fun read_frag inS = let
          val kind = read_frag_kind inS
          val lab = LambdaVarMemoryPickle.read_lvar inS
          val params = readSeq read_param inS
          val body = read_stm inS
          in
              CFG.Frag {kind = kind, lab = lab, params = params, body = body}
          end
    fun write_cluster (outS, obj) = let
          val CFG.Cluster{attrs, frags} = obj
          in
            write_attrs (outS, attrs);
            writeSeq write_frag (outS, frags)
          end
    fun read_cluster inS = let
          val attrs = read_attrs inS
          val frags = readSeq read_frag inS
          in
              CFG.Cluster {attrs = attrs, frags = frags}
          end
    fun write_comp_unit (outS, obj) = let
          val {srcFile, entry, fns} = obj
          in
            ASDLMemoryPickle.writeString (outS, srcFile);
            write_cluster (outS, entry);
            writeSeq write_cluster (outS, fns)
          end
    fun read_comp_unit inS = let
          val srcFile = ASDLMemoryPickle.readString inS
          val entry = read_cluster inS
          val fns = readSeq read_cluster inS
          in
              {srcFile = srcFile, entry = entry, fns = fns}
          end
  end


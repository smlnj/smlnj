(* contract-prim.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Contraction for CPS primitive operations.  For details about the fusion of conversion
 * operators, see
 *
 *      https://github.com/smlnj/.github/wiki/Integer-Word-Conversions-Explained
 *)

structure ContractPrim : sig

    (* information about a variable's binding *)
    datatype info
      = FNinfo of {
            args: CPS.lvar list,
            body : CPS.cexp option ref,
            specialuse: int ref option ref,
            liveargs : bool list option ref
          }
      | RECinfo of CPS.record_kind * (CPS.value * CPS.accesspath) list
      | SELinfo of int * CPS.value * CPS.cty
      | ARITHinfo of CPS.P.arith * CPS.value list
      | PUREinfo of CPS.P.pure * CPS.value list
      | IFIDIOMinfo of {body : (CPS.lvar * CPS.cexp * CPS.cexp) option ref}
      | MISCinfo of CPS.cty

    type get_info = CPS.lvar -> {info: info, used : int ref, called : int ref}

    val infoToString : info -> string

  (* the result of contracting an arithmetic operation *)
    datatype result
      = None                                    (* no contraction *)
      | Val of CPS.value                        (* contract to value *)
      | Arith of CPS.P.arith * CPS.value list   (* strength reduction *)
      | Pure of CPS.P.pure * CPS.value list     (* strength reduction *)

    val arith : get_info -> CPS.P.arith * CPS.value list -> result

    val pure : get_info -> CPS.P.pure * CPS.value list -> result

    val branch : get_info -> CPS.P.branch * CPS.value list -> bool option

  end = struct

    structure P = CPS.P
    structure CA = ConstArith

    fun bug s = ErrorMsg.impossible ("ContractPrim: " ^ s)

    datatype value = datatype CPS.value

  (* information about a variable *)
    datatype info
      = FNinfo of {
            args: CPS.lvar list,
            body : CPS.cexp option ref,
            specialuse: int ref option ref,
            liveargs : bool list option ref
          }
      | RECinfo of CPS.record_kind * (value * CPS.accesspath) list
      | SELinfo of int * value * CPS.cty
      | ARITHinfo of P.arith * value list
      | PUREinfo of P.pure * value list
      | IFIDIOMinfo of {body : (CPS.lvar * CPS.cexp * CPS.cexp) option ref}
      | MISCinfo of CPS.cty

    type get_info = CPS.lvar -> {info: info, used : int ref, called : int ref}

    fun infoToString info = (case info
           of FNinfo _ => "FNinfo{...}"
            | RECinfo(_, args) => concat[
                  "RECinfo(_, [", String.concatWithMap "," PPCps.vpathToString args, "])"
                ]
            | SELinfo(i, v, cty) => concat[
                  "SELinfo(", Int.toString i, ", ", PPCps.value2str v, ", ",
                  CPSUtil.ctyToString cty, ")"
                ]
            | ARITHinfo(p, vs) => concat[
                  "ARITHinfo(", PPCps.arithToString p, ", [",
                  String.concatWithMap "," PPCps.value2str vs, "])"
                ]
            | PUREinfo(p, vs) => concat[
                  "PUREinfo(", PPCps.pureToString p, ", [",
                  String.concatWithMap "," PPCps.value2str vs, "])"
                ]
            | IFIDIOMinfo _ => "IFIDIOMinfo{...}"
            | MISCinfo cty => concat[
                  "MISCinfo(", CPSUtil.ctyToString cty, ")"
                ]
          (* end case *))

    (* integer types/values *)
    local
      val tt = {sz = Target.defaultIntSz, tag = true}
    in
    fun tagInt n = NUM{ival = IntInf.fromInt n, ty = tt}
    end

  (* get the size of an integer operation *)
    fun sizeOfKind (P.INT sz) = sz
      | sizeOfKind (P.UINT sz) = sz
      | sizeOfKind (P.FLOAT _) = bug "sizeOfKind(FLOAT _)"

    fun mkNum (sz, ival) = let
        (* NOTE: currently all tagged integer constants have the default size *)
          val ty = if (sz <= Target.defaultIntSz)
                then {sz=Target.defaultIntSz, tag=true}
                else {sz=sz, tag=false}
          in
            NUM{ival=ival, ty=ty}
          end

    fun log2 n = if (n > 0)
          then let
            val k = IntInf.log2 n
            in
              if (IntInf.<<(1, Word.fromInt k) = n)
                then SOME k
                else NONE
            end
          else NONE

  (* the result of contracting an arithmetic operation *)
    datatype result
      = None                                    (* no contraction *)
      | Val of CPS.value                        (* contract to value *)
      | Arith of CPS.P.arith * CPS.value list   (* strength reduction *)
      | Pure of CPS.P.pure * CPS.value list     (* strength reduction *)

    fun lshift sz = P.PURE_ARITH{oper=P.LSHIFT, kind=P.UINT sz}
    fun rshift sz = P.PURE_ARITH{oper=P.RSHIFT, kind=P.INT sz}
    fun rshiftl sz = P.PURE_ARITH{oper=P.RSHIFTL, kind=P.UINT sz}
    fun andb sz = P.PURE_ARITH{oper=P.ANDB, kind=P.UINT sz}

  (* optimize non-trapping multiplication by a power of two.
   * Eventually, we might generalize this to non-power-of-2 constants.
   *)
    fun mulByConst (sz, v, ival) = (case log2 ival
           of SOME k => Pure(lshift sz, [v, tagInt k])
            | NONE => None
          (* end case *))

  (* smart constructor for COPY conversion that detects when it is the identity *)
    fun mkCOPY (from, to, arg) = if (from = to)
          then Val arg
          else Pure(P.COPY{from=from, to=to}, [arg])

  (* contraction for impure arithmetic operations; note that 64-bit IMUL, IDIV,
   * IMOD, IQUOT, and IREM have three arguments on 32-bit targets, so we need
   * to allow for the extra argument in the patterns.
   *)
    fun arith (get : get_info) arg = ((case arg
            (***** IADD *****)
           of (P.IARITH{oper=P.IADD, ...}, [NUM{ival=0, ...}, v]) => Val v
            | (P.IARITH{oper=P.IADD, ...}, [v, NUM{ival=0, ...}]) => Val v
            | (P.IARITH{oper=P.IADD, sz}, [NUM i, NUM j]) =>
                Val(NUM{ival = CA.sAdd(sz, #ival i, #ival j), ty = #ty i})
            (***** ISUB *****)
            | (P.IARITH{oper=P.ISUB, ...}, [v, NUM{ival=0, ...}]) => Val v
            | (P.IARITH{oper=P.ISUB, sz}, [NUM i, NUM j]) =>
                Val(NUM{ival = CA.sSub(sz, #ival i, #ival j), ty = #ty i})
            (***** IMUL *****)
            | (P.IARITH{oper=P.IMUL, ...}, NUM{ival=1, ...} :: v :: _) => Val v
            | (P.IARITH{oper=P.IMUL, ...}, v :: NUM{ival=1, ...} :: _) => Val v
            | (P.IARITH{oper=P.IMUL, ...}, (z as NUM{ival=0, ...}) :: _) => Val z
            | (P.IARITH{oper=P.IMUL, ...}, _ :: (z as NUM{ival=0, ...}) :: _) => Val z
            | (P.IARITH{oper=P.IMUL, sz=sz}, NUM i :: NUM j :: _) =>
                Val(NUM{ival = CA.sMul(sz, #ival i, #ival j), ty = #ty i})
            | (P.IARITH{oper=P.IMUL, sz}, NUM{ival= ~1, ...} :: v :: _) =>
                Arith(P.IARITH{oper=P.INEG, sz=sz}, [v])
            | (P.IARITH{oper=P.IMUL, sz}, v :: NUM{ival= ~1, ...} :: _) =>
                Arith(P.IARITH{oper=P.INEG, sz=sz}, [v])
            | (P.IARITH{oper=P.IMUL, sz}, NUM{ival= 2, ...} :: v :: _) =>
                Arith(P.IARITH{oper=P.IADD, sz=sz}, [v, v])
            | (P.IARITH{oper=P.IMUL, sz}, v :: NUM{ival= 2, ...} :: _) =>
                Arith(P.IARITH{oper=P.IADD, sz=sz}, [v, v])
            (***** IDIV *****)
            | (P.IARITH{oper=P.IDIV, ...}, v :: NUM{ival=1, ...} :: _) => Val v
            | (P.IARITH{oper=P.IDIV, ...}, _ :: NUM{ival=0, ...} :: _) => None
            | (P.IARITH{oper=P.IDIV, sz=sz}, NUM i :: NUM j :: _) =>
                Val(NUM{ival = CA.sDiv(sz, #ival i, #ival j), ty = #ty i})
            | (P.IARITH{oper=P.IDIV, sz}, v :: NUM{ival= ~1, ...} :: _) =>
                Arith(P.IARITH{oper=P.INEG, sz=sz}, [v])
            | (P.IARITH{oper=P.IDIV, sz}, v :: NUM{ival, ...} :: _) => (case log2 ival
                 of SOME k => Pure(rshift sz, [v, tagInt k])
                  | NONE => None
                (* end case *))
            (***** IMOD *****)
            | (P.IARITH{oper=P.IMOD, sz=sz}, NUM i :: NUM j :: _) =>
                Val(NUM{ival = CA.sMod(sz, #ival i, #ival j), ty = #ty i})
            | (P.IARITH{oper=P.IMOD, sz}, v :: NUM{ival, ...} :: _) => (case log2 ival
                 of SOME k => Pure(andb sz, [v, mkNum(sz, ival-1)])
                  | NONE => None
                (* end case *))
            (***** IQUOT *****)
            | (P.IARITH{oper=P.IQUOT, ...}, v :: NUM{ival=1, ...} :: _) => Val v
            | (P.IARITH{oper=P.IQUOT, ...}, _ :: NUM{ival=0, ...} :: _) => None
            | (P.IARITH{oper=P.IQUOT, sz=sz}, NUM i :: NUM j :: _) =>
                Val(NUM{ival = CA.sQuot(sz, #ival i, #ival j), ty = #ty i})
            | (P.IARITH{oper=P.IQUOT, sz}, v :: NUM{ival= ~1, ...} :: _) =>
                Arith(P.IARITH{oper=P.INEG, sz=sz}, [v])
            (***** IREM *****)
            | (P.IARITH{oper=P.IREM, sz=sz}, NUM i :: NUM j :: _) =>
                Val(NUM{ival = CA.sRem(sz, #ival i, #ival j), ty = #ty i})
            (***** INEG *****)
            | (P.IARITH{oper=P.INEG, sz}, [NUM i]) =>
                Val(NUM{ival = CA.sNeg(sz, #ival i), ty = #ty i})
            (***** TEST *****
             *
             * Note that on 32-bit platforms, `TEST` will have an extra argument
             * (the `w64ToInt32X` function) when `from` is 64.
             *)
            | (P.TEST{from=n, to=p}, v::_) => if (n = p)
                (* TEST(n, n) => IDENTITY *)
                then Val v
                else (case v
                   of NUM{ival, ...} => let
                        (* first convert to signed representation and then narrow.  The
                         * narrow operation mau raise Overflow, which is caught and
                         * mapped to `None` below.
                         *)
                        val ival' = CA.sNarrow(p, CA.toSigned(n, ival))
                        in
                          Val(mkNum(p, ival'))
                        end
                    | VAR x => (case #info(get x)
                         of ARITHinfo(P.TEST{from=m, ...}, args) =>
                              (* TEST(n,p) o TEST(m,n) ==> TEST(m, p) *)
                              Arith(P.TEST{from=m, to=p}, args)
                          | ARITHinfo(P.TEST_INF _, [u, f]) =>
                              (* TEST(n,p) o TEST(∞,n) ==> TEST(∞, p) *)
                              Arith(P.TEST_INF p, [u, f])
                          | PUREinfo(P.COPY{from=m, ...}, [u]) =>
                              if (p >= m)
                                (* TEST(n,p) o COPY(m,n) ==> COPY(m, p) if (p >= m) *)
                                then mkCOPY(m, p, u)
                                (* TEST(n,p) o COPY(m,n) ==> TEST(m, p) if (p < m) *)
                                else Arith(P.TEST{from=m, to=p}, [u])
                          | PUREinfo(P.EXTEND{from=m, ...}, [u]) =>
                              if (p >= m)
                                (* TEST(n,p) o EXTEND(m,n) ==> EXTEND(m, p) if (p >= m) *)
                                then Pure(P.EXTEND{from=m, to=p}, [u])
                                (* TEST(n,p) o EXTEND(m,n) ==> TEST(m, p) if (p < m) *)
                                else Arith(P.TEST{from=m, to=p}, [u])
                          | _ => None
                        (* end case *))
                    | _ => bug "bogus argument to TEST"
                  (* end case *))
            (***** TESTU *****
             *
             * Unlike the other conversions, the `TESTU` operator is not the identity
             * when `from = to`.  Also note that on 32-bit platforms, `TESTU` will
             * have an extra argument (the `w64ToInt32` function) when `from` is 64.
             *)
            | (P.TESTU{from, to}, NUM{ival, ...}::_) =>
                Val(mkNum(to, CA.sNarrow(to, ival)))
            | (P.TESTU{to=p, ...}, VAR v::_) => (case #info(get v)
                 of ARITHinfo(P.TESTU{from=m, ...}, [u]) =>
                      (* TESTU(n,p) o TESTU(m,n) ==> TESTU(m, p) *)
                      Arith(P.TESTU{from=m, to=p}, [u])
                  | PUREinfo(P.COPY{from=m, ...}, [u]) =>
                      if (p >= m)
                        (* TESTU(n,p) o COPY(m,n) ==> COPY(m, p) if (p >= m) *)
                        then mkCOPY(m, p, u)
                        (* TESTU(n,p) o COPY(m,n) ==> TESTU(m, p) if (p < m) *)
                        else Arith(P.TESTU{from=m, to=p}, [u])
                  | PUREinfo(P.EXTEND{from=m, ...}, [u]) =>
                      if (p >= m)
                        (* TESTU(n,p) o EXTEND(m,n) ==> EXTEND(m, p) if (p >= m) *)
                        then Pure(P.EXTEND{from=m, to=p}, [u])
                        (* TESTU(n,p) o EXTEND(m,n) ==> TESTU(m, p) if (p < m) *)
                        else Arith(P.TESTU{from=m, to=p}, [u])
                  | _ => None
                (* end case *))
            | _ => None
          (* end case *))
            handle _ => None)

  (* contraction for pure operations; note that 64-bit MUL, QUOT, and REM
   * have three arguments on 32-bit targets, so we need to allow for the
   * extra argument in the patterns.
   *)
    fun pure (get : get_info) arg = (case arg
            (***** ADD *****)
           of (P.PURE_ARITH{oper=P.ADD, ...}, [NUM{ival=0, ...}, v]) => Val v
            | (P.PURE_ARITH{oper=P.ADD, ...}, [v, NUM{ival=0, ...}]) => Val v
            | (P.PURE_ARITH{oper=P.ADD, kind=P.UINT sz}, [NUM i, NUM j]) =>
                Val(NUM{ival = CA.uAdd(sz, #ival i, #ival j), ty = #ty i})
            (***** SUB *****)
            | (P.PURE_ARITH{oper=P.SUB, ...}, [v, NUM{ival=0, ...}]) => Val v
            | (P.PURE_ARITH{oper=P.SUB, kind=P.UINT sz}, [NUM i, NUM j]) =>
                Val(NUM{ival = CA.uSub(sz, #ival i, #ival j), ty = #ty i})
            (***** MUL *****)
            | (P.PURE_ARITH{oper=P.MUL, ...}, NUM{ival=1, ...} :: v :: _) => Val v
            | (P.PURE_ARITH{oper=P.MUL, ...}, v :: NUM{ival=1, ...} :: _) => Val v
            | (P.PURE_ARITH{oper=P.MUL, ...}, (v as NUM{ival=0, ...}) :: _) => Val v
            | (P.PURE_ARITH{oper=P.MUL, ...}, _ :: (v as NUM{ival=0, ...}) :: _) => Val v
            | (P.PURE_ARITH{oper=P.MUL, kind=P.UINT sz}, NUM i :: NUM j :: _) =>
                Val(NUM{ival = CA.uMul(sz, #ival i, #ival j), ty = #ty i})
            | (P.PURE_ARITH{oper=P.MUL, kind=P.INT sz}, NUM{ival, ...} :: v :: _) =>
                mulByConst (sz, v, ival)
            | (P.PURE_ARITH{oper=P.MUL, kind=P.INT sz}, v :: NUM{ival, ...} :: _) =>
                mulByConst (sz, v, ival)
            | (P.PURE_ARITH{oper=P.MUL, kind=P.UINT sz}, NUM{ival, ...} :: v :: _) =>
                mulByConst (sz, v, ival)
            | (P.PURE_ARITH{oper=P.MUL, kind=P.UINT sz}, v :: NUM{ival, ...} :: _) =>
                mulByConst (sz, v, ival)
            (***** QUOT *****)
            | (P.PURE_ARITH{oper=P.QUOT, ...}, v :: NUM{ival=1, ...} :: _) => Val v
            | (P.PURE_ARITH{oper=P.QUOT, kind}, v :: NUM{ival= ~1, ...} :: _) =>
                Pure(P.PURE_ARITH{oper=P.NEG, kind=kind}, [v])
            | (P.PURE_ARITH{oper=P.QUOT, ...}, _ :: NUM{ival=0, ...} :: _) => None
            | (P.PURE_ARITH{oper=P.QUOT, kind=P.UINT sz}, NUM i :: NUM j :: _) =>
                Val(NUM{ival = CA.uDiv(sz, #ival i, #ival j), ty = #ty i})
            | (P.PURE_ARITH{oper=P.QUOT, kind=P.UINT sz}, v :: NUM{ival, ...} :: _) => (
                case log2 ival
                 of SOME k => Pure(rshiftl sz, [v, tagInt k])
                  | NONE => None
                (* end case *))
            (***** REM *****)
            | (P.PURE_ARITH{oper=P.REM, ...}, v :: NUM{ival=1, ty} :: _) =>
                Val(NUM{ival=0, ty=ty})
            | (P.PURE_ARITH{oper=P.REM, ...}, _ :: NUM{ival=0, ...} :: _) => None
            | (P.PURE_ARITH{oper=P.REM, kind=P.UINT sz}, NUM i :: NUM j :: _) =>
                Val(NUM{ival = CA.uMod(sz, #ival i, #ival j), ty = #ty i})
            | (P.PURE_ARITH{oper=P.REM, kind=P.UINT sz}, v :: NUM{ival, ...} :: _) => (
                case log2 ival
                 of SOME k => if (k < sz)
                      then Pure(andb sz, [v, mkNum(sz, ival-1)])
                      else None
                  | NONE => None
                (* end case *))
            (***** NEG *****)
            | (P.PURE_ARITH{oper=P.NEG, kind=P.UINT sz}, [NUM i]) =>
                Val(NUM{ival = CA.uNeg(sz, #ival i), ty = #ty i})
            (***** LSHIFT *****)
            | (P.PURE_ARITH{oper=P.LSHIFT, ...}, [v as NUM{ival=0, ...}, _]) => Val v
            | (P.PURE_ARITH{oper=P.LSHIFT, ...}, [v, NUM{ival=0, ...}]) => Val v
            | (P.PURE_ARITH{oper=P.LSHIFT, kind=P.INT sz}, [NUM i, NUM j]) => (
                Val(NUM{ival = CA.sShL(sz, #ival i, #ival j), ty = #ty i})
                  handle Overflow => None)
            | (P.PURE_ARITH{oper=P.LSHIFT, kind=P.UINT sz}, [NUM i, NUM j]) =>
                Val(NUM{ival = CA.uShL(sz, #ival i, #ival j), ty = #ty i})
            (***** RSHIFT *****)
            | (P.PURE_ARITH{oper=P.RSHIFT, ...}, [i as NUM{ival=0, ...}, _]) => Val i
            | (P.PURE_ARITH{oper=P.RSHIFT, ...}, [v, NUM{ival=0, ...}]) => Val v
            | (P.PURE_ARITH{oper=P.RSHIFT, kind=P.INT sz}, [NUM i, NUM j]) =>
                Val(NUM{ival = CA.sShR(sz, #ival i, #ival j), ty = #ty i})
            | (P.PURE_ARITH{oper=P.RSHIFT, kind=P.UINT sz}, [NUM i, NUM j]) => let
              (* to get the sign-extension right, we need to convert to a signed literal
               * and then back to unsigned.
               *)
                val res = CA.toUnsigned(sz, CA.sShR(sz, CA.toSigned(sz, #ival i), #ival j))
                in
                  Val(NUM{ival = res, ty = #ty i})
                end
            (***** RSHIFTL *****)
            | (P.PURE_ARITH{oper=P.RSHIFTL, ...}, [i as NUM{ival=0, ...}, _]) => Val i
            | (P.PURE_ARITH{oper=P.RSHIFTL, ...}, [v, NUM{ival=0, ...}]) => Val v
            | (P.PURE_ARITH{oper=P.RSHIFTL, kind=P.UINT sz}, [NUM i, NUM j]) =>
                Val(NUM{ival = CA.uShR(sz, #ival i, #ival j), ty = #ty i})
            (***** ORB *****)
            | (P.PURE_ARITH{oper=P.ORB, ...}, [NUM{ival=0, ...}, v]) => Val v
            | (P.PURE_ARITH{oper=P.ORB, ...}, [v, NUM{ival=0, ...}]) => Val v
            | (P.PURE_ARITH{oper=P.ORB, kind}, [NUM i, NUM j]) =>
                Val(NUM{ival = CA.bOr(sizeOfKind kind, #ival i, #ival j), ty = #ty i})
            (***** XORB *****)
            | (P.PURE_ARITH{oper=P.XORB, ...}, [NUM{ival=0, ...}, v]) => Val v
            | (P.PURE_ARITH{oper=P.XORB, ...}, [v, NUM{ival=0, ...}]) => Val v
            | (P.PURE_ARITH{oper=P.XORB, kind}, [NUM i, NUM j]) =>
                Val(NUM{ival = CA.bXor(sizeOfKind kind, #ival i, #ival j), ty = #ty i})
            (***** ANDB *****)
            | (P.PURE_ARITH{oper=P.ANDB, ...}, [v as NUM{ival=0, ...}, _]) => Val v
            | (P.PURE_ARITH{oper=P.ANDB, ...}, [_, v as NUM{ival=0, ...}]) => Val v
            | (P.PURE_ARITH{oper=P.ANDB, kind}, [NUM i, NUM j]) =>
                Val(NUM{ival = CA.bAnd(sizeOfKind kind, #ival i, #ival j), ty = #ty i})
            (***** NOTB *****)
            | (P.PURE_ARITH{oper=P.NOTB, kind}, [NUM i]) =>
                Val(NUM{ival = CA.bNot(sizeOfKind kind, #ival i), ty = #ty i})
            (***** COPY *****)
            | (P.COPY{from=n, to=p}, [v]) => if (n = p)
                (* COPY(n, n) ==> IDENTITY *)
                then Val v
                else (case v
                   of NUM{ival, ...} => Val(mkNum(p, ival))
                    | VAR x => (case #info(get x)
                         of PUREinfo(P.COPY{from=m, ...}, [u]) =>
                            (* COPY(n,p) o COPY(m,n) ==> COPY(m,p) *)
                            mkCOPY(m, p, u)
                          | _ => None
                        (* end case *))
                    | _ => bug "bogus argument to COPY"
                  (* end case *))
            (***** EXTEND *****)
            | (P.EXTEND{from=n, to=p}, [v]) => if (n = p)
                (* EXTEND(n, n) ==> IDENTITY *)
                then Val v
                else (case v
                   of NUM{ival, ...} =>
                        if (ival > 0)
                        andalso (IntInf.andb(IntInf.<<(1, Word.fromInt(n-1)), ival) <> 0)
                          then Val(mkNum(p, ival - IntInf.<<(1, Word.fromInt n)))
                          else Val(mkNum(p, ival))
                    | VAR x => (case #info(get x)
                         of PUREinfo(P.EXTEND{from=m, ...}, arg) =>
                              (* EXTEND(n,p) o EXTEND(m,n) ==> EXTEND(m,p) *)
                              Pure(P.EXTEND{from=m, to=p}, arg)
                          | PUREinfo(P.COPY{from=m, to=n}, [u]) =>
                              if (n > m)
                                (* EXTEND(n,p) o COPY(m,n) ==> COPY(m,p) if (n > m) *)
                                then mkCOPY(m, p, u)
                                else None
                          | _ => None
                        (* end case *))
                    | _ => bug "bogus argument to EXTEND"
                  (* end case *))
            (***** TRUNC *****)
            | (P.TRUNC{from=n, to=p}, [v]) => if (n = p)
                (* TRUNC(n, n) ==> IDENTITY *)
                then Val v
                else (case v
                   of NUM{ival, ...} => let
                        val ival' = IntInf.andb(IntInf.<<(1, Word.fromInt p)-1, ival)
                        in
                          Val(mkNum(p, ival'))
                        end
                    | VAR x => (case #info(get x)
                         of PUREinfo(P.TRUNC{from=m, ...}, arg) =>
                              (* TRUNC(n,p) o TRUNC(m,n) ==> TRUNC(m,p) *)
                              Pure(P.TRUNC{from=m, to=p}, arg)
                          | PUREinfo(P.TRUNC_INF _, arg) =>
                              (* TRUNC(n,p) o TRUNC(∞,n) ==> TRUNC(∞,p) *)
                              Pure(P.TRUNC_INF p, arg)
                          | PUREinfo(P.COPY{from=m, ...}, [u]) =>
                              if (p >= m)
                                (* TRUNC(n,p) o COPY(m,n) ==> COPY(m,p) if (p >= m) *)
                                then mkCOPY(m, p, u)
                                (* TRUNC(n,p) o COPY(m,n) ==> TRUNC(m,p) if (p < m) *)
                                else Pure(P.TRUNC{from=m, to=p}, [u])
                          | PUREinfo(P.EXTEND{from=m, to=n}, arg) =>
                              if (p >= m)
                                (* TRUNC(n,p) o EXTEND(m,n) ==> EXTEND(m,p) if (p >= m) *)
                                then Pure(P.EXTEND{from=m, to=p}, arg)
                                (* TRUNC(n,p) o EXTEND(m,n) ==> TRUNC(m,p) if (p < m) *)
                                else Pure(P.TRUNC{from=m, to=p}, arg)
                          | _ => None
                        (* end case *))
                    | _ => bug "bogus argument to TRUNC"
                  (* end case *))
            (***** COPY_INF *****)
            | (P.COPY_INF _, [VAR v, f]) => (case #info(get v)
                 of PUREinfo(P.COPY{from=m, ...}, [u]) =>
                      (* COPY(n,∞) o COPY(m,n) ==> COPY(m,∞) *)
                      Pure(P.COPY_INF m, [u, f])
                  | _ => None
                (* end case *))
            (***** EXTEND_INF *****)
            | (P.EXTEND_INF n, [VAR v, f]) => (case #info(get v)
                 of PUREinfo(P.EXTEND{from=m, ...}, arg) =>
                      (* EXTEND(n,∞) o EXTEND(m,n) ==> EXTEND(m,∞) *)
                      Pure(P.EXTEND_INF m, arg)
                  | PUREinfo(P.COPY{from=m, ...}, [u]) => if (m < n)
                      (* EXTEND(n,∞) o COPY(m,n) ==> COPY(m,∞) when (m < n) *)
                      then Pure(P.COPY_INF m, [u, f])
                      (* EXTEND(n,∞) o COPY(m,n) ==> EXTEND(m,∞) when (m = n) *)
                      else Pure(P.EXTEND_INF m, [u, f])
                  | _ => None
                (* end case *))
            (***** TRUNC_INF *****)
            | (P.TRUNC_INF p, [VAR v, _]) => (case #info(get v)
                 of PUREinfo(P.COPY_INF m, [u, _]) => if (p >= m)
                      (* TRUNC(∞,p) o COPY(m,∞) ==> COPY(m,p) when (p >= m) *)
                      then mkCOPY(m, p, u)
                      (* TRUNC(∞,p) o COPY(m,∞) ==> TRUNC(m,p) when (m > p) *)
                      else Pure(P.TRUNC{from=m, to=p}, [u])
                  | PUREinfo(P.EXTEND_INF m, [u, _]) => if (p >= m)
                      (* TRUNC(∞,p) o EXTEND(m,∞) ==> EXTEND(m,p) when (p >= m) *)
                      then Pure(P.EXTEND{from=m, to=p}, [u])
                      (* TRUNC(∞,p) o EXTEND(m,∞) ==> TRUNC(m,p) when (m > p) *)
                      else Pure(P.TRUNC{from=m, to=p}, [u])
                  | _ => None
                (* end case *))
            (***** Other primops *****)
            | (P.LENGTH, [STRING s]) => Val(tagInt(size s))
            | (P.INT_TO_REAL{to, ...}, [NUM{ival, ...}]) =>
                (* NOTE: this conversion might lose precision *)
                Val(REAL{rval = RealLit.fromInt ival, ty=to})
            | (P.BOX, [VAR v]) => (case #info(get v)
                 of PUREinfo(P.UNBOX, [u]) => Val u
                  | _ => None
                 (* end case *))
            | (P.UNBOX, [VAR v]) => (case #info(get v)
                 of PUREinfo(P.BOX, [u]) => Val u
                  | _ => None
                 (* end case *))
            | (P.WRAP(P.INT sz), [x as VAR v]) => (case #info(get v)
                  of PUREinfo(P.UNWRAP(P.INT sz'), [u]) => if (sz = sz')
                       then Val u
                       else bug "wrap(unwrap int) size conflict"
                   | _ => None
                 (* end case *))
            | (P.WRAP(P.FLOAT sz), [x as VAR v]) => (case #info(get v)
                  of PUREinfo(P.UNWRAP(P.FLOAT sz'), [u]) => if (sz = sz')
                       then Val u
                       else bug "wrap(unwrap float) size conflict"
                   | _ => None
                 (* end case *))
            | (P.UNWRAP(P.INT sz), [x as VAR v]) => (case #info(get v)
                  of PUREinfo(P.WRAP(P.INT sz'), [u]) => if (sz = sz')
                       then Val u
                       else bug "unwrap(wrap int) size conflict"
                   | _ => None
                 (* end case *))
            | (P.UNWRAP(P.FLOAT sz), [x as VAR v]) => (case #info(get v)
                  of PUREinfo(P.WRAP(P.FLOAT sz'), [u]) => if (sz = sz')
                       then Val u
                       else bug "unwrap(wrap float) size conflict"
                   | _ => None
                 (* end case *))
            | _ => None
          (* end case *))

  (* contraction for branch operations *)
    fun branch (get : get_info) = let
          fun cond (P.UNBOXED, vl) = notCond(P.BOXED, vl)
            | cond (P.BOXED, [NUM{ty={tag, ...}, ...}]) = SOME(not tag)
            | cond (P.BOXED, [STRING s]) = SOME true
            | cond (P.BOXED, [VAR v]) = (case #info(get v)
                 of RECinfo _ => SOME true
                  | PUREinfo(P.MKSPECIAL, _) => SOME true
                  | PUREinfo(P.BOX, _) => SOME true
                  | PUREinfo(P.WRAP _, _) => SOME true
                  | _ => NONE
                (* end case *))
            | cond (P.CMP{oper=P.LT, ...}, [VAR v, VAR w]) =
                if v=w then SOME false else NONE
            | cond (P.CMP{oper=P.LT, kind=P.INT _}, [NUM i, NUM j]) =
                SOME(#ival i < #ival j)
            | cond (P.CMP{oper=P.LT, kind=P.UINT sz}, [NUM i, NUM j]) =
                SOME(CA.uLess(sz, #ival i, #ival j))
            | cond (P.CMP{oper=P.LT, kind=P.UINT sz}, [_, NUM{ival=0, ...}]) =
                SOME false (* no unsigned value is < 0 *)
            | cond (P.CMP{oper=P.LT, kind=P.UINT _}, [VAR v, NUM{ival=256, ...}]) = (
                (* this might be a `Word8.word` to `char` conversion *)
                case get v
                 of {info=PUREinfo(P.COPY{from=8, to}, [x]), ...} => if (to > 8)
                      (* no sign extension, so `0 <= v < 256` *)
                      then SOME true
                      else NONE
                  | _ => NONE
                (* end case *))
            | cond (P.CMP{oper=P.LTE, ...}, [VAR v, VAR w]) =
                if v=w then SOME true else NONE
            | cond (P.CMP{oper=P.LTE, kind=P.INT _}, [NUM i, NUM j]) =
                SOME(#ival i <= #ival j)
            | cond (P.CMP{oper=P.LTE, kind=P.UINT sz}, [NUM i, NUM j]) =
                SOME(CA.uLessEq(sz, #ival i, #ival j))
            | cond (P.CMP{oper=P.LTE, kind=P.UINT sz}, [NUM{ival=0, ...}, _]) =
                SOME true (* 0 is <= all unsigned values *)
            | cond (P.CMP{oper=P.GT, kind}, [w,v]) =
                (* flip comparison and operands: `w > v` iff `v < w` *)
                cond (P.CMP{oper=P.LT, kind=kind}, [v,w])
            | cond (P.CMP{oper=P.GTE, kind}, vl) =
                (* negate comparison: `w >= v` iff `not(w < v)` *)
                notCond (P.CMP{oper=P.LT, kind=kind}, vl)
(* TODO: if both arguments are literals, we can optimize equality tests on floats,
 * but we need to be careful about inexact representations and NaNs.
 *)
            | cond (P.CMP{oper=P.EQL, kind=P.FLOAT _}, _) = NONE (* in case of NaN's *)
            | cond (P.CMP{oper=P.EQL, ...}, [VAR v, VAR w]) = if v=w then SOME true else NONE
            | cond (P.CMP{oper=P.EQL, ...}, [NUM i, NUM j]) = SOME( #ival i = #ival j)
            | cond (P.CMP{oper=P.NEQ, kind}, vl) = notCond (P.CMP{oper=P.EQL, kind=kind}, vl)
            | cond (P.PEQL, [NUM i, NUM j]) = SOME(#ival i = #ival j)
            | cond (P.PNEQ, vl) = notCond(P.PEQL, vl)
            | cond _ = NONE
          and notCond arg = Option.map not (cond arg)
          in
            cond
          end

  end

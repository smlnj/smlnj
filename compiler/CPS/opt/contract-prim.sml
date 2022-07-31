(* contract-prim.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Contraction for CPS primitive operations.
 *)

structure ContractPrim : sig

  (* information about a variable *)
    datatype info
      = FNinfo of {
	    args: CPS.lvar list,
	    body : CPS.cexp option ref,
	    specialuse: int ref option ref,
	    liveargs : bool list option ref
	  }
      | RECinfo of CPS.record_kind * (CPS.value * CPS.accesspath) list
      | SELinfo of int * CPS.value * CPS.cty
      | OFFinfo of int * CPS.value
      | WRPinfo of CPS.P.numkind * CPS.value			(* CPS.P.wrap of a value *)
      | IFIDIOMinfo of {body : (CPS.lvar * CPS.cexp * CPS.cexp) option ref}
      | MISCinfo of CPS.cty

    type get_info = CPS.lvar -> {info: info, used : int ref, called : int ref}

    val infoToString : info -> string

  (* the result of contracting an arithmetic operation *)
    datatype result
      = None					(* no contraction *)
      | Val of CPS.value			(* contract to value *)
      | Arith of CPS.P.arith * CPS.value list	(* strength reduction *)
      | Pure of CPS.P.pure * CPS.value list	(* strength reduction *)

    val arith : CPS.P.arith * CPS.value list -> result

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
      | OFFinfo of int * value
      | WRPinfo of CPS.P.numkind * value			(* CPS.P.wrap of a value *)
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
	    | OFFinfo(i, v) => concat[
		  "OFFinfo(", Int.toString i, ", ", PPCps.value2str v, ")"
		]
	    | WRPinfo(nk, v) => concat[
		  "WRPinfo(", PPCps.numkindToString nk, ", ", PPCps.value2str v, ")"
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
      = None					(* no contraction *)
      | Val of CPS.value			(* contract to value *)
      | Arith of CPS.P.arith * CPS.value list	(* strength reduction *)
      | Pure of CPS.P.pure * CPS.value list	(* strength reduction *)

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

  (* contraction for impure arithmetic operations; note that 64-bit IMUL, IDIV,
   * IMOD, IQUOT, and IREM have three arguments on 32-bit targets, so we need
   * to allow for the extra argument in the patterns.
   *)
    fun arith (rator, args) = ((case (rator, args)
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
	    (***** TEST *****)
	    | (P.TEST{from, to}, [NUM{ival, ...}]) => let
	      (* first convert to signed representation and then narrow *)
		val ival' = CA.sNarrow(to, CA.toSigned(from, ival))
		in
		  Val(mkNum(to, ival'))
		end
	    (***** TESTU *****)
	    | (P.TESTU{from, to}, [NUM{ival, ...}]) =>
		Val(mkNum(to, CA.sNarrow(to, ival)))
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
	    (***** Other primops *****)
	    | (P.LENGTH, [STRING s]) => Val(tagInt(size s))
	    | (P.COPY{from, to}, [NUM{ival, ...}]) => Val(mkNum(to, ival))
	    | (P.EXTEND{from, to}, [NUM{ival, ...}]) =>
		if (ival > 0)
		andalso (IntInf.andb(IntInf.<<(1, Word.fromInt(from-1)), ival) <> 0)
		  then Val(mkNum(to, ival - IntInf.<<(1, Word.fromInt from)))
		  else Val(mkNum(to, ival))
	    | (P.TRUNC{from, to}, [NUM{ival, ...}]) => let
		val ival' = IntInf.andb(IntInf.<<(1, Word.fromInt to)-1, ival)
		in
		  Val(mkNum(to, ival'))
		end
	    | (P.INT_TO_REAL{to, ...}, [NUM{ival, ...}]) =>
	      (* NOTE: this conversion might lose precision *)
		Val(REAL{rval = RealLit.fromInt ival, ty=to})
	    | (P.UNWRAP(P.INT sz), [x as VAR v]) => (case get v
		  of {info=WRPinfo(P.INT sz', u), ...} => if (sz = sz')
		       then Val u
		       else bug "wrap/unwrap float size conflict"
		   | _ => None
		 (* end case *))
	    | (P.UNWRAP(P.FLOAT sz), [x as VAR v]) => (case get v
		  of {info=WRPinfo(P.FLOAT sz', u), ...} => if (sz = sz')
		       then Val u
		       else bug "wrap/unwrap int size conflict"
		   | _ => None
		 (* end case *))
	    | _ => None
	  (* end case *))

  (* contraction for branch operations *)
    fun branch (get : get_info) = let
	  fun cond (P.UNBOXED, vl) = notCond(P.BOXED, vl)
	    | cond (P.BOXED, [NUM{ty={tag, ...}, ...}]) = SOME(not tag)
	    | cond (P.BOXED, [STRING s]) = SOME true
	    | cond (P.BOXED, [VAR v]) = (case get v
		 of {info=RECinfo _, ...} => SOME true
		  | {info=WRPinfo _, ...} => SOME true
		  | _ => NONE
		(* end case *))
	    | cond (P.CMP{oper=P.LT, ...}, [VAR v, VAR w]) = if v=w then SOME false else NONE
	    | cond (P.CMP{oper=P.LTE, ...}, [VAR v, VAR w]) = if v=w then SOME true else NONE
	    | cond (P.CMP{oper=P.LT, kind=P.INT _}, [NUM i, NUM j]) = SOME(#ival i < #ival j)
	    | cond (P.CMP{oper=P.LT, kind=P.UINT sz}, [NUM i, NUM j]) =
		SOME(CA.uLess(sz, #ival i, #ival j))
	    | cond (P.CMP{oper=P.LT, kind=P.UINT sz}, [_, NUM{ival=0, ...}]) =
		SOME false (* no unsigned value is < 0 *)
	    | cond (P.CMP{oper=P.LTE, kind=P.INT _}, [NUM i, NUM j]) =
		SOME(#ival i <= #ival j)
	    | cond (P.CMP{oper=P.LTE, kind=P.UINT sz}, [NUM i, NUM j]) =
		SOME(CA.uLessEq(sz, #ival i, #ival j))
	    | cond (P.CMP{oper=P.LTE, kind=P.UINT sz}, [NUM{ival=0, ...}, _]) =
		SOME true (* 0 is <= all unsigned values *)
	    | cond (P.CMP{oper=P.GT, kind}, [w,v]) = cond (P.CMP{oper=P.LT, kind=kind}, [v,w])
	    | cond (P.CMP{oper=P.GTE, kind}, vl) = notCond (P.CMP{oper=P.LT, kind=kind}, vl)
(* TODO: if both arguments are literals, we can optimize this, but we need to be careful
 * about inexact representations.
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

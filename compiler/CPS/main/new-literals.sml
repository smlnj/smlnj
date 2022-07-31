(* new-literals.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This file implements support for heap-allocated literals.  Our approach
 * is to split out the literals from the CPS representation and create a
 * bytecode program that the runtime execures to allocate the literals.
 *
 * The implementation of the bytecode interpreter for the literal language
 * is in base/runtime/gc/build-literals.c.  A description of the bytecode
 * language is in dev-notes/new-literals.md.
 *)

(* redundant for now
signature LITERALS =
  sig

  (** `litsplit f` takes a CPS function and splits out the heap-allocated
   * literal values from it.  At runtime, these literals will be accessed via
   * a record of literals that is allocated by the runtime system.  This
   * function returns a rewriten version of its argument that accesses
   * literals from the record and a byte-vector that encodes the program
   * for generating the literals.
   *)
    val split : CPS.function -> CPS.function * Word8Vector.vector

  end
*)

structure NewLiterals : LITERALS =
  struct

    structure W8V = Word8Vector
    structure W8B = Word8Buffer
    structure LV = LambdaVar
    structure LVTbl = LV.Tbl
    structure WordTbl = WordHashTable
    structure C = CPS

    fun bug msg = ErrorMsg.impossible ("Literals: "^msg)

    val debugFlg = Control.CG.debugLits
    val say = Control.Print.say

  (* number of bytes per ML value *)
    val valueSzb = Target.mlValueSz div 8

  (****************************************************************************
   *                 TRANSLATING THE LITERAL EXP TO BYTES                     *
   ****************************************************************************)

  (* Literals are encoded as instructions for a "literal machine."  The abstract
   * description of these instructions is given in dev-notes/new-literals.md
   *)

  (* magic number for V2 literal bytecodes.  This needs to agree with the runtime
   * constant `V2_MAGIC` in `runtime/gc/build-literals.c`
   *)
    val magicV2 : int = 0x20190921

  (* `INT` opcodes *)
    fun opINT_0_10 n = Word8.fromLargeInt n
    fun opINT_m5_m1 n = Word8.fromLargeInt(0x0A - n)
    val opINTb : Word8.word = 0wx10
    val opINTh : Word8.word = 0wx11
    val opINTw : Word8.word = 0wx12
    val opINTlw : Word8.word = 0wx13
  (* `INT32` opcodes *)
    val opINT32b : Word8.word = 0wx14
    val opINT32h : Word8.word = 0wx15
    val opINT32w : Word8.word = 0wx16
  (* `INT64` opcodes *)
    val opINT64b : Word8.word = 0wx17
    val opINT64h : Word8.word = 0wx18
    val opINT64w : Word8.word = 0wx19
    val opINT64lw : Word8.word = 0wx1A
  (* real literal opcode *)
    val opREAL32 : Word8.word = 0wx26
    val opREAL64 : Word8.word = 0wx27
  (* `STR` opcodes *)
    val opSTRb : Word8.word = 0wx2C
    val opSTRn : Word8.word = 0wx2D
  (* record opcodes *)
    fun opRECORD_1_7 len = Word8.fromInt(0x2F + len)
    val opRECORDb: Word8.word = 0wx37
    val opRECORDw: Word8.word = 0wx38
  (* vector opcodes *)
    val opVECTORb: Word8.word = 0wx39
    val opVECTORw: Word8.word = 0wx3A
  (* raw data vectors *)
    fun opRAW_1_2 n = Word8.fromInt(0x3A + n)
    val opRAWb : Word8.word = 0wx3D
    val opRAWw : Word8.word = 0wx3E
    val opRAW64b : Word8.word = 0wx41
    val opRAW64w : Word8.word = 0wx42
  (* save/load opcodes *)
    val opSAVEb : Word8.word = 0wx44
    val opSAVEh : Word8.word = 0wx45
    val opLOADb : Word8.word = 0wx46
    val opLOADh : Word8.word = 0wx47
  (* return *)
    val opRETURN : Word8.word = 0wxff

    fun ~>> (n : int, w : word) = Word.toIntX(Word.~>>(Word.fromInt n, w))

  (* encode 8-bit signed value as a byte list *)
    fun addInt8 (buf, n) = W8B.add1(buf, Word8.fromInt n)
    fun addLargeInt8 (buf, n) = W8B.add1(buf, Word8.fromLargeInt n)
  (* encode 16-bit signed value as a byte list *)
    fun addInt16 (buf, n) = (
	  W8B.add1(buf, Word8.fromInt(~>>(n, 0w8)));
	  W8B.add1(buf, Word8.fromInt n))
    fun addLargeInt16 (buf, n) = (
	  W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w8)));
	  W8B.add1(buf, Word8.fromLargeInt n))
  (* encode 32-bit signed value as a byte list *)
    fun addInt32 (buf, n) = (
	  W8B.add1(buf, Word8.fromInt(~>>(n, 0w24)));
	  W8B.add1(buf, Word8.fromInt(~>>(n, 0w16)));
	  W8B.add1(buf, Word8.fromInt(~>>(n, 0w8)));
	  W8B.add1(buf, Word8.fromInt n))
    fun addLargeInt32 (buf, n) = (
	  W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w24)));
	  W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w16)));
	  W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w8)));
	  W8B.add1(buf, Word8.fromLargeInt n))
  (* encode a 64-bit signed value as a byte list *)
    fun addLargeInt64 (buf, n) = (
	  W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w56)));
	  W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w48)));
	  W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w40)));
	  W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w32)));
	  addLargeInt32 (buf, n))
    fun addLargeInt64' (buf, n) = addLargeInt64 (buf, IntInf.fromInt n)

    fun intToBytes32 n = W8V.fromList[
	    Word8.fromInt(~>>(n, 0w24)),
	    Word8.fromInt(~>>(n, 0w16)),
	    Word8.fromInt(~>>(n, 0w8)),
	    Word8.fromInt n
	  ]

    fun largeIntToBytes32 n = W8V.fromList[
	    Word8.fromLargeInt(IntInf.~>>(n, 0w24)),
	    Word8.fromLargeInt(IntInf.~>>(n, 0w16)),
	    Word8.fromLargeInt(IntInf.~>>(n, 0w8)),
	    Word8.fromLargeInt n
	  ]

    fun largeIntToBytes64 n = W8V.fromList[
	    Word8.fromLargeInt(IntInf.~>>(n, 0w56)),
	    Word8.fromLargeInt(IntInf.~>>(n, 0w48)),
	    Word8.fromLargeInt(IntInf.~>>(n, 0w40)),
	    Word8.fromLargeInt(IntInf.~>>(n, 0w32)),
	    Word8.fromLargeInt(IntInf.~>>(n, 0w24)),
	    Word8.fromLargeInt(IntInf.~>>(n, 0w16)),
	    Word8.fromLargeInt(IntInf.~>>(n, 0w8)),
	    Word8.fromLargeInt n
	  ]

    val intToBytes = if Target.is64
	  then largeIntToBytes64 o Int.toLarge
	  else intToBytes32

    fun largeIntToBytes (32, n) = largeIntToBytes32 n
      | largeIntToBytes (64, n) = largeIntToBytes64 n
      | largeIntToBytes _ = bug "bogus integer size"

    fun real64ToBytes r = #1(Real64ToBits.toBits r)

  (* encode the literal header block *)
    fun headerToBytes {maxstk, maxsaved} = W8V.concat[
	  intToBytes32 magicV2,
	  intToBytes32 maxstk,
	  intToBytes32 Target.mlValueSz,
	  intToBytes32 maxsaved]

(* 64BIT: workaround for compiler bug *)
    val minInt32 = Word64.toLargeIntX 0wxFFFFFFFF80000000
    val maxInt32 = Word64.toLargeInt 0wx80000000

  (* encode tagged integers *)
    fun encINT (buf, n) = if (0 <= n) andalso (n <= 10)
	    then W8B.add1(buf, opINT_0_10 n)
	  else if (~5 <= n) andalso (n < 0)
	    then W8B.add1(buf, opINT_m5_m1 n)
	  else if (~128 <= n) andalso (n <= 127)
	    then (W8B.add1(buf, opINTb); addLargeInt8(buf, n))
	  else if (~32768 <= n) andalso (n <= 32767)
	    then (W8B.add1(buf, opINTh); addLargeInt16(buf, n))
	  else if (minInt32 <= n) andalso (n <= maxInt32)
	    then (W8B.add1(buf, opINTw); addLargeInt32(buf, n))
	    else (W8B.add1(buf, opINTlw); addLargeInt64(buf, n))

  (* encode 32-bit boxed integers *)
    fun encINT32 (buf, n) = if (~128 <= n) andalso (n <= 127)
	    then (W8B.add1(buf, opINTb); addLargeInt8(buf, n))
	  else if (~32768 <= n) andalso (n <= 32767)
	    then (W8B.add1(buf, opINTb); addLargeInt16(buf, n))
	    else (W8B.add1(buf, opINTb); addLargeInt32(buf, n))

  (* encode 64-bit boxed integers *)
    fun encINT64 (buf, n) = if (~128 <= n) andalso (n <= 127)
	    then (W8B.add1(buf, opINT64b); addLargeInt8(buf, n))
	  else if (~32768 <= n) andalso (n <= 32767)
	    then (W8B.add1(buf, opINT64h); addLargeInt16(buf, n))
	  else if (minInt32 <= n) andalso (n <= maxInt32)
	    then (W8B.add1(buf, opINT64w); addLargeInt32(buf, n))
	    else (W8B.add1(buf, opINT64lw); addLargeInt64(buf, n))

(* REAL32: FIXME *)
  (* endcode a 64-bit real literal *)
    fun encREAL64 (buf, bits) = (W8B.add1(buf, opREAL64); W8B.addVec(buf, bits))

  (* encode a STR opcode and length *)
    fun encSTR (buf, len) = if (len <= 255)
	    then (W8B.add1(buf, opSTRb); addInt8(buf, len))
	    else (W8B.add1(buf, opSTRn); W8B.addVec(buf, intToBytes len))

  (* encode a RECORD opcode and length *)
    fun encRECORD (buf, len) = if (len <= 7)
	    then W8B.add1(buf, opRECORD_1_7 len)
	  else if (len <= 255)
	    then (W8B.add1(buf, opRECORDb); addInt8(buf, len))
	  else if (len <= Vector.maxLen)
	    then (W8B.add1(buf, opRECORDw); addInt32(buf, len))
	    else bug "record too big"

  (* encode a VECTOR opcode and length *)
    fun encVECTOR (buf, len) = if (len <= 255)
	    then (W8B.add1(buf, opVECTORb); addInt8(buf, len))
	  else if (len <= Vector.maxLen)
	    then (W8B.add1(buf, opVECTORw); addInt32(buf, len))
	    else bug "vector too big"

  (* encode RAW data *)
    fun encRAW (buf, data) = let
	  val len = W8V.length data div valueSzb
	  in
	    if (len <= 2)
	      then W8B.add1(buf, opRAW_1_2 len)
	    else if (len <= 255)
	      then (W8B.add1(buf, opRAWb); addInt8(buf, len))
	      else (W8B.add1(buf, opRAWw); addInt32(buf, len));
	    W8B.addVec(buf, data)
	  end

  (* encode RAW64 data *)
    fun encRAW64 (buf, data) = let
	  val len = W8V.length data div 8
	  in
	    if (len <= 255)
	      then (W8B.add1(buf, opRAW64b); addInt8(buf, len))
	      else (W8B.add1(buf, opRAW64w); addInt32(buf, len));
	    W8B.addVec(buf, data)
	  end

  (* encode a SAVE/LOAD opcode *)
    local
      fun enc (opb, oph) (buf, slot) = if (slot <= 255)
	  then (W8B.add1(buf, opb); addInt8(buf, slot))
	  else (W8B.add1(buf, oph); addInt16(buf, slot))
    in
    val encSAVE = enc (opSAVEb, opSAVEh)
    val encLOAD = enc (opLOADb, opLOADh)
    end (* local *)

  (****************************************************************************
   *                    LIFTING LITERALS ON CPS                               *
   ****************************************************************************)

    datatype literal_value
      = LV_REAL of int RealConst.t			(* real number of given size *)
      | LV_STR of string				(* string *)
      | LV_RECORD of C.record_kind * literal list	(* record/vector/raw record *)
      | LV_RAW of W8V.vector				(* raw data vector (target word size) *)
      | LV_RAW64 of W8V.vector				(* raw data vector (64-bit aligned data) *)

    and literal
      = LIT of {			(* heap-allocated literal value *)
	    refCnt : int ref,		(* count of uses of this literal value from other
					 * literals; when > 1, then we have shared structure. *)
	    useCnt : int ref,		(* count of all uses of this literal.  When this count
					 * is > refCnt, then the literal will need to be bound
					 * to a variable in the residual program.
					 *)
	    id : word,			(* unique ID *)
	    value : literal_value	(* value *)
	  }
      | IMMED of C.intty IntConst.t	(* immediate integer/word literal *)

    fun useLit (LIT{useCnt, ...}) = useCnt := !useCnt + 1
      | useLit (IMMED _) = ()
    fun refLit (LIT{refCnt, ...}) = refCnt := !refCnt + 1
      | refLit (IMMED _) = ()

  (* return the CPS type for a literal value *)
    fun cpsTypeOf (LV_REAL{ty, ...}) = C.FLTt ty
      | cpsTypeOf (LV_STR _) = CPSUtil.BOGt
      | cpsTypeOf (LV_RECORD(C.RK_RECORD, lits)) = C.PTRt(C.RPT(List.length lits))
      | cpsTypeOf (LV_RECORD _) = CPSUtil.BOGt
      | cpsTypeOf (LV_RAW _) = CPSUtil.BOGt
      | cpsTypeOf (LV_RAW64 bv) = C.PTRt(C.FPT(W8V.length bv div 8))

  (* is a literal used as value outside of being part of another literal? *)
    fun litIsUsed (LIT{refCnt, useCnt, ...}) = (!refCnt < !useCnt)
      | litIsUsed _ = bug "impossible"

  (* is a literal shared?  This happens when its refCnt is > 1 or when its refCnt = 1 and
   * its useCnt > 1.
   *)
    fun litIsShared (LIT{refCnt=ref rc, useCnt, ...}) =
	  (rc > 1) orelse ((rc = 1) andalso (!useCnt > 1))
      | litIsShared _ = bug "impossible"

  (* print the list of "top-level" literals (for debugging purposes) *)
    fun printLits lits = let
	  val id2s = Word.fmt StringCvt.DEC
	  fun prIndent 0 = ()
	    | prIndent n = (say "  "; prIndent(n-1))
	  fun prLIT indent {refCnt, useCnt, id, value} = prValue indent (value, concat[
		  "#", id2s id, " ", Int.toString(!refCnt), "/", Int.toString(!useCnt)
		])
	  and prLiteral indent lit = (
		prIndent indent;
		case lit
		 of LIT arg => prLIT indent arg
		  | (IMMED{ty={sz, tag}, ival}) => say(concat[
			IntInf.toString ival, ":i", Int.toString sz, "\n"
		      ])
		(* end case *))
	  and prValue indent (lv, suffix) = (case lv
		 of (LV_REAL{rval, ...}) => say(concat[RealLit.toString rval, " ", suffix, "\n"])
		  | (LV_STR s) => say (concat["\"", String.toString s, "\" ", suffix, "\n"])
		  | (LV_RECORD(rk, lits)) => (
		      case rk
		       of C.RK_VECTOR => say(concat["VECTOR ", suffix, "\n"])
			| C.RK_RECORD => say(concat["RECORD ", suffix, "\n"])
		      (* end case *);
		      List.app (prLiteral (indent+1)) lits)
		  | (LV_RAW v) => say(concat[
			"RAW(", Int.toString(W8V.length v), " bytes) ", suffix, "\n"
		      ])
		  | (LV_RAW64 v) => say(concat[
			"RAW64(", Int.toString(W8V.length v), " bytes) ", suffix, "\n"
		      ])
		(* end case *))
	  fun prSlot (i, LIT arg) = (
		say (StringCvt.padLeft #" " 4 (Int.toString i) ^ ": ");
		prLIT 3 arg)
	    | prSlot (i, IMMED _) = bug "unexpected top-level IMMED"
	  in
	    List.appi prSlot lits
	  end (* printLits *)

  (* an environment for tracking literals *)
    structure LitEnv : sig

	type t

      (* a variable that is bound to a literal is either used to build a literal
       * record, in which case the bool is false, or is used as an argument to
       * some other operation (including non-literal records).
       *)
	type var_info = bool * literal

      (* create a new environment *)
	val new : unit -> t
      (* add a literal record value to the environment *)
	val addRecord : t -> C.record_kind * literal list * C.lvar -> unit
	val addRaw : t -> W8V.vector * C.lvar -> unit
	val addRaw64 : t -> W8V.vector * C.lvar -> unit
      (* return the literal that a variable is bound to *)
	val findVar : t -> C.lvar -> var_info option
      (* is a value representable as a literal? *)
	val isConst : t -> C.value -> bool
      (* find the literal value for the given value.  Note that for NUM values, we
       * return NONE, since they are represented as IMMED literals.
       *)
	val findValue : t -> C.value -> literal option
      (* record the use of a value in a non-literal context *)
	val useValue : t -> C.value -> unit
      (* like useValue, but for values embedded in literal records.  This function
       * returns the literal that the value maps to.
       *)
	val useValue' : t -> C.value -> literal
      (* return the number of literals in the environment *)
	val numLits : t -> int
      (* return true if there are no literals defined in the environment *)
	val isEmpty : t -> bool
      (* return true if the environment has unbound 64-bit real literals (e.g.,
       * the arguments to arithmetic operations).
       *)
	val hasReal64 : t -> bool
      (* return a list of all of the literals defined in the environment (not counting
       * the IMMED literals, which are not recorded in the environment)
       *)
	val allLits : t -> literal list
      (* return a list of the variables that are bound to top-level literalsn paired
       * with their binding.
       *)
	val boundVars : t -> (C.lvar * literal) list

      end = struct

	fun hashLV (LV_REAL{ty, rval}) = RealLit.hash rval + 0w179
	  | hashLV (LV_STR s) = HashString.hashString s + 0w283
	  | hashLV (LV_RECORD(rk, lits)) = let
	      fun f (LIT{id, ...}, h) = 0w3 * id + 0w419
		| f (IMMED{ty={tag, sz}, ival}, h) = if tag
		      then Word.fromLargeInt ival + 0w157
		      else Word.fromLargeInt ival + Word.fromInt sz + 0w257
	      val h0 = (case rk
		     of C.RK_VECTOR => 0w197
		      | C.RK_RECORD => 0w313
		      | _ => bug("unexpected record kind " ^ PPCps.rkToString rk)
		    (* end case *))
	      in
		List.foldl f h0 lits
	      end
	  | hashLV (LV_RAW v) = HashString.hashString(Byte.bytesToString v) + 0w127
	  | hashLV (LV_RAW64 v) = HashString.hashString(Byte.bytesToString v) + 0w233

	fun sameLV (LV_REAL{ty=ty1, rval=rv1}, LV_REAL{ty=ty2, rval=rv2}) =
	      (ty1 = ty2) andalso RealLit.same(rv1, rv2)
	  | sameLV (LV_STR s1, LV_STR s2) = (s1 = s2)
	  | sameLV (LV_RECORD(rk1, lvs1), LV_RECORD(rk2, lvs2)) =
	      (rk1 = rk2) andalso ListPair.allEq sameLit (lvs1, lvs2)
	  | sameLV (LV_RAW v1, LV_RAW v2) = (v1 = v2)
	  | sameLV (LV_RAW64 v1, LV_RAW64 v2) = (v1 = v2)
	  | sameLV _ = false

	and sameLit (LIT{useCnt=u1, ...}, LIT{useCnt=u2, ...}) = (u1 = u2)
	  | sameLit (IMMED n1, IMMED n2) = (n1 = n2)
	  | sameLit _ = false

	structure LTbl = HashTableFn(
	  struct
	    type hash_key = literal_value
	    val hashVal = hashLV
	    val sameKey = sameLV
	  end)

	type var_info = bool * literal

	datatype t = LE of {
	    hasReal64Lits : bool ref,		(* true if there are unbound real64 literals *)
	    lits : literal LTbl.hash_table,	(* table of unique literals in the module *)
	    vMap : var_info LV.Tbl.hash_table	(* map from variables to the literals that they *)
						(* are bound to *)
	  }

	fun new () = LE{
		hasReal64Lits = ref false,
		lits = LTbl.mkTable(32, Fail "LitTbl"),
		vMap = LV.Tbl.mkTable(32, Fail "VarTbl")
	      }

	fun setHasReal64 (LE{hasReal64Lits, ...}) = (hasReal64Lits := true)
	fun hasReal64 (LE{hasReal64Lits, ...}) = !hasReal64Lits

	fun add lits = let
	      val find = LTbl.find lits
	      val insert = LTbl.insert lits
	      in
		fn lv => (case find lv
		     of SOME lit => lit
		      | NONE => let
			  val lit = LIT{
				  useCnt = ref 0,
				  refCnt = ref 0,
				  id = Word.fromInt(LTbl.numItems lits),
				  value = lv
				}
			  in
			    LTbl.insert lits (lv, lit);
			    lit
			  end
		    (* end case *))
	      end

	local
	  fun addLiteral wrap (LE{lits, ...}) = let
		val addL = add lits
		in
		  fn v => addL (wrap v)
		end
	in
	val addString = addLiteral LV_STR
	val addReal = addLiteral LV_REAL
	end (* local *)

	fun addRecord (tbl as LE{lits, vMap, ...}) = let
	      val add = add lits
	      val insert = LV.Tbl.insert vMap
	      in
		fn (rk, flds, v) => insert (v, (false, add (LV_RECORD(rk, flds))))
	      end

	local
	  fun addRawLit wrap (tbl as LE{lits, vMap, ...}) = let
		val add = add lits
		val insert = LV.Tbl.insert vMap
		in
		  fn (data, v) => insert (v, (false, add (wrap data)))
		end
	in
	val addRaw = addRawLit LV_RAW
	val addRaw64 = addRawLit LV_RAW64
	end (* local *)

	fun findVar (LE{vMap, ...}) = LV.Tbl.find vMap

	fun insertVar (LE{vMap, ...}) = LV.Tbl.insert vMap

	fun isConst (LE{vMap, ...}) = let
	      val inDomain = LV.Tbl.inDomain vMap
	      in
		fn (C.VAR x) => inDomain x
		 | (C.LABEL _) => bug "unexpected LABEL"
		 | (C.NUM n) => true
		 | (C.REAL r) => true
		 | (C.STRING s) => true
		 | C.VOID => false
	      end

	fun findValue (LE{lits, vMap, ...}) = let
	      val findLit = LTbl.find lits
	      val findVar = LV.Tbl.find vMap
	      in
		fn (C.VAR x) => Option.map #2 (findVar x)
		 | (C.LABEL _) => bug "unexpected LABEL"
		 | (C.NUM n) => NONE
		 | (C.REAL r) => findLit (LV_REAL r)
		 | (C.STRING s) => findLit (LV_STR s)
		 | C.VOID => bug "unexpected VOID"
	      end

	fun useValue env = let
	      val findVar = findVar env
	      val addReal = addReal env
	      val addString = addString env
	      val insert = insertVar env
	      in
		fn (C.VAR x) => (case findVar x
		       of SOME(flg, lit) => (
			    useLit lit;
			    if flg then () else insert (x, (true, lit)))
			| NONE => ()
		      (* end case *))
		 | (C.LABEL _) => bug "unexpected LABEL"
		 | (C.NUM n) => ()
		 | (C.REAL r) => (setHasReal64 env; useLit(addReal r))
		 | (C.STRING s) => useLit(addString s)
		 | C.VOID => ()
	      end

	fun useValue' env = let
	      val findVar = findVar env
	      val addReal = addReal env
	      val addString = addString env
	      fun use lit = (useLit lit; refLit lit; lit)
	      in
		fn (C.VAR x) => (case findVar x
		      of SOME(_, lit) => use lit
		       | NONE => bug "expected literal"
		     (* end case *))
		 | (C.LABEL _) => bug "unexpected LABEL"
		 | (C.NUM n) => IMMED n
		 | (C.REAL r) => bug "unexpected REAL"
		 | (C.STRING s) => use(addString s)
		 | C.VOID => bug "unexpected VOID"
	      end

	fun numLits (LE{lits, ...}) = LTbl.numItems lits

	fun isEmpty (LE{lits, ...}) = (LTbl.numItems lits = 0)

	fun allLits (LE{lits, ...}) = LTbl.listItems lits

	fun boundVars (LE{vMap, ...}) =
	      LV.Tbl.foldi
		(fn (x, (true, lit), acc) => (x, lit)::acc | (_, _, acc) => acc)
		  [] vMap

      end (* LitEnv *)


  (* The first pass initializes the literal table by walking the CPS module.  After
   * this pass, we have identified any literal value that needs to be included in the
   * literal section.  Furthermore, we have identified which literal values are used
   * in non-literal contexts.
   *)
    fun identifyLiterals body = let
	  val env = LitEnv.new()
	  val isConst = LitEnv.isConst env
	  val useValue = LitEnv.useValue env
	  val useValues = List.app useValue
	  val useValue' = LitEnv.useValue' env
	  val addRecord = LitEnv.addRecord env
	  val addRaw = LitEnv.addRaw env
	  val addRaw64 = LitEnv.addRaw64 env
	  fun fieldToValue (u, C.OFFp 0) = u
	    | fieldToValue _ = bug "unexpected access in field"
	(* process a CPS function *)
	  fun doFun (fk, f, vl, cl, e) = doExp e
	(* process a CPS expression *)
	  and doExp ce = (case ce
		 of C.RECORD(rk, fields, v, e) => let
		      val ul = List.map fieldToValue fields
		      in
			case rk
			 of C.RK_RAWBLOCK => let
			      fun isImmed (C.NUM _) = true
				| isImmed _ = false
			      fun encode (C.NUM{ty={sz, ...}, ival}) =
				    largeIntToBytes(sz, ival)
				| encode _ = bug "RAWBLOCK: impossible"
			      in
				if List.all isImmed ul
				  then addRaw (W8V.concat(List.map encode ul), v)
				  else useValues ul
			      end
			  | C.RK_RAW64BLOCK => let
			      fun isImmed (C.REAL _) = true
				| isImmed _ = false
			      fun encode (C.REAL{ty, rval}) = real64ToBytes rval
				| encode _ = bug "RAWBLOCK: impossible"
			      in
				if List.all isImmed ul
				  then addRaw64 (W8V.concat(List.map encode ul), v)
				  else useValues ul
			      end
			  | _ => if List.all isConst ul
			      then addRecord (rk, List.map useValue' ul, v)
			      else useValues ul
			(* end case *);
			doExp e
		      end
		  | C.SELECT(i, u, v, t, e) => (useValue u; doExp e)
		  | C.OFFSET _ => bug "unexpected OFFSET in doExp"
		  | C.APP(u, ul) => useValues ul
		  | C.FIX(fns, e) => (List.app doFun fns; doExp e)
		  | C.SWITCH(u, v, es) => (useValue u; List.app doExp es)
		  | C.BRANCH(p, ul, v, e1, e2) => (useValues ul; doExp e1; doExp e2)
		  | C.SETTER(p, ul, e) => (useValues ul; doExp e)
		  | C.LOOKER(p, ul, v, t, e) => (useValues ul; doExp e)
		  | C.ARITH(p, ul, v, t, e) => (useValues ul; doExp e)
		  | C.PURE(C.P.WRAP(C.P.INT sz), [C.NUM{ival, ...}], v, t, e) => (
		      addRaw (largeIntToBytes(sz, ival), v);
		      doExp e)
(* REAL32: FIXME *)
		  | C.PURE(C.P.WRAP(C.P.FLOAT 64), [C.REAL{ty=64, rval}], v, t, e) => (
		      addRaw64 (real64ToBytes rval, v);
		      doExp e)
		  | C.PURE (p, ul, v, t, e) => (useValues ul; doExp e)
		  | C.RCC (k, l, p, ul, vtl, e) => (useValues ul; doExp e)
		(* end case *))
	  in
	    doExp body;
	    env
	  end

  (* literal values are either in the main literal vector or in the vector
   * of real literals.
   *)
    datatype lit_loc = LitSlot of int | Real64Slot of int

  (* build the representation of the literals; return a table mapping literal IDs
   * to their locations, the bytecode for building the literal vector, and a boolean
   * that is true if there is a real-literal vector.
   *)
    fun buildLiterals env = let
	(* generate bytecode for the literals *)
	  val buf = W8B.new (2 * LitEnv.numLits env * valueSzb)
	(* track the maximum stack depth required *)
	  val stkDepth = ref 0
	  fun depth d = if d > !stkDepth then stkDepth := d else ()
	(* get a list of the literals that are bound to variables in order of their
	 * definition.
	 *)
	  val lits = let
		fun gt (LIT{id=a, ...}, LIT{id=b, ...}) = (a > b)
		  | gt _ = bug "unexpected IMMED literal"
		in
		  ListMergeSort.sort gt
		    (List.filter litIsUsed (LitEnv.allLits env))
		end
	  val numNamedLits = List.length lits
	(* tracking the location of literals in the literal/real vector *)
	  val nLits = ref(if LitEnv.hasReal64 env then 1 else 0)
	  val nReal64Lits = ref 0
	  val real64Lits = ref []
	  val litIdTbl = WordTbl.mkTable(numNamedLits, Fail "litIdTbl")
	  val insertLit = let
		val insert = WordTbl.insert litIdTbl
		in
		  fn id => let val slot = !nLits
		      in
			nLits := slot + 1;
			insert (id, LitSlot slot)
		      end
		end
	  val insertReal64 = let
		val insert = WordTbl.insert litIdTbl
		in
		  fn (id, rval) => let val slot = !nReal64Lits
		      in
			nReal64Lits := slot + 1;
			insert (id, Real64Slot slot);
			real64Lits := real64ToBytes rval :: !real64Lits
		      end
		end
	(* table to track shared literals (indexed by literal ID) *)
	  val sharedLitTbl = WordTbl.mkTable(numNamedLits, Fail "sharedLitTbl")
	  val insertSharedLit = let
		val insert = WordTbl.insert sharedLitTbl
		in
		  fn id => let val loc = WordTbl.numItems sharedLitTbl
		      in
			insert (id, loc); loc
		      end
		end
	  val findSharedLit = WordTbl.find sharedLitTbl
	(* generate code for a literal *)
	  fun genLiteral (d, lit as LIT{id, value, ...}) = let
		fun genLV (d, LV_REAL _) = bug "unexpected embedded LV_REAL"
		  | genLV (d, LV_STR s) = (
		      encSTR(buf, size s); W8B.addVec(buf, Byte.stringToBytes s))
		  | genLV (d, LV_RECORD(rk, lits)) = let
		      fun genFld (lit, d) = (genLit (d, lit); d+1)
		      in
			depth (Int.max(d+1, foldl genFld d lits));
			case rk
			 of C.RK_VECTOR => encVECTOR (buf, List.length lits)
			  | C.RK_RECORD => encRECORD (buf, List.length lits)
			  | _ => bug "unexpected record kind"
			(* end case *)
		      end
		  | genLV (d, LV_RAW v) = (depth(d+1); encRAW(buf, v))
		  | genLV (d, LV_RAW64 v) = (depth(d+1); encRAW64(buf, v))
		and genLit (d, lit as LIT{id, value, ...}) = if litIsShared lit
		      then ( (* shared literal, so either load or save it *)
			case findSharedLit id
			 of SOME slot => (depth(d+1); encLOAD(buf, slot))
			  | NONE => (genLV(d, value); encSAVE(buf, insertSharedLit id))
			(* end case *))
		      else genLV (d, value)
		  | genLit (d, IMMED{ty={tag=true, ...}, ival}) = (depth(d+1); encINT (buf, ival))
		  | genLit (d, IMMED{ty={sz=32, ...}, ival}) = (depth(d+1); encINT32 (buf, ival))
		  | genLit (d, IMMED{ty={sz=64, ...}, ival}) = (depth(d+1); encINT64 (buf, ival))
		  | genLit _ = bug "unsupported IMMED type"
		in
		  case value
		   of LV_REAL{ty=64, rval} => insertReal64(id, rval)
		    | _ => (insertLit id; genLit (d, lit))
		  (* end case *)
		end
	    | genLiteral _ = bug "unexpected top-level IMMED literal"
	(* add literals to buffer *)
	  val _ = List.appi genLiteral lits
	(* generate the code to create the real-literal vector (if necessary) *)
	  val (rcode, litVecSz) = (case List.rev (!real64Lits)
		 of [] => (W8V.fromList[], numNamedLits)
		  | rlits => let
		      val rbuf = W8B.new(8 * !nReal64Lits + 5)
		      in
			depth (1);
			encRAW64 (rbuf, W8V.concat rlits);
			(W8B.contents rbuf, numNamedLits + 1 - !nReal64Lits)
		      end
		(* end case *))
	(* add the instruction to build the literal vector and to return the result *)
	  val _ = (encRECORD(buf, litVecSz); W8B.add1(buf, opRETURN))
	(* create literal program *)
	  val code = W8V.concat[
		  headerToBytes {maxstk = !stkDepth, maxsaved = WordTbl.numItems sharedLitTbl},
		  rcode,
		  W8B.contents buf
		]
	  in
	    if !debugFlg
	      then let
		fun prBV (x, LIT{id, ...}) = (
		      say(concat["LET ", LV.lvarName x, " = "]);
		      case WordTbl.find litIdTbl id
		       of NONE => say "<no slot>\n"
			| SOME(LitSlot n) => say(concat["literal-", Int.toString n, "\n"])
			| SOME(Real64Slot n) => say(concat["real64-", Int.toString n, "\n"])
		      (* end case *))
		fun prByte (i, w) = (
		      say(StringCvt.padLeft #"0" 2 (Word8.toString w));
		      if (i mod 16 = 15)
			then say "\n"
			else say " ")
		in
		  say "==========\n";
		  say(concat["== bytecode size: ", Int.toString(W8V.length code), "\n"]);
		  printLits lits;
		  say "==========\n";
		  List.app prBV (LitEnv.boundVars env);
		  say "==========\n";
		  W8V.appi prByte code;
		  if (W8V.length code mod 16 <> 15) then say "\n" else ();
		  say "==========\n"
		end
	      else ();
	    (litIdTbl, code, litVecSz, !nReal64Lits)
	  end

(* TODO: keep an environment of available literal bindings to avoid redundant SELECTs *)
  (* rewrite the program, removing unused variables *)
    fun liftLiterals (env, idTbl, litVec, fltVec, body) = let
	  val findValue = LitEnv.findValue env
	  val findVar = LitEnv.findVar env
	  fun getSlot id = (case WordTbl.find idTbl id
		 of SOME slot => slot
		  | NONE => bug("no slot for literal " ^ Word.fmt StringCvt.DEC id)
		(* end case *))
	(* rewrite a value *)
	  fun rewriteValue (u, k : C.value -> C.cexp) = (case findValue u
		 of SOME(LIT{id, value, ...}) => let
		      val v = LambdaVar.mkLvar()
		      val ty = cpsTypeOf value
		      in
			case getSlot id
			 of LitSlot n => C.SELECT(n, litVec, v, ty, k(C.VAR v))
			  | Real64Slot n => C.SELECT(n, fltVec, v, ty, k(C.VAR v))
			(* end case *)
		      end
		  | _ => k u
		(* end case *))
handle ex => (say(concat["rewriteValue (", PPCps.value2str u, ", -): error\n"]); raise ex)
	(* rewrite a list of values *)
	  fun rewriteValues (ul, k : C.value list -> C.cexp) = let
		fun rewrite ([], ul') = k(List.rev ul')
		  | rewrite (u::ul, ul') = rewriteValue (u, fn u' => rewrite(ul, u'::ul'))
		in
		  rewrite (ul, [])
		end
	(* rewrite a list of record fields *)
	  fun rewriteFields (ul, k : (C.value * C.accesspath) list -> C.cexp) = let
		fun rewrite ([], ul') = k(List.rev ul')
		  | rewrite ((u, acc)::ul, ul') =
		      rewriteValue (u, fn u' => rewrite(ul, (u',acc)::ul'))
		in
		  rewrite (ul, [])
		end
handle ex => (say "rewriteFields\n"; raise ex)
	(* rewrite a variable that might be bound to a record literal *)
	  fun rewriteVar (x, mkOrig, k) = (case findVar x
		 of SOME _ => k()
		  | _ => mkOrig()
		(* end case *))
handle ex => (say(concat["rewriteVar (", LV.lvarName x, ", -, -): error\n"]); raise ex)
	(* process a CPS function *)
	  fun doFun (fk, f, vl, cl, e) = (fk, f, vl, cl, doExp e)
	(* process a CPS expression *)
	  and doExp ce = (case ce
		 of C.RECORD(rk, ul, v, e) =>
		      rewriteVar (v,
			fn () => rewriteFields (ul, fn ul' => C.RECORD(rk, ul', v, doExp e)),
			fn () => doExp e)
		  | C.SELECT(i, u, v, t, e) =>
		      rewriteValue(u, fn u' => C.SELECT(i, u', v, t, doExp e))
		  | C.OFFSET _ => bug "unexpected OFFSET in doExp"
		  | C.APP(u, ul) =>
		      rewriteValues (ul, fn ul' => C.APP(u, ul'))
		  | C.FIX(fns, e) => C.FIX(map doFun fns, doExp e)
		  | C.SWITCH(u, v, es) =>
		      rewriteValue (u, fn u' => C.SWITCH(u', v, List.map doExp es))
		  | C.BRANCH(p, ul, v, e1, e2) =>
		      rewriteValues (ul, fn ul' => C.BRANCH(p, ul', v, doExp e1, doExp e2))
		  | C.SETTER(p, ul, e) =>
		      rewriteValues (ul, fn ul' => C.SETTER(p, ul', doExp e))
		  | C.LOOKER(p, ul, v, t, e) =>
		      rewriteValues (ul, fn ul' => C.LOOKER(p, ul', v, t, doExp e))
		  | C.ARITH(p, ul, v, t, e) =>
		      rewriteValues (ul, fn ul' => C.ARITH(p, ul', v, t, doExp e))
		  | C.PURE(C.P.WRAP nk, [u], v, t, e) =>
		      rewriteVar (v,
			fn () => rewriteValue (u, fn u' => C.PURE(C.P.WRAP nk, [u'], v, t, doExp e)),
			fn () => doExp e)
		  | C.PURE(p, ul, v, t, e) =>
		      rewriteValues (ul, fn ul' => C.PURE(p, ul', v, t, doExp e))
		  | C.RCC(k, l, p, ul, vtl, e) =>
		      rewriteValues (ul, fn ul' => C.RCC(k, l, p, ul', vtl, doExp e))
		(* end case *))
	  in
	  (* process the module *)
	    doExp body
	  end

 (* the main function *)
    fun split (func as (fk, f, vl as [_,x], [C.CNTt, t as C.PTRt(C.RPT n)], body)) = let
	(* new argument type has an additional argument for the literals *)
	  val nt = C.PTRt(C.RPT(n+1))
	  val _ = if !debugFlg
		then (
		  say (concat["\n==== Before Literals.liftLiterals\n"]);
		  PPCps.printcps0 func)
		else ()
	  val env = identifyLiterals body
	  val (nbody, code) = if LitEnv.isEmpty env
		then (body, W8V.fromList[opRETURN])
		else let
(* REAL32: FIXME *)
		  val (idTbl, code, nLits, nReal64Lits) = buildLiterals env
		  val lvv = LambdaVar.mkLvar()
		  val rvv = LambdaVar.mkLvar()
		  val nbody = liftLiterals (env, idTbl, C.VAR lvv, C.VAR rvv, body)
		(* add code to bind the real-literal vector (if necessary) *)
		  val nbody = if nReal64Lits > 0
			then C.SELECT(0, C.VAR lvv, rvv, C.PTRt(C.FPT nReal64Lits), nbody)
			else nbody
		(* add code to bind the literal vector *)
		  val nbody = C.SELECT(n, C.VAR x, lvv, C.PTRt(C.RPT nLits), nbody)
		  in
		    (nbody, code)
		  end
	  val nfunc = (fk, f, vl, [C.CNTt, nt], nbody)
	  in
	    if !debugFlg
	      then (
		say (concat["==== After Literals.liftLiterals\n"]);
		PPCps.printcps0 nfunc)
	      else ();
	    (nfunc, code)
	  end
      | split _ = bug "unexpected CPS header in split"

  end (* Literals *)

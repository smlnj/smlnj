(* literals.sml
 *
 * NOTE: this file implements the "old" bytecode instruction set, which is
 * only valid on 32-bit targets.  See new-literals.sml for the new encoding.
 *
 * This file implements support for heap-allocated literals.  Our approach
 * is to split out the literals from the CPS representation and create a
 * bytecode program that for allocating the literals.
 *
 * The implementation of the bytecode interpreter for the literal language
 * is in base/runtime/gc/old-literals.c.
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * TODO:
 *   REAL32: need support for 32-bit floats
 *   add support for IntInf.int as type
 *)

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

  end;

structure Literals : LITERALS =
  struct

    structure W8V = Word8Vector
    structure LV = LambdaVar
    structure Set = struct
	type intset = LV.Set.set ref
	fun new() = ref LV.Set.empty
	fun add set i = set := LV.Set.add(!set, i)
	fun mem set i =  LV.Set.member(!set, i)
	fun rmv set i = set := LV.Set.delete(!set, i)
      end

    open CPS

    fun bug msg = ErrorMsg.impossible ("Literals: "^msg)
    fun mkv _ = LV.mkLvar()

    val debugFlg = Control.CG.debugLits
    val say = Control.Print.say

  (****************************************************************************
   *                         A MINI-LITERAL LANGUAGE                          *
   ****************************************************************************)
    datatype lit_val
      = LI_INT of IntInf.int	(* tagged integer literal *)
      | LI_STRING of string
      | LI_VAR of lvar

    datatype block_kind
      = LI_RECORD		(* record of tagged ML values *)
      | LI_VECTOR		(* vector of tagged ML values *)

    datatype lit_exp
      = LI_TOP of lit_val list
      | LI_BLOCK of (block_kind * lit_val list * lvar * lit_exp)
      | LI_F64BLOCK of (RealLit.t list * lvar * lit_exp)
      | LI_RAWBLOCK of (IntInf.int list * lvar * lit_exp)

    fun rk2bk CPS.RK_VECTOR = LI_VECTOR
      | rk2bk CPS.RK_RECORD = LI_RECORD
      | rk2bk _ = bug "rk2bk: unexpected block kind"

    fun val2lit (CPS.VAR v) = LI_VAR v
      | val2lit (CPS.NUM{ival, ty={tag=true, ...}}) = LI_INT ival
      | val2lit (CPS.STRING s) = LI_STRING s
      | val2lit _ = bug "unexpected case in val2lit"

  (* printing a literal expression *)
    fun printLits lexp = let
	  fun prIndent 0 = ()
	    | prIndent n = (say "  "; prIndent(n-1))
	  fun valToStr (LI_INT n) = IntInf.toString n
	    | valToStr (LI_STRING s) = concat["\"", String.toString s, "\""]
	    | valToStr (LI_VAR x) = LV.lvarName x
	  fun pr indent (le : lit_exp) = (
		prIndent indent;
		case le
		 of LI_TOP lits => (
		      say "TOP\n";
		      List.app (fn lit => (prIndent(indent+1); say(valToStr lit); say "\n")) lits;
		      prIndent (indent+1);
		      say "RETURN")
		  | LI_BLOCK(rk, lits, x, k) => (
		      say "LET "; say(LV.lvarName x); say " = ";
		      case rk
		       of LI_VECTOR => say "VECTOR("
			| LI_RECORD => say "RECORD("
		      (* end case *);
		      say (String.concatWithMap "," valToStr lits);
		      say ")\n";
		      pr indent k)
		  | LI_F64BLOCK(lits, x, k) => (
		      say "LET "; say(LV.lvarName x); say " = RAW64(";
		      say (String.concatWithMap "," RealLit.toString lits);
		      say ")\n";
		      pr indent k)
		  | LI_RAWBLOCK(lits, x, k) => (
		      say "LET "; say(LV.lvarName x); say " = RAW(";
		      say (String.concatWithMap "," IntInf.toString lits);
		      say ")\n";
		      pr indent k)
		(* end case *))
	  in
	    pr 0 lexp
	  end

  (****************************************************************************
   *                 TRANSLATING THE LITERAL EXP TO BYTES                     *
   ****************************************************************************)

  (* Literals are encoded as instructions for a "literal machine."  The abstract
   * description of these instructions is as follows:
   *
   *	INT(i)			-- push the int31 literal i on the stack
   *	RAW32[i1,...,in]	-- form a 32-bit raw data record from the
   *				   i1..in and push a pointer to it.
   *	RAW64[r1,...,rn]	-- form a 64-bit raw data record from the
   *				   r1..rn and push a pointer to it.
   *	STR[c1,...,cn]		-- form a string from the characters c1..cn
   *				   and push it on the stack.
   *	LIT(k)			-- push the contents of the stack element
   *				   that is k slots from the top of the stack.
   *	VECTOR(n)		-- pop n elements from the stack, make a vector
   *				   from them and push a pointer to the vector.
   *	RECORD(n)		-- pop n elements from the stack, make a record
   *				   from them and push a pointer.
   *	RETURN			-- return the literal that is on the top of the
   *				   stack.
   *
   * Encoding:
   *   INT(i)		0x01 <i>
   *   RAW32[i]		0x02 <i>
   *   RAW32[i1,..,in]	0x03 <n> <i1> ... <in>
   *   RAW64[r]		0x04 <r>
   *   RAW64[r1,..,rn]	0x05 <n> <r1> ... <rn>
   *   STR[c1,..,cn]	0x06 <n> <c1> ... <cn>
   *   LIT(k)		0x07 <k>
   *   VECTOR(n)	0x08 <n>
   *   RECORD(n)	0x09 <n>
   *   RETURN		0xff
   *)

    fun toBytes32 (n, l) =
	  Word8.fromLargeInt(IntInf.~>>(n, 0w24)) ::
	  Word8.fromLargeInt(IntInf.~>>(n, 0w16)) ::
	  Word8.fromLargeInt(IntInf.~>>(n, 0w8)) ::
	  Word8.fromLargeInt n :: l
    fun toBytes32' (n, l) = toBytes32 (IntInf.fromInt n, l)
    fun intToBytes n = toBytes32' (n, [])
    fun strToBytes s = map Byte.charToByte (explode s)

    val emit_MAGIC = W8V.fromList[0wx19, 0wx98, 0wx10, 0wx22]
    fun emit_DEPTH n = W8V.fromList(intToBytes n)
    fun emit_INT i = W8V.fromList(0wx01 :: toBytes32(i, []))
    fun emit_RAW32 [i] = W8V.fromList(0wx02 :: toBytes32(i, []))
      | emit_RAW32 l =
	  W8V.fromList(0wx03 :: (toBytes32'(length l, List.foldr toBytes32 [] l)))
    fun emit_RAW64 [r] = W8V.fromList(0wx04 :: Word8Vector.toList r)
      | emit_RAW64 l = W8V.concat(W8V.fromList(0wx05 :: intToBytes(length l)) :: l)
    fun emit_STR s = W8V.concat[
	    W8V.fromList(0wx06 :: intToBytes(size s)),
	    Byte.stringToBytes s
	  ]
    fun emit_LIT k = W8V.fromList(0wx07 :: intToBytes k)
    fun emit_VECTOR n = W8V.fromList(0wx08 :: intToBytes n)
    fun emit_RECORD n = W8V.fromList(0wx09 :: intToBytes n)
    val emit_RETURN = W8V.fromList[0wxff]

    fun litToBytes (LI_TOP[]) = W8V.fromList[]
      | litToBytes litExp = let
	  fun depth (LI_TOP ls, d, maxDepth) = Int.max(maxDepth, d+length ls)
	    | depth (LI_BLOCK(_, ls, _, rest), d, maxDepth) =
		depth (rest, d+1, Int.max(maxDepth, d+length ls))
	    | depth (LI_F64BLOCK(ls, _, rest), d, maxDepth) =
		depth (rest, d+1, Int.max(maxDepth, d+length ls))
	    | depth (LI_RAWBLOCK(ls, _, rest), d, maxDepth) =
		depth (rest, d+1, Int.max(maxDepth, d+length ls))
	  fun emitLitExp (env, exp, code) = let
		fun emitLitVals ([], _, code) = code
		  | emitLitVals (lit::r, d, code) = let
		      val instr = (case lit
			     of (LI_INT i) => emit_INT i
			      | (LI_STRING s) => emit_STR s
			      | (LI_VAR v) => let
				  fun f ([], _) = bug "unbound lvar"
				    | f (v'::r, d) = if (v = v') then d else f(r, d+1)
				  in
				    emit_LIT(f (env, d))
				  end
			    (* end case *))
		      in
			emitLitVals (r, d+1, instr::code)
		      end
		fun emitBlock (LI_RECORD, ls, code) =
		      emit_RECORD(length ls) :: emitLitVals(ls, 0, code)
		  | emitBlock (LI_VECTOR, ls, code) =
		      emit_VECTOR(length ls) :: emitLitVals(ls, 0, code)
		fun emitF64Block (ls, code) = let
		      val toBits = #1 o Real64ToBits.toBits
		      in
		        emit_RAW64(map toBits ls) :: code
		      end
		fun emitI32Block (ls, code) = emit_RAW32 ls :: code
		in
		  case exp
		   of (LI_TOP ls) => emit_RETURN :: emitBlock(LI_RECORD, ls, code)
		    | (LI_BLOCK(bk, ls, v, rest)) =>
			emitLitExp (v::env, rest, emitBlock(bk, ls, code))
		    | (LI_F64BLOCK(ls, v, rest)) =>
			emitLitExp (v::env, rest, emitF64Block(ls, code))
		    | (LI_RAWBLOCK(ls, v, rest)) =>
			emitLitExp (v::env, rest, emitI32Block(ls, code))
		  (* end case *)
		end
	  val maxDepth = depth (litExp, 0, 1)
	  val code = emit_MAGIC
		:: emit_DEPTH maxDepth
		:: List.rev(emitLitExp([], litExp, []))
	  in
	    W8V.concat code
	  end

  (****************************************************************************
   *                    LIFTING LITERALS ON CPS                               *
   ****************************************************************************)
    datatype info
      = ZZ_STR of string
      | ZZ_RCD of record_kind * value list

    exception LitInfo

(* FIXME: we should probably either use hash tables or the raw comparison
 * functions to implement the dictionaries.
 *)

  (* string literal dictionary *)
    datatype slit = SLIT of string * word
    fun toSlit s = SLIT(s, HashString.hashString s)
    fun fromSlit (SLIT(s, _)) = s
    structure SlitDict = RedBlackMapFn(
      struct
        type ord_key = slit
	fun compare (SLIT(s1,i1), SLIT(s2,i2)) =
	      if i1 < i2 then LESS
	      else if i1 > i2 then GREATER
	      else String.compare(s1, s2)
      end)

  (* real literal dictionary *)
    datatype rlit = RLIT of RealLit.t * word
    fun toRlit r = RLIT(r, RealLit.hash r)
    fun fromRlit (RLIT(r, _)) = r
    structure RlitDict = RedBlackMapFn(
      struct
        type ord_key = rlit
	fun compare (RLIT(r1,i1), RLIT(r2,i2)) =
	  if i1 < i2 then LESS
	  else if i1 > i2 then GREATER
	  else RealLit.compare(r1, r2)
      end)

    (* lifting all literals from a CPS program *)
    fun liftlits (body, root, offset) = let
        (* the list of record, string, and real constants *)
	  val m : info LV.Tbl.hash_table = LV.Tbl.mkTable(32, LitInfo)
	  val freevars : lvar list ref = ref []
	  fun addv x = (freevars := (x :: (!freevars)))
	(* check if an lvar is used by the main program *)
	  val refset : Set.intset = Set.new()
	  val used : lvar -> unit = Set.add refset
	  val isUsed : lvar -> bool = Set.mem refset
	(* memoize the information on which corresponds to what *)
	  fun enter (v, i) = (LV.Tbl.insert m (v, i); addv v)
	  fun const (VAR v) = ((LV.Tbl.lookup m v; true) handle _ => false)
	    | const (NUM _ | REAL _ | STRING _) = true
	    | const _ = bug "unexpected case in const"
	  fun cstlit (VAR v) = ((LV.Tbl.lookup m v; true) handle _ => false)
	    | cstlit (REAL _ | STRING _) = true
	    | cstlit _ = false
	(* register a string literal *)
	  local val strs : string list ref = ref []
		val strsN : int ref = ref 0
		val sdict = ref (SlitDict.empty)
		val srtv = mkv()
		val srtval = VAR srtv
	  in
	  fun entStr s = let
	        val v = mkv()  (** should hash to remove duplicates **)
		val sd = !sdict
		val rlit = toSlit s
		val n = (case SlitDict.find(sd, rlit)
		       of SOME k => k
			| _ => let
			    val _ = (strs := (s :: (!strs)))
			    val k = !strsN
			    val _ = (strsN := (k+1))
			    val _ = (sdict := (SlitDict.insert(sd, rlit, k)))
			    in
			      k
			    end)
	        in
		  (VAR v, fn ce => SELECT(n, srtval, v, CPSUtil.BOGt, ce))
	        end
	  fun appStr () = let
		fun g (a::r, z) = g(r, (STRING a)::z)
		  | g ([], z) = z (* reverse to reflecting the correct order *)
	        in
		  case !strs
		   of [] => ()
		    | xs => (enter(srtv, ZZ_RCD(RK_RECORD, g(xs,[]))); used srtv)
		  (* end case *)
	        end
	  end (* local of processing string literals *)
	(* register a real literal *)
	  local val reals : RealLit.t list ref = ref []
		val realsN : int ref = ref 0
		val rdict = ref (RlitDict.empty)
		val rrtv = mkv()
		val rrtval = VAR rrtv
	  in
	  fun entReal s = let
	        val v = mkv()  (** should hash to remove duplicates **)
		val rd = !rdict
		val rlit = toRlit s
		val n = (case RlitDict.find(rd, rlit)
		       of SOME k => k
			| _ => let
			    val _ = (reals := (s :: (!reals)))
			    val k = !realsN
			    val _ = (realsN := (k+1))
			    val _ = (rdict := (RlitDict.insert(rd, rlit, k)))
			    in
			      k
			    end)
	        in
		  (VAR v, fn ce => SELECT(n, rrtval, v, FLTt 64, ce))	(* REAL32: FIXME *)
	        end
	  fun appReal () = let
	        fun g (a::r, z) = g(r, REAL{rval=a, ty=64} :: z)	(* REAL32: FIXME *)
		  | g ([], z) = z (* reverse to reflecting the correct order *)
	        in
		  case !reals
		   of [] => ()
		    | xs => (enter(rrtv, ZZ_RCD(RK_RAW64BLOCK, g(xs,[]))); used rrtv)
	        end
	  end (* local of processing real literals *)
	(* translation on the CPS values *)
	  fun lpsv u = (case u
		 of REAL{rval, ...} => entReal rval	(* REAL32: FIXME *)
		  | STRING s => entStr s
		  | VAR v => (used v; (u, Fn.id))
		  | _ => (u, Fn.id)
		(* end case *))
	  fun lpvs vs = let
		fun g (u, (xs, hh)) = let
		      val (nu, nh) = lpsv u
		      in
			(nu::xs, nh o hh)
		      end
	        in
		  foldr g ([], Fn.id) vs
	        end
	(* if all fields of a record are "constant", then we lift it *)
	  fun field ul = let
	       fun h ((x, OFFp 0)::r, z) = if const x
			then h(r, x::z)
			else NONE
		  | h ([], z) = SOME(rev z)
		  | h _ = bug "unexpected case in field"
	        in
		  h (ul, [])
	        end
	(* register a constant record *)
	  fun record (rk, ul, v) = (case field ul
		 of SOME xl => (enter(v, ZZ_RCD(rk, xl)); Fn.id)
		  | NONE => let
		      fun g ((u, p as OFFp 0), (r, hh)) = let
			      val (nu, nh) = lpsv u
			      in
				((nu, p)::r, nh o hh)
			      end
			 | g _ = bug "unexpected non-zero OFFp in record"
		      val (nl, hdr) = foldr g ([], Fn.id) ul
		      in
			fn ce => hdr(RECORD(rk, nl, v, ce))
		      end)
	(* register a wrapped float literal *)
	  fun wrapfloat (sz, u, v, t) = if const u
		then (enter(v, ZZ_RCD(RK_RAW64BLOCK, [u])); Fn.id)
		else let val (nu, hh) = lpsv u
		      in (fn ce => hh(PURE(P.WRAP(P.FLOAT sz), [nu], v, t, ce)))
		     end
	(* fetch out the literal information *)
	  fun getInfo () = let
	        val _ = appReal()   (* register all Reals as a record *)
		val _ = appStr()   (* register all Strings as a record *)
		val allvars = !freevars
		val exports = List.filter isUsed allvars

		val toplit =
		  let fun g ([], z) = LI_TOP z
			| g (x::r, z) =
			     (case LV.Tbl.lookup m x
			       of ZZ_STR s => g(r, (LI_STRING s)::z)
				| _ => g(r, (LI_VAR x)::z))
		   in g(exports, [])
		  end

		fun mklit (v, lit) = let
		    fun unREAL (CPS.REAL{rval, ...}) = rval	(* REAL32: FIXME *)
		      | unREAL _ = bug "unREAL"
		    fun unINT32 (CPS.NUM{ival, ...}) = ival
		      | unINT32 _ = bug "unINT32"
		    in
		      case LV.Tbl.lookup m v
		       of (ZZ_STR s) =>
			    bug "currently we don't expect ZZ_STR in mklit"
			(* lit   --- or we could inline string *)
			| (ZZ_RCD(CPS.RK_RAW64BLOCK, vs)) =>
			    LI_F64BLOCK(map unREAL vs, v, lit)
			| (ZZ_RCD(CPS.RK_RAWBLOCK, vs)) =>
			    LI_RAWBLOCK(map unINT32 vs, v, lit)
			| (ZZ_RCD(rk, vs)) =>
			    LI_BLOCK(rk2bk rk, map val2lit vs, v, lit)
		    end

		(** build up the literal structure *)
		val lit = foldl mklit toplit allvars

		val n = length exports
		val hdr =
		  if n = 0 then Fn.id
		  else let val rv = mkv()
			   val rval = VAR rv
			   val rhdr =
			     fn ce => SELECT(offset, root, rv, PTRt(RPT n), ce)

			   fun mkhdr (v, (i, hh)) =
			     let val nh =
				   (case LV.Tbl.lookup m v
				     of (ZZ_STR s) => bug "ZZ_STR in mkhdr"
					  (* (fn ce =>
						SELECT(i, rval, v, CPSUtil.BOGt, ce)) *)
				      | (ZZ_RCD (rk, vs)) =>
					  let val n = length vs
					      val t =
						case rk
						 of RK_RAW64BLOCK => PTRt(FPT n)
						  | RK_VECTOR => CPSUtil.BOGt
						  | _ => PTRt(RPT n)
					   in fn ce => SELECT(i, rval, v, t, ce)
					  end)
			      in (i+1, hh o nh)
			     end
			in #2 (foldr mkhdr (0, rhdr) exports)
		       end
	     in (lit, hdr)
	    end (* function getInfo *)

	  fun lpfn (fk, f, vl, cl, e) = (fk, f, vl, cl, loop e)

	  and loop ce = (case ce
	       of RECORD (rk, ul, v, e) => record (rk, ul, v) (loop e)
		| SELECT (i, u, v, t, e) =>
		    let val (nu, hh) = lpsv u
		     in hh(SELECT(i, nu, v, t, loop e))
		    end
		| OFFSET _ => bug "unexpected OFFSET in loop"
		| APP (u, ul) =>
		    let val (nu, h1) = lpsv u
			val (nl, h2) = lpvs ul
		     in h1(h2(APP(nu, nl)))
		    end
		| FIX (fns, e) => FIX(map lpfn fns, loop e)
		| SWITCH (u, v, es) =>
		    let val (nu, hh) = lpsv u
		     in hh(SWITCH(nu, v, map loop es))
		    end
		| BRANCH (p, ul, v, e1, e2) =>
		    let val (nl, hh) = lpvs ul
		     in hh(BRANCH(p, nl, v, loop e1, loop e2))
		    end
		| SETTER (p, ul, e) =>
		    let val (nl, hh) = lpvs ul
		     in hh(SETTER(p, nl, loop e))
		    end
		| LOOKER (p, ul, v, t, e) =>
		    let val (nl, hh) = lpvs ul
		     in hh(LOOKER(p, nl, v, t, loop e))
		    end
		| ARITH (p, ul, v, t, e) =>
		    let val (nl, hh) = lpvs ul
		     in hh(ARITH(p, nl, v, t, loop e))
		    end
(* QUESTION: should there be a case for `P.WRAP(P.INT _)` here? *)
		| PURE (P.WRAP(P.FLOAT sz), [u], v, t, e) => wrapfloat (sz, u, v, t) (loop e)
		| PURE (p, ul, v, t, e) =>
		    let val (nl, hh) = lpvs ul
		     in hh(PURE(p, nl, v, t, loop e))
		    end
		| RCC (k, l, p, ul, vtl, e) =>
		    let val (nl, hh) = lpvs ul
		     in hh(RCC(k, l, p, nl, vtl, loop e))
		    end
	      (* end case *))

	  val newbody = loop body
	  val (lit, hdr) = getInfo ()
       in (hdr newbody, lit)
      end

 (* the main function *)
    fun split (fk, f, vl as [_,x], [CNTt, t as PTRt(RPT n)], body) = let
	  val nt = PTRt(RPT (n+1))
	  val (nbody, lit) = liftlits(body, VAR x, n)
	  val nfn = (fk, f, vl, [CNTt, nt], nbody)
	  in
	    if !debugFlg
	      then (
		say (concat["\n[After Literals.split ...]\n"]);
		PPCps.printcps0 nfn;
	        say "==========\n";
		printLits lit;
		say "\n")
	      else ();
	    (nfn, litToBytes lit)
	  end
      | split _ = bug "unexpected CPS header in split"

  end (* Literals *)

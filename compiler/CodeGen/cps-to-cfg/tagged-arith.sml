(* tagged-arith.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Support for tagged arithmetic, which we lower to machine arithmetic
 * as part of the translation to C.  A tagged integer "n" is represented
 * as "2*n + 1" (i.e., its low bit is always 1).
 *)

structure TaggedArith : sig

  (* convert pure tagged arithmetic to CFG expressions *)
    val pure : (CPS.value -> CFG.exp)
	  -> CPS.P.pureop * bool * int * CPS.value list -> CFG.exp

  (* convert tagged trapping arithmetic to CFG *)
    val trapping : (CPS.value -> CFG.exp)
	  -> CPS.P.arithop * CPS.value list * LambdaVar.lvar * (CFG.exp -> CFG.stm)
	  -> CFG.stm

  end = struct

    structure P = CFG_Prim
    structure C = CFG

    datatype pureop = datatype CPS.P.pureop
    datatype arithop = datatype CPS.P.arithop
    datatype value = datatype CPS.value

    fun error msg = ErrorMsg.impossible(String.concat("TaggedArith" :: msg))

    val ity = Target.mlValueSz
    val ity' = C.NUMt{sz = ity}

    fun pureOp (rator, sz, args) =
	  C.PURE{oper=P.PURE_ARITH{oper=rator, sz=sz}, args=args}

    fun var x = C.VAR{name=x}

  (* CFG integer constants *)
    fun num iv = C.NUM{iv = iv, sz = ity}
    fun w2Num iv = num(Word.toLargeInt iv)
    val zero = num 0
    val one = num 1
    val two = num 2
    val allOnes = num(ConstArith.bNot(ity, 0))		(* machine-word all 1s *)

  (* tagging/untagging operations *)
    fun orTag e = pureOp (P.ORB, ity, [e, one])
    fun addTag e = pureOp (P.ADD, ity, [e, one])
    fun stripTag e = pureOp (P.SUB, ity, [e, one])
    fun untagInt e = pureOp (P.RSHIFT, ity, [e, one])
    fun untagUInt e = pureOp (P.RSHIFTL, ity, [e, one])
    fun untag (false, e) = untagUInt e
      | untag (true, e) = untagInt e
    fun tag e = orTag (pureOp (P.LSHIFT, ity, [e, one]))

  (* pure tagged arithmetic; a number "n" is represented as "2*n+1" *)
    fun pure comp (rator, signed, sz, args) = (case (rator, args)
	   of (ADD, [NUM{ival, ...}, v2]) =>
		pureOp (P.ADD, ity, [num (ival+ival), comp v2])
	    | (ADD, [v1, NUM{ival, ...}]) =>
		pureOp (P.ADD, ity, [comp v1, num (ival+ival)])
	    | (ADD, [v1, v2]) =>
		pureOp (P.ADD, ity, [comp v1, stripTag (comp v2)])
	    | (SUB, [NUM{ival, ...}, v2]) =>
(* QUESTION: if ival is the maximum tagged word value, then ival+ival+2 is 0w0 in
 * the native integer size.  Does this cause problems?
 *)
		pureOp (P.SUB, ity, [num (ival+ival+2), comp v2])
	    | (SUB, [v1, NUM{ival, ...}]) =>
		pureOp (P.SUB, ity, [comp v1, num (ival+ival)])
	    | (SUB, [v1, v2]) =>
		addTag(pureOp (P.SUB, ity, [comp v1, comp v2]))
	    | (MUL, [v1, v2]) => let
		val (v1, v2) = (case (v1, v2)
		       of (NUM{ival=n, ...}, NUM{ival=m, ...}) => (num(n+n), num m)
			| (NUM{ival, ...}, _) => (num(ival+ival), untag (signed, comp v2))
			| (_, NUM{ival, ...}) => (untag (signed, comp v1), num(ival+ival))
			| _ => (stripTag (comp v1), untag (signed, comp v2))
		      (* end case *))
		val oper = if signed then P.SMUL else P.UMUL
		in
		  addTag (pureOp (oper, ity, [v1, v2]))
		end
	    | (QUOT, [v1, v2]) => let
		val e1 = (case v1
		       of NUM{ival, ...} => num ival
			| _ => untag (signed, comp v1)
		      (* end case *))
		val e2 = (case v2
		       of NUM{ival, ...} => num ival
			| _ => untag (signed, comp v2)
		      (* end case *))
		in
		  tag (pureOp (P.UDIV, ity, [e1, e2]))
		end
	    | (REM, [v1, v2]) => let
		val e1 = (case v1
		       of NUM{ival, ...} => num ival
			| _ => untag (signed, comp v1)
		      (* end case *))
		val e2 = (case v2
		       of NUM{ival, ...} => num ival
			| _ => untag (signed, comp v2)
		      (* end case *))
		in
		  tag (pureOp (P.UREM, ity, [e1, e2]))
		end
	    | (NEG, [v]) => pureOp (P.SUB, ity, [two, comp v])
	    | (LSHIFT, [v1, NUM{ival, ...}]) =>
		addTag (pureOp (P.LSHIFT, ity, [stripTag(comp v1), num ival]))
	    | (LSHIFT, [NUM{ival, ...}, v2]) =>
		addTag (pureOp (P.LSHIFT, ity, [num(ival+ival), untagUInt(comp v2)]))
	    | (LSHIFT, [v1, v2]) =>
		addTag (pureOp (P.LSHIFT, ity, [stripTag(comp v1), untagUInt(comp v2)]))
	    | (RSHIFT, [v1, NUM{ival, ...}]) =>
		orTag (pureOp (P.RSHIFT, ity, [comp v1, num ival]))
	    | (RSHIFT, [v1, v2]) =>
		orTag (pureOp (P.RSHIFT, ity, [comp v1, untagUInt(comp v2)]))
	    | (RSHIFTL, [v1, NUM{ival, ...}]) =>
		orTag (pureOp (P.RSHIFTL, ity, [comp v1, num ival]))
	    | (RSHIFTL, [v1, v2]) =>
		orTag (pureOp (P.RSHIFTL, ity, [comp v1, untagUInt(comp v2)]))
	    | (ORB, [v1, v2]) => pureOp (P.ORB, ity, [comp v1, comp v2])
	    | (XORB, [NUM{ival, ...}, v2]) =>
		pureOp (P.XORB, ity, [num (ival+ival), comp v2])
	    | (XORB, [v1, NUM{ival, ...}]) =>
		pureOp (P.XORB, ity, [comp v1, num (ival+ival)])
	    | (XORB, [v1, v2]) =>
		addTag (pureOp (P.XORB, ity, [comp v1, comp v2]))
	    | (ANDB, [v1, v2]) => pureOp (P.ANDB, ity, [comp v1, comp v2])
	    | (NOTB, [v]) => let
	      (* xor with mask that is all ones for sz bits, but 0 for the tag *)
		val mask = IntInf.<<(1, Word.fromInt sz + 0w1) - 2
		in
		  pureOp (P.XORB, ity, [comp v, num mask])
		end
	    | (rator, _) => error [".pure: ", PPCps.pureopToString rator]
	  (* end case *))

    fun trapping comp (rator, args, x, k) = let
	  fun arith oper = P.ARITH{oper = oper, sz = ity}
	  fun param (x, ty) = {name = x, ty = ty}
	  fun continue (oper, args) =
		C.ARITH(arith oper, args, param(x, ity'), k(var x))
	  fun tagResult (oper, args) = let
		val tmp = LambdaVar.mkLvar()
		in
		  C.ARITH(arith oper, args, param(tmp, ity'), k(addTag(var tmp)))
		end
	(* The only way a tagged-int div can overflow is when the result
	 * gets retagged, therefore we can use a pure operation for the division.
	 *)
	  fun divOp (oper, a, b) = let
		val tmp1 = LambdaVar.mkLvar()
		val tmp2 = LambdaVar.mkLvar()
		val exp = (case (a, b)
		       of (NUM{ival=m, ...}, NUM{ival=n, ...}) =>
			    pureOp(oper, ity, [num m, num n])
			| (NUM{ival, ...}, b) =>
			    pureOp(oper, ity, [num ival, untagInt(comp b)])
			| (a, NUM{ival, ...}) =>
			    pureOp(oper, ity, [untagInt(comp a), num ival])
			| (a, b) =>
			    pureOp(oper, ity, [untagInt(comp a), untagInt(comp b)])
		      (* end case *))
		in
		  C.LET(exp, param(tmp1, ity'),
		  C.ARITH(arith P.IADD, [var tmp1, var tmp1], param(tmp2, ity'),
		    k(addTag(var tmp2))))
		end
	  in
	    case (rator, args)
	     of (IADD, [NUM{ival, ...}, b]) => continue (P.IADD, [num(ival+ival), comp b])
	      | (IADD, [a, NUM{ival, ...}]) => continue (P.IADD, [comp a, num(ival+ival)])
	      | (IADD, [a, b]) => continue (P.IADD, [comp a, stripTag(comp b)])
	      | (ISUB, [NUM{ival, ...}, b]) => continue (P.ISUB, [num(ival+ival+2), comp b])
	      | (ISUB, [a, NUM{ival, ...}]) => continue (P.ISUB, [comp a, num(ival+ival)])
	      | (ISUB, [a, b]) => tagResult (P.ISUB, [comp a, comp b])
	      | (IMUL, [NUM{ival=m, ...}, NUM{ival=n, ...}]) =>
		  tagResult (P.IMUL, [num(m+m), num n])
	      | (IMUL, [NUM{ival, ...}, b]) =>
		  tagResult (P.IMUL, [num(ival+ival), untagInt(comp b)])
	      | (IMUL, [a, NUM{ival, ...}]) =>
		  tagResult (P.IMUL, [untagInt(comp a), num(ival+ival)])
	      | (IMUL, [a, b]) =>
		  tagResult (P.IMUL, [stripTag(comp a), untagInt(comp b)])
	      | (IQUOT, [a, b]) => divOp (P.SDIV, a, b)
	      | (IREM, [a, b]) => divOp (P.SREM, a, b)
	      | (INEG, [a]) => continue (P.ISUB, [two, comp a])
	      | _ => error ["trapping: ", PPCps.arithopToString rator]
	    (* end case *)
	  end

  end

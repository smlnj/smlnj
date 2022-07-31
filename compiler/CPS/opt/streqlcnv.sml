(* streqlcnv.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Code to lower the `STREQL` branch to a sequence of explicit
 * word-sized equality tests.
 *)

structure StrEqlCnv : sig

    val strEql : CPS.value * string * CPS.cexp * CPS.cexp -> CPS.cexp

  end = struct

    structure C = CPS
    structure LV = LambdaVar
    structure CVS = CharVectorSlice

    fun bug s = ErrorMsg.impossible ("StrEqlCnv: " ^ s)

    val wordSzInBytes = if Target.is64 then 8 else 4
    val bytesToWords = if Target.is64
	  then (fn n => Word.toIntX(Word.>>(Word.fromInt n + 0w7, 0w3)))
	  else (fn n => Word.toIntX(Word.>>(Word.fromInt n + 0w7, 0w3)))

    val wordTy = C.NUMt{sz = Target.mlValueSz, tag = false}
    val neqTest = C.P.CMP{oper = C.P.NEQ, kind = C.P.UINT Target.mlValueSz}
    fun num n = C.NUM{ival = n, ty = {sz = Target.mlValueSz, tag = false}}

  (* convert a word sized substring to an unsigned integer; we take into account
   * the endianess of the target machine.
   *)
    val cvsToWord = let
	  fun getByte (c, n) = IntInf.<<(n, 0w8) + IntInf.fromInt(Char.ord c)
	  in
	    if Target.bigEndian
	      then CVS.foldl getByte 0
	      else CVS.foldr getByte 0
	  end

  (* `getWord s i` returns the ith word extracted from the string s *)
    fun getWord s i = cvsToWord (CVS.slice(s, wordSzInBytes * i, SOME wordSzInBytes))

    fun strEql (s, lit, e1, e2) = let
	(* number of words to compare *)
	  val nw = bytesToWords (size lit)
	(* zero pad the literal to a multiple of word-size characters *)
	  val lit = StringCvt.padRight #"\000" (wordSzInBytes * nw) lit
	(* variable for string data pointer *)
	  val dp = LV.mkLvar()
	(* wrap expression in binding of `dp` *)
	  fun withStrData exp =
		C.PURE(C.P.GETSEQDATA, [s], dp, C.PTRt C.VPT, exp)
	(* test the i'th word against `w` *)
	  fun mkTest falseExp (i, w, k) = let
		val tmp = LV.mkLvar()
		in
		  C.SELECT(i, C.VAR dp, tmp, wordTy,
		    C.BRANCH(neqTest, [C.VAR tmp, num w], LV.mkLvar(), falseExp, k))
		end
	  in
	  (* partition the literal into word-sized chunks *)
	    if (nw > 1)
	      then let
		val words = List.tabulate (nw, getWord lit)
	      (* since we have multiple jumps to the false code, we wrap it in a continuation *)
		val kFalse = LV.mkLvar()
		val falseCont = (C.CONT, kFalse, [], [], e2)
		in
		  C.FIX([falseCont],
		    withStrData (List.foldri (mkTest (C.APP(C.VAR kFalse, []))) e1 words))
		end
	    else if (nw = 1)
	      then (* simple test of a single word *)
		withStrData (mkTest e2 (0, getWord lit 0, e1))
	      else bug "unexpected STREQL test of empty string"
	  end

  end

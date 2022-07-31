(* scan.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * C-style conversions from string representations.
 *
 * TODO: replace the fmt_item type with a datatype that reflects the more
 * limited set of possible results.  Also replace the shared implementation
 * of format-string processing with processing that is specific to scanning.
 *)

structure Scan : SCAN =
  struct

    structure SS = Substring
    structure SC = StringCvt

    open FmtFields

  (* character sets *)
    abstype charset = CS of Word8Array.array
    with
      fun mkCharSet () = CS(Word8Array.array(Char.maxOrd+1, 0w0))
      fun addChar (CS ba, c) = Word8Array.update(ba, Char.ord c, 0w1)
      fun addRange (CS ba, c1, c2) = let
	    val ord_c2 = Char.ord c2
	    fun add i = if (i <= ord_c2)
		  then (Word8Array.update(ba, i, 0w1); add(i+1))
		  else ()
	    in
	      if (c1 <= c2) then (add(Char.ord c1)) else raise BadFormat
	    end
      fun inSet (CS ba) arg = (Word8Array.sub(ba, Char.ord arg) = 0w1)
      fun notInSet (CS ba) arg = (Word8Array.sub(ba, Char.ord arg) = 0w0)
    end

    fun scanCharSet fmtStr = let
	  val cset = mkCharSet()
	  val (isNot, fmtStr) = (case SS.getc fmtStr
		 of (SOME(#"^", ss)) => (true, ss)
		  | _ => (false, fmtStr)
		(* end case *))
	  fun scan (nextChar, ss) = (case (SS.getc ss)
		 of (SOME(#"-", ss)) => (case (SS.getc ss)
		       of (SOME(#"]", ss)) => (
			    addChar(cset, nextChar);
			    addChar(cset, #"-");
			    ss)
			| (SOME(c, ss)) => (
			    addRange(cset, nextChar, c);
			    scanNext ss)
			| NONE => raise BadFormat
		      (* end case *))
		  | (SOME(#"]", ss)) => (addChar(cset, nextChar); ss)
		  | (SOME(c, ss)) => (addChar(cset, nextChar); scan(c, ss))
		  | NONE => raise BadFormat
		(* end case *))
	  and scanNext ss = (case (SS.getc ss)
		 of (SOME(#"-", ss)) => raise BadFormat
		  | (SOME(#"]", ss)) => ss
		  | (SOME(c, ss)) => scan(c, ss)
		  | NONE => raise BadFormat
		(* end case *))
	  and scanChar (SOME arg) = scan arg
	    | scanChar NONE = raise BadFormat
	  val fmtStr = scanChar (SS.getc fmtStr)
	  in
	    if isNot
	      then (CharSet(notInSet cset), fmtStr)
	      else (CharSet(inSet cset), fmtStr)
	  end

    fun compileScanFormat str = let
	  val split = SS.splitl (Char.notContains "\n\t %[")
	  fun scan (ss, l) =
		if (SS.isEmpty ss)
		  then rev l
		  else let val (ss1, ss2) = split ss
		    in
		      case (SS.getc ss2)
		       of (SOME(#"%", ss')) => let val (field, ss3) = scanField ss'
			    in
			      scan(ss3, field :: (Raw ss1) :: l)
			    end
			| (SOME(#"[", ss')) => let val (cs, ss3) = scanCharSet ss'
			    in
			      scan (ss3, cs :: (Raw ss1) :: l)
			    end
			| (SOME(_, ss')) =>
			    scan (SS.dropl Char.isSpace ss', (Raw ss1) :: l)
			| NONE => rev((Raw ss1)::l)
		      (* end case *)
		    end
	  in
	    scan (SS.full str, [])
	  end

(** NOTE: for the time being, this function ignores flags and field width **)
    fun scanf fmt getc strm = let
	  val fmts = compileScanFormat fmt
	  val skipWS = SC.dropl Char.isSpace getc
	  fun scan (strm, [], items) = SOME(rev items, strm)
	    | scan (strm, (Raw ss)::rf, items) = let
		fun match (strm, ss) = (case (getc strm, SS.getc ss)
		       of (SOME(c', strm'), SOME(c, ss)) =>
			    if (c' = c) then match (strm', ss) else NONE
			| (_, NONE) => scan (strm, rf, items)
			| _ => NONE
		      (* end case *))
		in
		  match (skipWS strm, ss)
		end
	    | scan (strm, (CharSet pred)::rf, items) = let
		fun scanSet strm = (case (getc strm)
		       of (SOME(c, strm')) =>
			    if (pred c) then scanSet strm' else strm
			| NONE => strm
		    (* end case *))
		in
		  scan (scanSet strm, rf, items)
		end
	    | scan (strm, Field(flags, wid, ty)::rf, items) = let
		val strm = skipWS strm
		fun next (con, SOME(x, strm')) = scan (strm', rf, con(x)::items)
		  | next _ = NONE
		fun getInt fmt = if (#large flags)
		      then next(LINT, LargeInt.scan fmt getc strm)
		      else next(INT, Int.scan fmt getc strm)
		in
		  case ty
		   of OctalField => getInt SC.OCT
		    | IntField => getInt SC.DEC
		    | HexField => getInt SC.HEX
		    | CapHexField => getInt SC.HEX
		    | CharField => next(CHR, getc strm)
		    | BoolField => next(BOOL, Bool.scan getc strm)
(* QUESTION: should we use the precision? *)
		    | StrField prec => let
			val notSpace = not o Char.isSpace
			val pred = (case wid
			       of NoPad => notSpace
				| (Wid n) => let val cnt = ref n
				    in
				      fn c => (case !cnt
					 of 0 => false
					  | n => (cnt := n-1; notSpace c)
					(* end case *))
				    end
			      (* end case *))
			val (s, strm) = SC.splitl pred getc strm
			in
			  scan (strm, rf, STR s :: items)
			end
		    | (RealField _) => next(REAL, LargeReal.scan getc strm)
		  (* end case *)
		end
	  in
	    scan(strm, fmts, [])
	  end (* scanf *)

    fun sscanf fmt = SC.scanString (scanf fmt)

  end (* Scan *)

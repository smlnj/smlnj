(* char.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Char : sig

    include CHAR

  (* internal scanning function for C-style escape sequences *)
    val scanC : (char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader

  end = struct

    structure C = InlineT.Char

    val op + = InlineT.Int.+
    val op - = InlineT.Int.-
    val op * = InlineT.Int.*

    val itoc : int -> char = InlineT.cast (* using cast here avoids the bounds check *)
    val ctoi : char -> int = C.ord

    type char = char
    type string = string

    val minChar : char	= C.chr 0
    val maxChar	: char	= C.chr C.maxOrd
    val maxOrd		= C.maxOrd

    fun pred (c : char) : char = let
	  val c' = (ctoi c - 1)
	  in
	    if InlineT.Int.< (c', 0) then raise General.Chr else (itoc c')
	  end
    fun succ (c : char) : char = let
	  val c' = (ctoi c + 1)
	  in
	    if InlineT.Int.< (maxOrd, c') then raise General.Chr else (itoc c')
	  end

    val chr = C.chr
    val ord = C.ord

    val (op <)  = C.<
    val (op <=) = C.<=
    val (op >)  = C.>
    val (op >=) = C.>=

    fun compare (c1 : char, c2 : char) =
	  if (c1 = c2) then EQUAL
	  else if (c1 < c2) then LESS
	  else GREATER

  (* testing character membership *)
    local
      fun mkArray (s, sLen) = let
	    val cv = Assembly.A.create_s(maxOrd+1)
	    fun init i = if InlineT.Int.<= (i, maxOrd)
		  then (InlineT.CharVector.update(cv, i, #"\000"); init(i+1))
		  else ()
	    fun ins i = if InlineT.Int.< (i, sLen)
		  then (
		    InlineT.CharVector.update (
		      cv, ord(InlineT.CharVector.sub(s, i)), #"\001");
		    ins(i+1))
		  else ()
	    in
	      init 0; ins 0; cv
	    end
    in
    fun contains "" = (fn c => false)
      | contains s = let val sLen = InlineT.CharVector.length s
	  in
	    if (sLen = 1)
	      then let val c' = InlineT.CharVector.sub(s, 0)
		in fn c => (c = c') end
	      else let val cv = mkArray (s, sLen)
		in fn c => (InlineT.CharVector.sub(cv, ord c) <> #"\000") end
	  end
    fun notContains "" = (fn c => true)
      | notContains s = let val sLen = InlineT.CharVector.length s
	  in
	    if (sLen = 1)
	      then let val c' = InlineT.CharVector.sub(s, 0)
		in fn c => (c <> c') end
	      else let val cv = mkArray (s, sLen)
		in fn c => (InlineT.CharVector.sub(cv, ord c) = #"\000") end
	  end
    end (* local *)

  (* For each character code we have an 8-bit vector, which is interpreted
   * as follows:
   *   0x01  ==  set for upper-case letters
   *   0x02  ==  set for lower-case letters
   *   0x04  ==  set for digits
   *   0x08  ==  set for white space characters
   *   0x10  ==  set for punctuation characters
   *   0x20  ==  set for control characters
   *   0x40  ==  set for hexadecimal characters
   *   0x80  ==  set for SPACE
   *)
    val ctypeTbl = "\
	    \\032\032\032\032\032\032\032\032\032\040\040\040\040\040\032\032\
	    \\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\
	    \\136\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
	    \\068\068\068\068\068\068\068\068\068\068\016\016\016\016\016\016\
	    \\016\065\065\065\065\065\065\001\001\001\001\001\001\001\001\001\
	    \\001\001\001\001\001\001\001\001\001\001\001\016\016\016\016\016\
	    \\016\066\066\066\066\066\066\002\002\002\002\002\002\002\002\002\
	    \\002\002\002\002\002\002\002\002\002\002\002\016\016\016\016\032\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	    \\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
	  \"
    fun inSet (c, s) = let
	  val m = ord(InlineT.CharVector.sub(ctypeTbl, ord c))
	  in
	    (InlineT.Int.andb(m, s) <> 0)
	  end

  (* predicates on integer coding of Ascii values *)
    fun isAlpha c	= inSet(c, 0x03)
    fun isUpper c	= inSet(c, 0x01)
    fun isLower c	= inSet(c, 0x02)
    fun isDigit c	= inSet(c, 0x04)
    fun isHexDigit c	= inSet(c, 0x40)
    fun isAlphaNum c	= inSet(c, 0x07)
    fun isSpace c	= inSet(c, 0x08)
    fun isPunct c	= inSet(c, 0x10)
    fun isGraph c	= inSet(c, 0x17)
    fun isPrint c	= inSet(c, 0x97)
    fun isCntrl c	= inSet(c, 0x20)
    fun isAscii c    	= InlineT.Int.< (ord c, 128)

    val offset = ctoi #"a" - ctoi #"A"
    fun toUpper c = if (isLower c) then itoc(ctoi c - offset) else c
    fun toLower c = if (isUpper c) then itoc(ctoi c + offset) else c

    fun scanDigits isDigit getc n strm = let
	  fun scan (strm, 0, l) = (List.rev l, strm)
	    | scan (strm, i, l) = (case getc strm
		 of NONE => (List.rev l, strm)
		  | SOME(c, strm') => if isDigit c
		      then scan (strm', i-1, c::l)
		      else (List.rev l, strm)
		(* end case *))
	  in
	    scan (strm, n, [])
	  end

    fun chkDigits radix (l, strm) = let
	  fun next (x::r) = SOME(x, r)
	    | next [] = NONE
	  in
	    case (NumScan32.scanInt radix next l)
	     of NONE => NONE
	      | SOME(i, _) => if InlineT.Int32.<(i, 256)
		  then SOME(chr(InlineT.Int32.toInt i), strm)
		  else NONE
	    (* end case *)
	  end

  (* conversions between characters and printable representations *)
    fun scan getc = let
	  fun scan' rep = let
		fun get2 rep = (case (getc rep)
		       of (SOME(c1, rep')) => (case (getc rep')
			     of (SOME(c2, rep'')) => SOME(c1, c2, rep'')
			      | _ => NONE
			    (* end case *))
			| _ => NONE
		      (* end case *))
		in
		  case (getc rep)
		   of NONE => NONE
	 	   | (SOME(#"\\", rep')) => (case (getc rep')
			 of NONE => NONE
			  | (SOME(#"\\", rep'')) => (SOME(#"\\", rep''))
			  | (SOME(#"\"", rep'')) => (SOME(#"\"", rep''))
			  | (SOME(#"a", rep'')) => (SOME(#"\a", rep''))
			  | (SOME(#"b", rep'')) => (SOME(#"\b", rep''))
			  | (SOME(#"t", rep'')) => (SOME(#"\t", rep''))
			  | (SOME(#"n", rep'')) => (SOME(#"\n", rep''))
			  | (SOME(#"v", rep'')) => (SOME(#"\v", rep''))
			  | (SOME(#"f", rep'')) => (SOME(#"\f", rep''))
			  | (SOME(#"r", rep'')) => (SOME(#"\r", rep''))
			  | (SOME(#"u", rep'')) => (case (scanDigits isHexDigit getc 4 rep'')
			       of res as ([_, _, _, _], _) => chkDigits StringCvt.HEX res
				| _ => NONE
			      (* end case *))
			  | (SOME(#"^", rep'')) => (case (getc rep'')
			       of NONE => NONE
	    			| (SOME(c, rep''')) =>
				    if ((#"@" <= c) andalso (c <= #"_"))
				      then SOME(chr(ord c - ord #"@"), rep''')
				      else NONE
			      (* end case *))
			  | (SOME(d1, rep'')) =>
			       if (isDigit d1)
				then (case (get2 rep'')
				   of SOME(d2, d3, rep''') => let
					fun cvt d = (ord d - ord #"0")
					in
					  if (isDigit d2 andalso isDigit d3)
				 	   then let
				 	     val n = 100*(cvt d1) + 10*(cvt d2) + (cvt d3)
				 	     in
						if InlineT.Int.<(n, 256)
						  then SOME(chr n, rep''')
						  else NONE
					      end
					    else NONE
					end
				    | NONE => NONE
				  (* end case *))
			      else if (isSpace d1)
				then let (* skip over \<ws>+\ *)
				  fun skipWS strm = (case (getc strm)
					 of NONE => NONE
					  | (SOME(#"\\", strm')) => scan' strm'
					  | (SOME(c, strm')) => if (isSpace c)
					      then skipWS strm'
					      else NONE
					(* end case *))
				  in
				    skipWS rep''
				  end
				else NONE
			(* end case *))
		    | (SOME(#"\"", rep')) => NONE	(* " *)
		    | (SOME(c, rep')) =>
			if (isPrint c) then (SOME(c, rep')) else NONE
		  (* end case *)
		end
	  in
	    scan'
	  end

    val fromString = StringCvt.scanString scan

    val itoa = (NumFormat32.fmtInt StringCvt.DEC) o InlineT.Int32.fromInt

    fun toString #"\a" = "\\a"
      | toString #"\b" = "\\b"
      | toString #"\t" = "\\t"
      | toString #"\n" = "\\n"
      | toString #"\v" = "\\v"
      | toString #"\f" = "\\f"
      | toString #"\r" = "\\r"
      | toString #"\"" = "\\\""
      | toString #"\\" = "\\\\"
      | toString c =
	  if (isPrint c)
	    then InlineT.PolyVector.sub (PreString.chars, ord c)
(** NOTE: we should probably recognize the control characters **)
	    else let
	      val c' = ord c
	      in
		if InlineT.Int.>(c', 32)
		  then PreString.concat2("\\", itoa c')
		  else PreString.concat2("\\^",
		    InlineT.PolyVector.sub (PreString.chars, c'+64))
	      end

  (* scanning function for C escape sequences *)
    fun scanC getc = let
	  fun isOctDigit d = (#"0" <= d) andalso (d <= #"7")
	  fun scan strm = (case getc strm
		 of NONE => NONE
		  | SOME(#"\\", strm') => (case getc strm'
		       of NONE => NONE
			| (SOME(#"a", strm'')) => SOME(#"\a", strm'')
			| (SOME(#"b", strm'')) => SOME(#"\b", strm'')
			| (SOME(#"t", strm'')) => SOME(#"\t", strm'')
			| (SOME(#"n", strm'')) => SOME(#"\n", strm'')
			| (SOME(#"v", strm'')) => SOME(#"\v", strm'')
			| (SOME(#"f", strm'')) => SOME(#"\f", strm'')
			| (SOME(#"r", strm'')) => SOME(#"\r", strm'')
			| (SOME(#"\\", strm'')) => SOME(#"\\", strm'')
			| (SOME(#"\"", strm'')) => SOME(#"\"", strm'')
			| (SOME(#"'", strm'')) => SOME(#"'", strm'')
			| (SOME(#"?", strm'')) => SOME(#"?", strm'')
			| (SOME(#"x", strm'')) => (* hex escape code *)
			    chkDigits StringCvt.HEX
			      (scanDigits isHexDigit getc ~1 strm'')
			| _ => (* should be octal escape code *)
			    chkDigits StringCvt.OCT
			      (scanDigits isOctDigit getc 3 strm')
		      (* end case *))
(** NOT SURE ABOUT THE FOLLOWING TWO CASES:
		  | (SOME(#"\"", strm'')) => NONE (* error --- not escaped *)
		  | (SOME(#"\'", strm'')) => NONE (* error --- not escaped *)
**)
		  | (SOME(c, strm'')) =>
		      if (isPrint c) then SOME(c, strm'') else NONE
		(* end case *))
	  in
	    scan
	  end

    val fromCString = StringCvt.scanString scanC

    fun toCString #"\a" = "\\a"
      | toCString #"\b" = "\\b"
      | toCString #"\t" = "\\t"
      | toCString #"\n" = "\\n"
      | toCString #"\v" = "\\v"
      | toCString #"\f" = "\\f"
      | toCString #"\r" = "\\r"
      | toCString #"\"" = "\\\""
      | toCString #"\\" = "\\\\"
      | toCString #"?" = "\\?"
      | toCString #"'" = "\\'"
      | toCString c = if (isPrint c)
	  then InlineT.PolyVector.sub (PreString.chars, ord c)
	  else let
	    val i = InlineT.Int32.fromInt(ord c)
	    val prefix = if InlineT.Int32.<(i, 8)
		    then "\\00"
		  else if InlineT.Int32.<(i, 64)
		    then "\\0"
		    else "\\"
	    in
	      PreString.concat2(prefix, NumFormat32.fmtInt StringCvt.OCT i)
	    end

  end (* Char *)

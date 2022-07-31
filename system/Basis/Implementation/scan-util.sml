(* scan-util.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Utility code for scanning modules (ScanNum32 and ScanNum64).
 *)

structure ScanUtil : sig

    type prefix_pat = {
	wOkay : bool,           (* true if 0[wW] prefix is okay; if this is
                                 * true, then signs (+, -, ~) are not okay.
                                 *)
	xOkay : bool,           (* true if 0[xX] prefix is okay *)
	maxDigit : word         (* maximum digit (i.e., 7, 9, or 15) *)
      }

  (* scans prefix for a number:
   *
   *   binPat(true)  {wOkay=true, xOkay=false, ptOkay=false, isBinDigit} =>
   *       (0[wW])?b (b binary digit)
   *   binPat(false) {wOkay=true, xOkay=false, ptOkay=false, isBinDigit} =>
   *       [-~+]?b
   *   octPat(true)  {wOkay=true, xOkay=false, ptOkay=false, isOctDigit} =>
   *       (0[wW])?o (o octal digit)
   *   octPat(false) {wOkay=false, xOkay=false, ptOkay=false, isOctDigit} =>
   *       [-~+]?o
   *   hexPat(true)  {wOkay=true, xOkay=true, ptOkay=false, isHexDigit} =>
   *       (0[wW][xX])?h (h hex digit)
   *   hexPat(false) {wOkay=false, xOkay=true, ptOkay=false, isHexDigit} =>
   *       [-~+]?(0[xX])?h
   *   decPat(true)  {wOkay=true, xOkay=false, ptOkay=false, isDecDigit} =>
   *       (0[wW][xX])?d (d decimal digit)
   *   decPat(false) {wOkay=false, xOkay=false, ptOkay=false, isDecDigit} =>
   *       [-~+]?d
   *
   * Sign characters, initial 0x, 0w, etc are consumed.  The initial
   * digit is returned as the value of next.
   *)
    val scanPrefix : prefix_pat
          -> ('a -> (char * 'a) option)
            -> 'a
              -> {neg: bool, next: word, rest: 'a} option

  (* standard prefix patterns *)
    val binPat : bool -> prefix_pat
    val octPat : bool -> prefix_pat
    val decPat : bool -> prefix_pat
    val hexPat : bool -> prefix_pat

  (* map character to its hex value (e.g., #"3" ==> 0w3, #"e" ==> 0w14, etc). *)
    val code : char -> word

  end = struct

  (* A table for mapping digits to values.  Whitespace characters map to
   * 128, "+" maps to 129, "-","~" map to 130, and the characters 0-9,A-Z,a-z
   * map to their base-36 value.  All other characters map to 255.
   *)
    val cvtTable = "\
	  \\255\255\255\255\255\255\255\255\255\128\128\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\128\255\255\255\255\255\255\255\255\255\255\129\255\130\255\255\
	  \\000\001\002\003\004\005\006\007\008\009\255\255\255\255\255\255\
	  \\255\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\
	  \\025\026\027\028\029\030\031\255\033\034\035\255\255\255\255\255\
	  \\255\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\
	  \\025\026\027\028\029\030\031\032\033\034\035\255\255\255\130\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	  \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
	\"

    val ord = InlineT.Char.ord

    fun code (c : char) =
	  InlineT.Word.fromInt(ord(InlineT.CharVector.sub(cvtTable, ord c)))
    val wsCode : word = 0w128		(* code for whitespace *)
    val plusCode : word = 0w129		(* code for #"+" *)
    val minusCode : word = 0w130	(* code for #"-" and #"~" *)
    val eCode : word = 0w14		(* code for #"e" and #"E" *)
    val wCode : word = 0w32		(* code for #"w" *)
    val xCode : word = 0w33		(* code for #"X" and #"X" *)

    type prefix_pat = {
	wOkay : bool,		(* true if 0[wW] prefix is okay; if this is
				 * true, then signs (+, -, ~) are not okay.
				 *)
	xOkay : bool,		(* true if 0[xX] prefix is okay *)
	maxDigit : word         (* maximum digit (i.e., 7, 9, or 15) *)
      }

    fun scanPrefix (p : prefix_pat) getc cs = let
	  fun getNext cs = (case (getc cs)
		 of NONE => NONE
		  | (SOME(c, cs)) => SOME(code c, cs)
		(* end case *))
	  fun skipWS cs = (case (getNext cs)
		 of NONE => NONE
		  | (SOME(c, cs')) =>
		      if (c = wsCode) then skipWS cs' else SOME(c, cs')
		(* end case *))
	  fun getOptSign NONE = NONE
	    | getOptSign (next as SOME(c, cs)) =
		if (#wOkay p)
		  then getOpt0 (false, SOME(c, cs)) (* no sign for words *)
		else if (c = minusCode)
		  then getOpt0 (true, getNext cs)
		else if (c = plusCode)
		  then getOpt0 (false, getNext cs)
		  else getOpt0 (false, next)
	  and getOpt0 (neg, NONE) = NONE
	    | getOpt0 (neg, SOME(c, cs)) = (case (c, #wOkay p, #xOkay p)
		 of (0w0, true, true) => getOptWX (neg, (c, cs), getNext cs)
		  | (0w0, true, false) => getOptW (neg, (c, cs), getNext cs)
		  | (0w0, false, true) => getOptX (neg, (c, cs), getNext cs)
		  | _ => finish (neg, (c, cs))
		(* end case *))
	(* consume an optional "0[wW]?[xX]" prefix having seen "0" *)
	  and getOptWX (neg, savedCS, NONE) = finish (neg, savedCS)
	    | getOptWX (neg, savedCS, arg as SOME(c, cs)) =
		if (c = wCode)
		  then (case getNext cs
		     of SOME(c', cs') => if (c' = xCode)
			  then chkDigit (neg, savedCS, getNext cs')
			  else finish (neg, savedCS) (* saw "0[wW]" but no "[xX]" *)
		      | NONE => finish (neg, savedCS)
		    (* end case *))
		  else getOptX (neg, savedCS, arg)
	(* consume an optional "0[wW]" prefix having seen "0" *)
	  and getOptW (neg, savedCS, NONE) = finish (neg, savedCS)
	    | getOptW (neg, savedCS, arg as SOME(c, cs)) =
		if (c = wCode)
		  then chkDigit (neg, savedCS, getNext cs)
		  else finish (neg, savedCS)
	(* consume an optional "0[xX]" prefix having seen "0" *)
	  and getOptX (neg, savedCS, NONE) = finish (neg, savedCS)
	    | getOptX (neg, savedCS, arg as SOME(c, cs)) =
		if (c = xCode)
		  then chkDigit (neg, savedCS, getNext cs)
		  else chkDigit (neg, savedCS, arg)
	(* check if the character following the prefix is a valid digit; if it
	 * is, then we consume the prefix, otherwise we reset to savedCS.
	 *)
	  and chkDigit (neg, savedCS, NONE) = finish (neg, savedCS)
	    | chkDigit (neg, savedCS, SOME(c, cs)) =
		if (#maxDigit p >= c)
		  then SOME{neg=neg, next = c, rest = cs}
		  else finish (neg, savedCS)
	  and finish (neg, (c, cs)) =
		if (#maxDigit p >= c)
		  then SOME{neg=neg, next = c, rest = cs}
		  else NONE
	  in
	    getOptSign (skipWS cs)
	  end


    fun binPat wOkay = {wOkay=wOkay, xOkay=false, maxDigit=0w1} : prefix_pat
    fun octPat wOkay = {wOkay=wOkay, xOkay=false, maxDigit=0w7} : prefix_pat
    fun decPat wOkay = {wOkay=wOkay, xOkay=false, maxDigit=0w9} : prefix_pat
    fun hexPat wOkay = {wOkay=wOkay, xOkay=true,  maxDigit=0w15} : prefix_pat

  end

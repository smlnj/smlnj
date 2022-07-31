structure HTML4Lexer  = struct

    datatype yystart_state = 
COM2 | COM1 | INTAG | INITIAL
    local

    structure UserDeclarations = 
      struct

 
open HTML4TokenUtils

fun eof() = EOF
type lex_result = token

val buffer = ref ([] : string list)

fun addStr s = (buffer := s :: !buffer)
fun getStr () = (String.concat(List.rev(!buffer)) before (buffer := []))

(* trim an optional ";" from a non-empty substring *)
fun trimSemi ss = if Substring.sub(ss, Substring.size ss - 1) = #";"
      then Substring.trimr 1 ss
      else ss
val scanInt = #1 o valOf o IntInf.scan StringCvt.DEC Substring.getc
val scanHex = #1 o valOf o IntInf.scan StringCvt.HEX Substring.getc

      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
Vector.fromList []
    fun yystreamify' p input = ULexBuffer.mkStream (p, input)

    fun yystreamifyReader' p readFn strm = let
          val s = ref strm
	  fun iter(strm, n, accum) = 
	        if n > 1024 then (String.implode (rev accum), strm)
		else (case readFn strm
		       of NONE => (String.implode (rev accum), strm)
			| SOME(c, strm') => iter (strm', n+1, c::accum))
          fun input() = let
	        val (data, strm) = iter(!s, 0, [])
	        in
	          s := strm;
		  data
	        end
          in
            yystreamify' p input
          end

    fun yystreamifyInstream' p strm = yystreamify' p (fn ()=>TextIO.input strm)

    fun innerLex 
(yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yysetStrm strm = yystrm := strm
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
	  fun yystreamify input = yystreamify' (yygetPos()) input
	  fun yystreamifyReader readFn strm = yystreamifyReader' (yygetPos()) readFn strm
	  fun yystreamifyInstream strm = yystreamifyInstream' (yygetPos()) strm
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case ULexBuffer.getu strm
                of (SOME (0w10, s')) => 
		     (AntlrStreamPos.markNewLine yysm (ULexBuffer.getpos strm);
		      SOME (0w10, s'))
		 | x => x)
          fun yygetList getc strm = let
            val get1 = UTF8.getu getc
            fun iter (strm, accum) = 
	        (case get1 strm
	          of NONE => rev accum
	           | SOME (w, strm') => iter (strm', w::accum)
	         (* end case *))
          in
            iter (strm, [])
          end
	(* create yytext *)
	  fun yymksubstr(strm) = ULexBuffer.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = yygetList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex () = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yygetPos()
	    fun yygetlineNo strm = AntlrStreamPos.lineNo yysm (ULexBuffer.getpos strm)
	    fun yygetcolNo  strm = AntlrStreamPos.colNo  yysm (ULexBuffer.getpos strm)
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    val yylastwasnref = ref (ULexBuffer.lastWasNL (!yystrm))
	    fun continue() = let val yylastwasn = !yylastwasnref in
let
fun yyAction0 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr yytext; YYBEGIN INTAG; continue()
      end
fun yyAction1 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr yytext; YYBEGIN INITIAL; mkOpenTag(getStr())
      end
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr yytext; continue()
      end
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  mkCloseTag yytext
      end
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr yytext; YYBEGIN COM1; continue()
      end
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr yytext; YYBEGIN COM2; continue()
      end
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr yytext; continue()
      end
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr yytext; YYBEGIN COM1; continue()
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr yytext; YYBEGIN INITIAL; COMMENT (getStr())
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr yytext; continue()
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr yytext; continue()
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         raise (Fail ("Unexpected text in comment: \"" ^ yytext ^ "\""))
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  DOCTYPE yytext
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  XML_PROCESSING yytext
      end
fun yyAction14 (strm, lastMatch : yymatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;
         CHAR_REF(scanInt (trimSemi (Substring.slice(yysubstr, 2, NONE))))
      end
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;
         CHAR_REF(scanHex (trimSemi (Substring.slice(yysubstr, 3, NONE))))
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;
         ENTITY_REF(Atom.atom (Substring.string (Substring.slice(yysubstr, 1, NONE))))
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  PCDATA yytext
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  PCDATA yytext
      end
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wx3A
              then if inp = 0wx2F
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < 0wx2F
                  then if inp <= 0wx2C
                      then yyAction0(strm, yyNO_MATCH)
                      else yyQ22(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ22(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx5B
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp <= 0wx40
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ22(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ22(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction0(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ22(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3F
              then yyQ23(strm', lastMatch)
              else yyQ21(strm', lastMatch)
      (* end case *))
and yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3F
              then yyQ23(strm', lastMatch)
            else if inp < 0wx3F
              then if inp = 0wx3E
                  then yyQ24(strm', lastMatch)
                  else yyQ21(strm', lastMatch)
              else yyQ21(strm', lastMatch)
      (* end case *))
and yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3F
              then yyQ23(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyQ21(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx20
              then yyQ26(strm', lastMatch)
            else if inp < 0wx20
              then if inp = 0wx9
                  then yyQ26(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx3E
              then yyQ27(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ25(strm', lastMatch)
            else if inp < 0wx30
              then if inp = 0wx20
                  then yyQ26(strm', lastMatch)
                else if inp < 0wx20
                  then if inp = 0wx9
                      then yyQ26(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = 0wx2D
                  then yyQ25(strm', lastMatch)
                else if inp < 0wx2D
                  then yystuck(lastMatch)
                else if inp = 0wx2F
                  then yystuck(lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp = 0wx41
              then yyQ25(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx3E
                  then yyQ27(strm', lastMatch)
                else if inp < 0wx3E
                  then if inp <= 0wx39
                      then yyQ25(strm', lastMatch)
                      else yystuck(lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ25(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ25(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ25(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yystuck(lastMatch)
            else if inp < 0wx5B
              then if inp <= 0wx40
                  then yystuck(lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp = 0wx61
              then yyQ25(strm', lastMatch)
            else if inp < 0wx61
              then yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ25(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ37(strm', lastMatch)
              else yyQ36(strm', lastMatch)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yystuck(lastMatch)
              else yyQ36(strm', lastMatch)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx45
              then yyQ35(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx50
              then yyQ34(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx59
              then yyQ33(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx54
              then yyQ32(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx43
              then yyQ31(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx4F
              then yyQ30(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ38(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2E
              then yystuck(lastMatch)
            else if inp < 0wx2E
              then if inp = 0wx2D
                  then yyQ28(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx44
              then yyQ29(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3F
              then yyQ21(strm', lastMatch)
            else if inp < 0wx3F
              then if inp = 0wx22
                  then yystuck(lastMatch)
                else if inp < 0wx22
                  then if inp = 0wx21
                      then yyQ19(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = 0wx2F
                  then yyQ20(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx5B
              then yystuck(lastMatch)
            else if inp < 0wx5B
              then if inp = 0wx40
                  then yystuck(lastMatch)
                  else yyQ22(strm', lastMatch)
            else if inp = 0wx61
              then yyQ22(strm', lastMatch)
            else if inp < 0wx61
              then yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ22(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyQ41(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < 0wx3B
              then if inp = 0wx2F
                  then yyAction16(strm, yyNO_MATCH)
                else if inp < 0wx2F
                  then if inp <= 0wx2C
                      then yyAction16(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp = 0wx3A
                  then yyAction16(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp = 0wx5B
              then yyAction16(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp <= 0wx40
                  then yyAction16(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ40(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction16(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ40(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3C
              then yyAction15(strm, yyNO_MATCH)
            else if inp < 0wx3C
              then if inp = 0wx3A
                  then yyAction15(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction15(strm, yyNO_MATCH)
                      else yyQ44(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyQ45(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp = 0wx47
              then yyAction15(strm, yyNO_MATCH)
            else if inp < 0wx47
              then if inp <= 0wx40
                  then yyAction15(strm, yyNO_MATCH)
                  else yyQ44(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ44(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction15(strm, yyNO_MATCH)
            else if inp <= 0wx66
              then yyQ44(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ44(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ44(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ44(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ44(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx46
                  then yyQ44(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx66
              then yyQ44(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyAction14(strm, yyNO_MATCH)
            else if inp < 0wx3A
              then if inp <= 0wx2F
                  then yyAction14(strm, yyNO_MATCH)
                  else yyQ42(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp = 0wx3B
              then yyQ46(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yystuck(lastMatch)
            else if inp < 0wx3A
              then if inp <= 0wx2F
                  then yystuck(lastMatch)
                  else yyQ42(strm', lastMatch)
            else if inp = 0wx78
              then yyQ43(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ40(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx23
                  then yyQ39(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ40(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ40(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ40(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx27
              then yyQ16(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < 0wx27
              then if inp = 0wx26
                  then yyAction18(strm, yyNO_MATCH)
                  else yyQ16(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp = 0wx3C
              then yyAction18(strm, yyNO_MATCH)
              else yyQ16(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx27
              then yyQ16(strm', lastMatch)
            else if inp < 0wx27
              then if inp = 0wx26
                  then yyQ17(strm', lastMatch)
                  else yyQ16(strm', lastMatch)
            else if inp = 0wx3C
              then yyQ18(strm', lastMatch)
              else yyQ16(strm', lastMatch)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyAction2(strm, yyNO_MATCH)
              else yyQ14(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ15(strm', lastMatch)
              else yyQ14(strm', lastMatch)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ13(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ12(strm', lastMatch)
              else yyQ11(strm', lastMatch)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ9(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyAction10(strm, yyNO_MATCH)
            else if inp < 0wxA
              then if inp = 0wx9
                  then yyQ10(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = 0wx20
              then yyQ10(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyAction10(strm, yyNO_MATCH)
            else if inp < 0wxA
              then if inp = 0wx9
                  then yyQ10(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = 0wx20
              then yyQ10(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx21
              then yyQ4(strm', lastMatch)
            else if inp < 0wx21
              then if inp = 0wxA
                  then yyQ6(strm', lastMatch)
                else if inp < 0wxA
                  then if inp = 0wx9
                      then yyQ5(strm', lastMatch)
                      else yyQ4(strm', lastMatch)
                else if inp = 0wx20
                  then yyQ5(strm', lastMatch)
                  else yyQ4(strm', lastMatch)
            else if inp = 0wx2E
              then yyQ4(strm', lastMatch)
            else if inp < 0wx2E
              then if inp = 0wx2D
                  then yyQ7(strm', lastMatch)
                  else yyQ4(strm', lastMatch)
            else if inp = 0wx3E
              then yyQ8(strm', lastMatch)
              else yyQ4(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of COM2 => yyQ0(!(yystrm), yyNO_MATCH)
    | COM1 => yyQ1(!(yystrm), yyNO_MATCH)
    | INTAG => yyQ2(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ3(!(yystrm), yyNO_MATCH)
  (* end case *))
end
end
            and skip() = (yystartPos := yygetPos(); 
			  yylastwasnref := ULexBuffer.lastWasNL (!yystrm);
			  continue())
	    in (continue(), (!yystartPos, yygetPos()-1), !yystrm, !yyss) end
          in 
            lex()
          end
  in
    type pos = AntlrStreamPos.pos
    type span = AntlrStreamPos.span
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of ULexBuffer.stream * 
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex sm 
(STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex 
(yystrm, ss, sm)
	     val strm' = STRM (yystrm', ref NONE);
	     in 
	       memo := SOME (ss, tok, span, strm', ss');
	       (tok, span, (strm', ss'))
	     end
	   | SOME (ss', tok, span, strm', ss'') => 
	       if ss = ss' then
		 (tok, span, (strm', ss''))
	       else (
		 memo := NONE;
		 lex sm 
(STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (yystreamify' 0 input, ref NONE), INITIAL)
    fun streamifyReader readFn strm = (STRM (yystreamifyReader' 0 readFn strm, ref NONE), 
				       INITIAL)
    fun streamifyInstream strm = (STRM (yystreamifyInstream' 0 strm, ref NONE), 
				  INITIAL)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end


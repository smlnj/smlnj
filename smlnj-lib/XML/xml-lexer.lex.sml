structure XMLLexer  = struct

    datatype yystart_state = 
DOCTYPE | COM | LIT2 | LIT1 | INITIAL | TAG
    local

    structure UserDeclarations = 
      struct

 
  structure T = XMLTokens
  type lex_result = T.token
  fun eof () = T.EOF

(* list of strings to build attribute values *)
  val text : string list ref = ref []
  fun addText s = (text := s :: !text)
  fun addDecimalEscape s = addText(UTF8.encode(Word.fromInt(Option.valOf(Int.fromString s))))
  fun addHexEscape s = addText(UTF8.encode(Option.valOf(Word.fromString s)))
  fun textToString () = let
	val s = String.concat(List.rev(!text))
	in
	  text := []; s
	end

(* trim m characters from the left and n characters from the right *)
  fun trim (m, ss, n) = Substring.string(Substring.triml m (Substring.trimr n ss))

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
(yyarg as  lexErr)(yystrm_, yyss_, yysm) = let
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
        yystrm := strm;  addText yytext; YYBEGIN COM; continue()
      end
fun yyAction1 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; YYBEGIN INITIAL; T.COM(textToString())
      end
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN TAG; T.OPEN_START_TAG)
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN TAG; T.OPEN_END_TAG)
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN TAG; T.OPEN_XML_TAG)
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN DOCTYPE; T.OPEN_DOCTYPE)
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;  T.PUBLIC)
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;  T.SYSTEM)
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.LIT(String.substring(yytext, 1, size yytext - 2))
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.LIT(String.substring(yytext, 1, size yytext - 2))
      end
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; T.CLOSE_TAG)
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;  skip())
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; T.CLOSE_PI_TAG)
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; T.CLOSE_TAG)
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; T.CLOSE_EMPTY_TAG)
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;  T.SYM_EQ)
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.ID yytext
      end
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN LIT1; continue())
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN LIT2; continue())
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN TAG; T.LIT(textToString()))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN TAG; T.LIT(textToString()))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
       addText ("\""); continue())
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
       addText ("<"); continue())
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
       addText (">"); continue())
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
       addText ("&"); continue())
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
       addText ("'"); continue())
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;  addDecimalEscape(trim(2, yysubstr, 1)); continue()
      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;  addHexEscape(trim(3, yysubstr, 1)); continue()
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction31 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.WS yytext
      end
fun yyAction32 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.TEXT yytext
      end
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;  T.TEXT "\"")
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;  T.TEXT "<")
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;  T.TEXT ">")
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;  T.TEXT "&")
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;  T.TEXT "'")
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yysubstr = yymksubstr(strm)
      in
        yystrm := strm;  T.CDATA(trim (9, yysubstr, 3))
      end
fun yyAction39 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         lexErr(yypos, [
                                        "bad character `", String.toString yytext, "'"
                                      ]);
                                    continue()
      end
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         lexErr(yypos, [
                                        "bad character `", String.toString yytext, "' in DOCTYPE"
                                      ]);
                                    continue()
      end
fun yyAction41 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         lexErr(yypos, [
                                        "bad character `", String.toString yytext, "' in tag"
                                      ]);
                                    continue()
      end
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         lexErr(yypos, [
                                        "bad character `", String.toString yytext, "' in attribute value"
                                      ]);
                                    continue()
      end
fun yyQ128 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ127 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ128(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
              else yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ126 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ125 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx2F
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < 0wx2F
                  then if inp <= 0wx2C
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp <= 0wx3A
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction17(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ124 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx2F
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < 0wx2F
                  then if inp <= 0wx2C
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp <= 0wx3A
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction17(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ129 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ123 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ129(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
              else yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ122 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ121 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction12(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp <= 0wx8
                  then yyAction12(strm, yyNO_MATCH)
                  else yyQ29(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ29(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ120 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction12(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp <= 0wx8
                  then yyAction12(strm, yyNO_MATCH)
                  else yyQ29(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ29(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ119 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
            if inp = 0wx3A
              then yyQ124(strm', lastMatch)
            else if inp < 0wx3A
              then if inp = 0wx22
                  then yyQ121(strm', lastMatch)
                else if inp < 0wx22
                  then if inp = 0wxE
                      then yyQ119(strm', lastMatch)
                    else if inp < 0wxE
                      then if inp <= 0wx8
                          then yyQ119(strm', lastMatch)
                          else yyQ120(strm', lastMatch)
                    else if inp = 0wx20
                      then yyQ120(strm', lastMatch)
                      else yyQ119(strm', lastMatch)
                else if inp = 0wx28
                  then yyQ119(strm', lastMatch)
                else if inp < 0wx28
                  then if inp = 0wx27
                      then yyQ122(strm', lastMatch)
                      else yyQ119(strm', lastMatch)
                else if inp = 0wx2F
                  then yyQ123(strm', lastMatch)
                  else yyQ119(strm', lastMatch)
            else if inp = 0wx41
              then yyQ124(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx3E
                  then yyQ126(strm', lastMatch)
                else if inp < 0wx3E
                  then if inp = 0wx3D
                      then yyQ125(strm', lastMatch)
                      else yyQ119(strm', lastMatch)
                else if inp = 0wx3F
                  then yyQ127(strm', lastMatch)
                  else yyQ119(strm', lastMatch)
            else if inp = 0wx60
              then yyQ119(strm', lastMatch)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyQ119(strm', lastMatch)
                else if inp < 0wx5B
                  then yyQ124(strm', lastMatch)
                else if inp = 0wx5F
                  then yyQ124(strm', lastMatch)
                  else yyQ119(strm', lastMatch)
            else if inp <= 0wx7A
              then yyQ124(strm', lastMatch)
              else yyQ119(strm', lastMatch)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx4D
              then yystuck(lastMatch)
            else if inp < 0wx4D
              then if inp = 0wx4C
                  then yyQ77(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx6C
              then yyQ77(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx4E
              then yystuck(lastMatch)
            else if inp < 0wx4E
              then if inp = 0wx4D
                  then yyQ76(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx6D
              then yyQ76(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx59
              then yystuck(lastMatch)
            else if inp < 0wx59
              then if inp = 0wx58
                  then yyQ75(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx78
              then yyQ75(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx5D
              then yyQ87(strm', lastMatch)
              else yyQ86(strm', lastMatch)
      (* end case *))
and yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx5D
              then yyQ88(strm', lastMatch)
              else yyQ86(strm', lastMatch)
      (* end case *))
and yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3F
              then yyQ86(strm', lastMatch)
            else if inp < 0wx3F
              then if inp = 0wx3E
                  then yyQ89(strm', lastMatch)
                  else yyQ86(strm', lastMatch)
            else if inp = 0wx5D
              then yyQ88(strm', lastMatch)
              else yyQ86(strm', lastMatch)
      (* end case *))
and yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5D
              then yyQ87(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyQ86(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyQ86(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ85(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx54
              then yyQ84(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ83(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx44
              then yyQ82(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx43
              then yyQ81(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx45
              then yyQ95(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx50
              then yyQ94(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx59
              then yyQ93(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx54
              then yyQ92(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx43
              then yyQ91(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx4F
              then yyQ90(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ96(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx44
              then yyQ79(strm', lastMatch)
            else if inp < 0wx44
              then if inp = 0wx2D
                  then yyQ78(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx5B
              then yyQ80(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2F
              then yyQ73(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < 0wx2F
              then if inp = 0wx21
                  then yyQ72(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = 0wx3F
              then yyQ74(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyQ104(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ103(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6F
              then yyQ102(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx75
              then yyQ101(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyQ106(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ105(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ107 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyQ108(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ107(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ113 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ112 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyQ113(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ111 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ112(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ110 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6F
              then yyQ111(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ115 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ114 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyQ115(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx70
              then yyQ114(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6E
              then yystuck(lastMatch)
            else if inp < 0wx6E
              then if inp = 0wx6D
                  then yyQ109(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx70
              then yyQ110(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx68
              then yyAction39(strm, yyNO_MATCH)
            else if inp < 0wx68
              then if inp = 0wx62
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx62
                  then if inp = 0wx61
                      then yyQ97(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = 0wx67
                  then yyQ98(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyAction39(strm, yyNO_MATCH)
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ99(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx71
              then yyQ100(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ117 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp <= 0wx8
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ117(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ117(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ116 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx20
              then yyAction32(strm, yyNO_MATCH)
            else if inp < 0wx20
              then if inp = 0wxB
                  then yyQ116(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp <= 0wx8
                      then yyQ116(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                      else yyAction32(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyAction32(strm, yyNO_MATCH)
                  else yyQ116(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp = 0wx27
              then yyQ116(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < 0wx27
              then if inp = 0wx26
                  then yyAction32(strm, yyNO_MATCH)
                  else yyQ116(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp = 0wx3C
              then yyAction32(strm, yyNO_MATCH)
              else yyQ116(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
      (* end case *))
fun yyQ118 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx20
              then yyQ117(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx20
              then if inp = 0wxB
                  then yyQ118(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp <= 0wx8
                      then yyQ116(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyQ117(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wxD
                  then yyQ117(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp <= 0wxC
                  then yyQ118(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyQ116(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx27
              then yyQ116(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx27
              then if inp = 0wx26
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ116(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx3C
              then yyAction31(strm, yyNO_MATCH)
              else yyQ116(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx20
              then yyQ117(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx20
              then if inp = 0wxB
                  then yyQ118(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp <= 0wx8
                      then yyQ116(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyQ117(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wxD
                  then yyQ117(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp <= 0wxC
                  then yyQ118(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyQ116(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx27
              then yyQ116(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx27
              then if inp = 0wx26
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ116(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx3C
              then yyAction31(strm, yyNO_MATCH)
              else yyQ116(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp <= 0wx8
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ117(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ117(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx20
              then yyAction32(strm, yyNO_MATCH)
            else if inp < 0wx20
              then if inp = 0wxB
                  then yyQ116(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp <= 0wx8
                      then yyQ116(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                      else yyAction32(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyAction32(strm, yyNO_MATCH)
                  else yyQ116(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp = 0wx27
              then yyQ116(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < 0wx27
              then if inp = 0wx26
                  then yyAction32(strm, yyNO_MATCH)
                  else yyQ116(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp = 0wx3C
              then yyAction32(strm, yyNO_MATCH)
              else yyQ116(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
            if inp = 0wx20
              then yyQ68(strm', lastMatch)
            else if inp < 0wx20
              then if inp = 0wxB
                  then yyQ69(strm', lastMatch)
                else if inp < 0wxB
                  then if inp <= 0wx8
                      then yyQ67(strm', lastMatch)
                      else yyQ68(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ68(strm', lastMatch)
                else if inp <= 0wxC
                  then yyQ69(strm', lastMatch)
                  else yyQ67(strm', lastMatch)
            else if inp = 0wx27
              then yyQ67(strm', lastMatch)
            else if inp < 0wx27
              then if inp = 0wx26
                  then yyQ70(strm', lastMatch)
                  else yyQ67(strm', lastMatch)
            else if inp = 0wx3C
              then yyQ71(strm', lastMatch)
              else yyQ67(strm', lastMatch)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyQ46(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ45(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6F
              then yyQ44(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx75
              then yyQ43(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyQ48(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ47(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyQ50(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx74
              then yyQ49(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyQ55(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ54(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6F
              then yyQ53(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyQ57(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx70
              then yyQ56(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6E
              then yystuck(lastMatch)
            else if inp < 0wx6E
              then if inp = 0wx6D
                  then yyQ51(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx70
              then yyQ52(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3C
              then yystuck(lastMatch)
            else if inp < 0wx3C
              then if inp = 0wx3A
                  then yystuck(lastMatch)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yystuck(lastMatch)
                      else yyQ60(strm', lastMatch)
                  else yyQ61(strm', lastMatch)
            else if inp = 0wx47
              then yystuck(lastMatch)
            else if inp < 0wx47
              then if inp <= 0wx40
                  then yystuck(lastMatch)
                  else yyQ60(strm', lastMatch)
            else if inp = 0wx61
              then yyQ60(strm', lastMatch)
            else if inp < 0wx61
              then yystuck(lastMatch)
            else if inp <= 0wx66
              then yyQ60(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ60(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ60(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ60(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ60(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx46
                  then yyQ60(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx66
              then yyQ60(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yystuck(lastMatch)
            else if inp < 0wx3A
              then if inp <= 0wx2F
                  then yystuck(lastMatch)
                  else yyQ58(strm', lastMatch)
            else if inp = 0wx3B
              then yyQ62(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yystuck(lastMatch)
            else if inp < 0wx3A
              then if inp <= 0wx2F
                  then yystuck(lastMatch)
                  else yyQ58(strm', lastMatch)
            else if inp = 0wx78
              then yyQ59(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx67
              then yyQ40(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx24
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < 0wx24
                  then if inp = 0wx23
                      then yyQ38(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                      else yyAction42(strm, yyNO_MATCH)
                else if inp = 0wx61
                  then yyQ39(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyAction42(strm, yyNO_MATCH)
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ41(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp = 0wx71
              then yyQ42(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx27
              then yyQ66(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < 0wx27
              then if inp = 0wx23
                  then yyQ66(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < 0wx23
                  then if inp = 0wx22
                      then yyAction29(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp = 0wx26
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = 0wx3D
              then yyQ66(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < 0wx3D
              then if inp = 0wx3C
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = 0wx3E
              then yyAction29(strm, yyNO_MATCH)
              else yyQ66(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx27
              then yyQ66(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < 0wx27
              then if inp = 0wx23
                  then yyQ66(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < 0wx23
                  then if inp = 0wx22
                      then yyAction29(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp = 0wx26
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = 0wx3D
              then yyQ66(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < 0wx3D
              then if inp = 0wx3C
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = 0wx3E
              then yyAction29(strm, yyNO_MATCH)
              else yyQ66(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
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
              then yyQ64(strm', lastMatch)
            else if inp < 0wx27
              then if inp = 0wx23
                  then yyQ64(strm', lastMatch)
                else if inp < 0wx23
                  then if inp = 0wx22
                      then yyQ65(strm', lastMatch)
                      else yyQ64(strm', lastMatch)
                else if inp = 0wx26
                  then yyQ35(strm', lastMatch)
                  else yyQ64(strm', lastMatch)
            else if inp = 0wx3D
              then yyQ64(strm', lastMatch)
            else if inp < 0wx3D
              then if inp = 0wx3C
                  then yyQ37(strm', lastMatch)
                  else yyQ64(strm', lastMatch)
            else if inp = 0wx3E
              then yyQ37(strm', lastMatch)
              else yyQ64(strm', lastMatch)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3C
              then yyAction30(strm, yyNO_MATCH)
            else if inp < 0wx3C
              then if inp = 0wx26
                  then yyAction30(strm, yyNO_MATCH)
                else if inp < 0wx26
                  then yyQ63(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp <= 0wx27
                  then yyAction30(strm, yyNO_MATCH)
                  else yyQ63(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp = 0wx3E
              then yyAction30(strm, yyNO_MATCH)
              else yyQ63(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3C
              then yyAction30(strm, yyNO_MATCH)
            else if inp < 0wx3C
              then if inp = 0wx26
                  then yyAction30(strm, yyNO_MATCH)
                else if inp < 0wx26
                  then yyQ63(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp <= 0wx27
                  then yyAction30(strm, yyNO_MATCH)
                  else yyQ63(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp = 0wx3E
              then yyAction30(strm, yyNO_MATCH)
              else yyQ63(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
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
            if inp = 0wx3C
              then yyQ37(strm', lastMatch)
            else if inp < 0wx3C
              then if inp = 0wx27
                  then yyQ36(strm', lastMatch)
                else if inp < 0wx27
                  then if inp = 0wx26
                      then yyQ35(strm', lastMatch)
                      else yyQ34(strm', lastMatch)
                  else yyQ34(strm', lastMatch)
            else if inp = 0wx3E
              then yyQ37(strm', lastMatch)
              else yyQ34(strm', lastMatch)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ33(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ32(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
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
              then yyQ31(strm', lastMatch)
              else yyQ30(strm', lastMatch)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ14(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx2F
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx2F
                  then if inp <= 0wx2C
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ14(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx3A
                  then yyQ14(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ14(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ14(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ14(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx4E
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx4E
              then if inp = 0wx30
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx2D
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                    else if inp < 0wx2D
                      then yyAction17(strm, yyNO_MATCH)
                    else if inp = 0wx2F
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx3A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                else if inp = 0wx4D
                  then yyQ19(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx6D
                  then yyQ19(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx46
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx46
              then if inp = 0wx30
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx2D
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                    else if inp < 0wx2D
                      then yyAction17(strm, yyNO_MATCH)
                    else if inp = 0wx2F
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx3A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                else if inp = 0wx45
                  then yyQ18(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ18(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx55
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx55
              then if inp = 0wx30
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx2D
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                    else if inp < 0wx2D
                      then yyAction17(strm, yyNO_MATCH)
                    else if inp = 0wx2F
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx3A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                else if inp = 0wx54
                  then yyQ17(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ17(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx54
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx54
              then if inp = 0wx30
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx2D
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                    else if inp < 0wx2D
                      then yyAction17(strm, yyNO_MATCH)
                    else if inp = 0wx2F
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx3A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                else if inp = 0wx53
                  then yyQ16(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ16(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx5A
              then if inp = 0wx30
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx2D
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                    else if inp < 0wx2D
                      then yyAction17(strm, yyNO_MATCH)
                    else if inp = 0wx2F
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx3A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                else if inp = 0wx59
                  then yyQ15(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = 0wx7A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx7A
              then if inp = 0wx79
                  then yyQ15(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ14(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx2F
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < 0wx2F
                  then if inp <= 0wx2C
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ14(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp <= 0wx3A
                  then yyQ14(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction7(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ14(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ14(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ14(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx44
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx44
              then if inp = 0wx30
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx2D
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                    else if inp < 0wx2D
                      then yyAction17(strm, yyNO_MATCH)
                    else if inp = 0wx2F
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx3A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                else if inp = 0wx43
                  then yyQ24(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ24(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx4A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx4A
              then if inp = 0wx30
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx2D
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                    else if inp < 0wx2D
                      then yyAction17(strm, yyNO_MATCH)
                    else if inp = 0wx2F
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx3A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                else if inp = 0wx49
                  then yyQ23(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ23(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx4D
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx4D
              then if inp = 0wx30
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx2D
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                    else if inp < 0wx2D
                      then yyAction17(strm, yyNO_MATCH)
                    else if inp = 0wx2F
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx3A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                else if inp = 0wx4C
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx43
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx43
              then if inp = 0wx30
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx2D
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                    else if inp < 0wx2D
                      then yyAction17(strm, yyNO_MATCH)
                    else if inp = 0wx2F
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx3A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = 0wx63
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx63
              then yyQ21(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx56
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx56
              then if inp = 0wx30
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx2D
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                    else if inp < 0wx2D
                      then yyAction17(strm, yyNO_MATCH)
                    else if inp = 0wx2F
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx3A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                else if inp = 0wx55
                  then yyQ20(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx5F
                  then if inp <= 0wx5A
                      then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ20(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx2F
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < 0wx2F
                  then if inp <= 0wx2C
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp <= 0wx3A
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction17(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ14(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3C
              then yystuck(lastMatch)
            else if inp < 0wx3C
              then if inp = 0wx22
                  then yystuck(lastMatch)
                else if inp < 0wx22
                  then if inp = 0wxB
                      then yystuck(lastMatch)
                    else if inp < 0wxB
                      then if inp <= 0wx8
                          then yystuck(lastMatch)
                          else yyQ25(strm', lastMatch)
                    else if inp <= 0wx1F
                      then yystuck(lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = 0wx27
                  then yyQ26(strm', lastMatch)
                else if inp < 0wx27
                  then if inp = 0wx26
                      then yystuck(lastMatch)
                      else yyQ25(strm', lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp = 0wx5F
              then yyQ25(strm', lastMatch)
            else if inp < 0wx5F
              then if inp = 0wx3F
                  then yyQ25(strm', lastMatch)
                else if inp < 0wx3F
                  then if inp = 0wx3D
                      then yyQ25(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp <= 0wx5A
                  then yyQ25(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ25(strm', lastMatch)
            else if inp < 0wx61
              then yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ25(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3C
              then yyAction40(strm, yyNO_MATCH)
            else if inp < 0wx3C
              then if inp = 0wx22
                  then yyAction40(strm, yyNO_MATCH)
                else if inp < 0wx22
                  then if inp = 0wxB
                      then yyAction40(strm, yyNO_MATCH)
                    else if inp < 0wxB
                      then if inp <= 0wx8
                          then yyAction40(strm, yyNO_MATCH)
                          else yyQ25(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                    else if inp <= 0wx1F
                      then yyAction40(strm, yyNO_MATCH)
                      else yyQ25(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                else if inp = 0wx27
                  then yyQ26(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                else if inp < 0wx27
                  then if inp = 0wx26
                      then yyAction40(strm, yyNO_MATCH)
                      else yyQ25(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                  else yyQ25(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp = 0wx5F
              then yyQ25(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3F
                  then yyQ25(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx3D
                      then yyQ25(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                      else yyAction40(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ25(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                  else yyAction40(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ25(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction40(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ25(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3C
              then yystuck(lastMatch)
            else if inp < 0wx3C
              then if inp = 0wx22
                  then yyQ28(strm', lastMatch)
                else if inp < 0wx22
                  then if inp = 0wxB
                      then yystuck(lastMatch)
                    else if inp < 0wxB
                      then if inp <= 0wx8
                          then yystuck(lastMatch)
                          else yyQ27(strm', lastMatch)
                    else if inp <= 0wx1F
                      then yystuck(lastMatch)
                      else yyQ27(strm', lastMatch)
                else if inp = 0wx26
                  then yystuck(lastMatch)
                  else yyQ27(strm', lastMatch)
            else if inp = 0wx5F
              then yyQ27(strm', lastMatch)
            else if inp < 0wx5F
              then if inp = 0wx3F
                  then yyQ27(strm', lastMatch)
                else if inp < 0wx3F
                  then if inp = 0wx3D
                      then yyQ27(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp <= 0wx5A
                  then yyQ27(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ27(strm', lastMatch)
            else if inp < 0wx61
              then yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ27(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3C
              then yyAction40(strm, yyNO_MATCH)
            else if inp < 0wx3C
              then if inp = 0wx22
                  then yyQ28(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                else if inp < 0wx22
                  then if inp = 0wxB
                      then yyAction40(strm, yyNO_MATCH)
                    else if inp < 0wxB
                      then if inp <= 0wx8
                          then yyAction40(strm, yyNO_MATCH)
                          else yyQ27(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                    else if inp <= 0wx1F
                      then yyAction40(strm, yyNO_MATCH)
                      else yyQ27(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                else if inp = 0wx26
                  then yyAction40(strm, yyNO_MATCH)
                  else yyQ27(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp = 0wx5F
              then yyQ27(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3F
                  then yyQ27(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                else if inp < 0wx3F
                  then if inp = 0wx3D
                      then yyQ27(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                      else yyAction40(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ27(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                  else yyAction40(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ27(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction40(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ27(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction12(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp <= 0wx8
                  then yyAction12(strm, yyNO_MATCH)
                  else yyQ29(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ29(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
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
            if inp = 0wx41
              then yyQ10(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx23
                  then yyQ6(strm', lastMatch)
                else if inp < 0wx23
                  then if inp = 0wx20
                      then yyQ7(strm', lastMatch)
                    else if inp < 0wx20
                      then if inp = 0wx9
                          then yyQ7(strm', lastMatch)
                        else if inp < 0wx9
                          then yyQ6(strm', lastMatch)
                        else if inp <= 0wxD
                          then yyQ7(strm', lastMatch)
                          else yyQ6(strm', lastMatch)
                    else if inp = 0wx21
                      then yyQ6(strm', lastMatch)
                      else yyQ8(strm', lastMatch)
                else if inp = 0wx3A
                  then yyQ10(strm', lastMatch)
                else if inp < 0wx3A
                  then if inp = 0wx27
                      then yyQ9(strm', lastMatch)
                      else yyQ6(strm', lastMatch)
                else if inp = 0wx3E
                  then yyQ11(strm', lastMatch)
                  else yyQ6(strm', lastMatch)
            else if inp = 0wx60
              then yyQ6(strm', lastMatch)
            else if inp < 0wx60
              then if inp = 0wx53
                  then yyQ13(strm', lastMatch)
                else if inp < 0wx53
                  then if inp = 0wx50
                      then yyQ12(strm', lastMatch)
                      else yyQ10(strm', lastMatch)
                else if inp = 0wx5B
                  then yyQ6(strm', lastMatch)
                else if inp < 0wx5B
                  then yyQ10(strm', lastMatch)
                else if inp = 0wx5F
                  then yyQ10(strm', lastMatch)
                  else yyQ6(strm', lastMatch)
            else if inp = 0wx73
              then yyQ13(strm', lastMatch)
            else if inp < 0wx73
              then if inp = 0wx70
                  then yyQ12(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp <= 0wx7A
              then yyQ10(strm', lastMatch)
              else yyQ6(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of DOCTYPE => yyQ0(!(yystrm), yyNO_MATCH)
    | COM => yyQ1(!(yystrm), yyNO_MATCH)
    | LIT2 => yyQ2(!(yystrm), yyNO_MATCH)
    | LIT1 => yyQ3(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ4(!(yystrm), yyNO_MATCH)
    | TAG => yyQ5(!(yystrm), yyNO_MATCH)
  (* end case *))
end
end
            and skip() = (yystartPos := yygetPos(); 
			  yylastwasnref := ULexBuffer.lastWasNL (!yystrm);
			  continue())
	    in (continue(), (!yystartPos, yygetPos()), !yystrm, !yyss) end
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
(yyarg as  lexErr)(STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex 
yyarg(yystrm, ss, sm)
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
yyarg(STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (yystreamify' 0 input, ref NONE), INITIAL)
    fun streamifyReader readFn strm = (STRM (yystreamifyReader' 0 readFn strm, ref NONE), 
				       INITIAL)
    fun streamifyInstream strm = (STRM (yystreamifyInstream' 0 strm, ref NONE), 
				  INITIAL)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end

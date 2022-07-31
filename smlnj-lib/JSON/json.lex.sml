structure JSONLexer  = struct

    datatype yystart_state = 
S | INITIAL
    local

    structure UserDeclarations = 
      struct

 
  structure T = JSONTokens
  type lex_result = T.token
  fun eof () = T.EOF
  fun int s = T.INT(valOf(IntInf.fromString s))
  fun float s = T.FLOAT(valOf(LargeReal.fromString s))
(* support for incremental construction of strings *)
  val sbuf : string list ref = ref []
  fun addStr s = sbuf := s :: !sbuf
  fun addUChr lit = let
      (* trim the "\u" prefix *)
	val digits = Substring.triml 2 (Substring.full lit)
	val SOME(w, _) = Word.scan StringCvt.HEX Substring.getc digits
	in
	  addStr(UTF8.encode w)
	end
  fun finishString () = (T.STRING(String.concat(List.rev(!sbuf))) before sbuf := [])

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
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;   skip() )
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LCB )
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;   T.RCB )
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LB )
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;   T.RB )
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;   T.COMMA )
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;   T.COLON )
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_null )
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_true )
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_false )
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.INT(valOf(IntInf.fromString yytext)) 
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   float yytext 
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   float yytext 
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   float yytext 
      end
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
        YYBEGIN S; continue() )
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
        addStr "\\"; continue() )
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
        addStr "\""; continue() )
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
        addStr "/"; continue() )
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
        addStr "\b"; continue() )
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
        addStr "\f"; continue() )
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
        addStr "\n"; continue() )
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
        addStr "\r"; continue() )
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
        addStr "\t"; continue() )
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   addUChr yytext; continue() 
      end
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   addStr yytext; continue() 
      end
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
        YYBEGIN INITIAL; finishString() )
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;   skip() )
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
          T.ERROR["bad character '", String.toString yytext, "'"]
      end
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ36(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx75
              then yyQ35(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ34(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6C
              then yyQ39(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6C
              then yyQ38(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx75
              then yyQ37(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ43(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ42(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6C
              then yyQ41(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ40(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction12(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ48(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ48(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ47(strm', lastMatch)
            else if inp < 0wx2D
              then if inp = 0wx2B
                  then yyQ47(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx30
              then yyQ48(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ48(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ52(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction13(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ52(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ52(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ52(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ51(strm', lastMatch)
            else if inp < 0wx2D
              then if inp = 0wx2B
                  then yyQ51(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx30
              then yyQ52(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ52(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx45
              then yyQ50(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp < 0wx45
              then if inp = 0wx30
                  then yyQ49(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction11(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ49(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ50(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ49(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ49(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyAction10(strm, yyNO_MATCH)
            else if inp < 0wx3A
              then if inp = 0wx2F
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < 0wx2F
                  then if inp = 0wx2E
                      then yyQ44(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                      else yyAction10(strm, yyNO_MATCH)
                  else yyQ45(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = 0wx46
              then yyAction10(strm, yyNO_MATCH)
            else if inp < 0wx46
              then if inp = 0wx45
                  then yyQ46(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ46(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyAction10(strm, yyNO_MATCH)
            else if inp < 0wx3A
              then if inp = 0wx2F
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < 0wx2F
                  then if inp = 0wx2E
                      then yyQ44(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                      else yyAction10(strm, yyNO_MATCH)
                  else yyQ45(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = 0wx46
              then yyAction10(strm, yyNO_MATCH)
            else if inp < 0wx46
              then if inp = 0wx45
                  then yyQ46(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ46(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx45
              then yyQ46(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < 0wx45
              then if inp = 0wx2E
                  then yyQ44(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ46(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ57(strm', lastMatch)
              else yyQ56(strm', lastMatch)
      (* end case *))
and yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ57(strm', lastMatch)
              else yyQ56(strm', lastMatch)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ57(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyQ56(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ54(strm', lastMatch)
              else yyQ53(strm', lastMatch)
      (* end case *))
and yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2B
              then yyQ53(strm', lastMatch)
            else if inp < 0wx2B
              then if inp = 0wx2A
                  then yyQ54(strm', lastMatch)
                  else yyQ53(strm', lastMatch)
            else if inp = 0wx2F
              then yyQ55(strm', lastMatch)
              else yyQ53(strm', lastMatch)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ53(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyAction10(strm, yyNO_MATCH)
            else if inp < 0wx3A
              then if inp = 0wx2F
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < 0wx2F
                  then if inp = 0wx2E
                      then yyQ44(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                      else yyAction10(strm, yyNO_MATCH)
                  else yyQ45(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = 0wx46
              then yyAction10(strm, yyNO_MATCH)
            else if inp < 0wx46
              then if inp = 0wx45
                  then yyQ46(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ46(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx45
              then yyQ46(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < 0wx45
              then if inp = 0wx2E
                  then yyQ44(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ46(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx31
              then yyQ59(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < 0wx31
              then if inp = 0wx30
                  then yyQ58(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ59(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxD
              then yyQ60(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wxD
              then if inp = 0wx9
                  then yyQ60(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp <= 0wxA
                  then yyQ60(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = 0wx20
              then yyQ60(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxD
              then yyQ60(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wxD
              then if inp = 0wx9
                  then yyQ60(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp <= 0wxA
                  then yyQ60(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = 0wx20
              then yyQ60(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
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
            if inp = 0wx3A
              then yyQ26(strm', lastMatch)
            else if inp < 0wx3A
              then if inp = 0wx22
                  then yyQ20(strm', lastMatch)
                else if inp < 0wx22
                  then if inp = 0wxD
                      then yyQ19(strm', lastMatch)
                    else if inp < 0wxD
                      then if inp = 0wx9
                          then yyQ19(strm', lastMatch)
                        else if inp < 0wx9
                          then yyQ18(strm', lastMatch)
                        else if inp <= 0wxA
                          then yyQ19(strm', lastMatch)
                          else yyQ18(strm', lastMatch)
                    else if inp = 0wx20
                      then yyQ19(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                else if inp = 0wx2E
                  then yyQ18(strm', lastMatch)
                else if inp < 0wx2E
                  then if inp = 0wx2C
                      then yyQ21(strm', lastMatch)
                    else if inp = 0wx2D
                      then yyQ22(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                else if inp = 0wx30
                  then yyQ24(strm', lastMatch)
                else if inp = 0wx2F
                  then yyQ23(strm', lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp = 0wx6E
              then yyQ30(strm', lastMatch)
            else if inp < 0wx6E
              then if inp = 0wx5D
                  then yyQ28(strm', lastMatch)
                else if inp < 0wx5D
                  then if inp = 0wx5B
                      then yyQ27(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                else if inp = 0wx66
                  then yyQ29(strm', lastMatch)
                  else yyQ18(strm', lastMatch)
            else if inp = 0wx7B
              then yyQ32(strm', lastMatch)
            else if inp < 0wx7B
              then if inp = 0wx74
                  then yyQ31(strm', lastMatch)
                  else yyQ18(strm', lastMatch)
            else if inp = 0wx7D
              then yyQ33(strm', lastMatch)
              else yyQ18(strm', lastMatch)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ17(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ17(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ17(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ17(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx46
                  then yyQ17(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx66
              then yyQ17(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ16(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ16(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ16(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ16(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx46
                  then yyQ16(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx66
              then yyQ16(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ15(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ15(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ15(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ15(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx46
                  then yyQ15(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx66
              then yyQ15(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ14(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ14(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ14(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ14(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx46
                  then yyQ14(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx66
              then yyQ14(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx66
              then yyQ9(strm', lastMatch)
            else if inp < 0wx66
              then if inp = 0wx30
                  then yystuck(lastMatch)
                else if inp < 0wx30
                  then if inp = 0wx23
                      then yystuck(lastMatch)
                    else if inp < 0wx23
                      then if inp = 0wx22
                          then yyQ5(strm', lastMatch)
                          else yystuck(lastMatch)
                    else if inp = 0wx2F
                      then yyQ6(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = 0wx5D
                  then yystuck(lastMatch)
                else if inp < 0wx5D
                  then if inp = 0wx5C
                      then yyQ7(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = 0wx62
                  then yyQ8(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx73
              then yystuck(lastMatch)
            else if inp < 0wx73
              then if inp = 0wx6F
                  then yystuck(lastMatch)
                else if inp < 0wx6F
                  then if inp = 0wx6E
                      then yyQ10(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = 0wx72
                  then yyQ11(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx75
              then yyQ13(strm', lastMatch)
            else if inp = 0wx74
              then yyQ12(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ2(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction24(strm, yyNO_MATCH)
                  else yyQ2(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction24(strm, yyNO_MATCH)
              else yyQ2(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
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
            if inp = 0wx23
              then yyQ2(strm', lastMatch)
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyQ3(strm', lastMatch)
                  else yyQ2(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ4(strm', lastMatch)
              else yyQ2(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of S => yyQ0(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ1(!(yystrm), yyNO_MATCH)
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

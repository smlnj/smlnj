structure Mlex  = struct

    datatype yystart_state = 
INITIAL
    structure UserDeclarations = 
      struct

 
  open Tokens;
  fun eof() = EOF
  type lex_result = token


      end

    local
    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
#[
]

    fun innerLex (yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case UTF8.getu ULexBuffer.getc strm
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
		      val finals = map (fn i => Vector.sub (actTable, i)) finals'
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
	    fun continue() = 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;  skip())
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;  skip())
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_program)
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;  LP)
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;  RP)
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;  SEMI)
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;  DOT)
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;  COMMA)
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_var)
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;  COLON)
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_array)
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;  LSB)
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;  RSB)
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_of)
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_integer)
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_real)
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_function)
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_procedure)
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_begin)
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_end)
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;  ASSIGNOP)
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_if)
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_then)
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_else)
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_while)
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_do)
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  RELOP yytext
      end
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  ADDOP yytext
      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  MULOP yytext
      end
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;  KW_not)
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;  MINUS)
fun yyAction31 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  ID yytext
      end
fun yyAction32 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  INT (valOf (IntInf.fromString yytext))
      end
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         print (concat ["Unexpected character: '", yytext,
			           "'\n"]); continue()
      end
fun yyQ36 (strm, lastMatch : yymatch) = yyAction1(strm, yyNO_MATCH)
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx7D
              then yyQ36(strm', lastMatch)
              else yyQ35(strm', lastMatch)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx7D
              then yyQ36(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyQ35(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction24(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction24(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction24(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction24(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction24(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyAction24(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ41(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6C
              then yyQ40(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6C
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ39(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx68
              then yyQ38(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx68
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ43(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx62
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ42(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction22(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction22(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction22(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction22(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction22(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                  else yyAction22(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ46(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ45(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx68
              then yyQ44(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx68
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction15(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction15(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction15(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction15(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction15(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6C
              then yyQ49(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6C
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx62
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ47(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ56(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx62
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ54(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction17(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction17(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction17(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ61(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ60(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ59(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ58(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ57(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction31(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ53(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx63
                  then yyQ52(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ51(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ50(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction27(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction27(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction13(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction13(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction13(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction13(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction13(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction31(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ63(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx66
                  then yyQ62(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction29(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction29(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction29(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ65(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ64(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ67(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ66(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction14(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction14(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction14(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction14(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction14(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyAction14(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ74(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ72(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ71(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ70(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction21(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction21(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction21(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction21(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction31(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ69(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx66
                  then yyQ68(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction16(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction16(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction16(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction16(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction16(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ81(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ80(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ79(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ78(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx63
              then yyQ77(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx63
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ76(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ75(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction19(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction19(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction19(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction19(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ84(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction23(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction23(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ86(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ85(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction31(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ83(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx6C
                  then yyQ82(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction25(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction25(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction25(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction25(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction25(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ67(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction31(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ88(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx69
                  then yyQ87(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction18(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction18(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction18(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction18(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction18(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ92(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ91(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ90(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ89(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction10(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction10(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction10(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction10(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx79
              then yyQ96(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx79
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx62
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ95(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ94(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction31(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ93(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx6E
                  then yyQ66(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = yyAction12(strm, yyNO_MATCH)
fun yyQ18 (strm, lastMatch : yymatch) = yyAction11(strm, yyNO_MATCH)
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ37(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = yyAction26(strm, yyNO_MATCH)
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ97(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = yyAction26(strm, yyNO_MATCH)
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ97(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx3D
              then yyAction26(strm, yyNO_MATCH)
            else if inp <= 0wx3E
              then yyQ97(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ98 (strm, lastMatch : yymatch) = yyAction20(strm, yyNO_MATCH)
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ98(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ99(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction32(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ99(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ99(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction32(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ99(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = yyAction6(strm, yyNO_MATCH)
fun yyQ9 (strm, lastMatch : yymatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ8 (strm, lastMatch : yymatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ7 (strm, lastMatch : yymatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ6 (strm, lastMatch : yymatch) = yyAction28(strm, yyNO_MATCH)
fun yyQ5 (strm, lastMatch : yymatch) = yyAction4(strm, yyNO_MATCH)
fun yyQ4 (strm, lastMatch : yymatch) = yyAction3(strm, yyNO_MATCH)
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ100(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ101(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ100(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ100(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
and yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ100(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ101(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ100(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ100(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ100(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ101(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ100(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ100(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ100(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ101(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ100(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ100(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = yyAction33(strm, yyNO_MATCH)
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx5D
              then yyQ19(strm', lastMatch)
            else if inp < 0wx5D
              then if inp = 0wx2E
                  then yyQ10(strm', lastMatch)
                else if inp < 0wx2E
                  then if inp = 0wx28
                      then yyQ4(strm', lastMatch)
                    else if inp < 0wx28
                      then if inp = 0wxE
                          then yyQ1(strm', lastMatch)
                        else if inp < 0wxE
                          then if inp = 0wx9
                              then yyQ2(strm', lastMatch)
                            else if inp < 0wx9
                              then yyQ1(strm', lastMatch)
                            else if inp = 0wxD
                              then yyQ3(strm', lastMatch)
                              else yyQ2(strm', lastMatch)
                        else if inp = 0wx20
                          then yyQ2(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                    else if inp = 0wx2B
                      then yyQ7(strm', lastMatch)
                    else if inp < 0wx2B
                      then if inp = 0wx29
                          then yyQ5(strm', lastMatch)
                          else yyQ6(strm', lastMatch)
                    else if inp = 0wx2C
                      then yyQ8(strm', lastMatch)
                      else yyQ9(strm', lastMatch)
                else if inp = 0wx3D
                  then yyQ15(strm', lastMatch)
                else if inp < 0wx3D
                  then if inp = 0wx3A
                      then yyQ12(strm', lastMatch)
                    else if inp < 0wx3A
                      then if inp = 0wx2F
                          then yyQ6(strm', lastMatch)
                          else yyQ11(strm', lastMatch)
                    else if inp = 0wx3B
                      then yyQ13(strm', lastMatch)
                      else yyQ14(strm', lastMatch)
                else if inp = 0wx41
                  then yyQ17(strm', lastMatch)
                else if inp < 0wx41
                  then if inp = 0wx3E
                      then yyQ16(strm', lastMatch)
                      else yyQ1(strm', lastMatch)
                else if inp = 0wx5B
                  then yyQ18(strm', lastMatch)
                else if inp = 0wx5C
                  then yyQ1(strm', lastMatch)
                  else yyQ17(strm', lastMatch)
            else if inp = 0wx6F
              then yyQ28(strm', lastMatch)
            else if inp < 0wx6F
              then if inp = 0wx66
                  then yyQ24(strm', lastMatch)
                else if inp < 0wx66
                  then if inp = 0wx63
                      then yyQ17(strm', lastMatch)
                    else if inp < 0wx63
                      then if inp = 0wx61
                          then yyQ20(strm', lastMatch)
                        else if inp = 0wx62
                          then yyQ21(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                    else if inp = 0wx64
                      then yyQ22(strm', lastMatch)
                      else yyQ23(strm', lastMatch)
                else if inp = 0wx6A
                  then yyQ17(strm', lastMatch)
                else if inp < 0wx6A
                  then if inp = 0wx69
                      then yyQ25(strm', lastMatch)
                      else yyQ17(strm', lastMatch)
                else if inp = 0wx6D
                  then yyQ26(strm', lastMatch)
                else if inp = 0wx6E
                  then yyQ27(strm', lastMatch)
                  else yyQ17(strm', lastMatch)
            else if inp = 0wx75
              then yyQ17(strm', lastMatch)
            else if inp < 0wx75
              then if inp = 0wx72
                  then yyQ30(strm', lastMatch)
                else if inp < 0wx72
                  then if inp = 0wx70
                      then yyQ29(strm', lastMatch)
                      else yyQ17(strm', lastMatch)
                else if inp = 0wx73
                  then yyQ17(strm', lastMatch)
                  else yyQ31(strm', lastMatch)
            else if inp = 0wx78
              then yyQ17(strm', lastMatch)
            else if inp < 0wx78
              then if inp = 0wx76
                  then yyQ32(strm', lastMatch)
                  else yyQ33(strm', lastMatch)
            else if inp = 0wx7B
              then yyQ34(strm', lastMatch)
            else if inp <= 0wx7A
              then yyQ17(strm', lastMatch)
              else yyQ1(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of INITIAL => yyQ0(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            and skip() = (yystartPos := yygetPos(); continue())
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

    fun lex sm (STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex (yystrm, ss, sm)
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
		 lex sm (STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (ULexBuffer.mkStream input, ref NONE), 
			   INITIAL)

    fun streamifyReader readFn strm = let
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
            streamify input
          end

    fun streamifyInstream strm = streamify (fn ()=>TextIO.input strm)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end

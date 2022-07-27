structure ASDLLex  = struct

    datatype yystart_state = 
CODE | INITIAL | CODELN
    local

    structure UserDeclarations = 
      struct


  structure T = ASDLTokens

  (* some type lex_result is necessitated by ml-ulex *)
    type lex_result = T.token

  (* eof : unit -> lex_result *)
  (* ml-ulex requires this as well *)
    fun eof () = T.EOF

    val code = ref ([] : string list)
    fun addString s = code := s :: (!code)
    fun makeCode () = (T.CODE(concat(rev(!code))) before code := nil)


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
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;  skip())
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
       (* FIXME: resynch the sourcemap *) skip())
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;  skip())
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LPAREN)
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;  T.RPAREN)
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LBRACK)
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;  T.RBRACK)
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LBRACE)
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;  T.RBRACE)
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LEQ)
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;  T.COMMA)
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;  T.SEQUENCE)
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;  T.DOT)
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;  T.OPTIONAL)
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;  T.SHARED)
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;  T.PIPE)
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;  T.EQ)
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_alias)
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_attributes)
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_import)
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_include)
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_module)
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_primitive)
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_view)
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;  T.FILE)
fun yyAction25 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.UID(Atom.atom yytext)
      end
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.LID(Atom.atom yytext)
      end
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODELN; continue())
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  YYBEGIN INITIAL; T.CODE yytext
      end
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CODE; continue())
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; makeCode())
fun yyAction31 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addString(yytext); continue()
      end
fun yyAction32 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addString(yytext); continue()
      end
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;  T.FILE)
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         lexErr(yypos, ["bad character `", String.toString yytext, "'"]);
                            continue()
      end
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
              else yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxB
              then yyQ2(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < 0wxB
              then if inp = 0wxA
                  then if ULexBuffer.eof(!(yystrm))
                      then let
                        val yycolno = ref(yygetcolNo(!(yystrm)))
                        val yylineno = ref(yygetlineNo(!(yystrm)))
                        in
                          (case (!(yyss))
                           of _ => (UserDeclarations.eof())
                          (* end case *))
                        end
                      else yyAction28(strm, yyNO_MATCH)
                  else yyQ2(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = 0wxD
              then if ULexBuffer.eof(!(yystrm))
                  then let
                    val yycolno = ref(yygetcolNo(!(yystrm)))
                    val yylineno = ref(yygetlineNo(!(yystrm)))
                    in
                      (case (!(yyss))
                       of _ => (UserDeclarations.eof())
                      (* end case *))
                    end
                  else yyAction28(strm, yyNO_MATCH)
              else yyQ2(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction26(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction26(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
                      else yyQ32(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction23(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ32(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx77
              then yyQ35(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx77
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ34(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ33(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
                      else yyQ32(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction22(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction22(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ32(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                  else yyAction22(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ43(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ42(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ41(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ40(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ39(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ38(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ37(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ36(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
                      else yyQ32(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction21(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction21(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ32(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ48(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx6C
              then yyQ47(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx6C
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ46(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ45(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ44(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction20(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction20(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction20(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ32(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ55(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ54(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ53(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx6C
              then yyQ52(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx6C
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx63
              then yyQ51(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx63
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
                      else yyQ32(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction19(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction19(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ32(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ59(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ58(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ57(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx70
              then yyQ56(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction26(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction26(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                      else yyAction26(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ50(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx6D
                  then yyQ49(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction26(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction26(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
                      else yyQ32(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction18(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction18(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ32(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ69(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ68(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ67(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ66(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx62
              then yyQ65(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ64(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ63(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ62(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
                      else yyQ32(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction17(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction17(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ32(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ72(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx62
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ71(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction26(strm, yyNO_MATCH)
                      else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ70(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx60
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction26(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction26(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                      else yyAction26(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ61(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx6C
                  then yyQ60(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ32(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
                      else yyQ73(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction25(strm, yyNO_MATCH)
                  else yyQ73(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction25(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ73(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ73(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
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
                      else yyQ73(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction25(strm, yyNO_MATCH)
                  else yyQ73(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction25(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ73(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ73(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ83(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ84(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx46
              then yystuck(lastMatch)
            else if inp < 0wx46
              then if inp = 0wx45
                  then yyQ81(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx65
              then yyQ82(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx46
              then yystuck(lastMatch)
            else if inp < 0wx46
              then if inp = 0wx45
                  then yyQ81(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx65
              then yyQ81(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx4D
              then yystuck(lastMatch)
            else if inp < 0wx4D
              then if inp = 0wx4C
                  then yyQ79(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx6C
              then yyQ80(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx4D
              then yystuck(lastMatch)
            else if inp < 0wx4D
              then if inp = 0wx4C
                  then yyQ79(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx6C
              then yyQ79(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx4A
              then yystuck(lastMatch)
            else if inp < 0wx4A
              then if inp = 0wx49
                  then yyQ77(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx69
              then yyQ78(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx4A
              then yystuck(lastMatch)
            else if inp < 0wx4A
              then if inp = 0wx49
                  then yyQ77(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx69
              then yyQ77(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx46
              then yyQ75(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp < 0wx46
              then if inp = 0wx3D
                  then yyQ74(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                  else yyAction34(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ76(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyAction27(strm, yyNO_MATCH)
            else if inp < 0wxA
              then if inp = 0wx9
                  then yyQ85(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = 0wx20
              then yyQ85(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyAction27(strm, yyNO_MATCH)
            else if inp < 0wxA
              then if inp = 0wx9
                  then yyQ85(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = 0wx20
              then yyQ85(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx22
              then yyQ98(strm', lastMatch)
              else yyQ97(strm', lastMatch)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx20
              then yyQ96(strm', lastMatch)
            else if inp < 0wx20
              then if inp = 0wx9
                  then yyQ96(strm', lastMatch)
                else if inp < 0wx9
                  then yystuck(lastMatch)
                else if inp <= 0wxD
                  then yyQ96(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx22
              then yyQ97(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx20
              then yyQ96(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < 0wx20
              then if inp = 0wx9
                  then yyQ96(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction1(strm, yyNO_MATCH)
                else if inp <= 0wxD
                  then yyQ96(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = 0wx30
              then yyQ95(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction1(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ95(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx20
              then yyQ94(strm', lastMatch)
            else if inp < 0wx20
              then if inp = 0wx9
                  then yyQ94(strm', lastMatch)
                else if inp < 0wx9
                  then yystuck(lastMatch)
                else if inp <= 0wxD
                  then yyQ94(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx30
              then yyQ95(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ95(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxB
              then yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wxB
              then if inp = 0wxA
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wxD
              then yyAction2(strm, yyNO_MATCH)
              else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxB
              then yyQ87(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < 0wxB
              then if inp = 0wxA
                  then yyAction1(strm, yyNO_MATCH)
                  else yyQ87(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = 0wxD
              then yyAction1(strm, yyNO_MATCH)
              else yyQ87(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxD
              then yyQ97(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wxD
              then if inp = 0wxA
                  then yyQ97(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ101(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx22
              then yyQ102(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyQ101(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxA
                  then yyQ96(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < 0wxA
                  then if inp = 0wx9
                      then yyQ100(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = 0wxD
                  then yyQ96(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ100(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx21
              then yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx21
              then if inp = 0wx20
                  then yyQ100(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx22
              then yyQ101(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ87(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxA
                  then yyQ96(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < 0wxA
                  then if inp = 0wx9
                      then yyQ100(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ87(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = 0wxD
                  then yyQ96(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ100(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = 0wx21
              then yyQ87(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < 0wx21
              then if inp = 0wx20
                  then yyQ100(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ87(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = 0wx30
              then yyQ99(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < 0wx30
              then yyQ87(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp <= 0wx39
              then yyQ99(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyQ87(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxA
                  then yyQ94(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < 0wxA
                  then if inp = 0wx9
                      then yyQ93(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = 0wxD
                  then yyQ94(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ93(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx21
              then yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx21
              then if inp = 0wx20
                  then yyQ93(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx30
              then yyQ99(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx30
              then yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp <= 0wx39
              then yyQ99(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxD
              then yyQ94(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wxD
              then if inp = 0wxA
                  then yyQ94(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < 0wxA
                  then if inp = 0wx9
                      then yyQ93(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ93(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ93(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxD
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wxD
              then if inp = 0wxA
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx65
              then yyQ92(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxD
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wxD
              then if inp = 0wxA
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx6E
              then yyQ91(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxD
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wxD
              then if inp = 0wxA
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx69
              then yyQ90(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxD
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wxD
              then if inp = 0wxA
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx6C
              then yyQ89(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxD
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wxD
              then if inp = 0wxA
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx23
              then yyQ88(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyQ87(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ86(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction29(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp <= 0wx8
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ103(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ103(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx25
              then yyQ103(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp <= 0wx8
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ104(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ104(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp <= 0wx8
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ104(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ104(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
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
            if inp = 0wx3F
              then yyQ19(strm', lastMatch)
            else if inp < 0wx3F
              then if inp = 0wx2A
                  then yyQ12(strm', lastMatch)
                else if inp < 0wx2A
                  then if inp = 0wx22
                      then yyQ6(strm', lastMatch)
                    else if inp < 0wx22
                      then if inp = 0wxE
                          then yyQ6(strm', lastMatch)
                        else if inp < 0wxE
                          then if inp <= 0wx8
                              then yyQ6(strm', lastMatch)
                              else yyQ7(strm', lastMatch)
                        else if inp = 0wx20
                          then yyQ7(strm', lastMatch)
                        else if inp = 0wx21
                          then yyQ8(strm', lastMatch)
                          else yyQ6(strm', lastMatch)
                    else if inp = 0wx26
                      then yyQ6(strm', lastMatch)
                    else if inp < 0wx26
                      then if inp = 0wx25
                          then yyQ9(strm', lastMatch)
                          else yyQ6(strm', lastMatch)
                    else if inp = 0wx28
                      then yyQ10(strm', lastMatch)
                    else if inp = 0wx29
                      then yyQ11(strm', lastMatch)
                      else yyQ6(strm', lastMatch)
                else if inp = 0wx3A
                  then yyQ16(strm', lastMatch)
                else if inp < 0wx3A
                  then if inp = 0wx2D
                      then yyQ14(strm', lastMatch)
                    else if inp < 0wx2D
                      then if inp = 0wx2B
                          then yyQ6(strm', lastMatch)
                          else yyQ13(strm', lastMatch)
                    else if inp = 0wx2E
                      then yyQ15(strm', lastMatch)
                      else yyQ6(strm', lastMatch)
                else if inp = 0wx3D
                  then yyQ18(strm', lastMatch)
                else if inp < 0wx3D
                  then if inp = 0wx3B
                      then yyQ6(strm', lastMatch)
                      else yyQ17(strm', lastMatch)
                  else yyQ6(strm', lastMatch)
            else if inp = 0wx6D
              then yyQ26(strm', lastMatch)
            else if inp < 0wx6D
              then if inp = 0wx5E
                  then yyQ6(strm', lastMatch)
                else if inp < 0wx5E
                  then if inp = 0wx5B
                      then yyQ21(strm', lastMatch)
                    else if inp < 0wx5B
                      then if inp = 0wx40
                          then yyQ6(strm', lastMatch)
                          else yyQ20(strm', lastMatch)
                    else if inp = 0wx5C
                      then yyQ6(strm', lastMatch)
                      else yyQ22(strm', lastMatch)
                else if inp = 0wx62
                  then yyQ24(strm', lastMatch)
                else if inp < 0wx62
                  then if inp = 0wx61
                      then yyQ23(strm', lastMatch)
                      else yyQ6(strm', lastMatch)
                else if inp = 0wx69
                  then yyQ25(strm', lastMatch)
                  else yyQ24(strm', lastMatch)
            else if inp = 0wx77
              then yyQ24(strm', lastMatch)
            else if inp < 0wx77
              then if inp = 0wx71
                  then yyQ24(strm', lastMatch)
                else if inp < 0wx71
                  then if inp = 0wx70
                      then yyQ27(strm', lastMatch)
                      else yyQ24(strm', lastMatch)
                else if inp = 0wx76
                  then yyQ28(strm', lastMatch)
                  else yyQ24(strm', lastMatch)
            else if inp = 0wx7C
              then yyQ30(strm', lastMatch)
            else if inp < 0wx7C
              then if inp = 0wx7B
                  then yyQ29(strm', lastMatch)
                  else yyQ24(strm', lastMatch)
            else if inp = 0wx7D
              then yyQ31(strm', lastMatch)
              else yyQ6(strm', lastMatch)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx25
              then yyQ5(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx25
              then yyAction31(strm, yyNO_MATCH)
              else yyQ3(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
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
              else yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx25
              then yyQ4(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyQ3(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
      (* end case *))
in
  (case (!(yyss))
   of CODE => yyQ0(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ1(!(yystrm), yyNO_MATCH)
    | CODELN => yyQ2(!(yystrm), yyNO_MATCH)
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


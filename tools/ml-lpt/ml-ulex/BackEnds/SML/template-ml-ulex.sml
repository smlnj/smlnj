@header@
  = struct

    datatype yystart_state =
@startstates@

    local

    structure UserDeclarations =
      struct

@userdecls@

      end

    datatype yymatch
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector =
@table@

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
@args@
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
            fun yyactsToMatches (strm, [],        oldMatches) = oldMatches
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
@lexer@

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
@args@
(STRM (yystrm, memo), ss) = (case !memo
          of NONE => let
             val (tok, span, yystrm', ss') = innerLex
@pargs@
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
@pargs@
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


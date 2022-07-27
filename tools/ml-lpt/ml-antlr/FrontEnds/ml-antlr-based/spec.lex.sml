structure SpecLex  = struct

    datatype yystart_state = 
COM | CODE | STRING | PRECODE | PRECONSTR | CONSTR | INITIAL
    local

    structure UserDeclarations = 
      struct

 

structure Tok = SpecTokens

val comLvl : int ref = ref 0		(* nesting depth of comments *)
val comStart : int ref = ref 0		(* start line of current comment *)

type lex_result = Tok.token

fun err (lineNo, colNo, msg) = Err.errMsg [
	"Lexical error [", Int.toString lineNo, ".", Int.toString colNo, "]: ", msg
      ]

fun eof () = (
      if (!comLvl > 0)
        then ()
(* err(~1, "unclosed comment starting at line " ^ Int.toString(!comStart)) *)
        else ();
      Tok.EOF)

val text : string list ref = ref []
fun addText s = (text := s::(!text))
fun clrText () = (text := [])
fun getText () = concat (rev (!text))

val pcount = ref 0			(* nesting depth of parentheses in CODE *)
fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)

      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
#[([(0w0,0w12,7),
(0w14,0w39,7),
(0w41,0w41,7),
(0w43,0w2147483647,7),
(0w13,0w13,8),
(0w40,0w40,9),
(0w42,0w42,10)], []), ([(0w0,0w33,14),
(0w35,0w39,14),
(0w42,0w2147483647,14),
(0w34,0w34,15),
(0w40,0w40,16),
(0w41,0w41,17)], []), ([(0w0,0w9,20),
(0w11,0w12,20),
(0w14,0w33,20),
(0w35,0w91,20),
(0w93,0w2147483647,20),
(0w10,0w10,21),
(0w13,0w13,22),
(0w34,0w34,23),
(0w92,0w92,24)], []), ([(0w0,0w8,29),
(0w14,0w31,29),
(0w33,0w33,29),
(0w35,0w35,29),
(0w38,0w39,29),
(0w41,0w41,29),
(0w46,0w46,29),
(0w48,0w57,29),
(0w60,0w60,29),
(0w62,0w62,29),
(0w92,0w92,29),
(0w94,0w96,29),
(0w123,0w123,29),
(0w125,0w2147483647,29),
(0w9,0w12,30),
(0w32,0w32,30),
(0w13,0w13,31),
(0w34,0w34,32),
(0w36,0w36,33),
(0w37,0w37,34),
(0w40,0w40,35),
(0w42,0w42,36),
(0w43,0w43,37),
(0w44,0w44,38),
(0w45,0w45,39),
(0w47,0w47,40),
(0w58,0w58,41),
(0w59,0w59,42),
(0w61,0w61,43),
(0w63,0w63,44),
(0w64,0w64,45),
(0w65,0w90,46),
(0w97,0w110,46),
(0w112,0w122,46),
(0w91,0w91,47),
(0w93,0w93,48),
(0w111,0w111,49),
(0w124,0w124,50)], []), ([(0w0,0w2147483647,29)], []), ([(0w0,0w8,29),
(0w14,0w31,29),
(0w33,0w33,29),
(0w35,0w38,29),
(0w43,0w43,29),
(0w46,0w57,29),
(0w60,0w60,29),
(0w62,0w64,29),
(0w91,0w96,29),
(0w126,0w2147483647,29),
(0w9,0w12,151),
(0w32,0w32,151),
(0w13,0w13,152),
(0w34,0w34,153),
(0w39,0w39,154),
(0w40,0w40,155),
(0w41,0w41,156),
(0w42,0w42,157),
(0w44,0w44,158),
(0w45,0w45,159),
(0w58,0w58,160),
(0w59,0w59,161),
(0w61,0w61,162),
(0w65,0w90,163),
(0w97,0w99,163),
(0w101,0w110,163),
(0w112,0w122,163),
(0w100,0w100,164),
(0w111,0w111,165),
(0w123,0w123,166),
(0w124,0w124,167),
(0w125,0w125,168)], []), ([(0w0,0w8,29),
(0w14,0w31,29),
(0w33,0w33,29),
(0w35,0w35,29),
(0w38,0w39,29),
(0w46,0w46,29),
(0w48,0w57,29),
(0w60,0w60,29),
(0w62,0w62,29),
(0w92,0w92,29),
(0w94,0w96,29),
(0w123,0w123,29),
(0w125,0w2147483647,29),
(0w9,0w12,30),
(0w32,0w32,30),
(0w13,0w13,31),
(0w34,0w34,179),
(0w36,0w36,33),
(0w37,0w37,34),
(0w40,0w40,180),
(0w41,0w41,181),
(0w42,0w42,36),
(0w43,0w43,37),
(0w44,0w44,38),
(0w45,0w45,39),
(0w47,0w47,40),
(0w58,0w58,41),
(0w59,0w59,42),
(0w61,0w61,43),
(0w63,0w63,44),
(0w64,0w64,45),
(0w65,0w90,46),
(0w97,0w110,46),
(0w112,0w122,46),
(0w91,0w91,47),
(0w93,0w93,48),
(0w111,0w111,49),
(0w124,0w124,50)], []), ([], [44, 75]), ([(0w10,0w10,13)], [44, 75]), ([(0w42,0w42,12)], [44, 75]), ([(0w41,0w41,11)], [44, 75]), ([], [43]), ([], [42]), ([], [44]), ([(0w0,0w33,19),
(0w35,0w39,19),
(0w42,0w2147483647,19)], [50, 75]), ([], [49, 75]), ([(0w42,0w42,18)], [47, 75]), ([], [48, 75]), ([], [41]), ([(0w0,0w33,19),
(0w35,0w39,19),
(0w42,0w2147483647,19)], [50]), ([(0w0,0w9,28),
(0w11,0w12,28),
(0w14,0w33,28),
(0w35,0w91,28),
(0w93,0w2147483647,28)], [55, 75]), ([], [52, 75]), ([(0w10,0w10,27)], [52, 75]), ([], [51, 75]), ([(0w34,0w34,25),
(0w92,0w92,26)], [53, 75]), ([], [56]), ([], [54]), ([], [52]), ([(0w0,0w9,28),
(0w11,0w12,28),
(0w14,0w33,28),
(0w35,0w91,28),
(0w93,0w2147483647,28)], [55]), ([], [75]), ([(0w9,0w12,149),
(0w32,0w32,149),
(0w13,0w13,150)], [1, 75]), ([(0w9,0w12,149),
(0w32,0w32,149),
(0w13,0w13,150)], [1, 75]), ([], [46, 75]), ([], [22, 75]), ([(0w99,0w99,56),
(0w100,0w100,57),
(0w101,0w101,58),
(0w104,0w104,59),
(0w105,0w105,60),
(0w107,0w107,61),
(0w110,0w110,62),
(0w112,0w112,63),
(0w114,0w114,64),
(0w115,0w115,65),
(0w116,0w116,66),
(0w118,0w118,67),
(0w119,0w119,68)], [75]), ([(0w42,0w42,55)], [45, 75]), ([], [24, 75]), ([], [23, 75]), ([], [28, 75]), ([(0w62,0w62,54)], [75]), ([], [33, 75]), ([], [26, 75]), ([], [27, 75]), ([(0w62,0w62,53)], [34, 75]), ([], [25, 75]), ([], [21, 75]), ([(0w39,0w39,51),
(0w48,0w57,51),
(0w65,0w90,51),
(0w95,0w95,51),
(0w97,0w122,51)], [2, 75]), ([], [31, 75]), ([], [32, 75]), ([(0w39,0w39,51),
(0w48,0w57,51),
(0w65,0w90,51),
(0w95,0w95,51),
(0w97,0w101,51),
(0w103,0w122,51),
(0w102,0w102,52)], [2, 75]), ([], [20, 75]), ([(0w39,0w39,51),
(0w48,0w57,51),
(0w65,0w90,51),
(0w95,0w95,51),
(0w97,0w122,51)], [2]), ([(0w39,0w39,51),
(0w48,0w57,51),
(0w65,0w90,51),
(0w95,0w95,51),
(0w97,0w122,51)], [0, 2]), ([], [36]), ([], [35]), ([], [40]), ([(0w104,0w104,144)], []), ([(0w101,0w101,134),
(0w114,0w114,135)], []), ([(0w110,0w110,130)], []), ([(0w101,0w101,125)], []), ([(0w109,0w109,120)], []), ([(0w101,0w101,113)], []), ([(0w97,0w97,103),
(0w111,0w111,104)], []), ([(0w114,0w114,98)], []), ([(0w101,0w101,92)], []), ([(0w116,0w116,88)], []), ([(0w111,0w111,77),
(0w114,0w114,78)], []), ([(0w97,0w97,73)], []), ([(0w104,0w104,69)], []), ([(0w101,0w101,70)], []), ([(0w114,0w114,71)], []), ([(0w101,0w101,72)], []), ([], [19]), ([(0w108,0w108,74)], []), ([(0w117,0w117,75)], []), ([(0w101,0w101,76)], []), ([], [18]), ([(0w107,0w107,80)], []), ([(0w121,0w121,79)], []), ([], [17]), ([(0w101,0w101,81)], []), ([(0w110,0w110,82)], []), ([(0w115,0w115,83),
(0w116,0w116,84)], [15]), ([], [15]), ([(0w121,0w121,85)], []), ([(0w112,0w112,86)], []), ([(0w101,0w101,87)], []), ([], [16]), ([(0w97,0w97,89)], []), ([(0w114,0w114,90)], []), ([(0w116,0w116,91)], []), ([], [14]), ([(0w102,0w102,93)], []), ([(0w99,0w99,94)], []), ([(0w101,0w101,95)], []), ([(0w108,0w108,96)], []), ([(0w108,0w108,97)], []), ([], [13]), ([(0w101,0w101,99)], []), ([(0w102,0w102,100)], []), ([(0w101,0w101,101)], []), ([(0w114,0w114,102)], []), ([], [12]), ([(0w109,0w109,111)], []), ([(0w110,0w110,105)], []), ([(0w116,0w116,106)], []), ([(0w101,0w101,107)], []), ([(0w114,0w114,108)], []), ([(0w109,0w109,109)], []), ([(0w115,0w115,110)], [11]), ([], [11]), ([(0w101,0w101,112)], []), ([], [10]), ([(0w121,0w121,114)], []), ([(0w119,0w119,115)], []), ([(0w111,0w111,116)], []), ([(0w114,0w114,117)], []), ([(0w100,0w100,118)], []), ([(0w115,0w115,119)], [9]), ([], [9]), ([(0w112,0w112,121)], []), ([(0w111,0w111,122)], []), ([(0w114,0w114,123)], []), ([(0w116,0w116,124)], []), ([], [8]), ([(0w97,0w97,126)], []), ([(0w100,0w100,127)], []), ([(0w101,0w101,128)], []), ([(0w114,0w114,129)], []), ([], [7]), ([(0w116,0w116,131)], []), ([(0w114,0w114,132)], []), ([(0w121,0w121,133)], []), ([], [6]), ([(0w102,0w102,142)], []), ([(0w111,0w111,136)], []), ([(0w112,0w112,137)], []), ([(0w112,0w112,138)], []), ([(0w105,0w105,139)], []), ([(0w110,0w110,140)], []), ([(0w103,0w103,141)], []), ([], [5]), ([(0w115,0w115,143)], []), ([], [4]), ([(0w97,0w97,145)], []), ([(0w110,0w110,146)], []), ([(0w103,0w103,147)], []), ([(0w101,0w101,148)], []), ([], [3]), ([(0w9,0w12,149),
(0w32,0w32,149),
(0w13,0w13,150)], [1]), ([(0w9,0w12,149),
(0w32,0w32,149),
(0w13,0w13,150)], [1]), ([], [57, 75]), ([(0w10,0w10,178)], [57, 75]), ([], [73, 75]), ([(0w39,0w39,177),
(0w48,0w57,177),
(0w65,0w90,177),
(0w95,0w95,177),
(0w97,0w122,177)], [60, 75]), ([(0w42,0w42,176)], [68, 75]), ([], [69, 75]), ([], [64, 75]), ([], [66, 75]), ([(0w62,0w62,175)], [75]), ([], [65, 75]), ([], [67, 75]), ([], [74, 75]), ([(0w39,0w39,169),
(0w48,0w57,169),
(0w65,0w90,169),
(0w95,0w95,169),
(0w97,0w122,169),
(0w46,0w46,170)], [59, 75]), ([(0w39,0w39,169),
(0w48,0w57,169),
(0w65,0w90,169),
(0w95,0w95,169),
(0w97,0w104,169),
(0w106,0w122,169),
(0w46,0w46,170),
(0w105,0w105,172)], [59, 75]), ([(0w39,0w39,169),
(0w48,0w57,169),
(0w65,0w90,169),
(0w95,0w95,169),
(0w97,0w101,169),
(0w103,0w122,169),
(0w46,0w46,170),
(0w102,0w102,171)], [59, 75]), ([], [70, 75]), ([], [63, 75]), ([], [71, 75]), ([(0w39,0w39,169),
(0w48,0w57,169),
(0w65,0w90,169),
(0w95,0w95,169),
(0w97,0w122,169),
(0w46,0w46,170)], [59]), ([], [61]), ([(0w39,0w39,169),
(0w48,0w57,169),
(0w65,0w90,169),
(0w95,0w95,169),
(0w97,0w122,169),
(0w46,0w46,170)], [58, 59]), ([(0w39,0w39,169),
(0w48,0w57,169),
(0w65,0w90,169),
(0w95,0w95,169),
(0w97,0w102,169),
(0w104,0w122,169),
(0w46,0w46,170),
(0w103,0w103,173)], [59]), ([(0w39,0w39,169),
(0w48,0w57,169),
(0w65,0w90,169),
(0w95,0w95,169),
(0w97,0w104,169),
(0w106,0w122,169),
(0w46,0w46,170),
(0w105,0w105,174)], [59]), ([(0w39,0w39,169),
(0w48,0w57,169),
(0w65,0w90,169),
(0w95,0w95,169),
(0w97,0w115,169),
(0w117,0w122,169),
(0w46,0w46,170),
(0w116,0w116,174)], [59, 62]), ([], [72]), ([], [39]), ([(0w39,0w39,177),
(0w48,0w57,177),
(0w65,0w90,177),
(0w95,0w95,177),
(0w97,0w122,177)], [60]), ([], [57]), ([], [37, 75]), ([(0w42,0w42,182)], [29, 75]), ([], [30, 75]), ([], [38])]
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
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CONSTR; Tok.OF)
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;  skip())
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_change)
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN PRECODE; Tok.KW_defs)
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_dropping)
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_entry)
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN PRECODE; Tok.KW_header)
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_import)
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_keywords)
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_name)
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_nonterms)
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_prefer)
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CONSTR; Tok.KW_refcell)
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_start)
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CONSTR; Tok.KW_tokens)
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN CONSTR; Tok.KW_tokentype)
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.KW_try)
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN PRECODE; Tok.KW_value)
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN PRECODE; Tok.KW_where)
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.BAR)
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN PRECODE; Tok.AT)
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.DOLLAR)
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.PLUS)
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.STAR)
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.QUERY)
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COLON)
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.SEMI)
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LP)
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RP)
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LSB)
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RSB)
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.SLASH)
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.EQ)
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.ARROW)
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN PRECODE; Tok.DARROW)
fun yyAction37 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN INITIAL);
		    Tok.STRING (getText())
      end
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM;
	    ignore(continue() before YYBEGIN INITIAL);
	    continue()
      end
fun yyAction39 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM;
	    ignore(continue() before YYBEGIN CONSTR);
	    continue()
      end
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM;
	    ignore(continue() before YYBEGIN PRECODE);
	    continue()
      end
fun yyAction41 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
         comLvl := 1; comStart := !yylineno; YYBEGIN COM;
	    ignore(continue() before YYBEGIN CODE);
	    continue()
      end
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
       comLvl := !comLvl+1; continue())
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
       comLvl := !comLvl-1;
	    if (!comLvl = 0)
	      then (Tok.BOGUS)
	      else continue())
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;  continue())
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
       pcount := 1; YYBEGIN CODE; clrText(); continue())
fun yyAction46 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN PRECODE);
		    Tok.STRING (getText())
      end
fun yyAction47 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext;  (* NOTE: the initial "(" is consumed in the PRECODE state *)
		    inc pcount; continue()
      end
fun yyAction48 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         dec pcount;
		    if !pcount = 0
		      then (YYBEGIN INITIAL; Tok.CODE (getText()))
		      else (addText yytext; continue())
      end
fun yyAction49 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; YYBEGIN STRING;
		    ignore(continue() before YYBEGIN CODE);
		    continue()
      end
fun yyAction50 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction51 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; Tok.BOGUS
      end
fun yyAction52 (strm, lastMatch : yymatch) = let
      val yycolno = ref(yygetcolNo(!(yystrm)))
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         addText yytext; err (!yylineno, !yycolno, "unclosed string");
 	            Tok.BOGUS
      end
fun yyAction53 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction54 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction55 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText(String.toString yytext); continue()
      end
fun yyAction56 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addText yytext; continue()
      end
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;  continue())
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.OF)
fun yyAction59 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.ID yytext
      end
fun yyAction60 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.TYVAR yytext
      end
fun yyAction61 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.IDDOT yytext
      end
fun yyAction62 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.INT yytext
      end
fun yyAction63 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.BAR)
fun yyAction64 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.STAR)
fun yyAction65 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COLON)
fun yyAction66 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.COMMA)
fun yyAction67 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.SEMI)
fun yyAction68 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LP)
fun yyAction69 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RP)
fun yyAction70 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LCB)
fun yyAction71 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RCB)
fun yyAction72 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.ARROW)
fun yyAction73 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN CONSTR);
		    Tok.STRING (getText())
      end
fun yyAction74 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN PRECODE; Tok.EQ)
fun yyAction75 (strm, lastMatch : yymatch) = let
      val yycolno = ref(yygetcolNo(!(yystrm)))
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         err (!yylineno, !yycolno,
		 concat["illegal character '",
			String.toCString yytext, "'"]);
	    continue()
      end
val yyactTable = Vector.fromList([yyAction0, yyAction1, yyAction2, yyAction3,
  yyAction4, yyAction5, yyAction6, yyAction7, yyAction8, yyAction9, yyAction10,
  yyAction11, yyAction12, yyAction13, yyAction14, yyAction15, yyAction16,
  yyAction17, yyAction18, yyAction19, yyAction20, yyAction21, yyAction22,
  yyAction23, yyAction24, yyAction25, yyAction26, yyAction27, yyAction28,
  yyAction29, yyAction30, yyAction31, yyAction32, yyAction33, yyAction34,
  yyAction35, yyAction36, yyAction37, yyAction38, yyAction39, yyAction40,
  yyAction41, yyAction42, yyAction43, yyAction44, yyAction45, yyAction46,
  yyAction47, yyAction48, yyAction49, yyAction50, yyAction51, yyAction52,
  yyAction53, yyAction54, yyAction55, yyAction56, yyAction57, yyAction58,
  yyAction59, yyAction60, yyAction61, yyAction62, yyAction63, yyAction64,
  yyAction65, yyAction66, yyAction67, yyAction68, yyAction69, yyAction70,
  yyAction71, yyAction72, yyAction73, yyAction74, yyAction75])
in
  if ULexBuffer.eof(!(yystrm))
    then let
      val yycolno = ref(yygetcolNo(!(yystrm)))
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        (case (!(yyss))
         of _ => (UserDeclarations.eof())
        (* end case *))
      end
    else (case (!(yyss))
       of COM => yygo yyactTable (0, !(yystrm), yyNO_MATCH)
        | CODE => yygo yyactTable (1, !(yystrm), yyNO_MATCH)
        | STRING => yygo yyactTable (2, !(yystrm), yyNO_MATCH)
        | PRECODE => yygo yyactTable (3, !(yystrm), yyNO_MATCH)
        | PRECONSTR => yygo yyactTable (4, !(yystrm), yyNO_MATCH)
        | CONSTR => yygo yyactTable (5, !(yystrm), yyNO_MATCH)
        | INITIAL => yygo yyactTable (6, !(yystrm), yyNO_MATCH)
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

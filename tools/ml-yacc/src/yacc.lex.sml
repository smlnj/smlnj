
functor LexMLYACC(structure Tokens : Mlyacc_TOKENS
		  structure Hdr : HEADER (* = Header *)
		    where type prec = Header.prec
		      and type inputSource = Header.inputSource) : ARG_LEXER
  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
A | F | CODE | STRING | COMMENT | EMPTYCOMMENT | INITIAL
    structure UserDeclarations = 
      struct

(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi

   yacc.lex: Lexer specification
 *)

structure Tokens = Tokens
type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

type lexarg = Hdr.inputSource
type arg = lexarg

open Tokens
val error = Hdr.error
val lineno = Hdr.lineno
val text = Hdr.text

val pcount = ref 0
val commentLevel = ref 0
val actionstart = ref 0

val eof = fn i => (if (!pcount)>0 then
			error i (!actionstart)
			      " eof encountered in action beginning here !"
		   else (); EOF(!lineno,!lineno))

val Add = fn s => (text := s::(!text))


local val dict = [("%prec",PREC_TAG),("%term",TERM),
	       ("%nonterm",NONTERM), ("%eop",PERCENT_EOP),("%start",START),
	       ("%prefer",PREFER),("%subst",SUBST),("%change",CHANGE),
	       ("%keyword",KEYWORD),("%name",NAME),
	       ("%verbose",VERBOSE), ("%nodefault",NODEFAULT),
	       ("%value",VALUE), ("%noshift",NOSHIFT),
	       ("%header",PERCENT_HEADER),("%pure",PERCENT_PURE),
	       ("%token_sig_info",PERCENT_TOKEN_SIG_INFO),
	       ("%arg",PERCENT_ARG),
	       ("%pos",PERCENT_POS)]
in
fun lookup (s,left,right) = let
       fun f ((a,d)::b) = if a=s then d(left,right) else f b
	 | f nil = UNKNOWN(s,left,right)
       in
	  f dict
       end
end

fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as (inputSource)) () = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
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
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Add yytext; YYBEGIN COMMENT; commentLevel := 1;
		    continue(); YYBEGIN INITIAL; continue())
      end
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN EMPTYCOMMENT; commentLevel := 1; continue()))
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Add yytext; YYBEGIN COMMENT; commentLevel := 1;
		    continue(); YYBEGIN CODE; continue())
      end
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; continue())
      end
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN A; HEADER (concat (rev (!text)),!lineno,!lineno)))
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; inc lineno; continue())
      end
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; continue())
      end
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (inc lineno; continue ()))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (OF(!lineno,!lineno)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (FOR(!lineno,!lineno)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (LBRACE(!lineno,!lineno)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (RBRACE(!lineno,!lineno)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (COMMA(!lineno,!lineno)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (ASTERISK(!lineno,!lineno)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (ARROW(!lineno,!lineno)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (PREC(Hdr.LEFT,!lineno,!lineno)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (PREC(Hdr.RIGHT,!lineno,!lineno)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (PREC(Hdr.NONASSOC,!lineno,!lineno)))
fun yyAction19 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (lookup(yytext,!lineno,!lineno))
      end
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (TYVAR(yytext,!lineno,!lineno))
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (IDDOT(yytext,!lineno,!lineno))
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (INT (yytext,!lineno,!lineno))
      end
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (DELIMITER(!lineno,!lineno)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (COLON(!lineno,!lineno)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (BAR(!lineno,!lineno)))
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (ID ((yytext,!lineno),!lineno,!lineno))
      end
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (pcount := 1; actionstart := (!lineno);
		    text := nil; YYBEGIN CODE; continue() before YYBEGIN A))
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (UNKNOWN(yytext,!lineno,!lineno))
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (inc pcount; Add yytext; continue())
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (dec pcount;
		    if !pcount = 0 then
			 PROG (concat (rev (!text)),!lineno,!lineno)
		    else (Add yytext; continue()))
      end
fun yyAction31 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; YYBEGIN STRING; continue())
      end
fun yyAction32 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; continue())
      end
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; continue())
      end
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Add yytext; dec commentLevel;
		    if !commentLevel=0
			 then BOGUS_VALUE(!lineno,!lineno)
			 else continue()
		   )
      end
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; inc commentLevel; continue())
      end
fun yyAction36 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; continue())
      end
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (dec commentLevel;
		          if !commentLevel=0 then YYBEGIN A else ();
			  continue ()))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (inc commentLevel; continue()))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction41 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; YYBEGIN CODE; continue())
      end
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; continue())
      end
fun yyAction43 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Add yytext; error inputSource (!lineno) "unclosed string";
 	            inc lineno; YYBEGIN CODE; continue())
      end
fun yyAction44 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; continue())
      end
fun yyAction45 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; continue())
      end
fun yyAction46 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; inc lineno; YYBEGIN F; continue())
      end
fun yyAction47 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; YYBEGIN F; continue())
      end
fun yyAction48 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; continue())
      end
fun yyAction49 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Add yytext; YYBEGIN STRING; continue())
      end
fun yyAction50 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Add yytext; error inputSource (!lineno) "unclosed string";
		    YYBEGIN CODE; continue())
      end
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ91(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"%"
              then yyQ92(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ56(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ93(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ93(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyAction3(strm, yyNO_MATCH)
                      else yyQ93(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp = #"\r"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ93(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"&"
              then yyQ93(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"&"
              then if inp = #"%"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ93(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"("
              then yyAction3(strm, yyNO_MATCH)
              else yyQ93(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ93(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ93(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyAction3(strm, yyNO_MATCH)
                      else yyQ93(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp = #"\r"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ93(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"&"
              then yyQ93(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"&"
              then if inp = #"%"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ93(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"("
              then yyAction3(strm, yyNO_MATCH)
              else yyQ93(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ87(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ87(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ56(strm', lastMatch)
                      else yyQ87(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ88(strm', lastMatch)
                  else yyQ87(strm', lastMatch)
            else if inp = #"&"
              then yyQ87(strm', lastMatch)
            else if inp < #"&"
              then if inp = #"%"
                  then yyQ89(strm', lastMatch)
                  else yyQ87(strm', lastMatch)
            else if inp = #"("
              then yyQ90(strm', lastMatch)
              else yyQ87(strm', lastMatch)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ85(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ86(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ56(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyAction40(strm, yyNO_MATCH)
            else if inp < #"\r"
              then if inp = #"\n"
                  then yyAction40(strm, yyNO_MATCH)
                  else yyQ81(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp = #"("
              then yyAction40(strm, yyNO_MATCH)
            else if inp < #"("
              then yyQ81(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp <= #"*"
              then yyAction40(strm, yyNO_MATCH)
              else yyQ81(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ81(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ81(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ56(strm', lastMatch)
                      else yyQ81(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ61(strm', lastMatch)
                  else yyQ81(strm', lastMatch)
            else if inp = #")"
              then yyQ83(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ82(strm', lastMatch)
                  else yyQ81(strm', lastMatch)
            else if inp = #"*"
              then yyQ84(strm', lastMatch)
              else yyQ81(strm', lastMatch)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ79(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ80(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyAction36(strm, yyNO_MATCH)
            else if inp < #"\r"
              then if inp = #"\n"
                  then yyAction36(strm, yyNO_MATCH)
                  else yyQ75(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp = #"("
              then yyAction36(strm, yyNO_MATCH)
            else if inp < #"("
              then yyQ75(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp <= #"*"
              then yyAction36(strm, yyNO_MATCH)
              else yyQ75(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ75(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ75(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ56(strm', lastMatch)
                      else yyQ75(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ61(strm', lastMatch)
                  else yyQ75(strm', lastMatch)
            else if inp = #")"
              then yyQ77(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ76(strm', lastMatch)
                  else yyQ75(strm', lastMatch)
            else if inp = #"*"
              then yyQ78(strm', lastMatch)
              else yyQ75(strm', lastMatch)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ72(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\n"
                  then yyQ72(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ71(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                      else yyAction42(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ73(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp = #"!"
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ71(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp = #"\""
              then yyQ74(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ67(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ66(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ66(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyAction44(strm, yyNO_MATCH)
                      else yyQ66(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                else if inp = #"\r"
                  then yyAction44(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
            else if inp = #"#"
              then yyQ66(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #"\""
                  then yyAction44(strm, yyNO_MATCH)
                  else yyQ66(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
            else if inp = #"\\"
              then yyAction44(strm, yyNO_MATCH)
              else yyQ66(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ66(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ66(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ67(strm', lastMatch)
                      else yyQ66(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ68(strm', lastMatch)
                  else yyQ66(strm', lastMatch)
            else if inp = #"#"
              then yyQ66(strm', lastMatch)
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ69(strm', lastMatch)
                  else yyQ66(strm', lastMatch)
            else if inp = #"\\"
              then yyQ70(strm', lastMatch)
              else yyQ66(strm', lastMatch)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ65(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ60(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ60(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyAction32(strm, yyNO_MATCH)
                      else yyQ60(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                else if inp = #"\r"
                  then yyAction32(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp = #"#"
              then yyQ60(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #"\""
                  then yyAction32(strm, yyNO_MATCH)
                  else yyQ60(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp = #"("
              then yyAction32(strm, yyNO_MATCH)
            else if inp < #"("
              then yyQ60(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp <= #")"
              then yyAction32(strm, yyNO_MATCH)
              else yyQ60(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ62(strm', lastMatch)
            else if inp < #"\""
              then if inp = #"\v"
                  then yyQ60(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ56(strm', lastMatch)
                      else yyQ60(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ61(strm', lastMatch)
                  else yyQ60(strm', lastMatch)
            else if inp = #")"
              then yyQ64(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ63(strm', lastMatch)
                  else yyQ60(strm', lastMatch)
              else yyQ60(strm', lastMatch)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ56(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction48(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ59(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                  else yyAction48(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ59(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
              else yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction48(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ59(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                  else yyAction48(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ59(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
              else yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ54(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\n"
                  then yyQ56(strm', lastMatch)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ55(strm', lastMatch)
                      else yyQ54(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ57(strm', lastMatch)
                  else yyQ54(strm', lastMatch)
            else if inp = #"!"
              then yyQ54(strm', lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ55(strm', lastMatch)
                  else yyQ54(strm', lastMatch)
            else if inp = #"\\"
              then yyQ58(strm', lastMatch)
              else yyQ54(strm', lastMatch)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction26(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"."
                  then yyQ26(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"."
                  then if inp = #"'"
                      then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                      else yyAction26(strm, yyNO_MATCH)
                else if inp = #"/"
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp = #"_"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"A"
                  then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"a"
              then yyAction26(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"."
                  then yyQ26(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp < #"."
                  then if inp = #"'"
                      then yyQ25(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                      else yyAction9(strm, yyNO_MATCH)
                else if inp = #"/"
                  then yyAction9(strm, yyNO_MATCH)
                  else yyQ25(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp = #"_"
              then yyQ25(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"A"
                  then yyQ25(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction9(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ25(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ25(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp < #"a"
              then yyAction9(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ25(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"."
                  then yyQ26(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"."
                  then if inp = #"'"
                      then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                      else yyAction26(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"_"
                  then if inp <= #"Z"
                      then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                      else yyAction26(strm, yyNO_MATCH)
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"g"
              then if inp = #"f"
                  then yyQ27(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"."
                  then yyQ26(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp < #"."
                  then if inp = #"'"
                      then yyQ25(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                      else yyAction10(strm, yyNO_MATCH)
                else if inp = #"/"
                  then yyAction10(strm, yyNO_MATCH)
                  else yyQ25(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = #"_"
              then yyQ25(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"A"
                  then yyQ25(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction10(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ25(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ25(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < #"a"
              then yyAction10(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ25(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"."
                  then yyQ26(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"."
                  then if inp = #"'"
                      then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                      else yyAction26(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"_"
                  then if inp <= #"Z"
                      then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                      else yyAction26(strm, yyNO_MATCH)
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ29(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"."
                  then yyQ26(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"."
                  then if inp = #"'"
                      then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                      else yyAction26(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"_"
                  then if inp <= #"Z"
                      then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                      else yyAction26(strm, yyNO_MATCH)
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ28(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction26(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"."
                  then yyQ26(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"."
                  then if inp = #"'"
                      then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                      else yyAction26(strm, yyNO_MATCH)
                else if inp = #"/"
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp = #"_"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"A"
                  then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"a"
              then yyAction26(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ25(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ30(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp < #"0"
              then yyAction22(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ30(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ30(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp < #"0"
              then yyAction22(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ30(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ31(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ32(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ33(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ33(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ33(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ33(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ33(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ33(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ33(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ33(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ33(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ33(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ33(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ33(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ33(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ33(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction17(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"t"
                  then yyQ42(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"h"
                  then yyQ41(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"h"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"h"
              then if inp = #"g"
                  then yyQ40(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"j"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"j"
              then if inp = #"i"
                  then yyQ39(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction18(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"d"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"d"
              then if inp = #"c"
                  then yyQ49(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ48(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ47(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ46(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ45(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ44(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ43(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction16(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"t"
                  then yyQ52(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"g"
              then if inp = #"f"
                  then yyQ51(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ50(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"l"
              then yyQ36(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"_"
                  then yyQ35(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"_"
                  then if inp = #"%"
                      then yyQ34(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ35(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = #"r"
              then yyQ38(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"n"
                  then yyQ37(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ9(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ53(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ53(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ53(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ53(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ16(strm', lastMatch)
            else if inp < #"-"
              then if inp = #"%"
                  then yyQ11(strm', lastMatch)
                else if inp < #"%"
                  then if inp = #"\r"
                      then yyQ10(strm', lastMatch)
                    else if inp < #"\r"
                      then if inp = #"\n"
                          then yyQ9(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ8(strm', lastMatch)
                              else yyQ7(strm', lastMatch)
                          else yyQ7(strm', lastMatch)
                    else if inp = #" "
                      then yyQ8(strm', lastMatch)
                      else yyQ7(strm', lastMatch)
                else if inp = #")"
                  then yyQ7(strm', lastMatch)
                else if inp < #")"
                  then if inp = #"'"
                      then yyQ12(strm', lastMatch)
                    else if inp = #"&"
                      then yyQ7(strm', lastMatch)
                      else yyQ13(strm', lastMatch)
                else if inp = #"+"
                  then yyQ7(strm', lastMatch)
                else if inp = #"*"
                  then yyQ14(strm', lastMatch)
                  else yyQ15(strm', lastMatch)
            else if inp = #"f"
              then yyQ20(strm', lastMatch)
            else if inp < #"f"
              then if inp = #";"
                  then yyQ7(strm', lastMatch)
                else if inp < #";"
                  then if inp = #"0"
                      then yyQ17(strm', lastMatch)
                    else if inp < #"0"
                      then yyQ7(strm', lastMatch)
                    else if inp = #":"
                      then yyQ18(strm', lastMatch)
                      else yyQ17(strm', lastMatch)
                else if inp = #"["
                  then yyQ7(strm', lastMatch)
                else if inp < #"["
                  then if inp <= #"@"
                      then yyQ7(strm', lastMatch)
                      else yyQ19(strm', lastMatch)
                else if inp <= #"`"
                  then yyQ7(strm', lastMatch)
                  else yyQ19(strm', lastMatch)
            else if inp = #"{"
              then yyQ22(strm', lastMatch)
            else if inp < #"{"
              then if inp = #"o"
                  then yyQ21(strm', lastMatch)
                  else yyQ19(strm', lastMatch)
            else if inp = #"}"
              then yyQ24(strm', lastMatch)
            else if inp = #"|"
              then yyQ23(strm', lastMatch)
              else yyQ7(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of A => yyQ0(!(yystrm), yyNO_MATCH)
    | F => yyQ1(!(yystrm), yyNO_MATCH)
    | CODE => yyQ2(!(yystrm), yyNO_MATCH)
    | STRING => yyQ3(!(yystrm), yyNO_MATCH)
    | COMMENT => yyQ4(!(yystrm), yyNO_MATCH)
    | EMPTYCOMMENT => yyQ5(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ6(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end

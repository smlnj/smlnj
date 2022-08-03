functor BurgLexFun(structure Tokens : Burg_TOKENS)  = struct

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

	fun eof (Stream {strm, ...}) = TSIO.endOfStream strm

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
DUMP | POSTLUDE | COMMENT | INITIAL
    structure UserDeclarations = 
      struct

(* burg-lex
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * ML-Lex specification for ML-burg.
 *)

structure T 		= Tokens
structure E		= ErrorMsg
type pos 		= int
type svalue		= T.svalue
type ('a,'b) token 	= ('a,'b) T.token
type lexresult		= (svalue,pos) token

val comLevel		= ref 0
val lineNum		= ref 0
val verbatimLevel	= ref 0
val percentCount	= ref 0
val rawLine		= ref ""
val rawNoNewLine	= ref false
val raw:string list ref = ref []
val reachedEop		= ref false

fun resetState()	= (comLevel      := 0;
			   lineNum       := 0;
			   verbatimLevel := 0;
			   percentCount  := 0;
			   rawLine	 := "";
			   rawNoNewLine	 := false;
			   raw		 := [];
			   reachedEop	 := false)
			   
fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)

fun incVerbLvl()	= if !verbatimLevel <> 0 
			  then E.impossible "nested verbatim levels"
			  else inc verbatimLevel

fun outputRaw (s:string) = (rawLine := !rawLine^s; rawNoNewLine := true)

fun rawNextLine ()	= (raw := !rawLine^"\n":: (!raw);
			   rawLine := ""; rawNoNewLine := false)

fun rawStop ()		= if !rawNoNewLine then rawNextLine () else ()

fun eof()		= (if !comLevel > 0 then E.complain "unclosed comment"
			   else if !verbatimLevel <> 0 then
				   E.complain "unclosed user input"
			        else ();
			   if !reachedEop 
			   then T.K_EOF(!lineNum,!lineNum)
			   else	(rawStop ();
				 T.PPERCENT(rev(!raw),!lineNum,!lineNum)
				before (raw := [];
				        reachedEop := true)))



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
#[
]

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
(yyarg as ()) = let 
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
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (inc lineNum; continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (incVerbLvl(); YYBEGIN DUMP; continue()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (inc percentCount; 
			    if !percentCount = 2 
			    then (YYBEGIN POSTLUDE; continue())
			    else T.PPERCENT(rev(!raw),!lineNum,!lineNum)
					before raw := []))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (inc lineNum; continue()))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.K_LPAREN(!lineNum,!lineNum)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.K_RPAREN(!lineNum,!lineNum)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.K_COMMA(!lineNum,!lineNum)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.K_COLON(!lineNum,!lineNum)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.K_SEMICOLON(!lineNum,!lineNum)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.K_EQUAL(!lineNum,!lineNum)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.K_PIPE(!lineNum,!lineNum)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.K_TERM(!lineNum,!lineNum)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.K_START(!lineNum,!lineNum)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.K_TERMPREFIX(!lineNum,!lineNum)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.K_RULEPREFIX(!lineNum,!lineNum)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.K_SIG(!lineNum,!lineNum)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT; comLevel:=1; continue()))
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (T.INT(valOf(Int.fromString yytext),!lineNum,!lineNum))
      end
fun yyAction19 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (T.ID(yytext,!lineNum,!lineNum))
      end
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (inc comLevel; continue()))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (inc lineNum; continue()))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (dec comLevel;
			    if !comLevel=0 then YYBEGIN INITIAL else ();
			    continue()))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (rawStop(); dec verbatimLevel;
			    YYBEGIN INITIAL; continue()))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (rawNextLine (); inc lineNum; continue()))
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (outputRaw yytext; continue())
      end
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (rawNextLine (); inc lineNum; continue()))
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (outputRaw yytext; continue())
      end
fun yyQ27 (strm, lastMatch : yymatch) = yyAction11(strm, yyNO_MATCH)
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction19(strm, yyNO_MATCH)
                      else yyQ26(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction19(strm, yyNO_MATCH)
                  else yyQ26(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp = #"`"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ26(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ26(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ24 (strm, lastMatch : yymatch) = yyAction9(strm, yyNO_MATCH)
fun yyQ23 (strm, lastMatch : yymatch) = yyAction8(strm, yyNO_MATCH)
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ22(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"0"
              then yyAction18(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ22(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ20 (strm, lastMatch : yymatch) = yyAction6(strm, yyNO_MATCH)
fun yyQ28 (strm, lastMatch : yymatch) = yyAction17(strm, yyNO_MATCH)
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ28(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = yyAction1(strm, yyNO_MATCH)
fun yyQ42 (strm, lastMatch : yymatch) = yyAction14(strm, yyNO_MATCH)
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"x"
              then yyQ42(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"i"
              then yyQ41(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"f"
              then yyQ40(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ39(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ38(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"p"
              then yyQ37(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"m"
              then yyQ36(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ35(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ34(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = yyAction13(strm, yyNO_MATCH)
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ47(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ46(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ45(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = yyAction16(strm, yyNO_MATCH)
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"g"
              then yyQ48(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"j"
              then yystuck(lastMatch)
            else if inp < #"j"
              then if inp = #"i"
                  then yyQ43(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"t"
              then yyQ44(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = yyAction15(strm, yyNO_MATCH)
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"x"
              then yyQ57(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"i"
              then yyQ56(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"f"
              then yyQ55(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ54(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ53(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"p"
              then yyQ52(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ51(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"l"
              then yyQ50(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"u"
              then yyQ49(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = yyAction2(strm, yyNO_MATCH)
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"s"
              then yyQ31(strm', lastMatch)
            else if inp < #"s"
              then if inp = #"&"
                  then yystuck(lastMatch)
                else if inp < #"&"
                  then if inp = #"%"
                      then yyQ29(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"r"
                  then yyQ30(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"{"
              then yyQ33(strm', lastMatch)
            else if inp < #"{"
              then if inp = #"t"
                  then yyQ32(strm', lastMatch)
                  else yystuck(lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = yyAction0(strm, yyNO_MATCH)
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ16(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ16(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"-"
              then if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yyAction3(strm, yyNO_MATCH)
            else if inp < #"-"
              then if inp = #"%"
                  then yyQ18(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp < #"%"
                  then if inp = #"\v"
                      then if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yyAction3(strm, yyNO_MATCH)
                    else if inp < #"\v"
                      then if inp = #"\t"
                          then yyQ16(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                        else if inp = #"\n"
                          then yyQ17(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                        else if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yyAction3(strm, yyNO_MATCH)
                    else if inp = #" "
                      then yyQ16(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                    else if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yyAction3(strm, yyNO_MATCH)
                else if inp = #")"
                  then yyQ20(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp < #")"
                  then if inp = #"("
                      then yyQ19(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                    else if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yyAction3(strm, yyNO_MATCH)
                else if inp = #","
                  then yyQ21(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = #">"
              then if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yyAction3(strm, yyNO_MATCH)
            else if inp < #">"
              then if inp = #";"
                  then yyQ24(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp < #";"
                  then if inp = #"0"
                      then yyQ22(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                    else if inp < #"0"
                      then if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yyAction3(strm, yyNO_MATCH)
                    else if inp = #":"
                      then yyQ23(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                      else yyQ22(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp = #"<"
                  then if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yyAction3(strm, yyNO_MATCH)
                  else yyQ25(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"a"
              then yyQ26(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"A"
                  then yyQ26(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp < #"A"
                  then if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yyAction3(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ26(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = #"|"
              then yyQ27(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"|"
              then if inp = #"{"
                  then if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yyAction3(strm, yyNO_MATCH)
                  else yyQ26(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = yyAction22(strm, yyNO_MATCH)
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ14(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = yyAction20(strm, yyNO_MATCH)
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ15(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = yyAction21(strm, yyNO_MATCH)
fun yyQ10 (strm, lastMatch : yymatch) = yyAction23(strm, yyNO_MATCH)
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"("
              then yyQ12(strm', lastMatch)
            else if inp < #"("
              then if inp = #"\n"
                  then yyQ11(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp = #"*"
              then yyQ13(strm', lastMatch)
              else yyQ10(strm', lastMatch)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction28(strm, yyNO_MATCH)
              else yyQ8(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ9(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyQ8(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction26(strm, yyNO_MATCH)
              else yyQ4(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction24(strm, yyNO_MATCH)
              else yyQ4(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ4(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyAction26(strm, yyNO_MATCH)
                  else yyQ4(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp = #"}"
              then yyQ7(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyQ4(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = yyAction25(strm, yyNO_MATCH)
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ4(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ5(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyQ4(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp = #"%"
              then yyQ6(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyQ4(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
      (* end case *))
in
  (case (!(yyss))
   of DUMP => yyQ0(!(yystrm), yyNO_MATCH)
    | POSTLUDE => yyQ1(!(yystrm), yyNO_MATCH)
    | COMMENT => yyQ2(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ3(!(yystrm), yyNO_MATCH)
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

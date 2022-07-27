structure MLULexLex  = struct

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
COM | CODE | STRING | CHARSET | CHARCLASS | RESTRING | INITIAL | DIRECTIVE
    structure UserDeclarations = 
      struct

(* ml-ulex-bootstrap.lex
 *
 * COPYRIGHT (c) 2006 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * (With some code borrowed from ml-yacc)
 *)

val comLvl : int ref = ref 0		(* nesting depth of comments *)
val comStart : int ref = ref 0		(* start line of current comment *)

fun eof () = (
      if (!comLvl > 0)
        then print("unclosed comment starting at line " ^ Int.toString(!comStart) ^ "\n")
        else ();
      Tok.EOF)

val text : string list ref = ref []
fun addText s = (text := s::(!text))
fun clrText () = (text := [])
fun getText () = concat (rev (!text))

val pcount = ref 0
fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)

fun chomp s = String.substring (s, 1, String.size s - 2)



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
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN CODE; clrText(); Tok.KW_defs))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN DIRECTIVE; Tok.KW_name))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN DIRECTIVE; Tok.KW_states))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN DIRECTIVE; Tok.KW_let))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN CHARSET; Tok.KW_charset))
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tok.ID yytext)
      end
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.COMMA))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; Tok.SEMI))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; Tok.GT))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; Tok.EQ))
fun yyAction11 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      in
        yystrm := strm; (YYBEGIN INITIAL; REJECT())
      end
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; Tok.UTF8))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; Tok.ASCII7))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; Tok.ASCII8))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; Tok.SEMI))
fun yyAction16 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      in
        yystrm := strm; (YYBEGIN INITIAL; REJECT())
      end
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.BAR))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.AMP))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.DOT))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.DOLLAR))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.PLUS))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.STAR))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.QUERY))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.SEMI))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.LP))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.RP))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN CHARCLASS; Tok.LSB))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.RSB))
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tok.ID (chomp yytext))
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        ((Tok.REPEAT o valOf o Int.fromString o 
		     Substring.string o (Substring.triml 1) o
		     (Substring.trimr 1)o Substring.full) yytext)
      end
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN DIRECTIVE; Tok.LT))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.GT))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.COMMA))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.SLASH))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.EQ))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN CODE; clrText(); Tok.DARROW))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN RESTRING; continue()))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.CARAT))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.DASH))
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (let val c = Char.fromString yytext
            in case c
                of SOME c' => Tok.CHAR c'
		 | NONE => (print (concat [
		     Int.toString (!yylineno), ": unknown escape sequence '", 
		     yytext, "'\n"]);
		     continue())
            end)
      end
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; Tok.RSB))
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tok.CHAR (String.sub (yytext, 0)))
      end
fun yyAction43 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
        (comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN INITIAL);
	    continue())
      end
fun yyAction44 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    addText yytext;
	    ignore(continue() before YYBEGIN CODE);
	    continue())
      end
fun yyAction45 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; comLvl := !comLvl+1; continue())
      end
fun yyAction46 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addText yytext; comLvl := !comLvl-1; 
	    if (!comLvl = 0) 
	      then (Tok.BOGUS) 
	      else continue())
      end
fun yyAction47 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; continue())
      end
fun yyAction48 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if !pcount = 0 then () else addText yytext;
		    inc pcount; continue())
      end
fun yyAction49 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (dec pcount; 
		    if !pcount = 0 then
		      (YYBEGIN INITIAL; Tok.CODE (getText()))
		    else (addText yytext; continue()))
      end
fun yyAction50 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addText yytext; YYBEGIN STRING; 
		    ignore(continue() before YYBEGIN CODE);
		    addText "\""; continue())
      end
fun yyAction51 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; continue())
      end
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm; (Tok.BOGUS))
fun yyAction53 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addText yytext; print ("unclosed string");
 	            Tok.BOGUS)
      end
fun yyAction54 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; continue())
      end
fun yyAction55 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; continue())
      end
fun yyAction56 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; continue())
      end
fun yyAction57 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addText yytext; continue())
      end
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; continue()))
fun yyAction59 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print ("unclosed string\n"); continue()))
fun yyAction60 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tok.CHAR (String.sub (yytext, 0)))
      end
fun yyAction61 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tok.CHAR (String.sub (yytext, 0)))
      end
fun yyAction62 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (print (concat[Int.toString (!yylineno), ": illegal character '", 
			String.toCString yytext, "'\n"]);
	    continue())
      end
fun yyQ130 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ130(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ130(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = #"`"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ130(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ130(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ129 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ130(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ130(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = #"`"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ130(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ130(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ128 (strm, lastMatch : yymatch) = yyAction9(strm, yyNO_MATCH)
fun yyQ127 (strm, lastMatch : yymatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ126 (strm, lastMatch : yymatch) = yyAction8(strm, yyNO_MATCH)
fun yyQ125 (strm, lastMatch : yymatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ51(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
and yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ51(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ124 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ51(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ123 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ51(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ122 (strm, lastMatch : yymatch) = yyAction11(strm, yyMATCH(strm, yyAction62, yyNO_MATCH))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ122(strm', lastMatch)
            else if inp < #"-"
              then if inp = #"\r"
                  then yyQ124(strm', lastMatch)
                else if inp < #"\r"
                  then if inp = #"\n"
                      then yyQ31(strm', lastMatch)
                    else if inp < #"\n"
                      then if inp = #"\t"
                          then yyQ123(strm', lastMatch)
                          else yyQ122(strm', lastMatch)
                      else yyQ123(strm', lastMatch)
                else if inp = #"!"
                  then yyQ122(strm', lastMatch)
                else if inp < #"!"
                  then if inp = #" "
                      then yyQ123(strm', lastMatch)
                      else yyQ122(strm', lastMatch)
                else if inp = #","
                  then yyQ125(strm', lastMatch)
                  else yyQ122(strm', lastMatch)
            else if inp = #"?"
              then yyQ122(strm', lastMatch)
            else if inp < #"?"
              then if inp = #"<"
                  then yyQ122(strm', lastMatch)
                else if inp < #"<"
                  then if inp = #";"
                      then yyQ126(strm', lastMatch)
                      else yyQ122(strm', lastMatch)
                else if inp = #"="
                  then yyQ127(strm', lastMatch)
                  else yyQ128(strm', lastMatch)
            else if inp = #"["
              then yyQ122(strm', lastMatch)
            else if inp < #"["
              then if inp <= #"@"
                  then yyQ122(strm', lastMatch)
                  else yyQ129(strm', lastMatch)
            else if inp = #"a"
              then yyQ129(strm', lastMatch)
            else if inp < #"a"
              then yyQ122(strm', lastMatch)
            else if inp <= #"z"
              then yyQ129(strm', lastMatch)
              else yyQ122(strm', lastMatch)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = yyAction17(strm, yyNO_MATCH)
fun yyQ94 (strm, lastMatch : yymatch) = yyAction29(strm, yyNO_MATCH)
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ93(strm', lastMatch)
            else if inp < #"_"
              then if inp = #":"
                  then yystuck(lastMatch)
                else if inp < #":"
                  then if inp <= #"/"
                      then yystuck(lastMatch)
                      else yyQ93(strm', lastMatch)
                else if inp = #"A"
                  then yyQ93(strm', lastMatch)
                else if inp < #"A"
                  then yystuck(lastMatch)
                else if inp <= #"Z"
                  then yyQ93(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"{"
              then yystuck(lastMatch)
            else if inp < #"{"
              then if inp = #"`"
                  then yystuck(lastMatch)
                  else yyQ93(strm', lastMatch)
            else if inp = #"}"
              then yyQ94(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = yyAction30(strm, yyNO_MATCH)
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yystuck(lastMatch)
            else if inp < #":"
              then if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ92(strm', lastMatch)
            else if inp = #"}"
              then yyQ95(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ93(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ92(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction61(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ92(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                  else yyAction61(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ93(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ93(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                  else yyAction61(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ93(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
              else yyAction61(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = yyAction38(strm, yyNO_MATCH)
fun yyQ88 (strm, lastMatch : yymatch) = yyAction28(strm, yyNO_MATCH)
fun yyQ59 (strm, lastMatch : yymatch) = yyAction40(strm, yyNO_MATCH)
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ59(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ59(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ61(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ61(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ59(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"#"
                  then yyAction61(strm, yyNO_MATCH)
                else if inp < #"#"
                  then if inp = #"\""
                      then yyQ59(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                      else yyAction61(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ60(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction61(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ60(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                  else yyAction61(strm, yyNO_MATCH)
            else if inp = #"]"
              then yyAction61(strm, yyNO_MATCH)
            else if inp < #"]"
              then if inp = #"["
                  then yyAction61(strm, yyNO_MATCH)
                  else yyQ59(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp = #"a"
              then yyQ59(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp < #"a"
              then yyAction61(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ59(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
              else yyAction61(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ85 (strm, lastMatch : yymatch) = yyAction23(strm, yyNO_MATCH)
fun yyQ84 (strm, lastMatch : yymatch) = yyAction32(strm, yyNO_MATCH)
fun yyQ96 (strm, lastMatch : yymatch) = yyAction36(strm, yyNO_MATCH)
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ96(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = yyAction31(strm, yyNO_MATCH)
fun yyQ81 (strm, lastMatch : yymatch) = yyAction24(strm, yyNO_MATCH)
fun yyQ80 (strm, lastMatch : yymatch) = yyAction34(strm, yyNO_MATCH)
fun yyQ79 (strm, lastMatch : yymatch) = yyAction19(strm, yyNO_MATCH)
fun yyQ78 (strm, lastMatch : yymatch) = yyAction33(strm, yyNO_MATCH)
fun yyQ77 (strm, lastMatch : yymatch) = yyAction21(strm, yyNO_MATCH)
fun yyQ76 (strm, lastMatch : yymatch) = yyAction22(strm, yyNO_MATCH)
fun yyQ75 (strm, lastMatch : yymatch) = yyAction26(strm, yyNO_MATCH)
fun yyQ97 (strm, lastMatch : yymatch) = yyAction43(strm, yyNO_MATCH)
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ97(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = yyAction18(strm, yyNO_MATCH)
fun yyQ107 (strm, lastMatch : yymatch) = yyAction3(strm, yyNO_MATCH)
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"s"
              then yyQ107(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ106(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ105(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ104(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ103(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ110 (strm, lastMatch : yymatch) = yyAction2(strm, yyNO_MATCH)
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ110(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"m"
              then yyQ109(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ108(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ112 (strm, lastMatch : yymatch) = yyAction4(strm, yyNO_MATCH)
fun yyQ111 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ112(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ111(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ115 (strm, lastMatch : yymatch) = yyAction1(strm, yyNO_MATCH)
fun yyQ114 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"s"
              then yyQ115(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ113 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"f"
              then yyQ114(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ113(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ121 (strm, lastMatch : yymatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ120 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ121(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ119 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ120(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ118 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"s"
              then yyQ119(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ117 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ118(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ116 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ117(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"h"
              then yyQ116(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"m"
              then yyAction61(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"d"
                  then yyQ99(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp < #"d"
                  then if inp = #"c"
                      then yyQ98(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                      else yyAction61(strm, yyNO_MATCH)
                else if inp = #"l"
                  then yyQ100(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                  else yyAction61(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ102(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"n"
                  then yyQ101(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                  else yyAction61(strm, yyNO_MATCH)
              else yyAction61(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = yyAction20(strm, yyNO_MATCH)
fun yyQ70 (strm, lastMatch : yymatch) = yyAction37(strm, yyNO_MATCH)
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ51(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ51(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = yyAction61(strm, yyNO_MATCH)
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #","
              then yyQ78(strm', lastMatch)
            else if inp < #","
              then if inp = #"#"
                  then yyQ67(strm', lastMatch)
                else if inp < #"#"
                  then if inp = #"\r"
                      then yyQ69(strm', lastMatch)
                    else if inp < #"\r"
                      then if inp = #"\n"
                          then yyQ31(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ68(strm', lastMatch)
                              else yyQ67(strm', lastMatch)
                          else yyQ68(strm', lastMatch)
                    else if inp = #"!"
                      then yyQ67(strm', lastMatch)
                    else if inp < #"!"
                      then if inp = #" "
                          then yyQ68(strm', lastMatch)
                          else yyQ67(strm', lastMatch)
                      else yyQ70(strm', lastMatch)
                else if inp = #"("
                  then yyQ74(strm', lastMatch)
                else if inp < #"("
                  then if inp = #"&"
                      then yyQ73(strm', lastMatch)
                    else if inp < #"&"
                      then if inp = #"$"
                          then yyQ71(strm', lastMatch)
                          else yyQ72(strm', lastMatch)
                      else yyQ67(strm', lastMatch)
                else if inp = #"*"
                  then yyQ76(strm', lastMatch)
                else if inp = #")"
                  then yyQ75(strm', lastMatch)
                  else yyQ77(strm', lastMatch)
            else if inp = #"@"
              then yyQ67(strm', lastMatch)
            else if inp < #"@"
              then if inp = #";"
                  then yyQ81(strm', lastMatch)
                else if inp < #";"
                  then if inp = #"/"
                      then yyQ80(strm', lastMatch)
                    else if inp < #"/"
                      then if inp = #"-"
                          then yyQ67(strm', lastMatch)
                          else yyQ79(strm', lastMatch)
                      else yyQ67(strm', lastMatch)
                else if inp = #">"
                  then yyQ84(strm', lastMatch)
                else if inp < #">"
                  then if inp = #"<"
                      then yyQ82(strm', lastMatch)
                      else yyQ83(strm', lastMatch)
                  else yyQ85(strm', lastMatch)
            else if inp = #"^"
              then yyQ89(strm', lastMatch)
            else if inp < #"^"
              then if inp = #"\\"
                  then yyQ87(strm', lastMatch)
                else if inp < #"\\"
                  then if inp = #"["
                      then yyQ86(strm', lastMatch)
                      else yyQ67(strm', lastMatch)
                  else yyQ88(strm', lastMatch)
            else if inp = #"|"
              then yyQ91(strm', lastMatch)
            else if inp < #"|"
              then if inp = #"{"
                  then yyQ90(strm', lastMatch)
                  else yyQ67(strm', lastMatch)
              else yyQ67(strm', lastMatch)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction60(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ59(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"#"
                  then yyAction60(strm, yyNO_MATCH)
                else if inp < #"#"
                  then if inp = #"\""
                      then yyQ59(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                      else yyAction60(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ60(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction60(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ60(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                  else yyAction60(strm, yyNO_MATCH)
            else if inp = #"]"
              then yyAction60(strm, yyNO_MATCH)
            else if inp < #"]"
              then if inp = #"["
                  then yyAction60(strm, yyNO_MATCH)
                  else yyQ59(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
            else if inp = #"a"
              then yyQ59(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
            else if inp < #"a"
              then yyAction60(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ59(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
              else yyAction60(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = yyAction58(strm, yyNO_MATCH)
fun yyQ63 (strm, lastMatch : yymatch) = yyAction59(strm, yyNO_MATCH)
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ63(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
              else yyAction59(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = yyAction60(strm, yyNO_MATCH)
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ62(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ62(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ63(strm', lastMatch)
                      else yyQ62(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ64(strm', lastMatch)
                  else yyQ62(strm', lastMatch)
            else if inp = #"#"
              then yyQ62(strm', lastMatch)
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ65(strm', lastMatch)
                  else yyQ62(strm', lastMatch)
            else if inp = #"\\"
              then yyQ66(strm', lastMatch)
              else yyQ62(strm', lastMatch)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = yyAction38(strm, yyNO_MATCH)
fun yyQ57 (strm, lastMatch : yymatch) = yyAction41(strm, yyNO_MATCH)
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ59(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"#"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #"#"
                  then if inp = #"\""
                      then yyQ59(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                      else yyAction42(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ60(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ60(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp = #"]"
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"]"
              then if inp = #"["
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ59(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"a"
              then yyQ59(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"a"
              then yyAction42(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ59(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = yyAction39(strm, yyNO_MATCH)
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ51(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ51(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = yyAction42(strm, yyNO_MATCH)
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yyQ52(strm', lastMatch)
            else if inp < #"!"
              then if inp = #"\v"
                  then yyQ53(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\t"
                      then yyQ53(strm', lastMatch)
                    else if inp = #"\n"
                      then yyQ31(strm', lastMatch)
                      else yyQ52(strm', lastMatch)
                else if inp = #"\^N"
                  then yyQ52(strm', lastMatch)
                else if inp < #"\^N"
                  then if inp = #"\r"
                      then yyQ54(strm', lastMatch)
                      else yyQ53(strm', lastMatch)
                else if inp = #" "
                  then yyQ53(strm', lastMatch)
                  else yyQ52(strm', lastMatch)
            else if inp = #"\\"
              then yyQ56(strm', lastMatch)
            else if inp < #"\\"
              then if inp = #"-"
                  then yyQ55(strm', lastMatch)
                  else yyQ52(strm', lastMatch)
            else if inp = #"^"
              then yyQ58(strm', lastMatch)
            else if inp = #"]"
              then yyQ57(strm', lastMatch)
              else yyQ52(strm', lastMatch)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = yyAction12(strm, yyNO_MATCH)
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"8"
              then yyQ40(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"f"
              then yyQ39(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction62, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ38(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction62, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction62, yyNO_MATCH))
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = yyAction14(strm, yyNO_MATCH)
fun yyQ45 (strm, lastMatch : yymatch) = yyAction13(strm, yyNO_MATCH)
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"8"
              then yyQ46(strm', lastMatch)
            else if inp < #"8"
              then if inp = #"7"
                  then yyQ45(strm', lastMatch)
                  else yystuck(lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"i"
              then yyQ44(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"i"
              then yyQ43(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"c"
              then yyQ42(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction62, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = #"s"
              then yyQ41(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction62, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction62, yyNO_MATCH))
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"F"
              then yyQ39(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction62, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = #"T"
              then yyQ47(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction62, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction62, yyNO_MATCH))
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"I"
              then yyQ44(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"I"
              then yyQ50(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"C"
              then yyQ49(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyMATCH(strm, yyAction62, yyNO_MATCH))
        | SOME(inp, strm') =>
            if inp = #"S"
              then yyQ48(strm', yyMATCH(strm, yyAction16, yyMATCH(strm, yyAction62, yyNO_MATCH)))
              else yyAction16(strm, yyMATCH(strm, yyAction62, yyNO_MATCH))
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = yyAction15(strm, yyNO_MATCH)
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ51(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\^N"
              then if inp = #"\t"
                  then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyQ51(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #" "
              then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = yyAction16(strm, yyMATCH(strm, yyAction62, yyNO_MATCH))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"<"
              then yyQ29(strm', lastMatch)
            else if inp < #"<"
              then if inp = #"\r"
                  then yyQ32(strm', lastMatch)
                else if inp < #"\r"
                  then if inp = #"\n"
                      then yyQ31(strm', lastMatch)
                    else if inp < #"\n"
                      then if inp = #"\t"
                          then yyQ30(strm', lastMatch)
                          else yyQ29(strm', lastMatch)
                      else yyQ30(strm', lastMatch)
                else if inp = #"!"
                  then yyQ29(strm', lastMatch)
                else if inp < #"!"
                  then if inp = #" "
                      then yyQ30(strm', lastMatch)
                      else yyQ29(strm', lastMatch)
                else if inp = #";"
                  then yyQ33(strm', lastMatch)
                  else yyQ29(strm', lastMatch)
            else if inp = #"V"
              then yyQ29(strm', lastMatch)
            else if inp < #"V"
              then if inp = #"B"
                  then yyQ29(strm', lastMatch)
                else if inp < #"B"
                  then if inp = #"A"
                      then yyQ34(strm', lastMatch)
                      else yyQ29(strm', lastMatch)
                else if inp = #"U"
                  then yyQ35(strm', lastMatch)
                  else yyQ29(strm', lastMatch)
            else if inp = #"b"
              then yyQ29(strm', lastMatch)
            else if inp < #"b"
              then if inp = #"a"
                  then yyQ36(strm', lastMatch)
                  else yyQ29(strm', lastMatch)
            else if inp = #"u"
              then yyQ37(strm', lastMatch)
              else yyQ29(strm', lastMatch)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = yyAction55(strm, yyNO_MATCH)
fun yyQ26 (strm, lastMatch : yymatch) = yyAction57(strm, yyNO_MATCH)
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyAction54(strm, yyNO_MATCH)
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ26(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
                  else yyAction54(strm, yyNO_MATCH)
            else if inp = #"\\"
              then yyQ27(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = yyAction52(strm, yyNO_MATCH)
fun yyQ22 (strm, lastMatch : yymatch) = yyAction53(strm, yyNO_MATCH)
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ22(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ28(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ28(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyAction56(strm, yyNO_MATCH)
                      else yyQ28(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp = #"\r"
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ28(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp = #"#"
              then yyQ28(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #"\""
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ28(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp = #"\\"
              then yyAction56(strm, yyNO_MATCH)
              else yyQ28(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ28(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ28(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyAction56(strm, yyNO_MATCH)
                      else yyQ28(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp = #"\r"
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ28(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp = #"#"
              then yyQ28(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #"\""
                  then yyAction56(strm, yyNO_MATCH)
                  else yyQ28(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp = #"\\"
              then yyAction56(strm, yyNO_MATCH)
              else yyQ28(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ21(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ21(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ22(strm', lastMatch)
                      else yyQ21(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ23(strm', lastMatch)
                  else yyQ21(strm', lastMatch)
            else if inp = #"#"
              then yyQ21(strm', lastMatch)
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ24(strm', lastMatch)
                  else yyQ21(strm', lastMatch)
            else if inp = #"\\"
              then yyQ25(strm', lastMatch)
              else yyQ21(strm', lastMatch)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = yyAction49(strm, yyNO_MATCH)
fun yyQ20 (strm, lastMatch : yymatch) = yyAction44(strm, yyNO_MATCH)
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ20(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
              else yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = yyAction50(strm, yyNO_MATCH)
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ16(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #"\""
                  then yyAction51(strm, yyNO_MATCH)
                  else yyQ16(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp = #"("
              then yyAction51(strm, yyNO_MATCH)
            else if inp < #"("
              then yyQ16(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp <= #")"
              then yyAction51(strm, yyNO_MATCH)
              else yyQ16(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ16(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #"\""
                  then yyAction51(strm, yyNO_MATCH)
                  else yyQ16(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp = #"("
              then yyAction51(strm, yyNO_MATCH)
            else if inp < #"("
              then yyQ16(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
            else if inp <= #")"
              then yyAction51(strm, yyNO_MATCH)
              else yyQ16(strm', yyMATCH(strm, yyAction51, yyNO_MATCH))
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ15(strm', lastMatch)
            else if inp < #"#"
              then if inp = #"\v"
                  then yyQ15(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ16(strm', lastMatch)
                      else yyQ15(strm', lastMatch)
                else if inp = #"\""
                  then yyQ17(strm', lastMatch)
                  else yyQ15(strm', lastMatch)
            else if inp = #")"
              then yyQ19(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ18(strm', lastMatch)
                  else yyQ15(strm', lastMatch)
              else yyQ15(strm', lastMatch)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = yyAction46(strm, yyNO_MATCH)
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ13(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = yyAction45(strm, yyNO_MATCH)
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ14(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = yyAction47(strm, yyNO_MATCH)
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ9(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = yyAction47(strm, yyNO_MATCH)
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ8(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ8(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ9(strm', lastMatch)
                      else yyQ8(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ10(strm', lastMatch)
                  else yyQ8(strm', lastMatch)
            else if inp = #")"
              then yyQ8(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ11(strm', lastMatch)
                  else yyQ8(strm', lastMatch)
            else if inp = #"*"
              then yyQ12(strm', lastMatch)
              else yyQ8(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of COM => yyQ0(!(yystrm), yyNO_MATCH)
    | CODE => yyQ1(!(yystrm), yyNO_MATCH)
    | STRING => yyQ2(!(yystrm), yyNO_MATCH)
    | CHARSET => yyQ3(!(yystrm), yyNO_MATCH)
    | CHARCLASS => yyQ4(!(yystrm), yyNO_MATCH)
    | RESTRING => yyQ5(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ6(!(yystrm), yyNO_MATCH)
    | DIRECTIVE => yyQ7(!(yystrm), yyNO_MATCH)
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

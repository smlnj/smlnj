functor HTMLLexFn (
  structure Tokens : HTML_TOKENS
  structure Err : HTML_ERROR
  structure HTMLAttrs : HTML_ATTRS)  = struct

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
COM2 | COM1 | STAG | INITIAL
    structure UserDeclarations = 
      struct

(* html-lex
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * A scanner for HTML.
 *
 * TODO:
 *    Recognize the DOCTYPE element
 *	<!DOCTYPE HTML PUBLIC "...">
 *    Clean-up the scanning of start tags (do we need Err?).
 *    Whitespace in PRE elements should be preserved, but how?
 *)

structure T = Tokens
structure Elems = HTMLElementsFn (
  structure Tokens = Tokens
  structure Err = Err
  structure HTMLAttrs = HTMLAttrs)

type pos = int
type svalue = T.svalue
type arg = (((string * int * int) -> unit) * string option)
type ('a, 'b) token = ('a, 'b) T.token
type lexresult= (svalue, pos) token

fun eof _ = Tokens.EOF(0, 0)

(* a buffer for collecting a string piecewise *)
val buffer = ref ([] : string list)
fun addStr s = (buffer := s :: !buffer)
fun getStr () = (String.concat(List.rev(! buffer)) before (buffer := []))



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
(yyarg as (errorFn, file)) () = let 
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
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addStr yytext; YYBEGIN STAG; continue())
      end
fun yyAction1 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addStr yytext;
	    YYBEGIN INITIAL;
	    case Elems.startTag file (getStr(), !yylineno, !yylineno)
	     of NONE => continue()
	      | (SOME tag) => tag
	    (* end case *))
      end
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addStr " "; continue()))
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addStr yytext; continue())
      end
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addStr yytext; continue())
      end
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addStr yytext; continue())
      end
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addStr yytext; continue())
      end
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addStr yytext; continue())
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addStr yytext; continue())
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (case Elems.endTag file (yytext, !yylineno, !yylineno)
	     of NONE => continue()
	      | (SOME tag) => tag
	    (* end case *))
      end
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COM1; continue()))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COM2; continue()))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COM1; continue()))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; continue()))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
        (errorFn("bad comment syntax", !yylineno, !yylineno+1);
	    YYBEGIN INITIAL;
	    continue())
      end
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (
(** At some point, we should support &#SPACE; and &#TAB; **)
	    continue()))
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (T.CHAR_REF(yytext, !yylineno, !yylineno))
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (T.ENTITY_REF(yytext, !yylineno, !yylineno))
      end
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (T.PCDATA(yytext, !yylineno, !yylineno))
      end
fun yyAction25 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (errorFn(concat[
		"bogus character #\"", Char.toString(String.sub(yytext, 0)),
		"\" in PCDATA\n"
	      ], !yylineno, !yylineno+1);
	    continue())
      end
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp <= #","
                      then yyAction0(strm, yyNO_MATCH)
                      else yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"["
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"a"
              then yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"a"
              then yyAction0(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ37(strm', lastMatch)
            else if inp < #" "
              then if inp = #"\t"
                  then yyQ37(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #">"
              then yyQ38(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ36(strm', lastMatch)
            else if inp < #"0"
              then if inp = #" "
                  then yyQ37(strm', lastMatch)
                else if inp < #" "
                  then if inp = #"\t"
                      then yyQ37(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"-"
                  then yyQ36(strm', lastMatch)
                else if inp < #"-"
                  then yystuck(lastMatch)
                else if inp = #"/"
                  then yystuck(lastMatch)
                  else yyQ36(strm', lastMatch)
            else if inp = #"A"
              then yyQ36(strm', lastMatch)
            else if inp < #"A"
              then if inp = #">"
                  then yyQ38(strm', lastMatch)
                else if inp < #">"
                  then if inp <= #"9"
                      then yyQ36(strm', lastMatch)
                      else yystuck(lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ36(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ36(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"z"
              then yyQ36(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"["
              then yystuck(lastMatch)
            else if inp < #"["
              then if inp <= #"@"
                  then yystuck(lastMatch)
                  else yyQ36(strm', lastMatch)
            else if inp = #"a"
              then yyQ36(strm', lastMatch)
            else if inp < #"a"
              then yystuck(lastMatch)
            else if inp <= #"z"
              then yyQ36(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ40(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ39(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyAction25(strm, yyNO_MATCH)
            else if inp < #"0"
              then if inp = #"\""
                  then yyAction25(strm, yyNO_MATCH)
                else if inp < #"\""
                  then if inp = #"!"
                      then yyQ33(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                      else yyAction25(strm, yyNO_MATCH)
                else if inp = #"/"
                  then yyQ34(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp = #"["
              then yyAction25(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction25(strm, yyNO_MATCH)
                  else yyQ35(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp = #"a"
              then yyQ35(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp < #"a"
              then yyAction25(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ35(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"<"
              then yyAction24(strm, yyNO_MATCH)
              else yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"<"
              then yyAction21(strm, yyNO_MATCH)
              else yyQ41(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"<"
              then yyAction24(strm, yyNO_MATCH)
            else if inp < #"<"
              then if inp = #"0"
                  then yyQ43(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"-"
                      then yyQ43(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                    else if inp < #"-"
                      then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                    else if inp = #"/"
                      then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                      else yyQ43(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp = #":"
                  then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp = #";"
                  then yyQ44(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyQ43(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp = #"["
              then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"["
              then if inp <= #"@"
                  then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyQ43(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp = #"a"
              then yyQ43(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"a"
              then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ43(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"<"
              then yyAction19(strm, yyNO_MATCH)
              else yyQ41(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ46(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"<"
                  then yyAction24(strm, yyNO_MATCH)
                else if inp < #"<"
                  then if inp = #";"
                      then yyQ47(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                      else yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp = #"a"
              then yyQ46(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ46(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ46(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"<"
              then yyAction20(strm, yyNO_MATCH)
              else yyQ41(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyQ48(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #";"
              then if inp = #"0"
                  then yyQ45(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp < #"0"
                  then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp = #":"
                  then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyQ45(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp = #"<"
              then yyAction24(strm, yyNO_MATCH)
              else yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"="
              then if inp = #":"
                  then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp < #":"
                  then if inp <= #"/"
                      then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                      else yyQ45(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp = #"<"
                  then yyAction24(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp = #"["
              then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"["
              then if inp <= #"@"
                  then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyQ46(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp = #"a"
              then yyQ46(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"a"
              then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ46(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"="
              then if inp = #"$"
                  then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp < #"$"
                  then if inp = #"#"
                      then yyQ42(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                      else yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp = #"<"
                  then yyAction24(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp = #"["
              then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"["
              then if inp <= #"@"
                  then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyQ43(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp = #"a"
              then yyQ43(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"a"
              then yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ43(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"<"
              then yyAction22(strm, yyNO_MATCH)
              else yyQ41(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"<"
              then yyAction23(strm, yyNO_MATCH)
              else yyQ41(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"<"
              then yyAction24(strm, yyNO_MATCH)
              else yyQ41(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yyQ28(strm', lastMatch)
            else if inp < #"!"
              then if inp = #"\n"
                  then yyQ30(strm', lastMatch)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ29(strm', lastMatch)
                      else yyQ28(strm', lastMatch)
                else if inp = #" "
                  then yyQ29(strm', lastMatch)
                  else yyQ28(strm', lastMatch)
            else if inp = #"'"
              then yyQ28(strm', lastMatch)
            else if inp < #"'"
              then if inp = #"&"
                  then yyQ31(strm', lastMatch)
                  else yyQ28(strm', lastMatch)
            else if inp = #"<"
              then yyQ32(strm', lastMatch)
              else yyQ28(strm', lastMatch)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp <= #","
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ22(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyQ22(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = #"["
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ22(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = #"a"
              then yyQ22(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"a"
              then yyAction4(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ22(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp <= #","
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ22(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyQ22(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = #"["
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ22(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = #"a"
              then yyQ22(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"a"
              then yyAction4(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ22(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ23(strm', lastMatch)
            else if inp < #"\v"
              then if inp = #"\n"
                  then yystuck(lastMatch)
                  else yyQ23(strm', lastMatch)
            else if inp = #"'"
              then yyQ24(strm', lastMatch)
              else yyQ23(strm', lastMatch)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = #"'"
              then yyQ24(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ25(strm', lastMatch)
            else if inp < #"\v"
              then if inp = #"\n"
                  then yystuck(lastMatch)
                  else yyQ25(strm', lastMatch)
            else if inp = #"\""
              then yyQ26(strm', lastMatch)
              else yyQ25(strm', lastMatch)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ25(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ25(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = #"\""
              then yyQ26(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyQ25(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ27(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ27(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ27(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ27(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ19(strm', lastMatch)
            else if inp < #"-"
              then if inp = #"!"
                  then yyQ14(strm', lastMatch)
                else if inp < #"!"
                  then if inp = #"\n"
                      then yyQ16(strm', lastMatch)
                    else if inp < #"\n"
                      then if inp = #"\t"
                          then yyQ15(strm', lastMatch)
                          else yyQ14(strm', lastMatch)
                    else if inp = #" "
                      then yyQ15(strm', lastMatch)
                      else yyQ14(strm', lastMatch)
                else if inp = #"'"
                  then yyQ18(strm', lastMatch)
                else if inp < #"'"
                  then if inp = #"\""
                      then yyQ17(strm', lastMatch)
                      else yyQ14(strm', lastMatch)
                  else yyQ14(strm', lastMatch)
            else if inp = #">"
              then yyQ21(strm', lastMatch)
            else if inp < #">"
              then if inp = #"0"
                  then yyQ19(strm', lastMatch)
                else if inp < #"0"
                  then if inp = #"/"
                      then yyQ14(strm', lastMatch)
                      else yyQ19(strm', lastMatch)
                else if inp = #":"
                  then yyQ14(strm', lastMatch)
                else if inp < #":"
                  then yyQ19(strm', lastMatch)
                else if inp = #"="
                  then yyQ20(strm', lastMatch)
                  else yyQ14(strm', lastMatch)
            else if inp = #"["
              then yyQ14(strm', lastMatch)
            else if inp < #"["
              then if inp <= #"@"
                  then yyQ14(strm', lastMatch)
                  else yyQ19(strm', lastMatch)
            else if inp = #"a"
              then yyQ19(strm', lastMatch)
            else if inp < #"a"
              then yyQ14(strm', lastMatch)
            else if inp <= #"z"
              then yyQ19(strm', lastMatch)
              else yyQ14(strm', lastMatch)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ13(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ10(strm', lastMatch)
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ11(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp = #"-"
              then yyQ12(strm', lastMatch)
              else yyQ10(strm', lastMatch)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ9(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yyQ4(strm', lastMatch)
            else if inp < #"!"
              then if inp = #"\n"
                  then yyQ6(strm', lastMatch)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ5(strm', lastMatch)
                      else yyQ4(strm', lastMatch)
                else if inp = #" "
                  then yyQ5(strm', lastMatch)
                  else yyQ4(strm', lastMatch)
            else if inp = #"."
              then yyQ4(strm', lastMatch)
            else if inp < #"."
              then if inp = #"-"
                  then yyQ7(strm', lastMatch)
                  else yyQ4(strm', lastMatch)
            else if inp = #">"
              then yyQ8(strm', lastMatch)
              else yyQ4(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of COM2 => yyQ0(!(yystrm), yyNO_MATCH)
    | COM1 => yyQ1(!(yystrm), yyNO_MATCH)
    | STAG => yyQ2(!(yystrm), yyNO_MATCH)
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
    end

  end

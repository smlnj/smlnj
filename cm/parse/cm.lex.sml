functor CMLexFun (structure Tokens: CM_TOKENS)  = struct

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
MC | PM | PC | SS | PMC | C | M | P | S | INITIAL
    structure UserDeclarations = 
      struct

(* -*- sml-lex -*-
 *
 * cm.lex
 *
 * lexical analysis (ML-Lex specification) for CM description files
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)

structure S = CMSemant
structure SM = SourceMap

type svalue = Tokens.svalue
type pos = int

type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

type lexarg = {
	       enterC: unit -> unit,
	       leaveC: unit -> bool,
	       newS: pos -> unit,
	       addS: char -> unit,
	       addSC: string * int -> unit,
	       addSN: string * pos -> unit,
	       getS: pos * (string * pos * pos -> lexresult) -> lexresult,
	       handleEof: unit -> pos,
	       newline: pos -> unit,
	       obsolete: SourceMap.region -> unit,
	       error: SourceMap.region -> string -> unit,
	       sync: pos * string -> unit,
	       in_section2: bool ref
	      }

type arg = lexarg

fun eof (arg: lexarg) = let
    val pos = #handleEof arg ()
in
    Tokens.EOF (pos, pos)
end

fun errorTok (t, p) = let
    fun findGraph i =
	if Char.isGraph (String.sub (t, i)) then i
	else findGraph (i + 1)
    fun findError i =
	if String.sub (t, i) = #"e" then i
	else findError (i + 1)
    val start = findGraph (5 + findError 0)
    val msg = String.extract (t, start, NONE)
in
    Tokens.ERROR (msg, p + 1, p + size t)
end

fun plain t (_: bool ref, arg) = t arg : lexresult
fun is_token (r, arg) = (r := true; Tokens.IS arg) : lexresult

val cm_ids = [("Group", plain Tokens.GROUP),
	      ("GROUP", plain Tokens.GROUP),
	      ("group", plain Tokens.GROUP),
	      ("Library", plain Tokens.LIBRARY),
	      ("LIBRARY", plain Tokens.LIBRARY),
	      ("library", plain Tokens.LIBRARY),
	      ("IS", is_token),
	      ("is", is_token),
	      ("*", plain Tokens.STAR),
	      ("-", plain Tokens.DASH),
	      ("Source", plain Tokens.SOURCE),
	      ("SOURCE", plain Tokens.SOURCE),
	      ("source", plain Tokens.SOURCE)]

val ml_ids = [("structure", Tokens.STRUCTURE),
	      ("signature", Tokens.SIGNATURE),
	      ("functor", Tokens.FUNCTOR),
	      ("funsig", Tokens.FUNSIG)]

val pp_ids = [("defined", plain Tokens.DEFINED),
	      ("div", plain (fn (x, y) => Tokens.MULSYM (S.DIV, x, y))),
	      ("mod", plain (fn (x, y) => Tokens.MULSYM (S.MOD, x, y))),
	      ("andalso", plain Tokens.ANDALSO),
	      ("orelse", plain Tokens.ORELSE),
	      ("not", plain Tokens.NOT),
	      ("true", plain Tokens.TRUE),
	      ("false", plain Tokens.FALSE)]

fun idToken (t, p, idlist, default, chstate, in_section2) =
    case List.find (fn (id, _) => id = t) ml_ids of
	SOME (_, tok) => (chstate (); tok (p, p + size t))
      | NONE =>
	    (case List.find (fn (id, _) => id = t) idlist of
		 SOME (_, tok) => tok (in_section2, (p, p + size t))
	       | NONE => default (t, p, p + size t))

(* states:

     INITIAL -> C
       |
       +------> P -> PC
       |        |
       |        +--> PM -> PMC
       |
       +------> M -> MC
       |
       +------> S -> SS

   "C"  -- COMMENT
   "P"  -- PREPROC
   "M"  -- MLSYMBOL
   "S"  -- STRING
   "SS" -- STRINGSKIP
*)



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
(yyarg as ({ enterC, leaveC,
        newS, addS, addSC, addSN, getS,
        handleEof,
        newline,
	obsolete,
	error,
	sync,
	in_section2 })) () = let 
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
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (enterC (); YYBEGIN C; continue ()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (enterC (); YYBEGIN PC; continue ()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (enterC (); YYBEGIN PMC; continue ()))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (enterC (); YYBEGIN MC; continue ()))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (enterC (); continue ()))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (if leaveC () then YYBEGIN INITIAL else ();
			    continue ()))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (if leaveC () then YYBEGIN P else ();
			    continue ()))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (if leaveC () then YYBEGIN PM else ();
			    continue ()))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (if leaveC () then YYBEGIN M else ();
			    continue ()))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (newline yypos; continue ()))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm; (continue ()))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (error (SM.REGION (yypos, yypos+2))
				  "unmatched comment delimiter";
			    continue ()))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN S; newS yypos; continue ()))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addS #"\a"; continue ()))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addS #"\b"; continue ()))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addS #"\f"; continue ()))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addS #"\n"; continue ()))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addS #"\r"; continue ()))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addS #"\t"; continue ()))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addS #"\v"; continue ()))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addS (chr 0); continue ()))
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addSC (yytext, ord #"a"); continue ())
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addSC (yytext, ord #"A"); continue ())
      end
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addS (chr 27); continue ()))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addS (chr 28); continue ()))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addS (chr 29); continue ()))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addS (chr 30); continue ()))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addS (chr 31); continue ()))
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addSN (yytext, yypos); continue ())
      end
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addS #"\""; continue ()))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addS #"\\"; continue ()))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN SS; newline (yypos + 1); continue ()))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN SS; continue ()))
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (error (SM.REGION (yypos, yypos+2))
			     ("illegal escape character in string " ^ yytext);
			    continue ())
      end
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; getS (yypos, Tokens.FILE_NATIVE)))
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (newline yypos;
			    error (SM.REGION (yypos, yypos + size yytext))
			      "illegal linebreak in string";
			    continue ())
      end
fun yyAction36 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addS (String.sub (yytext, 0)); continue ())
      end
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (newline yypos; continue ()))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm; (continue ()))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN S; continue ()))
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (error (SM.REGION (yypos, yypos+1))
			     ("illegal character in stringskip " ^ yytext);
			    continue ())
      end
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN (yypos, yypos + 1)))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN (yypos, yypos + 1)))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COLON (yypos, yypos + 1)))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ADDSYM (S.PLUS, yypos, yypos + 1)))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ADDSYM (S.MINUS, yypos, yypos + 1)))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MULSYM (S.TIMES, yypos, yypos + 1)))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQSYM (S.NE, yypos, yypos + 2)))
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (obsolete (SM.REGION (yypos, yypos + 2));
			    Tokens.EQSYM (S.NE, yypos, yypos+2)))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INEQSYM (S.LE, yypos, yypos + 2)))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INEQSYM (S.LT, yypos, yypos + 1)))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INEQSYM (S.GE, yypos, yypos + 2)))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INEQSYM (S.GT, yypos, yypos + 1)))
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;
      (obsolete (SM.REGION (yypos, yypos + 2));
			    Tokens.EQSYM (S.EQ, yypos, yypos + 2)))
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQSYM (S.EQ, yypos, yypos + 1)))
fun yyAction55 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TILDE (yypos, yypos + 1)))
fun yyAction56 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.NUMBER
			     (valOf (Int.fromString yytext)
			      handle _ =>
				  (error (SM.REGION (yypos, yypos + size yytext))
				     "number too large";
				   0),
			      yypos, yypos + size yytext))
      end
fun yyAction57 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (idToken (yytext, yypos, pp_ids, Tokens.CM_ID,
				     fn () => YYBEGIN PM, in_section2))
      end
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;
      (obsolete (SM.REGION (yypos, yypos + 1));
			    Tokens.MULSYM (S.DIV, yypos, yypos + 1)))
fun yyAction59 (strm, lastMatch : yymatch) = (yystrm := strm;
      (obsolete (SM.REGION (yypos, yypos + 1));
			    Tokens.MULSYM (S.MOD, yypos, yypos + 1)))
fun yyAction60 (strm, lastMatch : yymatch) = (yystrm := strm;
      (obsolete (SM.REGION (yypos, yypos + 2));
			    Tokens.ANDALSO (yypos, yypos + 2)))
fun yyAction61 (strm, lastMatch : yymatch) = (yystrm := strm;
      (obsolete (SM.REGION (yypos, yypos + 2));
			    Tokens.ORELSE (yypos, yypos + 2)))
fun yyAction62 (strm, lastMatch : yymatch) = (yystrm := strm;
      (obsolete (SM.REGION (yypos, yypos + 1));
			    Tokens.NOT (yypos, yypos + 1)))
fun yyAction63 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (YYBEGIN INITIAL;
			    Tokens.ML_ID (yytext, yypos, yypos + size yytext))
      end
fun yyAction64 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (YYBEGIN P;
			    Tokens.ML_ID (yytext, yypos, yypos + size yytext))
      end
fun yyAction65 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (YYBEGIN P;
				     newline yypos;
				     Tokens.IF (yypos, yypos + size yytext))
      end
fun yyAction66 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (YYBEGIN P;
				     newline yypos;
				     Tokens.ELIF (yypos, yypos + size yytext))
      end
fun yyAction67 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (YYBEGIN P;
				     newline yypos;
				     Tokens.ELSE (yypos, yypos + size yytext))
      end
fun yyAction68 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (YYBEGIN P;
				      newline yypos;
				      Tokens.ENDIF (yypos,
						    yypos + size yytext))
      end
fun yyAction69 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (newline yypos;
						    errorTok (yytext, yypos))
      end
fun yyAction70 (strm, lastMatch : yymatch) = (yystrm := strm;
      (newline yypos; continue ()))
fun yyAction71 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; newline yypos; continue ()))
fun yyAction72 (strm, lastMatch : yymatch) = (yystrm := strm; (continue ()))
fun yyAction73 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (error (SM.REGION (yypos, yypos+1))
			    ("illegal character at start of ML symbol: " ^
			     yytext);
			    continue ())
      end
fun yyAction74 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (idToken (yytext, yypos,
				     if !in_section2 then [] else cm_ids,
				     Tokens.FILE_STANDARD,
				     fn () => YYBEGIN M, in_section2))
      end
fun yyAction75 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (error (SM.REGION (yypos, yypos+1))
			    ("illegal character: " ^ yytext);
			    continue ())
      end
fun yyAction76 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (newline yypos;
					sync (yypos, yytext);
					continue ())
      end
fun yyQ143 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ144 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction74(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction74(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"("
                  then yyAction74(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"\""
                      then yyAction74(strm, yyNO_MATCH)
                    else if inp < #"\""
                      then if inp = #"!"
                          then yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                          else yyAction74(strm, yyNO_MATCH)
                      else yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                else if inp = #":"
                  then yyAction74(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #")"
                      then yyAction74(strm, yyNO_MATCH)
                      else yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                  else yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
            else if inp = #"{"
              then yyAction74(strm, yyNO_MATCH)
            else if inp < #"{"
              then if inp = #"`"
                  then yyAction74(strm, yyNO_MATCH)
                else if inp < #"`"
                  then if inp <= #"]"
                      then yyAction74(strm, yyNO_MATCH)
                      else yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                  else yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
            else if inp = #"~"
              then yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
            else if inp < #"~"
              then if inp = #"|"
                  then yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                  else yyAction74(strm, yyNO_MATCH)
              else yyAction74(strm, yyNO_MATCH)
      (* end case *))
fun yyQ142 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction74(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction74(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"("
                  then yyAction74(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"\""
                      then yyAction74(strm, yyNO_MATCH)
                    else if inp < #"\""
                      then if inp = #"!"
                          then yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                          else yyAction74(strm, yyNO_MATCH)
                      else yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                else if inp = #":"
                  then yyAction74(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp = #")"
                      then yyQ35(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                      else yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                  else yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
            else if inp = #"{"
              then yyAction74(strm, yyNO_MATCH)
            else if inp < #"{"
              then if inp = #"`"
                  then yyAction74(strm, yyNO_MATCH)
                else if inp < #"`"
                  then if inp <= #"]"
                      then yyAction74(strm, yyNO_MATCH)
                      else yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                  else yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
            else if inp = #"~"
              then yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
            else if inp < #"~"
              then if inp = #"|"
                  then yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                  else yyAction74(strm, yyNO_MATCH)
              else yyAction74(strm, yyNO_MATCH)
      (* end case *))
fun yyQ141 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ145 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ140 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ145(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
              else yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ139 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ138 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction74(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction74(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"("
                  then yyAction74(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"\""
                      then yyAction74(strm, yyNO_MATCH)
                    else if inp < #"\""
                      then if inp = #"!"
                          then yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                          else yyAction74(strm, yyNO_MATCH)
                      else yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                else if inp = #":"
                  then yyAction74(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #")"
                      then yyAction74(strm, yyNO_MATCH)
                      else yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                  else yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
            else if inp = #"{"
              then yyAction74(strm, yyNO_MATCH)
            else if inp < #"{"
              then if inp = #"`"
                  then yyAction74(strm, yyNO_MATCH)
                else if inp < #"`"
                  then if inp <= #"]"
                      then yyAction74(strm, yyNO_MATCH)
                      else yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                  else yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
            else if inp = #"~"
              then yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
            else if inp < #"~"
              then if inp = #"|"
                  then yyQ144(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                  else yyAction74(strm, yyNO_MATCH)
              else yyAction74(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction76(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ23(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyAction76(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
            else if inp = #"\r"
              then yyAction76(strm, yyNO_MATCH)
              else yyQ23(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction76(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ24(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
            else if inp < #"\f"
              then if inp = #"\n"
                  then yyAction76(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ24(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
                      else yyQ23(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
                  else yyQ23(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
            else if inp = #" "
              then yyQ24(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\r"
                  then yyAction76(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
              else yyQ23(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction76(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ24(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
            else if inp < #"\f"
              then if inp = #"\n"
                  then yyAction76(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ24(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
                      else yyQ23(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
                  else yyQ23(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
            else if inp = #" "
              then yyQ24(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\r"
                  then yyAction76(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
              else yyQ23(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ22(strm', lastMatch)
            else if inp < #"\f"
              then if inp = #"\t"
                  then yyQ22(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #" "
              then yyQ22(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ21(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"n"
              then yyQ20(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"i"
              then yyQ19(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"f"
              then yyQ86(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction69(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ94(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyAction69(strm, yyNO_MATCH)
                  else yyQ94(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
            else if inp = #"\r"
              then yyAction69(strm, yyNO_MATCH)
              else yyQ94(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction69(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ95(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
            else if inp < #"\f"
              then if inp = #"\n"
                  then yyAction69(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ95(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
                      else yyQ94(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
                  else yyQ94(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
            else if inp = #" "
              then yyQ95(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\r"
                  then yyAction69(strm, yyNO_MATCH)
                  else yyQ94(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
              else yyQ94(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction69(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ95(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
            else if inp < #"\f"
              then if inp = #"\n"
                  then yyAction69(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ95(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
                      else yyQ94(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
                  else yyQ94(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
            else if inp = #" "
              then yyQ95(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\r"
                  then yyAction69(strm, yyNO_MATCH)
                  else yyQ94(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
              else yyQ94(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ93(strm', lastMatch)
            else if inp < #"\f"
              then if inp = #"\t"
                  then yyQ93(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #" "
              then yyQ93(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ92(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"o"
              then yyQ91(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ90(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction68(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction68(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"f"
              then yyQ98(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"i"
              then yyQ97(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"d"
              then yyQ96(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction67(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction67(strm, yyNO_MATCH)
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ101(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"f"
              then yyQ102(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"j"
              then yystuck(lastMatch)
            else if inp < #"j"
              then if inp = #"i"
                  then yyQ99(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"s"
              then yyQ100(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"n"
              then yyQ88(strm', lastMatch)
            else if inp < #"n"
              then if inp = #"l"
                  then yyQ87(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"r"
              then yyQ89(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yystuck(lastMatch)
            else if inp < #"!"
              then if inp = #"\f"
                  then yyQ83(strm', lastMatch)
                else if inp < #"\f"
                  then if inp = #"\t"
                      then yyQ83(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #" "
                  then yyQ83(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"i"
              then yyQ85(strm', lastMatch)
            else if inp < #"i"
              then if inp = #"e"
                  then yyQ84(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"l"
              then yyQ18(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ136 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction70(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ83(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
              else yyAction70(strm, yyNO_MATCH)
      (* end case *))
fun yyQ137 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction70(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyAction70(strm, yyNO_MATCH)
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ136(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
                  else yyAction70(strm, yyNO_MATCH)
            else if inp = #"#"
              then yyQ83(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
              else yyAction70(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction72(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ37(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
            else if inp < #"\f"
              then if inp = #"\t"
                  then yyQ37(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                  else yyAction72(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ37(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
              else yyAction72(strm, yyNO_MATCH)
      (* end case *))
fun yyQ135 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction72(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ37(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
            else if inp < #"\f"
              then if inp = #"\t"
                  then yyQ37(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                  else yyAction72(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ37(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
              else yyAction72(strm, yyNO_MATCH)
      (* end case *))
fun yyQ134 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction75(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction75(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ142(strm', lastMatch)
            else if inp < #"*"
              then if inp = #"\^N"
                  then yyQ134(strm', lastMatch)
                else if inp < #"\^N"
                  then if inp = #"\v"
                      then yyQ134(strm', lastMatch)
                    else if inp < #"\v"
                      then if inp = #"\t"
                          then yyQ135(strm', lastMatch)
                        else if inp = #"\n"
                          then yyQ136(strm', lastMatch)
                          else yyQ134(strm', lastMatch)
                    else if inp = #"\f"
                      then yyQ135(strm', lastMatch)
                      else yyQ137(strm', lastMatch)
                else if inp = #"\""
                  then yyQ139(strm', lastMatch)
                else if inp < #"\""
                  then if inp = #" "
                      then yyQ135(strm', lastMatch)
                    else if inp = #"!"
                      then yyQ138(strm', lastMatch)
                      else yyQ134(strm', lastMatch)
                else if inp = #"("
                  then yyQ140(strm', lastMatch)
                else if inp = #")"
                  then yyQ141(strm', lastMatch)
                  else yyQ138(strm', lastMatch)
            else if inp = #"a"
              then yyQ138(strm', lastMatch)
            else if inp < #"a"
              then if inp = #"["
                  then yyQ134(strm', lastMatch)
                else if inp < #"["
                  then if inp = #":"
                      then yyQ143(strm', lastMatch)
                      else yyQ138(strm', lastMatch)
                else if inp = #"^"
                  then yyQ138(strm', lastMatch)
                else if inp < #"^"
                  then yyQ134(strm', lastMatch)
                else if inp = #"`"
                  then yyQ134(strm', lastMatch)
                  else yyQ138(strm', lastMatch)
            else if inp = #"}"
              then yyQ134(strm', lastMatch)
            else if inp < #"}"
              then if inp = #"{"
                  then yyQ134(strm', lastMatch)
                  else yyQ138(strm', lastMatch)
            else if inp = #"~"
              then yyQ138(strm', lastMatch)
              else yyQ134(strm', lastMatch)
      (* end case *))
fun yyQ122 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ121 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ120 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ119 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ118 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ117 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ116 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ130 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ129 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ128 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ127 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ126 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ125 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ124 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ123 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ115 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"]"
              then yyQ127(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"]"
              then if inp = #"A"
                  then yyQ124(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"@"
                      then yyQ123(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyQ125(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp = #"\\"
                  then yyQ126(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ124(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"`"
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"^"
                  then yyQ128(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ129(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ130(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ114 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ132 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ131 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ132(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ132(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ113 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ131(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"0"
              then yyAction33(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ131(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ112 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ110 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ111 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ110(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ133 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ133(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < #"\f"
              then if inp = #"\t"
                  then yyQ133(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                  else yyAction32(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ133(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ133(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < #"\f"
              then if inp = #"\t"
                  then yyQ133(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                  else yyAction32(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ133(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ107 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"^"
              then yyQ115(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"^"
              then if inp = #" "
                  then yyQ109(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp < #" "
                  then if inp = #"\v"
                      then yyQ108(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                    else if inp < #"\v"
                      then if inp = #"\t"
                          then yyQ109(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                        else if inp = #"\n"
                          then yyQ110(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                          else yyQ108(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                    else if inp = #"\r"
                      then yyQ111(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                    else if inp = #"\f"
                      then yyQ109(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                      else yyQ108(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp = #"0"
                  then yyQ113(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"\""
                      then yyQ112(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                      else yyQ108(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp = #"\\"
                  then yyQ114(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp < #"\\"
                  then if inp <= #"9"
                      then yyQ113(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                      else yyQ108(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyQ108(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp = #"o"
              then yyQ108(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"c"
                  then yyQ108(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp < #"c"
                  then if inp = #"a"
                      then yyQ116(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                    else if inp = #"b"
                      then yyQ117(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                      else yyQ108(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp = #"g"
                  then yyQ108(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp < #"g"
                  then if inp = #"f"
                      then yyQ118(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                      else yyQ108(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp = #"n"
                  then yyQ119(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyQ108(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp = #"t"
              then yyQ121(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"r"
                  then yyQ120(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyQ108(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp = #"v"
              then yyQ122(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyQ108(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
      (* end case *))
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yystuck(lastMatch)
            else if inp < #"\r"
              then if inp = #"\n"
                  then yystuck(lastMatch)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ17(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"\f"
                  then yyQ17(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"!"
              then yystuck(lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ17(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"l"
              then yyQ18(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ17(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ104(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"#"
              then yyQ17(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ103(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ103(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ104(strm', lastMatch)
                      else yyQ103(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ105(strm', lastMatch)
                  else yyQ103(strm', lastMatch)
            else if inp = #"#"
              then yyQ103(strm', lastMatch)
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ106(strm', lastMatch)
                  else yyQ103(strm', lastMatch)
            else if inp = #"\\"
              then yyQ107(strm', lastMatch)
              else yyQ103(strm', lastMatch)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction55(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction61(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"|"
              then yyQ75(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ72(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction57(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ72(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                      else yyAction57(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ72(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction57(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ72(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                  else yyAction57(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction57(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction57(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ72(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ72(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                  else yyAction57(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ72(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
              else yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ76(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
              else yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ77(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ79(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ78(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ68(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < #"0"
              then yyAction56(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ68(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
              else yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ35(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ80(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
              else yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction60(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction60(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ81(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction59(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ82(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
              else yyAction62(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction71(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ83(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
              else yyAction71(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction71(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyAction71(strm, yyNO_MATCH)
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ57(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                  else yyAction71(strm, yyNO_MATCH)
            else if inp = #"#"
              then yyQ83(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
              else yyAction71(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #","
              then if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp < #","
              then if inp = #"!"
                  then yyQ59(strm', lastMatch)
                else if inp < #"!"
                  then if inp = #"\f"
                      then yyQ37(strm', lastMatch)
                    else if inp < #"\f"
                      then if inp = #"\n"
                          then yyQ57(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ37(strm', lastMatch)
                            else if yyInput.eof(!(yystrm))
                              then UserDeclarations.eof(yyarg)
                              else yystuck(lastMatch)
                        else if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                    else if inp = #"\^N"
                      then if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                    else if inp < #"\^N"
                      then yyQ58(strm', lastMatch)
                    else if inp = #" "
                      then yyQ37(strm', lastMatch)
                    else if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp = #"("
                  then yyQ62(strm', lastMatch)
                else if inp < #"("
                  then if inp = #"&"
                      then yyQ61(strm', lastMatch)
                    else if inp < #"&"
                      then if inp = #"%"
                          then yyQ60(strm', lastMatch)
                        else if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                    else if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp = #"*"
                  then yyQ64(strm', lastMatch)
                else if inp = #")"
                  then yyQ63(strm', lastMatch)
                  else yyQ65(strm', lastMatch)
            else if inp = #"?"
              then if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp < #"?"
              then if inp = #":"
                  then if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp < #":"
                  then if inp = #"/"
                      then yyQ67(strm', lastMatch)
                    else if inp < #"/"
                      then if inp = #"-"
                          then yyQ66(strm', lastMatch)
                        else if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                      else yyQ68(strm', lastMatch)
                else if inp = #"="
                  then yyQ70(strm', lastMatch)
                else if inp < #"="
                  then if inp = #"<"
                      then yyQ69(strm', lastMatch)
                    else if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                  else yyQ71(strm', lastMatch)
            else if inp = #"{"
              then if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if inp < #"{"
              then if inp = #"["
                  then if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                else if inp < #"["
                  then if inp <= #"@"
                      then if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                      else yyQ72(strm', lastMatch)
                else if inp <= #"`"
                  then if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                  else yyQ72(strm', lastMatch)
            else if inp = #"~"
              then yyQ74(strm', lastMatch)
            else if inp < #"~"
              then if inp = #"|"
                  then yyQ73(strm', lastMatch)
                else if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yystuck(lastMatch)
            else if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ54(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction63(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ54(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                      else yyAction63(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ54(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction63(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ54(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction63(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction63(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ54(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ54(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ54(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ54(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction63(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ54(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                      else yyAction63(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ54(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction63(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ54(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction63(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction63(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ54(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ54(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ54(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            else if inp < #":"
              then if inp = #"*"
                  then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp < #"*"
                  then if inp = #"\""
                      then yyAction63(strm, yyNO_MATCH)
                    else if inp < #"\""
                      then if inp = #"!"
                          then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                          else yyAction63(strm, yyNO_MATCH)
                    else if inp <= #"&"
                      then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                      else yyAction63(strm, yyNO_MATCH)
                else if inp = #"."
                  then yyAction63(strm, yyNO_MATCH)
                else if inp < #"."
                  then if inp = #","
                      then yyAction63(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp = #"/"
                  then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"^"
              then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            else if inp < #"^"
              then if inp = #"A"
                  then yyAction63(strm, yyNO_MATCH)
                else if inp < #"A"
                  then if inp = #";"
                      then yyAction63(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp = #"\\"
                  then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction63(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            else if inp < #":"
              then if inp = #"*"
                  then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp < #"*"
                  then if inp = #"#"
                      then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                    else if inp < #"#"
                      then if inp = #"!"
                          then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                          else yyAction63(strm, yyNO_MATCH)
                    else if inp = #"'"
                      then yyAction63(strm, yyNO_MATCH)
                    else if inp < #"'"
                      then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                    else if inp = #")"
                      then yyQ35(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                      else yyAction63(strm, yyNO_MATCH)
                else if inp = #"."
                  then yyAction63(strm, yyNO_MATCH)
                else if inp < #"."
                  then if inp = #","
                      then yyAction63(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp = #"/"
                  then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"^"
              then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            else if inp < #"^"
              then if inp = #"A"
                  then yyAction63(strm, yyNO_MATCH)
                else if inp < #"A"
                  then if inp = #";"
                      then yyAction63(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp = #"\\"
                  then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction63(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction73(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ56(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
              else yyAction73(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            else if inp < #":"
              then if inp = #"*"
                  then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp < #"*"
                  then if inp = #"\""
                      then yyAction63(strm, yyNO_MATCH)
                    else if inp < #"\""
                      then if inp = #"!"
                          then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                          else yyAction63(strm, yyNO_MATCH)
                    else if inp <= #"&"
                      then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                      else yyAction63(strm, yyNO_MATCH)
                else if inp = #"."
                  then yyAction63(strm, yyNO_MATCH)
                else if inp < #"."
                  then if inp = #","
                      then yyAction63(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp = #"/"
                  then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"^"
              then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            else if inp < #"^"
              then if inp = #"A"
                  then yyAction63(strm, yyNO_MATCH)
                else if inp < #"A"
                  then if inp = #";"
                      then yyAction63(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp = #"\\"
                  then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction63(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ55(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction70(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ17(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
              else yyAction70(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction70(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyAction70(strm, yyNO_MATCH)
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ27(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
                  else yyAction70(strm, yyNO_MATCH)
            else if inp = #"#"
              then yyQ17(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
              else yyAction70(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction72(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ37(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
            else if inp < #"\f"
              then if inp = #"\t"
                  then yyQ37(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                  else yyAction72(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ37(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
              else yyAction72(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction73(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction73(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ25(strm', lastMatch)
            else if inp < #"."
              then if inp = #"\""
                  then yyQ25(strm', lastMatch)
                else if inp < #"\""
                  then if inp = #"\f"
                      then yyQ26(strm', lastMatch)
                    else if inp < #"\f"
                      then if inp = #"\n"
                          then yyQ27(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ26(strm', lastMatch)
                              else yyQ25(strm', lastMatch)
                          else yyQ25(strm', lastMatch)
                    else if inp = #" "
                      then yyQ26(strm', lastMatch)
                    else if inp < #" "
                      then if inp = #"\r"
                          then yyQ28(strm', lastMatch)
                          else yyQ25(strm', lastMatch)
                      else yyQ50(strm', lastMatch)
                else if inp = #"*"
                  then yyQ52(strm', lastMatch)
                else if inp < #"*"
                  then if inp = #"("
                      then yyQ51(strm', lastMatch)
                    else if inp < #"("
                      then if inp = #"'"
                          then yyQ25(strm', lastMatch)
                          else yyQ50(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #","
                  then yyQ25(strm', lastMatch)
                  else yyQ50(strm', lastMatch)
            else if inp = #"]"
              then yyQ25(strm', lastMatch)
            else if inp < #"]"
              then if inp = #"<"
                  then yyQ50(strm', lastMatch)
                else if inp < #"<"
                  then if inp = #":"
                      then yyQ50(strm', lastMatch)
                    else if inp < #":"
                      then if inp = #"/"
                          then yyQ50(strm', lastMatch)
                          else yyQ25(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #"["
                  then yyQ25(strm', lastMatch)
                else if inp < #"["
                  then if inp <= #"@"
                      then yyQ50(strm', lastMatch)
                      else yyQ53(strm', lastMatch)
                  else yyQ50(strm', lastMatch)
            else if inp = #"|"
              then yyQ50(strm', lastMatch)
            else if inp < #"|"
              then if inp = #"a"
                  then yyQ53(strm', lastMatch)
                else if inp < #"a"
                  then if inp = #"^"
                      then yyQ50(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #"{"
                  then yyQ25(strm', lastMatch)
                  else yyQ53(strm', lastMatch)
            else if inp = #"~"
              then yyQ50(strm', lastMatch)
              else yyQ25(strm', lastMatch)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ49(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ16(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ17(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ11(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp = #"#"
              then yyQ17(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ10(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ10(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ11(strm', lastMatch)
                      else yyQ10(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ12(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp = #")"
              then yyQ10(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ13(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp = #"*"
              then yyQ48(strm', lastMatch)
              else yyQ10(strm', lastMatch)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ47(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ10(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ10(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ11(strm', lastMatch)
                      else yyQ10(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ12(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp = #")"
              then yyQ10(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ13(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp = #"*"
              then yyQ46(strm', lastMatch)
              else yyQ10(strm', lastMatch)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ17(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ42(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = #"#"
              then yyQ17(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ45(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"\f"
              then if inp = #"\t"
                  then yyQ45(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ45(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ45(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"\f"
              then if inp = #"\t"
                  then yyQ45(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ45(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyQ43(strm', lastMatch)
            else if inp < #"\r"
              then if inp = #"\n"
                  then yyQ42(strm', lastMatch)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ41(strm', lastMatch)
                      else yyQ40(strm', lastMatch)
                else if inp = #"\v"
                  then yyQ40(strm', lastMatch)
                  else yyQ41(strm', lastMatch)
            else if inp = #"!"
              then yyQ40(strm', lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ41(strm', lastMatch)
                  else yyQ40(strm', lastMatch)
            else if inp = #"\\"
              then yyQ44(strm', lastMatch)
              else yyQ40(strm', lastMatch)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ39(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ10(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ10(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ11(strm', lastMatch)
                      else yyQ10(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ12(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp = #")"
              then yyQ10(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ13(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp = #"*"
              then yyQ38(strm', lastMatch)
              else yyQ10(strm', lastMatch)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ33(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ33(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ33(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ33(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ33(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ33(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ33(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ33(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ33(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ33(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ33(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ33(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ33(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ33(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #":"
              then if inp = #"*"
                  then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"*"
                  then if inp = #"\""
                      then yyAction64(strm, yyNO_MATCH)
                    else if inp < #"\""
                      then if inp = #"!"
                          then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                          else yyAction64(strm, yyNO_MATCH)
                    else if inp <= #"&"
                      then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"."
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"."
                  then if inp = #","
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"/"
                  then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"^"
              then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"^"
              then if inp = #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"A"
                  then if inp = #";"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"\\"
                  then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #":"
              then if inp = #"*"
                  then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"*"
                  then if inp = #"#"
                      then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                    else if inp < #"#"
                      then if inp = #"!"
                          then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                          else yyAction64(strm, yyNO_MATCH)
                    else if inp = #"'"
                      then yyAction64(strm, yyNO_MATCH)
                    else if inp < #"'"
                      then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                    else if inp = #")"
                      then yyQ35(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"."
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"."
                  then if inp = #","
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"/"
                  then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"^"
              then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"^"
              then if inp = #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"A"
                  then if inp = #";"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"\\"
                  then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction73(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ36(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
              else yyAction73(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #":"
              then if inp = #"*"
                  then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"*"
                  then if inp = #"\""
                      then yyAction64(strm, yyNO_MATCH)
                    else if inp < #"\""
                      then if inp = #"!"
                          then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                          else yyAction64(strm, yyNO_MATCH)
                    else if inp <= #"&"
                      then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"."
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"."
                  then if inp = #","
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"/"
                  then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"^"
              then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"^"
              then if inp = #"A"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"A"
                  then if inp = #";"
                      then yyAction64(strm, yyNO_MATCH)
                      else yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"\\"
                  then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ34(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ25(strm', lastMatch)
            else if inp < #"."
              then if inp = #"\""
                  then yyQ25(strm', lastMatch)
                else if inp < #"\""
                  then if inp = #"\f"
                      then yyQ26(strm', lastMatch)
                    else if inp < #"\f"
                      then if inp = #"\n"
                          then yyQ27(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ26(strm', lastMatch)
                              else yyQ25(strm', lastMatch)
                          else yyQ25(strm', lastMatch)
                    else if inp = #" "
                      then yyQ26(strm', lastMatch)
                    else if inp < #" "
                      then if inp = #"\r"
                          then yyQ28(strm', lastMatch)
                          else yyQ25(strm', lastMatch)
                      else yyQ29(strm', lastMatch)
                else if inp = #"*"
                  then yyQ31(strm', lastMatch)
                else if inp < #"*"
                  then if inp = #"("
                      then yyQ30(strm', lastMatch)
                    else if inp < #"("
                      then if inp = #"'"
                          then yyQ25(strm', lastMatch)
                          else yyQ29(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #","
                  then yyQ25(strm', lastMatch)
                  else yyQ29(strm', lastMatch)
            else if inp = #"]"
              then yyQ25(strm', lastMatch)
            else if inp < #"]"
              then if inp = #"<"
                  then yyQ29(strm', lastMatch)
                else if inp < #"<"
                  then if inp = #":"
                      then yyQ29(strm', lastMatch)
                    else if inp < #":"
                      then if inp = #"/"
                          then yyQ29(strm', lastMatch)
                          else yyQ25(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #"["
                  then yyQ25(strm', lastMatch)
                else if inp < #"["
                  then if inp <= #"@"
                      then yyQ29(strm', lastMatch)
                      else yyQ32(strm', lastMatch)
                  else yyQ29(strm', lastMatch)
            else if inp = #"|"
              then yyQ29(strm', lastMatch)
            else if inp < #"|"
              then if inp = #"a"
                  then yyQ32(strm', lastMatch)
                else if inp < #"a"
                  then if inp = #"^"
                      then yyQ29(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #"{"
                  then yyQ25(strm', lastMatch)
                  else yyQ32(strm', lastMatch)
            else if inp = #"~"
              then yyQ29(strm', lastMatch)
              else yyQ25(strm', lastMatch)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ15(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ10(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ10(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ11(strm', lastMatch)
                      else yyQ10(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ12(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp = #")"
              then yyQ10(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ13(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp = #"*"
              then yyQ14(strm', lastMatch)
              else yyQ10(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of MC => yyQ0(!(yystrm), yyNO_MATCH)
    | PM => yyQ1(!(yystrm), yyNO_MATCH)
    | PC => yyQ2(!(yystrm), yyNO_MATCH)
    | SS => yyQ3(!(yystrm), yyNO_MATCH)
    | PMC => yyQ4(!(yystrm), yyNO_MATCH)
    | C => yyQ5(!(yystrm), yyNO_MATCH)
    | M => yyQ6(!(yystrm), yyNO_MATCH)
    | P => yyQ7(!(yystrm), yyNO_MATCH)
    | S => yyQ8(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ9(!(yystrm), yyNO_MATCH)
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

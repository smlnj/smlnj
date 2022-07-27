functor MLLexLexFun(structure Tok: MLLex_TOKENS)  = struct

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
RE | DEFS | RECB | STRING | CHARCLASS | LEXSTATES | ACTION | INITIAL
    structure UserDeclarations = 
      struct

type pos = int
type svalue = Tok.svalue
type ('a,'b) token = ('a,'b) Tok.token
type lexresult= (svalue,pos) token

open Tok

val eof = fn () => EOF(~1,~1)
val error = (* fn (e,l : int,_) =>
      output(std_out,"line " ^ (makestring l) ^
	     ": " ^ e ^ "\n") *)
     fn _ => ()

(* what to do (i.e. switch start states) after recognizing an action *)
val afterAction = ref (fn () => ())

(* paren counting for actions *)
val pcount = ref 0
val inquote = ref false
fun inc r = if !inquote then () else r := !r + 1
fun dec r = if !inquote then () else r := !r - 1

(* buffer for accumulating test across the rules for actions *)
local
val text = ref ([] : string list)
in
fun clrAction () = (text := ["("])
fun updAction str = if !pcount > 0
      then (text := str :: !text)
      else ()
fun getAction () = String.concat (rev (!text))
end

structure SIS = RegExp.SymSet
fun uniChar s = let
      fun toW32 (c : Char.char) : UTF8.wchar =
	(case c of #"0" => 0w0 | #"1" => 0w1 | #"2" => 0w2 | #"3" => 0w3
	 	 | #"4" => 0w4 | #"5" => 0w5 | #"6" => 0w6 | #"7" => 0w7
	 	 | #"8" => 0w8 | #"9" => 0w9 | #"a" => 0w10 | #"A" => 0w10
		 | #"b" => 0w11 | #"B" => 0w11 | #"c" => 0w12 | #"C" => 0w12
		 | #"d" => 0w13 | #"D" => 0w13 | #"e" => 0w14 | #"E" => 0w14
		 | #"f" => 0w15 | #"F" => 0w15
		 | _ => raise Fail "invalid unicode escape sequence")
      fun iter (#"u"::_, v) = v
        | iter (c::cs,   v) = iter (cs, 0w16*v + (toW32 c))
	| iter _ = raise Fail "invalid unicode escape sequence"
      in iter (List.rev (String.explode s), 0w0)
      end

val highAscii = SIS.interval(0w128, 0w255)



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
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN DEFS; LEXMARK(!yylineno, !yylineno))
      end
fun yyAction1 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (DECLS(yytext, !yylineno, !yylineno))
      end
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; LEXMARK(!yylineno, !yylineno))
      end
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN LEXSTATES; STATES(!yylineno, !yylineno))
      end
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
        (clrAction(); pcount := 1; inquote := false;
	            YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN DEFS);
		    HEADER(!yylineno, !yylineno))
      end
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (STRUCT(!yylineno, !yylineno))
      end
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
        (clrAction(); pcount := 1; inquote := false;
                    YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN DEFS);
		    ARG(!yylineno, !yylineno))
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (COUNT(!yylineno, !yylineno))
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (REJECTTOK(!yylineno, !yylineno))
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (UNICODE(!yylineno, !yylineno))
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (FULL(!yylineno, !yylineno))
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (ID(yytext, !yylineno, !yylineno))
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; EQ(!yylineno, !yylineno))
      end
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (QMARK(!yylineno, !yylineno))
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (STAR(!yylineno, !yylineno))
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (PLUS(!yylineno, !yylineno))
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (BAR(!yylineno, !yylineno))
      end
fun yyAction19 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (LP(!yylineno, !yylineno))
      end
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (RP(!yylineno, !yylineno))
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (DOLLAR(!yylineno, !yylineno))
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (SLASH(!yylineno, !yylineno))
      end
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (DOT(!yylineno, !yylineno))
      end
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (CARAT(!yylineno, !yylineno))
      end
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN RECB; lex()))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN STRING; lex()))
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN CHARCLASS; LB(!yylineno, !yylineno))
      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN LEXSTATES; LT(!yylineno, !yylineno))
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (GT(!yylineno, !yylineno))
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
        (clrAction(); pcount := 1; inquote := false;
                    YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN RE);
		    ARROW(!yylineno, !yylineno))
      end
fun yyAction31 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN DEFS; SEMI(!yylineno, !yylineno))
      end
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (ID(yytext, !yylineno, !yylineno))
      end
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (REPS(valOf (Int.fromString yytext), !yylineno, !yylineno))
      end
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (COMMA(!yylineno, !yylineno))
      end
fun yyAction36 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; RCB(!yylineno, !yylineno))
      end
fun yyAction37 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; RBD(!yylineno, !yylineno))
      end
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; RB(!yylineno, !yylineno))
      end
fun yyAction39 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (DASH(!yylineno, !yylineno))
      end
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (CARAT(!yylineno, !yylineno))
      end
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN RE; lex()))
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (CHAR(valOf (String.fromString yytext), !yylineno, !yylineno))
      end
fun yyAction43 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (UNICHAR(uniChar yytext, !yylineno, !yylineno))
      end
fun yyAction44 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (HIGH_CHAR(!yylineno, !yylineno))
      end
fun yyAction45 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (CHAR(String.substring (yytext, 1, 1), !yylineno, !yylineno))
      end
fun yyAction46 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (CHAR(yytext, !yylineno, !yylineno))
      end
fun yyAction47 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm; (LEXSTATE(yytext, !yylineno, !yylineno))
      end
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction49 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (COMMA(!yylineno, !yylineno))
      end
fun yyAction50 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN RE; GT(!yylineno, !yylineno))
      end
fun yyAction51 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm; (YYBEGIN DEFS; SEMI(!yylineno, !yylineno))
      end
fun yyAction52 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        yystrm := strm;
        (if !pcount = 0
		    then ((!afterAction)();
			  ACT(getAction(), !yylineno, !yylineno))
		    else (updAction ";"; lex()))
      end
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;
      (updAction "("; inc pcount; lex()))
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;
      (updAction ")"; dec pcount; lex()))
fun yyAction55 (strm, lastMatch : yymatch) = (yystrm := strm;
      (updAction "\\\""; lex()))
fun yyAction56 (strm, lastMatch : yymatch) = (yystrm := strm;
      (updAction "\\\\"; lex()))
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;
      (updAction "\\"; lex()))
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;
      (updAction "\""; inquote := not (!inquote); lex()))
fun yyAction59 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (updAction yytext; lex())
      end
fun yyAction60 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (print (concat[
		"[", Int.toString (!yylineno), "] Illegal character '",
		String.toCString yytext, "'\n"
	      ]);
            continue())
      end
fun yyQ125 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ124 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"%"
              then yyQ126(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyQ122(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
      (* end case *))
and yyQ126 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"%"
              then yystuck(lastMatch)
              else yyQ124(strm', lastMatch)
      (* end case *))
and yyQ122 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"%"
              then yyQ126(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyQ127(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
      (* end case *))
and yyQ127 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"%"
              then yyQ126(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyQ127(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
      (* end case *))
fun yyQ123 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction60(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"%"
              then yyQ125(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
              else yyQ124(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
      (* end case *))
fun yyQ121 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"%"
              then yyQ126(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyQ127(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ121(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ122(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ121(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"%"
              then yyQ123(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyQ121(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
      (* end case *))
fun yyQ120 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ119 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction55(strm, yyNO_MATCH)
      (* end case *))
fun yyQ118 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyAction57(strm, yyNO_MATCH)
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ119(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                  else yyAction57(strm, yyNO_MATCH)
            else if inp = #"\\"
              then yyQ120(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
              else yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ117 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ116 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ115 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ114 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ113 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ113(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
            else if inp < #"*"
              then if inp = #"#"
                  then yyQ113(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                else if inp < #"#"
                  then if inp = #"\""
                      then yyAction59(strm, yyNO_MATCH)
                      else yyQ113(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                else if inp <= #"'"
                  then yyQ113(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                  else yyAction59(strm, yyNO_MATCH)
            else if inp = #"<"
              then yyQ113(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
            else if inp < #"<"
              then if inp = #";"
                  then yyAction59(strm, yyNO_MATCH)
                  else yyQ113(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
            else if inp = #"\\"
              then yyAction59(strm, yyNO_MATCH)
              else yyQ113(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
      (* end case *))
fun yyQ112 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ113(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
            else if inp < #"*"
              then if inp = #"#"
                  then yyQ113(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                else if inp < #"#"
                  then if inp = #"\""
                      then yyAction59(strm, yyNO_MATCH)
                      else yyQ113(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                else if inp <= #"'"
                  then yyQ113(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                  else yyAction59(strm, yyNO_MATCH)
            else if inp = #"<"
              then yyQ113(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
            else if inp < #"<"
              then if inp = #";"
                  then yyAction59(strm, yyNO_MATCH)
                  else yyQ113(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
            else if inp = #"\\"
              then yyAction59(strm, yyNO_MATCH)
              else yyQ113(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ116(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
            else if inp < #")"
              then if inp = #"\""
                  then yyQ114(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                else if inp < #"\""
                  then if inp = #"\n"
                      then yyQ113(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                      else yyQ112(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                else if inp = #"("
                  then yyQ115(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                  else yyQ112(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
            else if inp = #"<"
              then yyQ112(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
            else if inp < #"<"
              then if inp = #";"
                  then yyQ117(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                  else yyQ112(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
            else if inp = #"\\"
              then yyQ118(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
              else yyQ112(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
      (* end case *))
fun yyQ111 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ111(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ111(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ111(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ111(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ111(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ111(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ111(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ110 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ111(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ111(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ111(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ111(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ111(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ111(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ111(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ107 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction60(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction60(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ42(strm', lastMatch)
            else if inp < #"-"
              then if inp = #"\r"
                  then yyQ105(strm', lastMatch)
                else if inp < #"\r"
                  then if inp = #"\n"
                      then yyQ106(strm', lastMatch)
                    else if inp < #"\n"
                      then if inp = #"\t"
                          then yyQ105(strm', lastMatch)
                          else yyQ42(strm', lastMatch)
                      else yyQ42(strm', lastMatch)
                else if inp = #"!"
                  then yyQ42(strm', lastMatch)
                else if inp < #"!"
                  then if inp = #" "
                      then yyQ105(strm', lastMatch)
                      else yyQ42(strm', lastMatch)
                else if inp = #","
                  then yyQ107(strm', lastMatch)
                  else yyQ42(strm', lastMatch)
            else if inp = #"?"
              then yyQ42(strm', lastMatch)
            else if inp < #"?"
              then if inp = #"<"
                  then yyQ42(strm', lastMatch)
                else if inp < #"<"
                  then if inp = #";"
                      then yyQ108(strm', lastMatch)
                      else yyQ42(strm', lastMatch)
                else if inp = #">"
                  then yyQ109(strm', lastMatch)
                  else yyQ42(strm', lastMatch)
            else if inp = #"["
              then yyQ42(strm', lastMatch)
            else if inp < #"["
              then if inp <= #"@"
                  then yyQ42(strm', lastMatch)
                  else yyQ110(strm', lastMatch)
            else if inp = #"a"
              then yyQ110(strm', lastMatch)
            else if inp < #"a"
              then yyQ42(strm', lastMatch)
            else if inp <= #"z"
              then yyQ110(strm', lastMatch)
              else yyQ42(strm', lastMatch)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ37(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ37(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ37(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ37(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ37(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ37(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ36(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ36(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ36(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ36(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ36(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ35(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ35(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ35(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ35(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ35(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ35(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ34(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ34(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ34(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ34(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ34(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ34(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ39(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ39(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ38(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"0"
              then yyAction45(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ38(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"c"
              then yyQ29(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"c"
              then if inp = #"0"
                  then yyQ31(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"\v"
                      then yyQ29(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                    else if inp < #"\v"
                      then if inp = #"\n"
                          then yyAction46(strm, yyNO_MATCH)
                          else yyQ29(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                    else if inp = #"\""
                      then yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyQ29(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"\\"
                  then yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"\\"
                  then if inp <= #"9"
                      then yyQ31(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyQ29(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"b"
                  then yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyQ29(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp = #"r"
              then yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"i"
                  then yyQ29(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"i"
                  then if inp = #"h"
                      then yyQ32(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyQ29(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"n"
                  then yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyQ29(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp = #"u"
              then yyQ33(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"s"
                  then yyQ29(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyQ30(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyQ29(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"]"
              then yyQ104(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ8(strm', lastMatch)
            else if inp < #"."
              then if inp = #"\v"
                  then yyQ8(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yystuck(lastMatch)
                      else yyQ8(strm', lastMatch)
                else if inp = #"-"
                  then yyQ101(strm', lastMatch)
                  else yyQ8(strm', lastMatch)
            else if inp = #"]"
              then yyQ102(strm', lastMatch)
            else if inp < #"]"
              then if inp = #"\\"
                  then yyQ25(strm', lastMatch)
                  else yyQ8(strm', lastMatch)
            else if inp = #"^"
              then yyQ103(strm', lastMatch)
              else yyQ8(strm', lastMatch)
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ100(strm', lastMatch)
            else if inp < #"\""
              then if inp = #"\n"
                  then if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                  else yyQ8(strm', lastMatch)
            else if inp = #"\\"
              then yyQ25(strm', lastMatch)
              else yyQ8(strm', lastMatch)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ98(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ98(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ98(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ98(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ98(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ98(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ98(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ98(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ98(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ98(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ98(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ98(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ98(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ98(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ99(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp < #"0"
              then yyAction34(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ99(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ99(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp < #"0"
              then yyAction34(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ99(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ42(strm', lastMatch)
            else if inp < #"-"
              then if inp = #"\r"
                  then yyQ92(strm', lastMatch)
                else if inp < #"\r"
                  then if inp = #"\n"
                      then yyQ93(strm', lastMatch)
                    else if inp < #"\n"
                      then if inp = #"\t"
                          then yyQ92(strm', lastMatch)
                          else yyQ42(strm', lastMatch)
                      else yyQ42(strm', lastMatch)
                else if inp = #"!"
                  then yyQ42(strm', lastMatch)
                else if inp < #"!"
                  then if inp = #" "
                      then yyQ92(strm', lastMatch)
                      else yyQ42(strm', lastMatch)
                else if inp = #","
                  then yyQ94(strm', lastMatch)
                  else yyQ42(strm', lastMatch)
            else if inp = #"["
              then yyQ42(strm', lastMatch)
            else if inp < #"["
              then if inp = #":"
                  then yyQ42(strm', lastMatch)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyQ42(strm', lastMatch)
                      else yyQ95(strm', lastMatch)
                else if inp <= #"@"
                  then yyQ42(strm', lastMatch)
                  else yyQ96(strm', lastMatch)
            else if inp = #"{"
              then yyQ42(strm', lastMatch)
            else if inp < #"{"
              then if inp <= #"`"
                  then yyQ42(strm', lastMatch)
                  else yyQ96(strm', lastMatch)
            else if inp = #"}"
              then yyQ97(strm', lastMatch)
              else yyQ42(strm', lastMatch)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                      else yyAction12(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction12(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                      else yyAction12(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction12(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ62(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"d"
              then yyQ61(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"o"
              then yyQ60(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"c"
              then yyQ59(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"i"
              then yyQ58(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"n"
              then yyQ57(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ70(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ69(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"u"
              then yyQ68(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ67(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"c"
              then yyQ66(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"u"
              then yyQ65(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ64(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ63(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, lastMatch)
        | SOME(inp, strm') => yyAction9(strm, lastMatch)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ75(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"c"
              then yyQ74(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ73(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"j"
              then yyQ72(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ71(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yystuck(lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yystuck(lastMatch)
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yystuck(lastMatch)
                      else yyQ80(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ80(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"!"
              then yystuck(lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ80(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"("
              then yyQ81(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ80(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ79(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"d"
              then yyQ78(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ77(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"e"
              then yyQ76(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"l"
              then yyQ84(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"l"
              then yyQ83(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"u"
              then yyQ82(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ88(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"n"
              then yyQ87(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"u"
              then yyQ86(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"o"
              then yyQ85(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yystuck(lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yystuck(lastMatch)
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yystuck(lastMatch)
                      else yyQ90(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ90(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"!"
              then yystuck(lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ90(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"("
              then yyQ91(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"g"
              then yyQ90(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ89(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction60(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"g"
              then yyAction60(strm, yyNO_MATCH)
            else if inp < #"g"
              then if inp = #"b"
                  then yyAction60(strm, yyNO_MATCH)
                else if inp < #"b"
                  then if inp = #"&"
                      then yyAction60(strm, yyNO_MATCH)
                    else if inp < #"&"
                      then if inp = #"%"
                          then yyQ49(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                          else yyAction60(strm, yyNO_MATCH)
                    else if inp = #"a"
                      then yyQ50(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                      else yyAction60(strm, yyNO_MATCH)
                else if inp = #"d"
                  then yyAction60(strm, yyNO_MATCH)
                else if inp < #"d"
                  then yyQ51(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                else if inp = #"f"
                  then yyQ52(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                  else yyAction60(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ55(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"i"
                  then yyAction60(strm, yyNO_MATCH)
                else if inp < #"i"
                  then yyQ53(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                else if inp = #"r"
                  then yyQ54(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                  else yyAction60(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ56(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
              else yyAction60(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"%"
              then yyQ45(strm', lastMatch)
            else if inp < #"%"
              then if inp = #"\r"
                  then yyQ43(strm', lastMatch)
                else if inp < #"\r"
                  then if inp = #"\n"
                      then yyQ44(strm', lastMatch)
                    else if inp < #"\n"
                      then if inp = #"\t"
                          then yyQ43(strm', lastMatch)
                          else yyQ42(strm', lastMatch)
                      else yyQ42(strm', lastMatch)
                else if inp = #" "
                  then yyQ43(strm', lastMatch)
                  else yyQ42(strm', lastMatch)
            else if inp = #"A"
              then yyQ47(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"="
                  then yyQ46(strm', lastMatch)
                  else yyQ42(strm', lastMatch)
            else if inp = #"a"
              then yyQ47(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ47(strm', lastMatch)
                  else yyQ42(strm', lastMatch)
            else if inp <= #"z"
              then yyQ47(strm', lastMatch)
              else yyQ42(strm', lastMatch)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yystuck(lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yystuck(lastMatch)
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yystuck(lastMatch)
                      else yyQ40(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ40(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"!"
              then yystuck(lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ40(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"("
              then yyQ41(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ40(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ17(strm', lastMatch)
            else if inp < #"."
              then if inp = #"\""
                  then yyQ11(strm', lastMatch)
                else if inp < #"\""
                  then if inp = #"\r"
                      then yyQ9(strm', lastMatch)
                    else if inp < #"\r"
                      then if inp = #"\n"
                          then yyQ10(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ9(strm', lastMatch)
                              else yyQ8(strm', lastMatch)
                          else yyQ8(strm', lastMatch)
                    else if inp = #" "
                      then yyQ9(strm', lastMatch)
                      else yyQ8(strm', lastMatch)
                else if inp = #")"
                  then yyQ14(strm', lastMatch)
                else if inp < #")"
                  then if inp = #"%"
                      then yyQ8(strm', lastMatch)
                    else if inp < #"%"
                      then if inp = #"#"
                          then yyQ8(strm', lastMatch)
                          else yyQ12(strm', lastMatch)
                    else if inp = #"("
                      then yyQ13(strm', lastMatch)
                      else yyQ8(strm', lastMatch)
                else if inp = #"+"
                  then yyQ16(strm', lastMatch)
                else if inp = #"*"
                  then yyQ15(strm', lastMatch)
                  else yyQ8(strm', lastMatch)
            else if inp = #"["
              then yyQ24(strm', lastMatch)
            else if inp < #"["
              then if inp = #"="
                  then yyQ21(strm', lastMatch)
                else if inp < #"="
                  then if inp = #";"
                      then yyQ19(strm', lastMatch)
                    else if inp < #";"
                      then if inp = #"/"
                          then yyQ18(strm', lastMatch)
                          else yyQ8(strm', lastMatch)
                      else yyQ20(strm', lastMatch)
                else if inp = #"?"
                  then yyQ23(strm', lastMatch)
                else if inp = #">"
                  then yyQ22(strm', lastMatch)
                  else yyQ8(strm', lastMatch)
            else if inp = #"_"
              then yyQ8(strm', lastMatch)
            else if inp < #"_"
              then if inp = #"]"
                  then yyQ8(strm', lastMatch)
                else if inp = #"\\"
                  then yyQ25(strm', lastMatch)
                  else yyQ26(strm', lastMatch)
            else if inp = #"|"
              then yyQ28(strm', lastMatch)
            else if inp < #"|"
              then if inp = #"{"
                  then yyQ27(strm', lastMatch)
                  else yyQ8(strm', lastMatch)
              else yyQ8(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of RE => yyQ0(!(yystrm), yyNO_MATCH)
    | DEFS => yyQ1(!(yystrm), yyNO_MATCH)
    | RECB => yyQ2(!(yystrm), yyNO_MATCH)
    | STRING => yyQ3(!(yystrm), yyNO_MATCH)
    | CHARCLASS => yyQ4(!(yystrm), yyNO_MATCH)
    | LEXSTATES => yyQ5(!(yystrm), yyNO_MATCH)
    | ACTION => yyQ6(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ7(!(yystrm), yyNO_MATCH)
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

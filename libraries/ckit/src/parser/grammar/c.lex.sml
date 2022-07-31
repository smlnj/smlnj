functor CLexFun(structure Tokens : C_TOKENS 
			 structure TokTable : TOKENTABLE 
			 sharing TokTable.Tokens = Tokens)  = struct

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
C | S | INITIAL
    structure UserDeclarations = 
      struct

(* Copyright (c) 1998 by Lucent Technologies *)

(*
 * The following replacement for c.lex should give correct (ANSI)
 * In particular, we don't allow
 * 
 * char *t = "abd
 * lkj";
 * 
 * GCC accepts this, but SGI cc does not. This program is not ANSI
 * compliant.
 *)

type svalue = Tokens.svalue
type pos = int
type lexresult = (svalue,pos) Tokens.token
type errWarn = {err: pos*pos*string->unit, warn: pos*pos*string->unit}
type lexarg =  {comLevel : int ref, 
                sourceMap : SourceMap.sourcemap,
		charlist : string list ref,
		stringstart : int ref,  (* start of current string or comment*)
	        errWarn: errWarn}

type arg = lexarg
type ('a,'b) token = ('a,'b) Tokens.token

fun ordof (s, i) = Char.ord (String.sub (s, i))
fun dec (iRef : int ref) = iRef := (!iRef) - 1
fun inc (iRef : int ref) = iRef := (!iRef) + 1
fun chr i = String.str(Char.chr i)
fun ord s = Char.ord(String.sub(s, 0))
fun explode s = CharVector.foldr (fn (c, l) => str c :: l) [] s
fun implode strList = String.concat strList
fun hd [] = (print "c.lex: hd of empty\n";
	     raise Empty)
  | hd (h :: l)	= h

val eof = fn ({comLevel,errWarn,sourceMap,stringstart,charlist}:lexarg) => 
	   let val pos = Int.max(!stringstart+2, SourceMap.currPos sourceMap)
	    in if !comLevel>0 then (#err errWarn) (!stringstart,pos, "unclosed comment" )
		  	      else ();
	       Tokens.EOF(pos,pos)
	   end	
fun addString (charlist,s:string) = charlist := s :: (!charlist)
fun makeString charlist = (implode(rev(!charlist)) before charlist := nil)

fun mkHexInt (s,a,b,errWarn:errWarn)=((case (StringCvt.scanString (LargeInt.scan StringCvt.HEX) s) of
		  SOME i => i
		| _ => ((#err errWarn)(a,b,"trouble in parsing int");Int.toLarge(0)))
			handle OverFlow => ((#err errWarn)(a,b,"large int const");Int.toLarge(0)))

fun mkHexChar (args as (s, a, b, errWarn:errWarn)) : int (* returns a character sized integer *) = 
	let val i = mkHexInt args
	in
	  if (i>255) then 
	    ((#warn errWarn) (a,b,"overflow in hexadecimal escape sequence");
	    IntInf.toInt(i mod 256))
	  else
       	    IntInf.toInt i
        end	

fun mkOctInt (s,a,b,errWarn:errWarn)
	= ((case (StringCvt.scanString (LargeInt.scan StringCvt.OCT) s) of
		  SOME i => i
		| _ => ((#err errWarn)(a,b,"trouble in parsing int");Int.toLarge(0)))
			handle OverFlow => ((#err errWarn)(a,b,"large int const");Int.toLarge(0)))


fun mkOctChar (args as (s, a, b, errWarn:errWarn)) (* returns a character sized integer *) = 
	let val i = mkOctInt args
	in
	  if (i>255) then 
	    ((#warn errWarn) (a,b,"overflow in octal escape sequence");
	    IntInf.toInt(i mod 256))
	  else
       	    IntInf.toInt i
        end	

fun mkInt (s,a,b,errWarn:errWarn) = ((case (StringCvt.scanString (LargeInt.scan StringCvt.DEC) s) of
		  SOME i => i
		| _ => ((#err errWarn)(a,b,"trouble in parsing int");Int.toLarge(0)))
			handle OverFlow => ((#err errWarn)(a,b,"large int const");Int.toLarge(0)))

fun mkRealNum (s,a,b,errWarn:errWarn) = ((case (StringCvt.scanString Real.scan s) of
		   SOME r => r
		 | _ => ((#err errWarn)(a,b,"trouble in parsing real");0.0))
			handle OverFlow => ((#err errWarn)(a,b,"large real const"); 0.0))

val backslasha = 7

fun special_char(c,fst,last,errWarn:errWarn) =
		(case c of
			"\\a" => 7
		      | "\\b" => 8
		      | "\\f" => 12
		      | "\\n" => 10
		      | "\\r" => 13
		      | "\\t" => 9
		      | "\\v" => 11
	              | _ => ordof(c,1)
	                      (* strictly speaking, should only handle
                                \?, \\, \", \', but it is common
                                to simply ignore slash, and just use next char *)
		)


(* Notes on lexer states:
   INITIAL -- predefined start state and the default token state
   S -- inside a string (entered from INTITAL with ")
   C -- inside a comment (entered from INITIAL with /* )
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
(yyarg as ({comLevel,errWarn,sourceMap,charlist,stringstart})) () = let 
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
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        if not yylastwasn then REJECT() else ((SourceMap.parseDirective sourceMap 
                         (yypos,yytext); continue()))
      end
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; continue()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN C; continue()))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; continue()))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (charlist := [""]; stringstart := yypos; YYBEGIN S; continue()))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL;Tokens.STRING(makeString charlist,!stringstart,yypos+1)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      ((#err errWarn) (!stringstart,yypos,"unclosed string");
		    SourceMap.newline sourceMap yypos;
		    YYBEGIN INITIAL; Tokens.STRING(makeString charlist,!stringstart,yypos)))
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addString(charlist,yytext); continue())
      end
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; continue()))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist,chr 0);continue()))
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addString(charlist, chr(mkOctChar(substring(yytext, 1, size(yytext)-1), yypos, yypos+size(yytext), errWarn))); continue())
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addString(charlist, chr(mkHexChar(substring(yytext, 2, size(yytext)-2), yypos, yypos+size(yytext), errWarn))); continue())
      end
fun yyAction14 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addString(charlist,chr(ordof(yytext,2)-ord("@"))); continue())
      end
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addString(charlist, chr(special_char(yytext, yypos, yypos+size(yytext), errWarn))); continue())
      end
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COLON(yypos,yypos+1)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMICOLON(yypos,yypos+1)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN(yypos,yypos+1)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN(yypos,yypos+1)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACE(yypos,yypos+1)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACE(yypos,yypos+1)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LCURLY(yypos,yypos+1)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RCURLY(yypos,yypos+1)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DOT(yypos,yypos+1)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ELIPSIS(yypos,yypos+3)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(yypos,yypos+1)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TIMES(yypos,yypos+1)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BANG(yypos,yypos+1)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.HAT(yypos,yypos+1)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUS(yypos,yypos+1)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MINUS(yypos,yypos+1)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INC(yypos,yypos+2)))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DEC(yypos,yypos+2)))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ARROW(yypos,yypos+1)))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIVIDE(yypos,yypos+1)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TILDE(yypos,yypos+1)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.QUESTION(yypos,yypos+1)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BAR(yypos,yypos+1)))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.AMP(yypos,yypos+1)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PERCENT(yypos,yypos+1)))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LTE(yypos,yypos+2)))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GTE(yypos,yypos+2)))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQ(yypos,yypos+2)))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQUALS(yypos,yypos+1)))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUSEQUALS(yypos,yypos+2)))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MINUSEQUALS(yypos,yypos+2)))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.XOREQUALS(yypos,yypos+2)))
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MODEQUALS(yypos,yypos+2)))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TIMESEQUALS(yypos,yypos+2)))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIVEQUALS(yypos,yypos+2)))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OREQUALS(yypos,yypos+2)))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ANDEQUALS(yypos,yypos+2)))
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LSHIFTEQUALS(yypos,yypos+3)))
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RSHIFTEQUALS(yypos,yypos+3)))
fun yyAction55 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LT(yypos,yypos+1)))
fun yyAction56 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GT(yypos,yypos+1)))
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEQ(yypos,yypos+2)))
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OR(yypos,yypos+2)))
fun yyAction59 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.AND(yypos,yypos+2)))
fun yyAction60 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LSHIFT(yypos,yypos+2)))
fun yyAction61 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RSHIFT(yypos,yypos+2)))
fun yyAction62 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.DECNUM(mkOctInt(yytext,yypos,yypos+size(yytext),errWarn),yypos, yypos+size(yytext)))
      end
fun yyAction63 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.DECNUM(mkHexInt(yytext,yypos,yypos+size(yytext),errWarn),yypos, yypos+size(yytext)))
      end
fun yyAction64 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.DECNUM(mkInt (yytext,yypos,yypos+size(yytext),errWarn), yypos,yypos+size(yytext)))
      end
fun yyAction65 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.REALNUM(mkRealNum(yytext,yypos,yypos+size(yytext),errWarn), yypos, yypos
+ size(yytext)))
      end
fun yyAction66 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (let val s = substring(yytext, 2, size(yytext)-3)
				     in Tokens.CCONST(IntInf.fromInt (mkOctChar(s,yypos,yypos+size(yytext),errWarn)),
						      yypos,
					      yypos+size(yytext))
	                             end)
      end
fun yyAction67 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (let val s = substring(yytext, 3, size(yytext)-4)
				     in Tokens.CCONST(IntInf.fromInt (mkHexChar(s,yypos,yypos+size(yytext),errWarn)),
						      yypos,
						      yypos+size(yytext))
	                             end)
      end
fun yyAction68 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (let val cval = ordof(yytext,1)
	                            in Tokens.CCONST(Int.toLarge cval,yypos,yypos+size(yytext))
                                    end)
      end
fun yyAction69 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.CCONST(IntInf.fromInt(special_char(substring(yytext,1,size(yytext)-2),yypos,yypos+size(yytext),errWarn)), yypos, yypos+size(yytext)))
      end
fun yyAction70 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (TokTable.checkToken(yytext,yypos))
      end
fun yyAction71 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #">"
              then if inp = #"="
                  then yyQ58(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"|"
              then yyQ59(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ60(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction70(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction70(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction70(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction70(strm, yyNO_MATCH)
                      else yyQ61(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction70(strm, yyNO_MATCH)
                  else yyQ61(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
            else if inp = #"`"
              then yyAction70(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ61(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
                  else yyAction70(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ61(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
              else yyAction70(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction70(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction70(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction70(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction70(strm, yyNO_MATCH)
                      else yyQ61(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction70(strm, yyNO_MATCH)
                  else yyQ61(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
            else if inp = #"`"
              then yyAction70(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ61(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
                  else yyAction70(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ61(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
              else yyAction70(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ64(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
              else yyAction61(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ63(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ62(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                  else yyAction56(strm, yyNO_MATCH)
              else yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ65(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction60(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ68(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
              else yyAction60(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ67(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp < #"="
              then if inp = #"<"
                  then yyQ66(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                  else yyAction55(strm, yyNO_MATCH)
              else yyAction55(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"M"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #"L"
                  then yyQ76(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ76(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"M"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #"L"
                  then yyQ75(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ75(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"V"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"V"
              then if inp = #"U"
                  then yyQ76(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ76(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"V"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"V"
              then if inp = #"M"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"M"
                  then if inp = #"L"
                      then yyQ77(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"U"
                  then yyQ76(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ77(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ76(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"L"
              then yyQ72(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp < #"L"
              then if inp = #":"
                  then yyAction65(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction65(strm, yyNO_MATCH)
                      else yyQ79(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp = #"F"
                  then yyQ72(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyAction65(strm, yyNO_MATCH)
            else if inp < #"g"
              then if inp = #"f"
                  then yyQ72(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ72(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
              else yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ79(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ79(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ78(strm', lastMatch)
            else if inp < #"-"
              then if inp = #"+"
                  then yyQ78(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"0"
              then yyQ79(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ79(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"L"
              then yyQ72(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp < #"L"
              then if inp = #"E"
                  then yyQ71(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp < #"E"
                  then if inp = #"0"
                      then yyQ80(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction65(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ80(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                      else yyAction65(strm, yyNO_MATCH)
                else if inp = #"F"
                  then yyQ72(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyAction65(strm, yyNO_MATCH)
            else if inp < #"g"
              then if inp = #"e"
                  then yyQ71(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp = #"f"
                  then yyQ72(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ72(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
              else yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ80(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ80(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"M"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp = #"/"
                      then yyAction64(strm, yyNO_MATCH)
                    else if inp < #"/"
                      then if inp = #"."
                          then yyQ69(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                          else yyAction64(strm, yyNO_MATCH)
                      else yyQ70(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"F"
                  then yyQ72(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"F"
                  then if inp = #"E"
                      then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"L"
                  then yyQ73(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"g"
              then if inp = #"V"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"V"
                  then if inp = #"U"
                      then yyQ74(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"e"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"f"
                  then yyQ72(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ73(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ74(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"M"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #":"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp = #"/"
                      then yyAction64(strm, yyNO_MATCH)
                    else if inp < #"/"
                      then if inp = #"."
                          then yyQ69(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                          else yyAction64(strm, yyNO_MATCH)
                      else yyQ70(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"F"
                  then yyQ72(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"F"
                  then if inp = #"E"
                      then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"L"
                  then yyQ73(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"g"
              then if inp = #"V"
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < #"V"
                  then if inp = #"U"
                      then yyQ74(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"e"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = #"f"
                  then yyQ72(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ73(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ74(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"M"
              then yyAction63(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #"L"
                  then yyQ88(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ88(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"M"
              then yyAction63(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #"L"
                  then yyQ87(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ87(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"V"
              then yyAction63(strm, yyNO_MATCH)
            else if inp < #"V"
              then if inp = #"U"
                  then yyQ88(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ88(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"V"
              then yyAction63(strm, yyNO_MATCH)
            else if inp < #"V"
              then if inp = #"M"
                  then yyAction63(strm, yyNO_MATCH)
                else if inp < #"M"
                  then if inp = #"L"
                      then yyQ89(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                      else yyAction63(strm, yyNO_MATCH)
                else if inp = #"U"
                  then yyQ88(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyAction63(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ89(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ88(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"U"
              then yyQ86(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            else if inp < #"U"
              then if inp = #"A"
                  then yyQ84(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ84(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction63(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ84(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                      else yyAction63(strm, yyNO_MATCH)
                else if inp = #"L"
                  then yyQ85(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp < #"L"
                  then if inp <= #"F"
                      then yyQ84(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                      else yyAction63(strm, yyNO_MATCH)
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ85(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"a"
                  then yyQ84(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp < #"a"
                  then yyAction63(strm, yyNO_MATCH)
                else if inp <= #"f"
                  then yyQ84(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ86(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ84(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ84(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ84(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ84(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ84(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ84(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"G"
              then yyAction65(strm, yyNO_MATCH)
            else if inp < #"G"
              then if inp = #"0"
                  then yyQ82(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"."
                      then yyQ69(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                      else yyAction65(strm, yyNO_MATCH)
                else if inp = #"E"
                  then yyQ71(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp < #"E"
                  then if inp <= #"9"
                      then yyQ82(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                      else yyAction65(strm, yyNO_MATCH)
                  else yyQ72(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp = #"f"
              then yyQ72(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"M"
                  then yyAction65(strm, yyNO_MATCH)
                else if inp < #"M"
                  then if inp = #"L"
                      then yyQ72(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                      else yyAction65(strm, yyNO_MATCH)
                else if inp = #"e"
                  then yyQ71(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ72(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
              else yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction62(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"M"
              then yyAction62(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #"L"
                  then yyQ93(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                  else yyAction62(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ93(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
              else yyAction62(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"M"
              then yyAction62(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #"L"
                  then yyQ92(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                  else yyAction62(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ92(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
              else yyAction62(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"V"
              then yyAction62(strm, yyNO_MATCH)
            else if inp < #"V"
              then if inp = #"U"
                  then yyQ93(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                  else yyAction62(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ93(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
              else yyAction62(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"V"
              then yyAction62(strm, yyNO_MATCH)
            else if inp < #"V"
              then if inp = #"M"
                  then yyAction62(strm, yyNO_MATCH)
                else if inp < #"M"
                  then if inp = #"L"
                      then yyQ94(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                      else yyAction62(strm, yyNO_MATCH)
                else if inp = #"U"
                  then yyQ93(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                  else yyAction62(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyAction62(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ94(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                  else yyAction62(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ93(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
              else yyAction62(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"M"
              then yyAction62(strm, yyNO_MATCH)
            else if inp < #"M"
              then if inp = #":"
                  then yyAction62(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp = #"/"
                      then yyAction62(strm, yyNO_MATCH)
                    else if inp < #"/"
                      then if inp = #"."
                          then yyQ69(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                          else yyAction62(strm, yyNO_MATCH)
                    else if inp <= #"7"
                      then yyQ81(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                      else yyQ82(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                else if inp = #"F"
                  then yyQ72(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                else if inp < #"F"
                  then if inp = #"E"
                      then yyQ71(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                      else yyAction62(strm, yyNO_MATCH)
                else if inp = #"L"
                  then yyQ90(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                  else yyAction62(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyAction62(strm, yyNO_MATCH)
            else if inp < #"g"
              then if inp = #"V"
                  then yyAction62(strm, yyNO_MATCH)
                else if inp < #"V"
                  then if inp = #"U"
                      then yyQ91(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                      else yyAction62(strm, yyNO_MATCH)
                else if inp = #"e"
                  then yyQ71(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                else if inp = #"f"
                  then yyQ72(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                  else yyAction62(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyAction62(strm, yyNO_MATCH)
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ90(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                  else yyAction62(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ91(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
              else yyAction62(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"V"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"V"
              then if inp = #"E"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"E"
                  then if inp = #"0"
                      then yyQ81(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                    else if inp < #"0"
                      then if inp = #"."
                          then yyQ69(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                          else yyAction64(strm, yyNO_MATCH)
                    else if inp = #"8"
                      then yyQ82(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                    else if inp < #"8"
                      then yyQ81(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                    else if inp <= #"9"
                      then yyQ82(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"L"
                  then yyQ73(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"L"
                  then if inp = #"F"
                      then yyQ72(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"U"
                  then yyQ74(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ73(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"e"
                  then yyQ71(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < #"e"
                  then if inp = #"X"
                      then yyQ83(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = #"f"
                  then yyQ72(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"v"
              then yyAction64(strm, yyNO_MATCH)
            else if inp < #"v"
              then if inp = #"u"
                  then yyQ74(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = #"x"
              then yyQ83(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"+"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #"+"
              then if inp = #"*"
                  then yyQ95(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"="
              then yyQ96(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ98(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction24(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ97(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyAction24(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ80(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ100(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < #"="
              then if inp = #"-"
                  then yyQ99(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = #">"
              then yyQ101(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #","
              then yyAction30(strm, yyNO_MATCH)
            else if inp < #","
              then if inp = #"+"
                  then yyQ102(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                  else yyAction30(strm, yyNO_MATCH)
            else if inp = #"="
              then yyQ103(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ104(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ112 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction67(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction67(strm, yyNO_MATCH)
      (* end case *))
fun yyQ111 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yystuck(lastMatch)
            else if inp < #":"
              then if inp = #"("
                  then yystuck(lastMatch)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ112(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ111(strm', lastMatch)
            else if inp = #"G"
              then yystuck(lastMatch)
            else if inp < #"G"
              then if inp <= #"@"
                  then yystuck(lastMatch)
                  else yyQ111(strm', lastMatch)
            else if inp = #"a"
              then yyQ111(strm', lastMatch)
            else if inp < #"a"
              then yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ111(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ110 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction69(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction69(strm, yyNO_MATCH)
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yystuck(lastMatch)
            else if inp < #":"
              then if inp = #"("
                  then yystuck(lastMatch)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ110(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ111(strm', lastMatch)
            else if inp = #"G"
              then yystuck(lastMatch)
            else if inp < #"G"
              then if inp <= #"@"
                  then yystuck(lastMatch)
                  else yyQ111(strm', lastMatch)
            else if inp = #"a"
              then yyQ111(strm', lastMatch)
            else if inp < #"a"
              then yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ111(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ115 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ116 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"'"
              then yyQ115(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ114 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"("
              then yystuck(lastMatch)
            else if inp < #"("
              then if inp = #"'"
                  then yyQ115(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"0"
              then yyQ116(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"7"
              then yyQ116(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ113 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"("
              then yystuck(lastMatch)
            else if inp < #"("
              then if inp = #"'"
                  then yyQ113(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"0"
              then yyQ114(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"7"
              then yyQ114(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ107 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"'"
              then yyQ110(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ108(strm', lastMatch)
            else if inp < #"0"
              then if inp = #"\n"
                  then yystuck(lastMatch)
                  else yyQ107(strm', lastMatch)
            else if inp = #"x"
              then yyQ109(strm', lastMatch)
            else if inp < #"x"
              then if inp <= #"7"
                  then yyQ108(strm', lastMatch)
                  else yyQ107(strm', lastMatch)
              else yyQ107(strm', lastMatch)
      (* end case *))
fun yyQ117 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction68(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction68(strm, yyNO_MATCH)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"'"
              then yyQ117(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction71(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ105(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyAction71(strm, yyNO_MATCH)
                  else yyQ105(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
            else if inp = #"\\"
              then yyQ106(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
              else yyQ105(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
      (* end case *))
fun yyQ119 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ118 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction59(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"'"
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"'"
              then if inp = #"&"
                  then yyQ118(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"="
              then yyQ119(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ120 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ120(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, lastMatch)
        | SOME(inp, strm') => yyAction0(strm, lastMatch)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ10(strm', lastMatch)
              else yyQ9(strm', lastMatch)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction71(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ10(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
              else yyQ9(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ121 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ121(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"\r"
              then if inp = #"\n"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ11(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = #"\f"
                  then yyQ11(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"!"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ11(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"#"
              then yyQ9(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"\r"
              then if inp = #"\n"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ11(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = #"\f"
                  then yyQ11(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"!"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ11(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"#"
              then yyQ9(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction71(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction71(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ42(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"%"
                  then yyQ31(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"%"
                  then if inp = #"\r"
                      then yyQ26(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp < #"\r"
                      then if inp = #"\n"
                          then yyQ5(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ27(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                              else yyQ26(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                        else if inp = #"\v"
                          then yyQ26(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                          else yyQ27(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp = #"\""
                      then yyQ29(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp < #"\""
                      then if inp = #" "
                          then yyQ27(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                        else if inp = #"!"
                          then yyQ28(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                          else yyQ26(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp = #"#"
                      then yyQ30(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyQ26(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #"+"
                  then yyQ37(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"+"
                  then if inp = #"("
                      then yyQ34(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp < #"("
                      then if inp = #"&"
                          then yyQ32(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                          else yyQ33(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp = #")"
                      then yyQ35(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyQ36(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #"."
                  then yyQ40(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"."
                  then if inp = #","
                      then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyQ39(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ41(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"\\"
              then yyQ26(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"\\"
              then if inp = #">"
                  then yyQ48(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #">"
                  then if inp = #";"
                      then yyQ45(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp < #";"
                      then if inp = #":"
                          then yyQ44(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                          else yyQ43(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp = #"<"
                      then yyQ46(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyQ47(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ50(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"?"
                      then yyQ49(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyQ26(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #"["
                  then yyQ51(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"{"
              then yyQ54(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"{"
              then if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"_"
                  then if inp = #"]"
                      then yyQ52(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyQ53(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #"`"
                  then yyQ26(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"~"
              then yyQ57(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"~"
              then if inp = #"|"
                  then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ56(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyQ26(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ22(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ22(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction13(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ22(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ22(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ22(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ22(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction15(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"@"
              then yyQ23(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"@"
              then yyAction15(strm, yyNO_MATCH)
            else if inp <= #"_"
              then yyQ23(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ25(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"7"
              then yyQ25(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ24(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"0"
              then yyAction15(strm, yyNO_MATCH)
            else if inp <= #"7"
              then yyQ24(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ24(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp < #"0"
              then yyAction11(strm, yyNO_MATCH)
            else if inp <= #"7"
              then yyQ24(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"8"
              then yyQ16(strm', lastMatch)
            else if inp < #"8"
              then if inp = #"\v"
                  then yyQ16(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ17(strm', lastMatch)
                      else yyQ16(strm', lastMatch)
                else if inp = #"0"
                  then yyQ18(strm', lastMatch)
                else if inp <= #"/"
                  then yyQ16(strm', lastMatch)
                  else yyQ19(strm', lastMatch)
            else if inp = #"_"
              then yyQ16(strm', lastMatch)
            else if inp < #"_"
              then if inp = #"^"
                  then yyQ20(strm', lastMatch)
                  else yyQ16(strm', lastMatch)
            else if inp = #"x"
              then yyQ21(strm', lastMatch)
              else yyQ16(strm', lastMatch)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"\""
              then if inp = #"\n"
                  then yyAction9(strm, yyNO_MATCH)
                  else yyQ12(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp = #"\\"
              then yyAction9(strm, yyNO_MATCH)
              else yyQ12(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ14(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp < #"\""
              then if inp = #"\n"
                  then yyQ13(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyQ12(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp = #"\\"
              then yyQ15(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyQ12(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ8(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ10(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyQ9(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"\r"
              then if inp = #"\n"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ11(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = #"\f"
                  then yyQ11(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"!"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ11(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"#"
              then yyQ9(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #" "
              then yyQ4(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #" "
              then if inp = #"\v"
                  then yyQ3(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp = #"\t"
                      then yyQ4(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp = #"\n"
                      then yyQ5(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyQ3(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #"\f"
                  then yyQ4(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ3(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"$"
              then yyQ3(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"$"
              then if inp = #"#"
                  then yyQ6(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ3(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"*"
              then yyQ7(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyQ3(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
in
  (case (!(yyss))
   of C => yyQ0(!(yystrm), yyNO_MATCH)
    | S => yyQ1(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ2(!(yystrm), yyNO_MATCH)
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

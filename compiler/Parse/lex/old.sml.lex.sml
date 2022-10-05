functor SMLLexFun (structure Tokens : SML_TOKENS)  = struct

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
LCOM | AQ | LL | LLC | F | A | L | Q | S | INITIAL | ALC | LLCQ
    structure UserDeclarations = 
      struct

(* sml.lex
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This version of the lexer supports the following SuccessorML lexical extensions:
 *
 *	- "_" as a separator in numeric literals; e.g., '123_456', '0xff_ff_ff_f3',
 *	  '123_456.1', ...
 *
 *	- end-of-line comments
 *
 *	- binary literals; e.g., '0b0101_1110'
 *)

(* The states that we use are as listed below.
 *
 * LCOM	-- SuccessorML end-of-line comment
 * A	-- bracketed comments
 * ALC  -- end-of-line comment inside bracketed comment
 * S	-- strings
 * F	-- formatting characters in a string
 * Q	-- quotation (extension)
 * AQ	-- antiquotation (extension)
 * L	-- #line comment
 * LL	-- more #line comment processing
 * LLC	-- rest of #line comment processing
 * LLCQ	-- quoted string in #line comment
 *
 * Note that this comment cannot appear where the states are defined, because
 * ml-lex's parser is broken.
 *)

open ErrorMsg;
open UserDeclarations;

structure TokTable = TokenTable(Tokens);

type svalue = Tokens.svalue

type lexresult = (svalue, pos) Tokens.token

type ('a,'b) token = ('a, 'b) Tokens.token

fun eof arg = let val pos = UserDeclarations.eof arg in Tokens.EOF(pos,pos) end

local
  fun cvt radix (s, i) = let
      (* strip any "_" separators *)
	val digits = Substring.foldr
	      (fn (#"_", ds) => ds | (d, ds) => d::ds)
		[]
		  (Substring.extract(s, i, NONE))
      (* convert to a IntInf.int value *)
	val SOME(n, _) = IntInf.scan radix List.getItem digits
	in
	  n
	end
in
val btoi = cvt StringCvt.BIN
val atoi = cvt StringCvt.DEC
val xtoi = cvt StringCvt.HEX
end (* local *)

(* strip "_" out of real literal *)
fun stripReal s = String.translate (fn #"_" => "" | c => str c) s

fun mysynch (srcmap, initpos, pos, args) =
    let fun cvt digits = getOpt(Int.fromString digits, 0)
	val resynch = SourceMap.resynch srcmap
     in case args
          of [col, line] =>
	       resynch (initpos, pos, cvt line, cvt col, NONE)
           | [file, col, line] =>
	       resynch (initpos, pos, cvt line, cvt col, SOME file)
           | _ => impossible "ill-formed args in (*#line...*)"
    end

fun has_quote s =
    let fun loop i = ((String.sub(s,i) = #"`") orelse loop (i+1))
	             handle _ => false
     in loop 0
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
#[([(#"\^@",#"\t",12),
(#"\v",#"\f",12),
(#"\^N",#"\255",12),
(#"\n",#"\n",13),
(#"\r",#"\r",14)], []), ([(#"\^@",#"\b",15),
(#"\v",#"\v",15),
(#"\^N",#"\^_",15),
(#"\"",#"\"",15),
(#"'",#"'",15),
(#")",#")",15),
(#",",#",",15),
(#".",#".",15),
(#"0",#"9",15),
(#";",#";",15),
(#"[",#"[",15),
(#"]",#"]",15),
(#"_",#"`",15),
(#"{",#"{",15),
(#"}",#"}",15),
(#"\127",#"\255",15),
(#"\t",#"\t",16),
(#"\f",#"\f",16),
(#" ",#" ",16),
(#"\n",#"\n",17),
(#"\r",#"\r",18),
(#"!",#"!",19),
(#"#",#"&",19),
(#"*",#"+",19),
(#"-",#"-",19),
(#"/",#"/",19),
(#":",#":",19),
(#"<",#"@",19),
(#"\\",#"\\",19),
(#"^",#"^",19),
(#"|",#"|",19),
(#"~",#"~",19),
(#"(",#"(",20),
(#"A",#"Z",21),
(#"a",#"z",21)], [88]), ([(#".",#".",25),
(#"0",#"0",26),
(#"1",#"9",27)], [43]), ([(#"\^@",#"\b",28),
(#"\v",#"\v",28),
(#"\r",#"\^_",28),
(#"!",#"!",28),
(#"#",#")",28),
(#"+",#"\255",28),
(#"\t",#"\t",29),
(#"\f",#"\f",29),
(#" ",#" ",29),
(#"\"",#"\"",30),
(#"*",#"*",31)], []), ([(#"\^@",#"\b",35),
(#"\v",#"\v",35),
(#"\^N",#"\^_",35),
(#"!",#"[",35),
(#"]",#"\255",35),
(#"\t",#"\t",36),
(#"\f",#"\f",36),
(#" ",#" ",36),
(#"\n",#"\n",37),
(#"\r",#"\r",38),
(#"\\",#"\\",39)], [78]), ([(#"\^@",#"\t",41),
(#"\v",#"\f",41),
(#"\^N",#"'",41),
(#")",#")",41),
(#"+",#"\255",41),
(#"\n",#"\n",42),
(#"\r",#"\r",43),
(#"(",#"(",44),
(#"*",#"*",45)], []), ([(#"\^@",#"\t",28),
(#"\v",#")",28),
(#"+",#"/",28),
(#":",#"\255",28),
(#"*",#"*",49),
(#"0",#"9",50)], []), ([(#"\^@",#"\t",53),
(#"\v",#"\f",53),
(#"\^N",#"]",53),
(#"_",#"_",53),
(#"a",#"\255",53),
(#"\n",#"\n",54),
(#"\r",#"\r",55),
(#"^",#"^",56),
(#"`",#"`",57)], []), ([(#"\^@",#"\t",60),
(#"\v",#"\f",60),
(#"\^N",#"\^_",60),
(#"\n",#"\n",61),
(#"\r",#"\r",62),
(#" ",#" ",63),
(#"\127",#"\255",63),
(#"!",#"!",64),
(#"#",#"[",64),
(#"]",#"~",64),
(#"\"",#"\"",65),
(#"\\",#"\\",66)], []), ([(#"\^@",#"\b",94),
(#"\v",#"\v",94),
(#"\^N",#"\^_",94),
(#"\127",#"\127",94),
(#"\t",#"\t",95),
(#"\f",#"\f",95),
(#" ",#" ",95),
(#"\n",#"\n",96),
(#"\r",#"\r",97),
(#"!",#"!",98),
(#"$",#"&",98),
(#"*",#"+",98),
(#"-",#"-",98),
(#"/",#"/",98),
(#":",#":",98),
(#"<",#"@",98),
(#"\\",#"\\",98),
(#"^",#"^",98),
(#"|",#"|",98),
(#"\"",#"\"",99),
(#"#",#"#",100),
(#"'",#"'",101),
(#"(",#"(",102),
(#")",#")",103),
(#",",#",",104),
(#".",#".",105),
(#"0",#"0",106),
(#"1",#"9",107),
(#";",#";",108),
(#"A",#"Z",109),
(#"a",#"z",109),
(#"[",#"[",110),
(#"]",#"]",111),
(#"_",#"_",112),
(#"`",#"`",113),
(#"{",#"{",114),
(#"}",#"}",115),
(#"~",#"~",116),
(#"\128",#"\255",117)], [0]), ([(#"\^@",#"\t",177),
(#"\v",#"\f",177),
(#"\^N",#"\255",177),
(#"\n",#"\n",178),
(#"\r",#"\r",179)], []), ([(#"\^@",#"\t",180),
(#"\v",#"!",180),
(#"#",#")",180),
(#"+",#"\255",180),
(#"\n",#"\n",181),
(#"\"",#"\"",182),
(#"*",#"*",183)], [46]), ([], [36]), ([], [35]), ([(#"\n",#"\n",13)], [35, 36]), ([], [92]), ([(#"\t",#"\t",24),
(#"\f",#"\f",24),
(#" ",#" ",24)], [88, 92]), ([], [87]), ([(#"\n",#"\n",17)], [87, 92]), ([(#"!",#"!",23),
(#"#",#"&",23),
(#"*",#"+",23),
(#"-",#"-",23),
(#"/",#"/",23),
(#":",#":",23),
(#"<",#"@",23),
(#"\\",#"\\",23),
(#"^",#"^",23),
(#"|",#"|",23),
(#"~",#"~",23)], [90, 92]), ([], [91, 92]), ([(#"'",#"'",22),
(#"0",#"9",22),
(#"A",#"Z",22),
(#"_",#"_",22),
(#"a",#"z",22)], [89, 92]), ([(#"'",#"'",22),
(#"0",#"9",22),
(#"A",#"Z",22),
(#"_",#"_",22),
(#"a",#"z",22)], [89]), ([(#"!",#"!",23),
(#"#",#"&",23),
(#"*",#"+",23),
(#"-",#"-",23),
(#"/",#"/",23),
(#":",#":",23),
(#"<",#"@",23),
(#"\\",#"\\",23),
(#"^",#"^",23),
(#"|",#"|",23),
(#"~",#"~",23)], [90]), ([(#"\t",#"\t",24),
(#"\f",#"\f",24),
(#" ",#" ",24)], [88]), ([], [41]), ([(#"0",#"0",26),
(#"1",#"9",27)], [42, 43]), ([(#"0",#"9",27)], [42]), ([], [49]), ([(#"\t",#"\t",33),
(#"\f",#"\f",33),
(#" ",#" ",33),
(#"\"",#"\"",34)], [49]), ([], [45, 49]), ([(#")",#")",32)], [49]), ([], [44, 48]), ([(#"\t",#"\t",33),
(#"\f",#"\f",33),
(#" ",#" ",33),
(#"\"",#"\"",34)], []), ([], [45]), ([], [80]), ([(#"\t",#"\t",40),
(#"\f",#"\f",40),
(#" ",#" ",40)], [78, 80]), ([], [77]), ([(#"\n",#"\n",37)], [77, 80]), ([], [79, 80]), ([(#"\t",#"\t",40),
(#"\f",#"\f",40),
(#" ",#" ",40)], [78]), ([], [56]), ([], [54]), ([(#"\n",#"\n",42)], [54, 56]), ([(#"*",#"*",47)], [56]), ([(#")",#")",46)], [56]), ([], [55]), ([(#")",#")",48)], [53]), ([], [50]), ([(#")",#")",52)], [49]), ([(#"0",#"9",51)], [40, 49]), ([(#"0",#"9",51)], [40]), ([], [48]), ([], [86]), ([], [85]), ([(#"\n",#"\n",54)], [85, 86]), ([(#"^",#"^",58),
(#"`",#"`",59)], [83, 86]), ([], [84, 86]), ([], [82]), ([], [81]), ([], [75, 76]), ([], [58, 75]), ([(#"\n",#"\n",93)], [58, 75, 76]), ([], [76]), ([(#"!",#"!",92),
(#"#",#"[",92),
(#"]",#"~",92)], [76]), ([], [57, 76]), ([(#"\^@",#"\b",67),
(#"\v",#"\v",67),
(#"\^N",#"\^_",67),
(#"!",#"!",67),
(#"#",#"/",67),
(#":",#"[",67),
(#"]",#"]",67),
(#"_",#"`",67),
(#"c",#"e",67),
(#"g",#"m",67),
(#"o",#"q",67),
(#"s",#"s",67),
(#"w",#"\127",67),
(#"\t",#"\t",68),
(#"\n",#"\n",69),
(#"\f",#"\f",70),
(#" ",#" ",70),
(#"\r",#"\r",71),
(#"\"",#"\"",72),
(#"0",#"9",73),
(#"\\",#"\\",74),
(#"^",#"^",75),
(#"a",#"a",76),
(#"b",#"b",77),
(#"f",#"f",78),
(#"n",#"n",79),
(#"r",#"r",80),
(#"t",#"t",81),
(#"u",#"u",82),
(#"v",#"v",83)], [60, 76]), ([], [74]), ([(#"\t",#"\t",68),
(#"\f",#"\f",68),
(#" ",#" ",68)], [60]), ([], [59]), ([(#"\t",#"\t",68),
(#"\f",#"\f",68),
(#" ",#" ",68)], [60, 74]), ([(#"\n",#"\n",69)], [59]), ([], [69]), ([(#"0",#"9",90)], []), ([], [68]), ([(#"\^@",#"\t",88),
(#"\v",#"?",88),
(#"`",#"\255",88),
(#"@",#"_",89)], []), ([], [61]), ([], [62]), ([], [63]), ([], [64]), ([], [65]), ([], [66]), ([(#"0",#"9",84),
(#"A",#"F",84),
(#"a",#"f",84)], [74]), ([], [67]), ([(#"0",#"9",85),
(#"A",#"F",85),
(#"a",#"f",85)], []), ([(#"0",#"9",86),
(#"A",#"F",86),
(#"a",#"f",86)], []), ([(#"0",#"9",87),
(#"A",#"F",87),
(#"a",#"f",87)], []), ([], [72]), ([], [71]), ([], [70, 71]), ([(#"0",#"9",91)], []), ([], [73]), ([(#"!",#"!",92),
(#"#",#"[",92),
(#"]",#"~",92)], [76]), ([], [58]), ([], [39]), ([(#"\t",#"\t",176),
(#"\f",#"\f",176),
(#" ",#" ",176)], [0, 39]), ([], [1]), ([(#"\n",#"\n",96)], [1, 39]), ([(#"!",#"!",118),
(#"#",#"&",118),
(#"*",#"+",118),
(#"-",#"-",118),
(#"/",#"/",118),
(#":",#":",118),
(#"<",#"@",118),
(#"\\",#"\\",118),
(#"^",#"^",118),
(#"|",#"|",118),
(#"~",#"~",118),
(#"`",#"`",121)], [17, 18, 39]), ([], [31, 39]), ([(#"!",#"!",118),
(#"#",#"&",118),
(#"*",#"+",118),
(#"-",#"-",118),
(#"/",#"/",118),
(#":",#":",118),
(#"<",#"@",118),
(#"\\",#"\\",118),
(#"^",#"^",118),
(#"|",#"|",118),
(#"~",#"~",118),
(#"\"",#"\"",174),
(#"[",#"[",175),
(#"`",#"`",121)], [17, 18, 39]), ([(#"'",#"'",173),
(#"0",#"9",173),
(#"A",#"Z",173),
(#"_",#"_",173),
(#"a",#"z",173)], [39]), ([(#"*",#"*",165)], [11, 39]), ([], [12, 39]), ([], [4, 39]), ([(#".",#".",163)], [13, 39]), ([(#".",#".",122),
(#"0",#"9",147),
(#"E",#"E",123),
(#"e",#"e",123),
(#"_",#"_",146),
(#"b",#"b",148),
(#"w",#"w",149),
(#"x",#"x",150)], [29, 39]), ([(#".",#".",122),
(#"0",#"9",145),
(#"E",#"E",123),
(#"e",#"e",123),
(#"_",#"_",146)], [21, 29, 39]), ([], [10, 39]), ([(#"'",#"'",144),
(#"0",#"9",144),
(#"A",#"Z",144),
(#"_",#"_",144),
(#"a",#"z",144)], [16, 39]), ([], [7, 39]), ([], [9, 39]), ([(#"o",#"o",136)], [3, 39]), ([(#"!",#"!",121),
(#"#",#"&",121),
(#"*",#"+",121),
(#"-",#"-",121),
(#"/",#"/",121),
(#":",#":",121),
(#"<",#"@",121),
(#"\\",#"\\",121),
(#"^",#"^",121),
(#"`",#"`",121),
(#"|",#"|",121),
(#"~",#"~",121)], [17, 19, 39]), ([], [5, 39]), ([], [6, 39]), ([(#"!",#"!",118),
(#"#",#"&",118),
(#"*",#"+",118),
(#"-",#"-",118),
(#"/",#"/",118),
(#":",#":",118),
(#"<",#"@",118),
(#"\\",#"\\",118),
(#"^",#"^",118),
(#"|",#"|",118),
(#"~",#"~",118),
(#"0",#"0",119),
(#"1",#"9",120),
(#"`",#"`",121)], [17, 18, 39]), ([], [38, 39]), ([(#"!",#"!",118),
(#"#",#"&",118),
(#"*",#"+",118),
(#"-",#"-",118),
(#"/",#"/",118),
(#":",#":",118),
(#"<",#"@",118),
(#"\\",#"\\",118),
(#"^",#"^",118),
(#"|",#"|",118),
(#"~",#"~",118),
(#"`",#"`",121)], [17, 18]), ([(#".",#".",122),
(#"0",#"9",120),
(#"E",#"E",123),
(#"e",#"e",123),
(#"_",#"_",124),
(#"b",#"b",130),
(#"x",#"x",131)], [30]), ([(#".",#".",122),
(#"0",#"9",120),
(#"E",#"E",123),
(#"e",#"e",123),
(#"_",#"_",124)], [30]), ([(#"!",#"!",121),
(#"#",#"&",121),
(#"*",#"+",121),
(#"-",#"-",121),
(#"/",#"/",121),
(#":",#":",121),
(#"<",#"@",121),
(#"\\",#"\\",121),
(#"^",#"^",121),
(#"`",#"`",121),
(#"|",#"|",121),
(#"~",#"~",121)], [17]), ([(#"0",#"9",128)], []), ([(#"0",#"9",125),
(#"~",#"~",126)], []), ([(#"0",#"9",120),
(#"_",#"_",124)], []), ([(#"0",#"9",125),
(#"_",#"_",127)], [20]), ([(#"0",#"9",125)], []), ([(#"0",#"9",125),
(#"_",#"_",127)], []), ([(#"0",#"9",128),
(#"E",#"E",123),
(#"e",#"e",123),
(#"_",#"_",129)], [20]), ([(#"0",#"9",128),
(#"_",#"_",129)], []), ([(#"0",#"1",134)], []), ([(#"0",#"9",132),
(#"A",#"F",132),
(#"a",#"f",132)], []), ([(#"0",#"9",132),
(#"A",#"F",132),
(#"a",#"f",132),
(#"_",#"_",133)], [25]), ([(#"0",#"9",132),
(#"A",#"F",132),
(#"a",#"f",132),
(#"_",#"_",133)], []), ([(#"0",#"1",134),
(#"_",#"_",135)], [23]), ([(#"0",#"1",134),
(#"_",#"_",135)], []), ([(#"v",#"v",137)], []), ([(#"e",#"e",138)], []), ([(#"r",#"r",139)], []), ([(#"l",#"l",140)], []), ([(#"o",#"o",141)], []), ([(#"a",#"a",142)], []), ([(#"d",#"d",143)], []), ([], [2]), ([(#"'",#"'",144),
(#"0",#"9",144),
(#"A",#"Z",144),
(#"_",#"_",144),
(#"a",#"z",144)], [16]), ([(#".",#".",122),
(#"0",#"9",145),
(#"E",#"E",123),
(#"e",#"e",123),
(#"_",#"_",146)], [21, 29]), ([(#"0",#"9",147),
(#"_",#"_",146)], []), ([(#".",#".",122),
(#"0",#"9",147),
(#"E",#"E",123),
(#"e",#"e",123),
(#"_",#"_",146)], [29]), ([(#"0",#"1",161)], []), ([(#"0",#"9",153),
(#"b",#"b",154),
(#"x",#"x",155)], []), ([(#"0",#"9",151),
(#"A",#"F",151),
(#"a",#"f",151)], []), ([(#"0",#"9",151),
(#"A",#"F",151),
(#"a",#"f",151),
(#"_",#"_",152)], [24]), ([(#"0",#"9",151),
(#"A",#"F",151),
(#"a",#"f",151),
(#"_",#"_",152)], []), ([(#"0",#"9",153),
(#"_",#"_",160)], [26]), ([(#"0",#"1",158)], []), ([(#"0",#"9",156),
(#"A",#"F",156),
(#"a",#"f",156)], []), ([(#"0",#"9",156),
(#"A",#"F",156),
(#"a",#"f",156),
(#"_",#"_",157)], [28]), ([(#"0",#"9",156),
(#"A",#"F",156),
(#"a",#"f",156),
(#"_",#"_",157)], []), ([(#"0",#"1",158),
(#"_",#"_",159)], [27]), ([(#"0",#"1",158),
(#"_",#"_",159)], []), ([(#"0",#"9",153),
(#"_",#"_",160)], []), ([(#"0",#"1",161),
(#"_",#"_",162)], [22]), ([(#"0",#"1",161),
(#"_",#"_",162)], []), ([(#".",#".",164)], []), ([], [14]), ([(#"#",#"#",166),
(#")",#")",167)], [37]), ([(#"l",#"l",168)], []), ([], [34]), ([(#"i",#"i",169)], []), ([(#"n",#"n",170)], []), ([(#"e",#"e",171)], []), ([(#"\t",#"\t",172),
(#"\f",#"\f",172),
(#" ",#" ",172)], []), ([(#"\t",#"\t",172),
(#"\f",#"\f",172),
(#" ",#" ",172)], [33]), ([(#"'",#"'",173),
(#"0",#"9",173),
(#"A",#"Z",173),
(#"_",#"_",173),
(#"a",#"z",173)], [15]), ([], [32]), ([], [8]), ([(#"\t",#"\t",176),
(#"\f",#"\f",176),
(#" ",#" ",176)], [0]), ([], [52]), ([], [51]), ([(#"\n",#"\n",178)], [51, 52]), ([(#"\^@",#"!",181),
(#"#",#"\255",181)], [46, 49]), ([(#"\^@",#"!",181),
(#"#",#"\255",181)], [46]), ([(#"*",#"*",185)], [49]), ([(#"\^@",#"!",181),
(#"#",#"(",181),
(#"*",#"\255",181),
(#")",#")",184)], [46, 49]), ([(#"\^@",#"!",181),
(#"#",#"\255",181)], [46, 48]), ([(#")",#")",186)], []), ([], [47])]
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
(yyarg as ({
  comLevel,
  sourceMap,
  err,
  charlist,
  stringstart,
  stringtype,
  brack_stack})) () = let 
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
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; continue()))
fun yyAction2 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      in
        yystrm := strm;
        (if !ParserControl.overloadKW then
                             Tokens.OVERLOAD(yypos,yypos+1)
                         else REJECT())
      end
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.WILD(yypos,yypos+1)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(yypos,yypos+1)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACE(yypos,yypos+1)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACE(yypos,yypos+1)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACKET(yypos,yypos+1)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.VECTORSTART(yypos,yypos+1)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACKET(yypos,yypos+1)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMICOLON(yypos,yypos+1)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (if (null(!brack_stack))
                    then ()
                    else inc (hd (!brack_stack));
                    Tokens.LPAREN(yypos,yypos+1)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (if (null(!brack_stack))
                    then ()
                    else if (!(hd (!brack_stack)) = 1)
                         then ( brack_stack := tl (!brack_stack);
                                charlist := [];
                                YYBEGIN Q)
                         else dec (hd (!brack_stack));
                    Tokens.RPAREN(yypos,yypos+1)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DOT(yypos,yypos+1)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DOTDOTDOT(yypos,yypos+3)))
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (TokTable.makeTyvar(yytext,yypos))
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (TokTable.checkId(yytext, yypos))
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if !ParserControl.quotation
                            then if (has_quote yytext)
                                 then REJECT()
                                 else TokTable.checkSymId(yytext,yypos)
                            else TokTable.checkSymId(yytext,yypos))
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (TokTable.checkSymId(yytext,yypos))
      end
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (if !ParserControl.quotation
                            then (YYBEGIN Q;
                                  charlist := [];
                                  Tokens.BEGINQ(yypos,yypos+1))
                            else (err(yypos, yypos+1)
                                     COMPLAIN "quotation implementation error"
				     nullErrorBody;
                                  Tokens.BEGINQ(yypos,yypos+1))))
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.REAL((yytext, RealLit.fromString yytext), yypos, yypos+size yytext))
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.INT((yytext, atoi(yytext, 0)), yypos, yypos+size yytext))
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.INT0((yytext, btoi(yytext, 2)), yypos, yypos+size yytext))
      end
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.INT0((yytext, IntInf.~(btoi(yytext, 3))), yypos, yypos+size yytext))
      end
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.INT0((yytext, xtoi(yytext, 2)), yypos, yypos+size yytext))
      end
fun yyAction25 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.INT0((yytext, IntInf.~(xtoi(yytext, 3))), yypos, yypos+size yytext))
      end
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.WORD((yytext, atoi(yytext, 2)), yypos, yypos+size yytext))
      end
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.WORD((yytext, btoi(yytext, 3)), yypos, yypos+size yytext))
      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.WORD((yytext, xtoi(yytext, 3)), yypos, yypos+size yytext))
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.INT0((yytext, atoi(yytext, 0)), yypos, yypos+size yytext))
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.INT0((yytext, atoi(yytext, 0)), yypos, yypos+size yytext))
      end
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (charlist := [""]; stringstart := yypos;
                    stringtype := true; YYBEGIN S; continue()))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (charlist := [""]; stringstart := yypos;
                    stringtype := false; YYBEGIN S; continue()))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN L; stringstart := yypos; comLevel := 1; continue()))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN LCOM; continue()))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; YYBEGIN INITIAL; continue()))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN A; stringstart := yypos; comLevel := 1; continue()))
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (err (yypos,yypos) COMPLAIN
		      (concat[
			  "non-Ascii character (ord ",
			  Int.toString(Char.ord(String.sub(yytext, 0))), ")"
			]) nullErrorBody;
		    continue())
      end
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (yypos,yypos) COMPLAIN "illegal token" nullErrorBody;
		    continue()))
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (YYBEGIN LL; charlist := [yytext]; continue())
      end
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      ((* cheat: take n > 0 dots *) continue()))
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (YYBEGIN LLC; addString(charlist, yytext); continue())
      end
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN LLC; addString(charlist, "1");    continue()
		(* note hack, since ml-lex chokes on the empty string for 0* *)))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; mysynch(sourceMap, !stringstart, yypos+2, !charlist);
		              comLevel := 0; charlist := []; continue()))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN LLCQ; continue()))
fun yyAction46 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addString(charlist, yytext); continue())
      end
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; mysynch(sourceMap, !stringstart, yypos+3, !charlist);
		              comLevel := 0; charlist := []; continue()))
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (!stringstart, yypos+1) WARN
                       "ill-formed (*#line...*) taken as comment" nullErrorBody;
                     YYBEGIN INITIAL; comLevel := 0; charlist := []; continue()))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (!stringstart, yypos+1) WARN
                       "ill-formed (*#line...*) taken as comment" nullErrorBody;
                     YYBEGIN A; continue()))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN ALC; continue()))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; YYBEGIN A; continue()))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;
      (inc comLevel; continue()))
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; continue()))
fun yyAction55 (strm, lastMatch : yymatch) = (yystrm := strm;
      (dec comLevel; if !comLevel=0 then YYBEGIN INITIAL else (); continue()))
fun yyAction56 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;
      (let val s = makeString charlist
                        val s = if size s <> 1 andalso not(!stringtype)
                                 then (err(!stringstart,yypos) COMPLAIN
                                      "character constant not length 1"
                                       nullErrorBody;
                                       substring(s^"x",0,1))
                                 else s
                        val t = (s,!stringstart,yypos+1)
                    in YYBEGIN INITIAL;
                       if !stringtype then Tokens.STRING t else Tokens.CHAR t
                    end))
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (!stringstart,yypos) COMPLAIN "unclosed string"
		        nullErrorBody;
		    SourceMap.newline sourceMap yypos;
		    YYBEGIN INITIAL; Tokens.STRING(makeString charlist,!stringstart,yypos)))
fun yyAction59 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap (yypos+1);
		    YYBEGIN F; continue()))
fun yyAction60 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN F; continue()))
fun yyAction61 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\007"); continue()))
fun yyAction62 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\008"); continue()))
fun yyAction63 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\012"); continue()))
fun yyAction64 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\010"); continue()))
fun yyAction65 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\013"); continue()))
fun yyAction66 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\009"); continue()))
fun yyAction67 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\011"); continue()))
fun yyAction68 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\\"); continue()))
fun yyAction69 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "\""); continue()))
fun yyAction70 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addChar(charlist,
			Char.chr(Char.ord(String.sub(yytext,2))-Char.ord #"@"));
		    continue())
      end
fun yyAction71 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err(yypos,yypos+2) COMPLAIN "illegal control escape; must be one of \
	  \@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_" nullErrorBody;
	 continue()))
fun yyAction72 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (let
                    val x = Word.toIntX (valOf (Word.fromString (String.substring(yytext, 2, 4))))
                    in
		      if x>255
			then err (yypos,yypos+4) COMPLAIN (concat[
                            "illegal string escape '", yytext, "' is too large"
                          ]) nullErrorBody
			else addChar(charlist, Char.chr x);
		      continue()
		    end)
      end
fun yyAction73 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (let val SOME x = Int.fromString (String.substring(yytext, 1, 3))
		    in
		      if x>255
			then err (yypos,yypos+4) COMPLAIN (concat[
                            "illegal string escape '", yytext, "' is too large"
                          ]) nullErrorBody
			else addChar(charlist, Char.chr x);
		      continue()
		    end)
      end
fun yyAction74 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (yypos,yypos+1) COMPLAIN "illegal string escape" nullErrorBody;
		    continue()))
fun yyAction75 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (yypos,yypos+1) COMPLAIN "illegal non-printing character in string" nullErrorBody;
                    continue()))
fun yyAction76 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addString(charlist,yytext); continue())
      end
fun yyAction77 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; continue()))
fun yyAction78 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction79 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN S; stringstart := yypos; continue()))
fun yyAction80 (strm, lastMatch : yymatch) = (yystrm := strm;
      (err (!stringstart,yypos) COMPLAIN "unclosed string"
		        nullErrorBody;
		    YYBEGIN INITIAL; Tokens.STRING(makeString charlist,!stringstart,yypos+1)))
fun yyAction81 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "`"); continue()))
fun yyAction82 (strm, lastMatch : yymatch) = (yystrm := strm;
      (addString(charlist, "^"); continue()))
fun yyAction83 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN AQ;
                    let val x = makeString charlist
                    in
                    Tokens.OBJL(x,yypos,yypos+(size x))
                    end))
fun yyAction84 (strm, lastMatch : yymatch) = (yystrm := strm;
      ((* a closing quote *)
                    YYBEGIN INITIAL;
                    let val x = makeString charlist
                    in
                    Tokens.ENDQ(x,yypos,yypos+(size x))
                    end))
fun yyAction85 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; addString(charlist,"\n"); continue()))
fun yyAction86 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addString(charlist,yytext); continue())
      end
fun yyAction87 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SourceMap.newline sourceMap yypos; continue()))
fun yyAction88 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction89 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (YYBEGIN Q;
                    let val hash = HashString.hashString yytext
                    in
                    Tokens.AQID(FastSymbol.rawSymbol(hash,yytext),
				yypos,yypos+(size yytext))
                    end)
      end
fun yyAction90 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (YYBEGIN Q;
                    let val hash = HashString.hashString yytext
                    in
                    Tokens.AQID(FastSymbol.rawSymbol(hash,yytext),
				yypos,yypos+(size yytext))
                    end)
      end
fun yyAction91 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL;
                    brack_stack := ((ref 1)::(!brack_stack));
                    Tokens.LPAREN(yypos,yypos+1)))
fun yyAction92 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (err (yypos,yypos+1) COMPLAIN
		       ("ml lexer: bad character after antiquote "^yytext)
		       nullErrorBody;
                    Tokens.AQID(FastSymbol.rawSymbol(0w0,""),yypos,yypos))
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
  yyAction71, yyAction72, yyAction73, yyAction74, yyAction75, yyAction76,
  yyAction77, yyAction78, yyAction79, yyAction80, yyAction81, yyAction82,
  yyAction83, yyAction84, yyAction85, yyAction86, yyAction87, yyAction88,
  yyAction89, yyAction90, yyAction91, yyAction92])
in
  if yyInput.eof(!(yystrm))
    then UserDeclarations.eof(yyarg)
    else (case (!(yyss))
       of LCOM => yygo yyactTable (0, !(yystrm), yyNO_MATCH)
        | AQ => yygo yyactTable (1, !(yystrm), yyNO_MATCH)
        | LL => yygo yyactTable (2, !(yystrm), yyNO_MATCH)
        | LLC => yygo yyactTable (3, !(yystrm), yyNO_MATCH)
        | F => yygo yyactTable (4, !(yystrm), yyNO_MATCH)
        | A => yygo yyactTable (5, !(yystrm), yyNO_MATCH)
        | L => yygo yyactTable (6, !(yystrm), yyNO_MATCH)
        | Q => yygo yyactTable (7, !(yystrm), yyNO_MATCH)
        | S => yygo yyactTable (8, !(yystrm), yyNO_MATCH)
        | INITIAL => yygo yyactTable (9, !(yystrm), yyNO_MATCH)
        | ALC => yygo yyactTable (10, !(yystrm), yyNO_MATCH)
        | LLCQ => yygo yyactTable (11, !(yystrm), yyNO_MATCH)
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

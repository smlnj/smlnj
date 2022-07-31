functor MDLLexFun(Tokens : MDL_TOKENS)  = struct

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
ASM | COMMENT | ASMQUOTE | INITIAL
    structure UserDeclarations = 
      struct

exception Error

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token
type lexarg = {srcMap  : SourceMapping.sourcemap,
               err     : pos * pos * string -> unit,
               MDLmode : bool
              }
type arg = lexarg

open Tokens

val commentLevel = ref 0
val metaLevel = ref 0

val asmLQuote = ref "``"
val asmRQuote = ref "''"
val asmLMeta  = ref "<"
val asmRMeta  = ref ">"

exception Error

fun init() = (commentLevel := 0; metaLevel := 0;
	      asmLQuote := "``"; asmRQuote := "''";
	      asmLMeta := "<"; asmRMeta := ">"
	     )

fun eof{srcMap,err,MDLmode} = 
    let val pos = SourceMapping.currPos srcMap
    in  EOF(pos,pos) end
fun debug _ = ()

fun check(err,_,_,SOME w) = w
  | check(err,pos,s,NONE) = 
      (err(pos,pos+size s,"bad literal "^s); raise Error)

fun strip k s = String.substring(s,k,String.size s - k)
fun scan err fmt (s,s') tok pos = 
      tok(check(err,pos,s,StringCvt.scanString fmt s'),
                pos,pos + size s) 
      handle _ => ID(s,pos,pos)

fun wdecimal(err,s,pos) = 
      scan err (Word32.scan StringCvt.DEC) (s,strip 2 s) WORD pos
fun whex(err,s,pos) = 
      scan err (Word32.scan StringCvt.HEX) (s,strip 3 s) WORD pos
fun woctal(err,s,pos) = scan err (Word32.scan StringCvt.OCT) (s,strip 3 s) WORD pos
fun wbinary(err,s,pos) = scan err (Word32.scan StringCvt.BIN) (s,strip 3 s) WORD pos
fun decimal(err,s,pos) = scan err (Int.scan StringCvt.DEC) (s,s) INT pos
fun real(err,s,pos) = scan err (Real.scan) (s,s) 
                       (fn (x,y,z) => REAL(Real.toString x, y, z)) pos
fun hex(err,s,pos) = scan err (Int.scan StringCvt.HEX) (s,strip 2 s) INT pos
fun octal(err,s,pos) = scan err (Int.scan StringCvt.OCT) (s,strip 2 s) INT pos
fun binary(err,s,pos) = scan err (Int.scan StringCvt.BIN) (s,strip 2 s) INT pos

fun decimalinf(err,s,pos) = scan err (IntInf.scan StringCvt.DEC) (s,s) INTINF pos
fun hexinf(err,s,pos) = scan err (IntInf.scan StringCvt.HEX) (s,strip 2 s) INTINF pos
fun octalinf(err,s,pos) = scan err (IntInf.scan StringCvt.OCT) (s,strip 2 s) INTINF pos
fun binaryinf(err,s,pos) = scan err (IntInf.scan StringCvt.BIN) (s,strip 2 s) INTINF pos

fun string(err,s,pos) = 
  STRING(
    check(err,pos,s,String.fromString(String.substring(s,1,String.size s-2))),
    pos, pos + size s)
fun char(err,s,pos) = 
  CHAR(check(err,pos,s,Char.fromString(String.substring(s,2,String.size s-3))),
       pos,pos + size s)
fun transAsm s = 
let fun loop(#"\\" :: #"<" ::s) = #"<"::loop s
      | loop(#"\\" :: #">" ::s) = #">"::loop s
      | loop(c::s) = c::loop s
      | loop [] = []  
in  String.implode(loop(String.explode s))
end

fun asmtext(err,s,pos) = 
  ASMTEXT(check(err,pos,s,String.fromString(transAsm s)),pos,pos + size s)

infix $$ 
fun x $$ y = y :: x 

exception NotFound

val keywords = HashTable.mkTable (HashString.hashString,op =) (13,NotFound) 
               : (string,int * int -> (svalue,int) token) HashTable.hash_table
val MDLkeywords = HashTable.mkTable (HashString.hashString,op =) (13,NotFound) 
               : (string,int * int -> (svalue,int) token) HashTable.hash_table
val symbols  = HashTable.mkTable (HashString.hashString,op =) (13,NotFound)
               : (string,int * int -> (svalue,int) token) HashTable.hash_table

val _ = app (HashTable.insert keywords) 
( nil       $$
 ("_",WILD) $$
 ("datatype", DATATYPE) $$
 ("type", TYPE) $$
 ("end", END) $$
 ("fun", FUN) $$
 ("fn", FN) $$
 ("val", VAL) $$
 ("raise", RAISE) $$
 ("handle", HANDLE) $$
 ("let", LET) $$
 ("local", LOCAL) $$
 ("exception", EXCEPTION) $$
 ("structure", STRUCTURE) $$
 ("signature", SIGNATURE) $$
 ("functor", FUNCTOR) $$
 ("sig", SIG) $$
 ("struct", STRUCT) $$
 ("sharing", SHARING) $$
 ("where", WHERE) $$
 ("withtype", WITHTYPE) $$
 ("if", IF) $$
 ("then", THEN) $$
 ("else", ELSE) $$
 ("in", IN) $$
 ("true", TRUE) $$
 ("false", FALSE) $$
 ("and", AND) $$
 ("at", AT) $$
 ("of", OF) $$
 ("case", CASE) $$
 ("as", AS) $$
 ("open", OPEN) $$
 ("op", OP) $$
 ("include", INCLUDE) $$
 ("infix", INFIX) $$
 ("infixr", INFIXR) $$
 ("nonfix", NONFIX) $$
 ("not", NOT) 
)

val _ = app (HashTable.insert MDLkeywords) 
( nil $$
 ("architecture", ARCHITECTURE) $$
 ("assembly", ASSEMBLY) $$
 ("storage", STORAGE) $$
 ("locations", LOCATIONS) $$
 ("equation", EQUATION) $$
 ("at", AT) $$
 ("vliw", VLIW) $$
 ("field", FIELD) $$
 ("fields", FIELDS) $$
 ("signed", SIGNED) $$
 ("unsigned", UNSIGNED) $$
 ("superscalar", SUPERSCALAR) $$
 ("bits", BITS) $$
 ("ordering", ORDERING) $$
 ("little", LITTLE) $$
 ("big", BIG) $$
 ("endian", ENDIAN) $$
 ("register", REGISTER) $$
 ("as", AS) $$
 ("cell", CELL) $$
 ("cells", CELLS) $$
 ("cellset", CELLSET) $$
 ("pipeline", PIPELINE) $$
 ("cpu", CPU) $$
 ("resource", RESOURCE) $$
 ("reservation", RESERVATION) $$
 ("table", TABLE) $$
 ("latency", LATENCY) $$
 ("predicated", PREDICATED) $$
 ("instruction", INSTRUCTION) $$
 ("formats", FORMATS) $$
 ("uppercase", UPPERCASE) $$
 ("lowercase", LOWERCASE) $$
 ("verbatim", VERBATIM) $$
 ("span", SPAN) $$
 ("dependent", DEPENDENT) $$
 ("always", ALWAYS) $$
 ("never", NEVER) $$
 ("delayslot", DELAYSLOT) $$
 (* ("branching", BRANCHING) $$ *)
 ("candidate", CANDIDATE) $$
 ("rtl", RTL) $$
 ("debug", DEBUG) $$
 ("aliasing", ALIASING) $$
 ("aggregable",AGGREGABLE) 
)

val _ = app (HashTable.insert symbols) 
(
  nil $$
  ("=",	EQ) $$
  ("*",	TIMES) $$
  (":",	COLON) $$
  (":>",COLONGREATER) $$
  ("|", BAR) $$
  ("->", ARROW) $$
  ("=>", DARROW) $$
  ("#", HASH) $$
  ("!", DEREF) $$
  ("^^", CONCAT)
)

fun lookup(MDLmode,s,yypos) =
let val l = String.size s
    fun id() = ID(UniqueSymbol.toString
                    (UniqueSymbol.fromString s), yypos, yypos + l)
in  HashTable.lookup keywords s (yypos,yypos + l) 
      handle _ => 
        (if MDLmode then 
           (HashTable.lookup MDLkeywords s (yypos,yypos + l) handle _ => id())
         else id()
        )
end

fun lookupSym(s,yypos) =
let val l = String.size s
in  HashTable.lookup symbols s (yypos,yypos + l) 
      handle _ => SYMBOL(UniqueSymbol.toString
                     (UniqueSymbol.fromString s), yypos, yypos + l)
end



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
#[
([(#"\^@",#"\b",4),
(#"\v",#"\^_",4),
(#"\127",#"\255",4),
(#"\t",#"\t",5),
(#" ",#" ",5),
(#"\n",#"\n",6),
(#"!",#"!",7),
(#"%",#"&",7),
(#"*",#"+",7),
(#"-",#"-",7),
(#"/",#"/",7),
(#":",#":",7),
(#"?",#"@",7),
(#"^",#"^",7),
(#"|",#"|",7),
(#"\"",#"\"",8),
(#"#",#"#",9),
(#"$",#"$",10),
(#"'",#"'",11),
(#"(",#"(",12),
(#")",#")",13),
(#",",#",",14),
(#".",#".",15),
(#"0",#"0",16),
(#"1",#"9",17),
(#";",#";",18),
(#"<",#"<",19),
(#"=",#"=",20),
(#">",#">",21),
(#"A",#"Z",22),
(#"_",#"_",22),
(#"b",#"b",22),
(#"e",#"l",22),
(#"o",#"o",22),
(#"q",#"q",22),
(#"s",#"z",22),
(#"[",#"[",23),
(#"\\",#"\\",24),
(#"]",#"]",25),
(#"`",#"`",26),
(#"a",#"a",27),
(#"c",#"c",28),
(#"d",#"d",29),
(#"m",#"m",30),
(#"n",#"n",31),
(#"p",#"p",32),
(#"r",#"r",33),
(#"{",#"{",34),
(#"}",#"}",35),
(#"~",#"~",36)], []), ([(#"\^@",#"\b",120),
(#"\v",#"\^_",120),
(#"!",#"'",120),
(#")",#")",120),
(#"+",#"\255",120),
(#"\t",#"\t",121),
(#" ",#" ",121),
(#"\n",#"\n",6),
(#"(",#"(",122),
(#"*",#"*",123)], []), ([(#"\^@",#"\b",126),
(#"\v",#" ",126),
(#"\"",#"\"",126),
(#"(",#")",126),
(#",",#",",126),
(#"0",#"9",126),
(#";",#";",126),
(#"A",#"[",126),
(#"]",#"]",126),
(#"_",#"_",126),
(#"a",#"{",126),
(#"}",#"}",126),
(#"\127",#"\255",126),
(#"\t",#"\t",4),
(#"\n",#"\n",127),
(#"!",#"!",128),
(#"#",#"&",128),
(#"*",#"+",128),
(#"-",#"/",128),
(#":",#":",128),
(#"?",#"@",128),
(#"^",#"^",128),
(#"|",#"|",128),
(#"'",#"'",129),
(#"<",#"<",130),
(#"=",#"=",131),
(#">",#">",132),
(#"\\",#"\\",133),
(#"`",#"`",134),
(#"~",#"~",135)], []), ([(#"\^@",#"\b",4),
(#"\v",#"\^_",4),
(#"\\",#"\\",4),
(#"\127",#"\255",4),
(#"\t",#"\t",5),
(#" ",#" ",5),
(#"\n",#"\n",6),
(#"!",#"!",145),
(#"%",#"&",145),
(#"*",#"+",145),
(#"-",#"-",145),
(#"/",#"/",145),
(#":",#":",145),
(#"<",#"@",145),
(#"^",#"^",145),
(#"|",#"|",145),
(#"\"",#"\"",8),
(#"#",#"#",146),
(#"$",#"$",147),
(#"'",#"'",148),
(#"(",#"(",149),
(#")",#")",13),
(#",",#",",14),
(#".",#".",150),
(#"0",#"0",16),
(#"1",#"9",17),
(#";",#";",18),
(#"A",#"Z",22),
(#"_",#"_",22),
(#"b",#"b",22),
(#"e",#"l",22),
(#"o",#"o",22),
(#"q",#"q",22),
(#"s",#"z",22),
(#"[",#"[",23),
(#"]",#"]",25),
(#"`",#"`",151),
(#"a",#"a",27),
(#"c",#"c",28),
(#"d",#"d",29),
(#"m",#"m",30),
(#"n",#"n",31),
(#"p",#"p",32),
(#"r",#"r",33),
(#"{",#"{",34),
(#"}",#"}",35),
(#"~",#"~",152)], []), ([], [48]), ([], [1, 48]), ([], [0]), ([(#"!",#"!",37),
(#"#",#"&",37),
(#"*",#"+",37),
(#"-",#"/",37),
(#":",#":",37),
(#"=",#"=",37),
(#"?",#"@",37),
(#"^",#"^",37),
(#"|",#"|",37),
(#"~",#"~",37)], [43, 48]), ([(#"\^@",#"\b",117),
(#"\v",#"!",117),
(#"#",#"[",117),
(#"]",#"\255",117),
(#"\"",#"\"",118),
(#"\\",#"\\",119)], [48]), ([(#"!",#"!",37),
(#"#",#"&",37),
(#"*",#"+",37),
(#"-",#"/",37),
(#":",#":",37),
(#"=",#"=",37),
(#"?",#"@",37),
(#"^",#"^",37),
(#"|",#"|",37),
(#"~",#"~",37),
(#"\"",#"\"",113),
(#"[",#"[",114)], [43, 48]), ([(#"!",#"!",37),
(#"#",#"&",37),
(#"*",#"+",37),
(#"-",#"/",37),
(#":",#":",37),
(#"=",#"=",37),
(#"?",#"@",37),
(#"^",#"^",37),
(#"|",#"|",37),
(#"~",#"~",37)], [19, 43, 48]), ([(#"'",#"'",111),
(#"A",#"Z",112),
(#"_",#"_",112),
(#"a",#"z",112)], [43, 48]), ([], [29, 48]), ([], [30, 48]), ([], [36, 48]), ([(#"!",#"!",37),
(#"#",#"&",37),
(#"*",#"+",37),
(#"-",#"-",37),
(#"/",#"/",37),
(#":",#":",37),
(#"=",#"=",37),
(#"?",#"@",37),
(#"^",#"^",37),
(#"|",#"|",37),
(#"~",#"~",37),
(#".",#".",109)], [38, 43, 48]), ([(#".",#".",41),
(#"0",#"7",93),
(#"8",#"9",38),
(#"b",#"b",94),
(#"i",#"i",42),
(#"w",#"w",95),
(#"x",#"x",96)], [4, 48]), ([(#".",#".",41),
(#"0",#"9",38),
(#"i",#"i",42)], [4, 48]), ([], [37, 48]), ([(#"<",#"<",92)], [43, 48]), ([(#"!",#"!",37),
(#"#",#"&",37),
(#"*",#"+",37),
(#"-",#"/",37),
(#":",#":",37),
(#"=",#"=",37),
(#"?",#"@",37),
(#"^",#"^",37),
(#"|",#"|",37),
(#"~",#"~",37),
(#">",#">",40)], [43, 48]), ([(#">",#">",91)], [43, 48]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"z",47)], [27, 48]), ([], [31, 48]), ([(#"\^@",#"\t",40),
(#"\v",#"\255",40)], [48]), ([], [33, 48]), ([(#"`",#"`",90)], [43, 48]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"r",47),
(#"t",#"z",47),
(#"s",#"s",87)], [27, 48]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"b",#"z",47),
(#"a",#"a",78)], [27, 48]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",69)], [27, 48]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"b",47),
(#"d",#"z",47),
(#"c",#"c",67)], [27, 48]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"t",47),
(#"v",#"z",47),
(#"u",#"u",58)], [27, 48]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"b",#"z",47),
(#"a",#"a",51)], [27, 48]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"s",47),
(#"u",#"z",47),
(#"t",#"t",48)], [27, 48]), ([], [34, 48]), ([], [35, 48]), ([(#"!",#"!",37),
(#"#",#"&",37),
(#"*",#"+",37),
(#"-",#"/",37),
(#":",#":",37),
(#"=",#"=",37),
(#"?",#"@",37),
(#"^",#"^",37),
(#"|",#"|",37),
(#"~",#"~",37),
(#"0",#"9",38),
(#">",#">",39)], [43, 48]), ([(#"!",#"!",37),
(#"#",#"&",37),
(#"*",#"+",37),
(#"-",#"/",37),
(#":",#":",37),
(#"=",#"=",37),
(#"?",#"@",37),
(#"^",#"^",37),
(#"|",#"|",37),
(#"~",#"~",37)], [43]), ([(#".",#".",41),
(#"0",#"9",38),
(#"i",#"i",42)], [4]), ([(#">",#">",40)], []), ([], [43]), ([(#"0",#"9",43)], []), ([], [8]), ([(#"0",#"9",43),
(#"e",#"e",44)], [18]), ([(#"0",#"9",45),
(#"~",#"~",46)], []), ([(#"0",#"9",45)], [18]), ([(#"0",#"9",45)], []), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"z",47)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"k",47),
(#"m",#"z",47),
(#"l",#"l",49)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"z",47),
(#":",#":",50)], [27]), ([], [22]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"c",47),
(#"e",#"z",47),
(#"d",#"d",52)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"c",47),
(#"e",#"z",47),
(#"d",#"d",53)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"h",47),
(#"j",#"z",47),
(#"i",#"i",54)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"m",47),
(#"o",#"z",47),
(#"n",#"n",55)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"f",47),
(#"h",#"z",47),
(#"g",#"g",56)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"z",47),
(#":",#":",57)], [27]), ([], [24]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"k",47),
(#"m",#"z",47),
(#"l",#"l",59)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"k",47),
(#"m",#"z",47),
(#"l",#"l",60)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"h",47),
(#"j",#"z",47),
(#"i",#"i",61)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"e",47),
(#"g",#"z",47),
(#"f",#"f",62)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"h",47),
(#"j",#"z",47),
(#"i",#"i",63)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",64)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"c",47),
(#"e",#"z",47),
(#"d",#"d",65)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"z",47),
(#":",#":",66)], [27]), ([], [25]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"z",47),
(#":",#":",68)], [27]), ([], [21]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"k",47),
(#"m",#"z",47),
(#"l",#"l",70)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"b",#"z",47),
(#"a",#"a",71)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"x",47),
(#"z",#"z",47),
(#"y",#"y",72)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"r",47),
(#"t",#"z",47),
(#"s",#"s",73)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"k",47),
(#"m",#"z",47),
(#"l",#"l",74)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"n",47),
(#"p",#"z",47),
(#"o",#"o",75)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"s",47),
(#"u",#"z",47),
(#"t",#"t",76)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"z",47),
(#":",#":",77)], [27]), ([], [23]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"m",47),
(#"o",#"z",47),
(#"n",#"n",79)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"c",47),
(#"e",#"z",47),
(#"d",#"d",80)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"h",47),
(#"j",#"z",47),
(#"i",#"i",81)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"c",47),
(#"e",#"z",47),
(#"d",#"d",82)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"b",#"z",47),
(#"a",#"a",83)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"s",47),
(#"u",#"z",47),
(#"t",#"t",84)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",85)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"z",47),
(#":",#":",86)], [27]), ([], [26]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"l",47),
(#"n",#"z",47),
(#"m",#"m",88)], [27]), ([(#"'",#"'",47),
(#"0",#"9",47),
(#"A",#"Z",47),
(#"_",#"_",47),
(#"a",#"z",47),
(#":",#":",89)], [27]), ([], [20]), ([(#"`",#"`",90)], [43]), ([(#">",#">",91)], [43]), ([(#"<",#"<",92)], [43]), ([(#".",#".",41),
(#"0",#"7",93),
(#"8",#"9",38),
(#"i",#"i",108)], [4, 6]), ([(#"0",#"1",106)], []), ([(#"0",#"0",99),
(#"1",#"9",100),
(#"b",#"b",101),
(#"x",#"x",102)], []), ([(#"0",#"9",97),
(#"A",#"F",97),
(#"a",#"f",97)], []), ([(#"0",#"9",97),
(#"A",#"F",97),
(#"a",#"f",97),
(#"i",#"i",98)], [5]), ([], [9]), ([(#"0",#"7",105),
(#"8",#"9",100)], [12]), ([(#"0",#"9",100)], [12]), ([(#"0",#"1",104)], []), ([(#"0",#"9",103),
(#"A",#"F",103),
(#"a",#"f",103)], []), ([(#"0",#"9",103),
(#"A",#"F",103),
(#"a",#"f",103)], [13]), ([(#"0",#"1",104)], [15]), ([(#"0",#"7",105),
(#"8",#"9",100)], [12, 14]), ([(#"0",#"1",106),
(#"i",#"i",107)], [7]), ([], [11]), ([], [8, 10]), ([(#"!",#"!",37),
(#"#",#"&",37),
(#"*",#"+",37),
(#"-",#"-",37),
(#"/",#"/",37),
(#":",#":",37),
(#"=",#"=",37),
(#"?",#"@",37),
(#"^",#"^",37),
(#"|",#"|",37),
(#"~",#"~",37),
(#".",#".",110)], [39, 43]), ([(#"!",#"!",37),
(#"#",#"&",37),
(#"*",#"+",37),
(#"-",#"/",37),
(#":",#":",37),
(#"=",#"=",37),
(#"?",#"@",37),
(#"^",#"^",37),
(#"|",#"|",37),
(#"~",#"~",37)], [40, 43]), ([(#"'",#"'",111)], [43]), ([(#"'",#"'",112),
(#"0",#"9",112),
(#"A",#"Z",112),
(#"_",#"_",112),
(#"a",#"z",112)], [28]), ([(#"\^@",#"\b",113),
(#"\v",#"!",113),
(#"#",#"[",113),
(#"]",#"\255",113),
(#"\"",#"\"",115),
(#"\\",#"\\",116)], []), ([], [32]), ([], [17]), ([(#"\^@",#"\t",113),
(#"\v",#"\255",113)], []), ([(#"\^@",#"\b",117),
(#"\v",#"!",117),
(#"#",#"[",117),
(#"]",#"\255",117),
(#"\"",#"\"",118),
(#"\\",#"\\",119)], []), ([], [16]), ([(#"\^@",#"\t",117),
(#"\v",#"\255",117)], []), ([], [47, 48]), ([], [1, 47, 48]), ([(#"*",#"*",125)], [47, 48]), ([(#")",#")",124)], [47, 48]), ([], [45]), ([], [46]), ([(#"\^@",#"\b",136),
(#"\v",#"&",136),
(#"(",#";",136),
(#"=",#"=",136),
(#"?",#"\255",136)], [44, 48]), ([], [2]), ([(#"\^@",#"\b",136),
(#"\v",#" ",136),
(#"\"",#"\"",136),
(#"(",#")",136),
(#",",#",",136),
(#"0",#"9",136),
(#";",#";",136),
(#"A",#"]",136),
(#"_",#"{",136),
(#"}",#"}",136),
(#"\127",#"\255",136),
(#"!",#"!",137),
(#"#",#"&",137),
(#"*",#"+",137),
(#"-",#"/",137),
(#":",#":",137),
(#"=",#"=",137),
(#"?",#"@",137),
(#"^",#"^",137),
(#"|",#"|",137),
(#"~",#"~",137)], [42, 44, 48]), ([(#"'",#"'",144)], [42, 44, 48]), ([(#"<",#"<",143)], [42, 48]), ([(#"\^@",#"\b",136),
(#"\v",#" ",136),
(#"\"",#"\"",136),
(#"(",#")",136),
(#",",#",",136),
(#"0",#"9",136),
(#";",#";",136),
(#"A",#"]",136),
(#"_",#"{",136),
(#"}",#"}",136),
(#"\127",#"\255",136),
(#"!",#"!",137),
(#"#",#"&",137),
(#"*",#"+",137),
(#"-",#"/",137),
(#":",#":",137),
(#"=",#"=",137),
(#"?",#"@",137),
(#"^",#"^",137),
(#"|",#"|",137),
(#"~",#"~",137),
(#">",#">",139)], [42, 44, 48]), ([(#">",#">",142)], [42, 48]), ([(#"\^@",#"\b",141),
(#"\v",#"&",141),
(#"(",#";",141),
(#"=",#"=",141),
(#"?",#"\255",141),
(#"\t",#"\t",139),
(#"'",#"'",139),
(#"<",#"<",139),
(#">",#">",139)], [44, 48]), ([(#"\^@",#"\b",136),
(#"\v",#"&",136),
(#"(",#";",136),
(#"=",#"=",136),
(#"?",#"_",136),
(#"a",#"\255",136),
(#"`",#"`",140)], [42, 44, 48]), ([(#"\^@",#"\b",136),
(#"\v",#" ",136),
(#"\"",#"\"",136),
(#"(",#")",136),
(#",",#",",136),
(#"0",#"9",136),
(#";",#";",136),
(#"A",#"]",136),
(#"_",#"{",136),
(#"}",#"}",136),
(#"\127",#"\255",136),
(#"!",#"!",137),
(#"#",#"&",137),
(#"*",#"+",137),
(#"-",#"/",137),
(#":",#":",137),
(#"=",#"=",137),
(#"?",#"@",137),
(#"^",#"^",137),
(#"|",#"|",137),
(#"~",#"~",137),
(#">",#">",138)], [42, 44, 48]), ([(#"\^@",#"\b",136),
(#"\v",#"&",136),
(#"(",#";",136),
(#"=",#"=",136),
(#"?",#"\255",136)], [44]), ([(#"\^@",#"\b",136),
(#"\v",#" ",136),
(#"\"",#"\"",136),
(#"(",#")",136),
(#",",#",",136),
(#"0",#"9",136),
(#";",#";",136),
(#"A",#"]",136),
(#"_",#"{",136),
(#"}",#"}",136),
(#"\127",#"\255",136),
(#"!",#"!",137),
(#"#",#"&",137),
(#"*",#"+",137),
(#"-",#"/",137),
(#":",#":",137),
(#"=",#"=",137),
(#"?",#"@",137),
(#"^",#"^",137),
(#"|",#"|",137),
(#"~",#"~",137)], [42, 44]), ([(#">",#">",139)], []), ([], [42]), ([(#"\^@",#"\b",136),
(#"\v",#"&",136),
(#"(",#";",136),
(#"=",#"=",136),
(#"?",#"_",136),
(#"a",#"\255",136),
(#"`",#"`",140)], [42, 44]), ([(#"\^@",#"\b",136),
(#"\v",#"&",136),
(#"(",#";",136),
(#"=",#"=",136),
(#"?",#"\255",136)], [42, 44]), ([(#">",#">",142)], [42]), ([(#"<",#"<",143)], [42]), ([(#"'",#"'",144)], [42]), ([(#"!",#"!",153),
(#"#",#"&",153),
(#"*",#"+",153),
(#"-",#"-",153),
(#"/",#"/",153),
(#":",#":",153),
(#"<",#"@",153),
(#"^",#"^",153),
(#"|",#"|",153),
(#"~",#"~",153)], [41, 48]), ([(#"!",#"!",153),
(#"#",#"&",153),
(#"*",#"+",153),
(#"-",#"-",153),
(#"/",#"/",153),
(#":",#":",153),
(#"<",#"@",153),
(#"^",#"^",153),
(#"|",#"|",153),
(#"~",#"~",153),
(#"\"",#"\"",113),
(#"[",#"[",114)], [41, 48]), ([(#"!",#"!",153),
(#"#",#"&",153),
(#"*",#"+",153),
(#"-",#"-",153),
(#"/",#"/",153),
(#":",#":",153),
(#"<",#"@",153),
(#"^",#"^",153),
(#"|",#"|",153),
(#"~",#"~",153)], [19, 41, 48]), ([(#"'",#"'",154),
(#"A",#"Z",112),
(#"_",#"_",112),
(#"a",#"z",112)], [48]), ([(#"*",#"*",157)], [29, 48]), ([(#".",#".",155)], [38, 48]), ([(#"`",#"`",154)], [48]), ([(#"!",#"!",153),
(#"#",#"&",153),
(#"*",#"+",153),
(#"-",#"-",153),
(#"/",#"/",153),
(#":",#":",153),
(#"<",#"@",153),
(#"^",#"^",153),
(#"|",#"|",153),
(#"~",#"~",153),
(#"0",#"9",38)], [41, 48]), ([(#"!",#"!",153),
(#"#",#"&",153),
(#"*",#"+",153),
(#"-",#"-",153),
(#"/",#"/",153),
(#":",#":",153),
(#"<",#"@",153),
(#"^",#"^",153),
(#"|",#"|",153),
(#"~",#"~",153)], [41]), ([], [41]), ([(#".",#".",156)], [39]), ([], [40]), ([], [3])]

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
(yyarg as ({srcMap,err,MDLmode})) () = let 
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
      (SourceMapping.newline srcMap yypos; continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (err(yypos,yypos+size yytext,
                                "newline in assembly text!"); continue())
      end
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (commentLevel := 1; YYBEGIN COMMENT; continue()))
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (decimal(err,yytext,yypos))
      end
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (hex(err,yytext,yypos))
      end
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (octal(err,yytext,yypos))
      end
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (binary(err,yytext,yypos))
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (decimalinf(err,yytext,yypos))
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (hexinf(err,yytext,yypos))
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (octalinf(err,yytext,yypos))
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (binaryinf(err,yytext,yypos))
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (wdecimal(err,yytext,yypos))
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (whex(err,yytext,yypos))
      end
fun yyAction14 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (woctal(err,yytext,yypos))
      end
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (wbinary(err,yytext,yypos))
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (string(err,yytext,yypos))
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (char(err,yytext,yypos))
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (real(err,yytext,yypos))
      end
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (if MDLmode then DOLLAR(yypos,yypos+1)
                            else SYMBOL("$",yypos,yypos+1)))
fun yyAction20 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if MDLmode then 
                              ASM_COLON(yypos,yypos+size yytext) else REJECT())
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if MDLmode then 
                               MC_COLON(yypos,yypos+size yytext) else REJECT())
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if MDLmode then 
                               RTL_COLON(yypos,yypos+size yytext) else REJECT())
      end
fun yyAction23 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if MDLmode then
                               DELAYSLOT_COLON(yypos,size yytext) else REJECT())
      end
fun yyAction24 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if MDLmode then  
                               PADDING_COLON(yypos,size yytext) else REJECT())
      end
fun yyAction25 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if MDLmode then
                                NULLIFIED_COLON(yypos,size yytext) else REJECT())
      end
fun yyAction26 (strm, lastMatch : yymatch) = let
      val oldStrm = !(yystrm)
      fun REJECT () = (yystrm := oldStrm; yystuck(lastMatch))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if MDLmode then  
                                CANDIDATE_COLON(yypos,size yytext) else REJECT())
      end
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (lookup(MDLmode,yytext,yypos))
      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (TYVAR(yytext,yypos,yypos + size yytext))
      end
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (LPAREN(yypos,yypos+1)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (RPAREN(yypos,yypos+1)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (LBRACKET(yypos,yypos+1)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (LHASHBRACKET(yypos,yypos+1)))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (RBRACKET(yypos,yypos+1)))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (LBRACE(yypos,yypos+1)))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (RBRACE(yypos,yypos+1)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (COMMA(yypos,yypos+1)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (SEMICOLON(yypos,yypos+1)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (DOT(yypos,yypos+1)))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (DOTDOT(yypos,yypos+2)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (DOTDOT(yypos,yypos+3)))
fun yyAction41 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if yytext = !asmLQuote then
				(debug("lquote "^yytext^"\n");
				 YYBEGIN ASMQUOTE; 
                                 LDQUOTE(yypos,yypos+size yytext))
			    else
			        lookupSym(yytext,yypos))
      end
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if yytext = !asmRQuote then
				(if !metaLevel <> 0 then
                                    err(yypos,yypos+size yytext,
                                       "Mismatch between "^(!asmLMeta)^
                                          " and "^(!asmRMeta)) else ();
				 debug("rquote "^yytext^"\n");
                                 YYBEGIN INITIAL; 
                                 RDQUOTE(yypos,yypos+size yytext))
			    else if yytext = !asmLMeta then
				(metaLevel := !metaLevel + 1;
				 debug("lmeta "^yytext^"\n");
				 YYBEGIN ASM; LMETA(yypos,yypos+size yytext))
			    else
			        asmtext(err,yytext,yypos))
      end
fun yyAction43 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (if yytext = !asmRMeta then
				(metaLevel := !metaLevel - 1;
				 debug("rmeta "^yytext^"("^Int.toString(!metaLevel)^")\n");
				 if !metaLevel = 0 then YYBEGIN ASMQUOTE
				 else (); RMETA(yypos,yypos+size yytext))
			    else
			        lookupSym(yytext,yypos))
      end
fun yyAction44 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (debug("text="^yytext^"\n"); 
                            asmtext(err,yytext,yypos))
      end
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (commentLevel := !commentLevel - 1;
			    if !commentLevel = 0 then YYBEGIN INITIAL else (); 
			    continue()))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (commentLevel := !commentLevel + 1; continue()))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction48 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (err(yypos,yypos+size yytext,
                                "unknown character "^String.toString yytext);
                            continue())
      end
val yyactTable = Vector.fromList([yyAction0, yyAction1, yyAction2, yyAction3,
  yyAction4, yyAction5, yyAction6, yyAction7, yyAction8, yyAction9, yyAction10,
  yyAction11, yyAction12, yyAction13, yyAction14, yyAction15, yyAction16,
  yyAction17, yyAction18, yyAction19, yyAction20, yyAction21, yyAction22,
  yyAction23, yyAction24, yyAction25, yyAction26, yyAction27, yyAction28,
  yyAction29, yyAction30, yyAction31, yyAction32, yyAction33, yyAction34,
  yyAction35, yyAction36, yyAction37, yyAction38, yyAction39, yyAction40,
  yyAction41, yyAction42, yyAction43, yyAction44, yyAction45, yyAction46,
  yyAction47, yyAction48])
in
  if yyInput.eof(!(yystrm))
    then UserDeclarations.eof(yyarg)
    else (case (!(yyss))
       of ASM => yygo yyactTable (0, !(yystrm), yyNO_MATCH)
        | COMMENT => yygo yyactTable (1, !(yystrm), yyNO_MATCH)
        | ASMQUOTE => yygo yyactTable (2, !(yystrm), yyNO_MATCH)
        | INITIAL => yygo yyactTable (3, !(yystrm), yyNO_MATCH)
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

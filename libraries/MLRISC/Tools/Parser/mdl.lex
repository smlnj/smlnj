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

%%

%header (functor MDLLexFun(Tokens : MDL_TOKENS));
%arg ({srcMap,err,MDLmode});
%reject

alpha=[A-Za-z];
digit=[0-9];
id=[A-Za-z_][A-Za-z0-9_\']*;
tyvar=\'{id};
decimal={digit}+;
integer=~?{decimal};
real={integer}\.{decimal}(e{integer})?;
octal=0[0-7]+;
hex=0x[0-9a-fA-F]+;
binary=0b[0-1]+;
wdecimal=0w{digit}+;
woctal=0w0[0-7]+;
whex=0wx[0-9a-fA-F]+;
wbinary=0wb[0-1]+;
ws=[\ \t];
string=\"([^\\\n\t"]|\\.)*\";
char=#\"([^\\\n\t"]|\\.)*\";
sym1=(\-|[=\.+~/*:!@#$%^&*|?])+;
sym2=`+|'+|\<+|\>+|\=\>|~\>\>;
sym3=\\.;
asmsymbol={sym1}|{sym2}|{sym3};
symbol=(\-|[=+~/*:!@#$%^&*|?<>])+|``|'';
asmtext=([^\n\t<>']+|');
inf=i;

%s COMMENT ASM ASMQUOTE;

%%
<INITIAL,COMMENT,ASM>\n		=> (SourceMapping.newline srcMap yypos; continue());
<INITIAL,COMMENT,ASM>{ws}	=> (continue());
<ASMQUOTE>\n		=> (err(yypos,yypos+size yytext,
                                "newline in assembly text!"); continue());
<INITIAL>"(*"		=> (commentLevel := 1; YYBEGIN COMMENT; continue());
<INITIAL,ASM>{integer}	=> (decimal(err,yytext,yypos));
<INITIAL,ASM>{hex}	=> (hex(err,yytext,yypos));
<INITIAL,ASM>{octal}	=> (octal(err,yytext,yypos));
<INITIAL,ASM>{binary}	=> (binary(err,yytext,yypos));
<INITIAL,ASM>{integer}{inf}	=> (decimalinf(err,yytext,yypos));
<INITIAL,ASM>{hex}{inf}		=> (hexinf(err,yytext,yypos));
<INITIAL,ASM>{octal}{inf}	=> (octalinf(err,yytext,yypos));
<INITIAL,ASM>{binary}{inf}	=> (binaryinf(err,yytext,yypos));
<INITIAL,ASM>{wdecimal}	=> (wdecimal(err,yytext,yypos));
<INITIAL,ASM>{whex}	=> (whex(err,yytext,yypos));
<INITIAL,ASM>{woctal}	=> (woctal(err,yytext,yypos));
<INITIAL,ASM>{wbinary}	=> (wbinary(err,yytext,yypos));
<INITIAL,ASM>{string}	=> (string(err,yytext,yypos));
<INITIAL,ASM>{char}	=> (char(err,yytext,yypos));
<INITIAL,ASM>{real}	=> (real(err,yytext,yypos));
<INITIAL,ASM>"$"	=> (if MDLmode then DOLLAR(yypos,yypos+1)
                            else SYMBOL("$",yypos,yypos+1));
<INITIAL,ASM>"asm:"     => (if MDLmode then 
                              ASM_COLON(yypos,yypos+size yytext) else REJECT());
<INITIAL,ASM>"mc:"      => (if MDLmode then 
                               MC_COLON(yypos,yypos+size yytext) else REJECT());
<INITIAL,ASM>"rtl:"     => (if MDLmode then 
                               RTL_COLON(yypos,yypos+size yytext) else REJECT());
<INITIAL,ASM>"delayslot:" => (if MDLmode then
                               DELAYSLOT_COLON(yypos,size yytext) else REJECT());
<INITIAL,ASM>"padding:" => (if MDLmode then  
                               PADDING_COLON(yypos,size yytext) else REJECT());
<INITIAL,ASM>"nullified:" => (if MDLmode then
                                NULLIFIED_COLON(yypos,size yytext) else REJECT());
<INITIAL,ASM>"candidate:" => (if MDLmode then  
                                CANDIDATE_COLON(yypos,size yytext) else REJECT());
<INITIAL,ASM>{id}	=> (lookup(MDLmode,yytext,yypos));
<INITIAL,ASM>{tyvar}	=> (TYVAR(yytext,yypos,yypos + size yytext));
<INITIAL,ASM>"("	=> (LPAREN(yypos,yypos+1));
<INITIAL,ASM>")"	=> (RPAREN(yypos,yypos+1));
<INITIAL,ASM>"["	=> (LBRACKET(yypos,yypos+1));
<INITIAL,ASM>"#["	=> (LHASHBRACKET(yypos,yypos+1));
<INITIAL,ASM>"]"	=> (RBRACKET(yypos,yypos+1));
<INITIAL,ASM>"{"	=> (LBRACE(yypos,yypos+1));
<INITIAL,ASM>"}"	=> (RBRACE(yypos,yypos+1));
<INITIAL,ASM>","	=> (COMMA(yypos,yypos+1));
<INITIAL,ASM>";"	=> (SEMICOLON(yypos,yypos+1));
<INITIAL,ASM>"."	=> (DOT(yypos,yypos+1));
<INITIAL,ASM>".."	=> (DOTDOT(yypos,yypos+2));
<INITIAL,ASM>"..."	=> (DOTDOT(yypos,yypos+3));
<INITIAL>{symbol}	=> (if yytext = !asmLQuote then
				(debug("lquote "^yytext^"\n");
				 YYBEGIN ASMQUOTE; 
                                 LDQUOTE(yypos,yypos+size yytext))
			    else
			        lookupSym(yytext,yypos));
<ASMQUOTE>{asmsymbol}	=> (if yytext = !asmRQuote then
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
			        asmtext(err,yytext,yypos));
<ASM>{asmsymbol}	=> (if yytext = !asmRMeta then
				(metaLevel := !metaLevel - 1;
				 debug("rmeta "^yytext^"("^Int.toString(!metaLevel)^")\n");
				 if !metaLevel = 0 then YYBEGIN ASMQUOTE
				 else (); RMETA(yypos,yypos+size yytext))
			    else
			        lookupSym(yytext,yypos));
<ASMQUOTE>{asmtext}	=> (debug("text="^yytext^"\n"); 
                            asmtext(err,yytext,yypos));
<COMMENT>"*)"		=> (commentLevel := !commentLevel - 1;
			    if !commentLevel = 0 then YYBEGIN INITIAL else (); 
			    continue());
<COMMENT>"(*"		=> (commentLevel := !commentLevel + 1; continue());
<COMMENT>.		=> (continue());
.			=> (err(yypos,yypos+size yytext,
                                "unknown character "^String.toString yytext);
                            continue());

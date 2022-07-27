
fun eof() = Tok.EOF

%%

id	= [_A-Za-z][_A-Za-z0-9]*; 
decnum	= (0|([1-9][0-9]*))[uUlL]?[uUlL]?;
realnum = (([0-9]+(\.[0-9]+)?)|(\.[0-9]+))([eE][+-]?[0-9]+)?[lL]?;
octdigit	= [0-7];
hexdigit	= [0-9a-fA-F];
hexnum	= 0[xX]{hexdigit}+[uUlL]?[uUlL]?; 
octnum	= 0{octdigit}+[uUlL]?[uUlL]?;

%s C S;

%%

<INITIAL>(":") => (Tok.COLON);
<INITIAL>(";") => (Tok.SEMICOLON);
<INITIAL>("(") => (Tok.LPAREN);
<INITIAL>(")") => (Tok.RPAREN);
<INITIAL>("{") => (Tok.LCURLY);
<INITIAL>("}") => (Tok.RCURLY);
<INITIAL>("[") => (Tok.LBRACE);
<INITIAL>("]") => (Tok.RBRACE);
<INITIAL>(".") => (Tok.DOT);
<INITIAL>(",") => (Tok.COMMA);
<INITIAL>("?") => (Tok.QUESTION);
<INITIAL>("%") => (Tok.PERCENT);
<INITIAL>("&") => (Tok.AMP);
<INITIAL>("|") => (Tok.BAR);
<INITIAL>("~") => (Tok.TILDE);
<INITIAL>("/") => (Tok.DIVIDE);
<INITIAL>("+") => (Tok.PLUS);
<INITIAL>("-") => (Tok.MINUS);
<INITIAL>("^") => (Tok.HAT);
<INITIAL>("!") => (Tok.BANG);
<INITIAL>("*") => (Tok.TIMES);
<INITIAL>("++") => (Tok.INC);
<INITIAL>("--") => (Tok.DEC);
<INITIAL>("->") => (Tok.ARROW);
<INITIAL>("=") => (Tok.EQUALS);
<INITIAL>("+=") => (Tok.PLUSEQUALS);
<INITIAL>("-=") => (Tok.MINUSEQUALS);
<INITIAL>("^=") => (Tok.XOREQUALS);
<INITIAL>("%=") => (Tok.MODEQUALS);
<INITIAL>("*=") => (Tok.TIMESEQUALS);
<INITIAL>("/=") => (Tok.DIVEQUALS);
<INITIAL>("|=") => (Tok.OREQUALS);
<INITIAL>("&=") => (Tok.ANDEQUALS);
<INITIAL>("<<=") => (Tok.LSHIFTEQUALS);
<INITIAL>(">>=") => (Tok.RSHIFTEQUALS);
<INITIAL>("<=") => (Tok.LTE);
<INITIAL>(">=") => (Tok.GTE);
<INITIAL>("<") => (Tok.LT);
<INITIAL>(">") => (Tok.GT);
<INITIAL>("==") => (Tok.EQ);
<INITIAL>("!=") => (Tok.NEQ);
<INITIAL>("||") => (Tok.OR);
<INITIAL>("&&") => (Tok.AND);
<INITIAL>("<<") => (Tok.LSHIFT);
<INITIAL>(">>") => (Tok.RSHIFT);
<INITIAL>("extern") => (Tok.EXTERN);
<INITIAL>("auto") => (Tok.AUTO);
<INITIAL>("static") => (Tok.STATIC);
<INITIAL>("register") => (Tok.REGISTER);
<INITIAL>("const") => (Tok.CONST);
<INITIAL>("volatile") => (Tok.VOLATILE);
<INITIAL>("if") => (Tok.IF);
<INITIAL>("else") => (Tok.ELSE);
<INITIAL>("for") => (Tok.FOR);
<INITIAL>("do") => (Tok.DO);
<INITIAL>("switch") => (Tok.SWITCH);
<INITIAL>("case") => (Tok.CASE);
<INITIAL>("default") => (Tok.DEFAULT);
<INITIAL>("while") => (Tok.WHILE);
<INITIAL>("return") => (Tok.RETURN);
<INITIAL>("break") => (Tok.BREAK);
<INITIAL>("continue") => (Tok.CONTINUE);
<INITIAL>("goto") => (Tok.GOTO);
<INITIAL>("char") => (Tok.CHAR);
<INITIAL>("double") => (Tok.DOUBLE);
<INITIAL>("enum") => (Tok.ENUM);
<INITIAL>("float") => (Tok.FLOAT);
<INITIAL>("int") => (Tok.INT);
<INITIAL>("long") => (Tok.LONG);
<INITIAL>("short") => (Tok.SHORT);
<INITIAL>("struct") => (Tok.STRUCT);
<INITIAL>("union") => (Tok.UNION);
<INITIAL>("unsigned") => (Tok.UNSIGNED);
<INITIAL>("signed") => (Tok.SIGNED);
<INITIAL>("void") => (Tok.VOID);
<INITIAL>("sizeof") => (Tok.SIZEOF);
<INITIAL>("typedef") => (Tok.TYPEDEF);
<INITIAL>("...") => (Tok.ELIPSIS);

<INITIAL>{id} => (Tok.ID yytext);
<INITIAL>{decnum} => (Tok.DECNUM (valOf (IntInf.fromString yytext)));
<INITIAL>{octnum}	=> (Tok.DECNUM 0);
<INITIAL>{hexnum}	=> (Tok.DECNUM 0);
<INITIAL>{realnum}	=> (Tok.REALNUM 0.0);
<INITIAL>"\"" => (YYBEGIN S; continue());
<INITIAL>"/*" => (YYBEGIN C; continue());

<INITIAL>"#" (.)* \n => (continue());

<INITIAL>. | \n => (continue());


<C>"*/" => (YYBEGIN INITIAL; continue());
<C>. | "\n" => (continue());

<S>\"	        => (YYBEGIN INITIAL; Tok.STRING(""));
<S>\n		=> (print "unclosed string..."; YYBEGIN INITIAL; continue());
<S>[^"\\\n]*	=> (continue());
<S>\\\n	       	=> (continue());
<S>\\0		 => (continue());
<S>\\{octdigit}{3} =>(continue());
<S>\\x{hexdigit}+ => (continue());
<S>\\\^[@-_]	=> (continue());
<S>\\.     	=> (continue());

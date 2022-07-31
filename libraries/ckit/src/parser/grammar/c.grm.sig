signature C_TOKENS =
sig
type ('a,'b) token
type svalue
val TYPE_NAME: (string) *  'a * 'a -> (svalue,'a) token
val ELIPSIS:  'a * 'a -> (svalue,'a) token
val UNARY:  'a * 'a -> (svalue,'a) token
val TYPEDEF:  'a * 'a -> (svalue,'a) token
val SIZEOF:  'a * 'a -> (svalue,'a) token
val VOID:  'a * 'a -> (svalue,'a) token
val SIGNED:  'a * 'a -> (svalue,'a) token
val UNSIGNED:  'a * 'a -> (svalue,'a) token
val UNION:  'a * 'a -> (svalue,'a) token
val STRUCT:  'a * 'a -> (svalue,'a) token
val SATURATE:  'a * 'a -> (svalue,'a) token
val FRACTIONAL:  'a * 'a -> (svalue,'a) token
val SHORT:  'a * 'a -> (svalue,'a) token
val LONG:  'a * 'a -> (svalue,'a) token
val INT:  'a * 'a -> (svalue,'a) token
val FLOAT:  'a * 'a -> (svalue,'a) token
val ENUM:  'a * 'a -> (svalue,'a) token
val DOUBLE:  'a * 'a -> (svalue,'a) token
val CHAR:  'a * 'a -> (svalue,'a) token
val GOTO:  'a * 'a -> (svalue,'a) token
val CONTINUE:  'a * 'a -> (svalue,'a) token
val BREAK:  'a * 'a -> (svalue,'a) token
val RETURN:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val DEFAULT:  'a * 'a -> (svalue,'a) token
val CASE:  'a * 'a -> (svalue,'a) token
val SWITCH:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val FOR:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val VOLATILE:  'a * 'a -> (svalue,'a) token
val CONST:  'a * 'a -> (svalue,'a) token
val REGISTER:  'a * 'a -> (svalue,'a) token
val STATIC:  'a * 'a -> (svalue,'a) token
val AUTO:  'a * 'a -> (svalue,'a) token
val EXTERN:  'a * 'a -> (svalue,'a) token
val CCONST: (LargeInt.int) *  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val REALNUM: (real) *  'a * 'a -> (svalue,'a) token
val DECNUM: (LargeInt.int) *  'a * 'a -> (svalue,'a) token
val RSHIFT:  'a * 'a -> (svalue,'a) token
val LSHIFT:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val NEQ:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val GTE:  'a * 'a -> (svalue,'a) token
val LTE:  'a * 'a -> (svalue,'a) token
val RSHIFTEQUALS:  'a * 'a -> (svalue,'a) token
val LSHIFTEQUALS:  'a * 'a -> (svalue,'a) token
val ANDEQUALS:  'a * 'a -> (svalue,'a) token
val OREQUALS:  'a * 'a -> (svalue,'a) token
val DIVEQUALS:  'a * 'a -> (svalue,'a) token
val TIMESEQUALS:  'a * 'a -> (svalue,'a) token
val MODEQUALS:  'a * 'a -> (svalue,'a) token
val XOREQUALS:  'a * 'a -> (svalue,'a) token
val MINUSEQUALS:  'a * 'a -> (svalue,'a) token
val PLUSEQUALS:  'a * 'a -> (svalue,'a) token
val EQUALS:  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val ARROW:  'a * 'a -> (svalue,'a) token
val DEC:  'a * 'a -> (svalue,'a) token
val INC:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val BANG:  'a * 'a -> (svalue,'a) token
val HAT:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val DIVIDE:  'a * 'a -> (svalue,'a) token
val TILDE:  'a * 'a -> (svalue,'a) token
val BAR:  'a * 'a -> (svalue,'a) token
val AMP:  'a * 'a -> (svalue,'a) token
val PERCENT:  'a * 'a -> (svalue,'a) token
val QUESTION:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RCURLY:  'a * 'a -> (svalue,'a) token
val LCURLY:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature C_LRVALS=
sig
structure Tokens : C_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end

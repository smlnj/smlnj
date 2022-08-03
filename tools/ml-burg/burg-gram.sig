signature Burg_TOKENS =
sig
type ('a,'b) token
type svalue
val RAW: (string list) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val PPERCENT: (string list) *  'a * 'a -> (svalue,'a) token
val K_PIPE:  'a * 'a -> (svalue,'a) token
val K_EQUAL:  'a * 'a -> (svalue,'a) token
val K_RPAREN:  'a * 'a -> (svalue,'a) token
val K_LPAREN:  'a * 'a -> (svalue,'a) token
val K_COMMA:  'a * 'a -> (svalue,'a) token
val K_SEMICOLON:  'a * 'a -> (svalue,'a) token
val K_COLON:  'a * 'a -> (svalue,'a) token
val K_SIG:  'a * 'a -> (svalue,'a) token
val K_RULEPREFIX:  'a * 'a -> (svalue,'a) token
val K_TERMPREFIX:  'a * 'a -> (svalue,'a) token
val K_START:  'a * 'a -> (svalue,'a) token
val K_TERM:  'a * 'a -> (svalue,'a) token
val K_EOF:  'a * 'a -> (svalue,'a) token
end
signature Burg_LRVALS=
sig
structure Tokens : Burg_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end

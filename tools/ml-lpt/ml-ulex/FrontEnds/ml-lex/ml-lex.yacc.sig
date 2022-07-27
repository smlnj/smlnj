signature MLLex_TOKENS =
sig
type ('a,'b) token
type svalue
val POSARG:  'a * 'a -> (svalue,'a) token
val ARG:  'a * 'a -> (svalue,'a) token
val HEADER:  'a * 'a -> (svalue,'a) token
val STRUCT:  'a * 'a -> (svalue,'a) token
val UNICODE:  'a * 'a -> (svalue,'a) token
val FULL:  'a * 'a -> (svalue,'a) token
val REJECTTOK:  'a * 'a -> (svalue,'a) token
val COUNT:  'a * 'a -> (svalue,'a) token
val LEXSTATE: (string) *  'a * 'a -> (svalue,'a) token
val STATES:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val LEXMARK:  'a * 'a -> (svalue,'a) token
val SEMI:  'a * 'a -> (svalue,'a) token
val ACT: (string) *  'a * 'a -> (svalue,'a) token
val ARROW:  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val REPS: (int) *  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val UNICHAR: (UTF8.wchar) *  'a * 'a -> (svalue,'a) token
val CHAR: (string) *  'a * 'a -> (svalue,'a) token
val HIGH_CHAR:  'a * 'a -> (svalue,'a) token
val DASH:  'a * 'a -> (svalue,'a) token
val SLASH:  'a * 'a -> (svalue,'a) token
val DOLLAR:  'a * 'a -> (svalue,'a) token
val CARAT:  'a * 'a -> (svalue,'a) token
val BAR:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val STAR:  'a * 'a -> (svalue,'a) token
val QMARK:  'a * 'a -> (svalue,'a) token
val RCB:  'a * 'a -> (svalue,'a) token
val LCB:  'a * 'a -> (svalue,'a) token
val RBD:  'a * 'a -> (svalue,'a) token
val RB:  'a * 'a -> (svalue,'a) token
val LB:  'a * 'a -> (svalue,'a) token
val RP:  'a * 'a -> (svalue,'a) token
val LP:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val DECLS: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature MLLex_LRVALS=
sig
structure Tokens : MLLex_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end

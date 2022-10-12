signature CM_TOKENS =
sig
type ('a,'b) token
type svalue
val SOURCE:  'a * 'a -> (svalue,'a) token
val DASH:  'a * 'a -> (svalue,'a) token
val STAR:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val ORELSE:  'a * 'a -> (svalue,'a) token
val ANDALSO:  'a * 'a -> (svalue,'a) token
val TILDE:  'a * 'a -> (svalue,'a) token
val INEQSYM: (CMSemant.ineqsym) *  'a * 'a -> (svalue,'a) token
val EQSYM: (CMSemant.eqsym) *  'a * 'a -> (svalue,'a) token
val MULSYM: (CMSemant.mulsym) *  'a * 'a -> (svalue,'a) token
val ADDSYM: (CMSemant.addsym) *  'a * 'a -> (svalue,'a) token
val DEFINED:  'a * 'a -> (svalue,'a) token
val FUNSIG:  'a * 'a -> (svalue,'a) token
val FUNCTOR:  'a * 'a -> (svalue,'a) token
val SIGNATURE:  'a * 'a -> (svalue,'a) token
val STRUCTURE:  'a * 'a -> (svalue,'a) token
val ERROR: (string) *  'a * 'a -> (svalue,'a) token
val ENDIF:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val ELIF:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val IS:  'a * 'a -> (svalue,'a) token
val LIBRARY:  'a * 'a -> (svalue,'a) token
val GROUP:  'a * 'a -> (svalue,'a) token
val NUMBER: (int) *  'a * 'a -> (svalue,'a) token
val ML_ID: (string) *  'a * 'a -> (svalue,'a) token
val CM_ID: (string) *  'a * 'a -> (svalue,'a) token
val FILE_NATIVE: (string) *  'a * 'a -> (svalue,'a) token
val FILE_STANDARD: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature CM_LRVALS=
sig
structure Tokens : CM_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end

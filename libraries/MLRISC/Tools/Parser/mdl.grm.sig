signature MDL_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val ASMTEXT: (string) *  'a * 'a -> (svalue,'a) token
val CHAR: (char) *  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val REAL: (string) *  'a * 'a -> (svalue,'a) token
val INTINF: (IntInf.int) *  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val WORD: (Word32.word) *  'a * 'a -> (svalue,'a) token
val TYVAR: (string) *  'a * 'a -> (svalue,'a) token
val SYMBOL: (string) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val EXCEPTION:  'a * 'a -> (svalue,'a) token
val EQUATION:  'a * 'a -> (svalue,'a) token
val LATENCY:  'a * 'a -> (svalue,'a) token
val TABLE:  'a * 'a -> (svalue,'a) token
val RESERVATION:  'a * 'a -> (svalue,'a) token
val CPU:  'a * 'a -> (svalue,'a) token
val RESOURCE:  'a * 'a -> (svalue,'a) token
val ALIASING:  'a * 'a -> (svalue,'a) token
val AGGREGABLE:  'a * 'a -> (svalue,'a) token
val CANDIDATE_COLON:  'a * 'a -> (svalue,'a) token
val PADDING_COLON:  'a * 'a -> (svalue,'a) token
val NULLIFIED_COLON:  'a * 'a -> (svalue,'a) token
val DELAYSLOT_COLON:  'a * 'a -> (svalue,'a) token
val RTL_COLON:  'a * 'a -> (svalue,'a) token
val MC_COLON:  'a * 'a -> (svalue,'a) token
val ASM_COLON:  'a * 'a -> (svalue,'a) token
val DEBUG:  'a * 'a -> (svalue,'a) token
val INFIXR:  'a * 'a -> (svalue,'a) token
val INFIX:  'a * 'a -> (svalue,'a) token
val NONFIX:  'a * 'a -> (svalue,'a) token
val CANDIDATE:  'a * 'a -> (svalue,'a) token
val BACKWARDS:  'a * 'a -> (svalue,'a) token
val NEVER:  'a * 'a -> (svalue,'a) token
val ALWAYS:  'a * 'a -> (svalue,'a) token
val FORWARDS:  'a * 'a -> (svalue,'a) token
val DELAYSLOT:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val DEPENDENT:  'a * 'a -> (svalue,'a) token
val SPAN:  'a * 'a -> (svalue,'a) token
val RTL:  'a * 'a -> (svalue,'a) token
val ASSEMBLY:  'a * 'a -> (svalue,'a) token
val TAKEN:  'a * 'a -> (svalue,'a) token
val BRANCHING:  'a * 'a -> (svalue,'a) token
val WHEN:  'a * 'a -> (svalue,'a) token
val VERBATIM:  'a * 'a -> (svalue,'a) token
val UPPERCASE:  'a * 'a -> (svalue,'a) token
val LOWERCASE:  'a * 'a -> (svalue,'a) token
val PREDICATED:  'a * 'a -> (svalue,'a) token
val PIPELINE:  'a * 'a -> (svalue,'a) token
val ENDIAN:  'a * 'a -> (svalue,'a) token
val BIG:  'a * 'a -> (svalue,'a) token
val LITTLE:  'a * 'a -> (svalue,'a) token
val OP:  'a * 'a -> (svalue,'a) token
val OPEN:  'a * 'a -> (svalue,'a) token
val INCLUDE:  'a * 'a -> (svalue,'a) token
val VAL:  'a * 'a -> (svalue,'a) token
val FUN:  'a * 'a -> (svalue,'a) token
val WITHTYPE:  'a * 'a -> (svalue,'a) token
val ENCODING:  'a * 'a -> (svalue,'a) token
val AS:  'a * 'a -> (svalue,'a) token
val FORMATS:  'a * 'a -> (svalue,'a) token
val UNSIGNED:  'a * 'a -> (svalue,'a) token
val SIGNED:  'a * 'a -> (svalue,'a) token
val SUPERSCALAR:  'a * 'a -> (svalue,'a) token
val VLIW:  'a * 'a -> (svalue,'a) token
val FIELDS:  'a * 'a -> (svalue,'a) token
val FIELD:  'a * 'a -> (svalue,'a) token
val ORDERING:  'a * 'a -> (svalue,'a) token
val CELLS:  'a * 'a -> (svalue,'a) token
val CELL:  'a * 'a -> (svalue,'a) token
val REGISTER:  'a * 'a -> (svalue,'a) token
val INSTRUCTION:  'a * 'a -> (svalue,'a) token
val SHARING:  'a * 'a -> (svalue,'a) token
val WHERE:  'a * 'a -> (svalue,'a) token
val STRUCT:  'a * 'a -> (svalue,'a) token
val SIG:  'a * 'a -> (svalue,'a) token
val SIGNATURE:  'a * 'a -> (svalue,'a) token
val FUNCTOR:  'a * 'a -> (svalue,'a) token
val STRUCTURE:  'a * 'a -> (svalue,'a) token
val LET:  'a * 'a -> (svalue,'a) token
val HANDLE:  'a * 'a -> (svalue,'a) token
val RAISE:  'a * 'a -> (svalue,'a) token
val WILD:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val BITS:  'a * 'a -> (svalue,'a) token
val PAR:  'a * 'a -> (svalue,'a) token
val DARROW:  'a * 'a -> (svalue,'a) token
val ARROW:  'a * 'a -> (svalue,'a) token
val BAR:  'a * 'a -> (svalue,'a) token
val AT:  'a * 'a -> (svalue,'a) token
val DOTDOT:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val COLONGREATER:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val HASH:  'a * 'a -> (svalue,'a) token
val LOCATIONS:  'a * 'a -> (svalue,'a) token
val STORAGE:  'a * 'a -> (svalue,'a) token
val FN:  'a * 'a -> (svalue,'a) token
val CELLSET:  'a * 'a -> (svalue,'a) token
val RMETA:  'a * 'a -> (svalue,'a) token
val LMETA:  'a * 'a -> (svalue,'a) token
val RDQUOTE:  'a * 'a -> (svalue,'a) token
val LDQUOTE:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RBRACKET:  'a * 'a -> (svalue,'a) token
val LBRACKET:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val LHASHBRACKET:  'a * 'a -> (svalue,'a) token
val RRBRACKET:  'a * 'a -> (svalue,'a) token
val LLBRACKET:  'a * 'a -> (svalue,'a) token
val CONCAT:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val DEREF:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val DOLLAR:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val TYPE:  'a * 'a -> (svalue,'a) token
val DATATYPE:  'a * 'a -> (svalue,'a) token
val CASE:  'a * 'a -> (svalue,'a) token
val OF:  'a * 'a -> (svalue,'a) token
val IN:  'a * 'a -> (svalue,'a) token
val LOCAL:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val ARCHITECTURE:  'a * 'a -> (svalue,'a) token
end
signature MDL_LRVALS=
sig
structure Tokens : MDL_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end

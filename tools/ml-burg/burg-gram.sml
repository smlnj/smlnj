functor BurgLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Burg_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* burg-gram
**
** ML-Yacc grammar for BURG.
*)

structure A = BurgAST;
fun outputRaw s = print (s:string)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\010\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\014\000\005\000\000\000\
\\001\000\007\000\024\000\000\000\
\\001\000\008\000\038\000\000\000\
\\001\000\011\000\040\000\000\000\
\\001\000\011\000\045\000\000\000\
\\001\000\012\000\029\000\000\000\
\\001\000\014\000\021\000\016\000\020\000\000\000\
\\001\000\015\000\039\000\000\000\
\\001\000\015\000\046\000\000\000\
\\001\000\016\000\012\000\000\000\
\\001\000\016\000\013\000\000\000\
\\001\000\016\000\014\000\000\000\
\\001\000\016\000\015\000\000\000\
\\001\000\016\000\018\000\000\000\
\\001\000\016\000\026\000\000\000\
\\001\000\016\000\028\000\000\000\
\\001\000\016\000\032\000\000\000\
\\049\000\000\000\
\\050\000\000\000\
\\051\000\000\000\
\\052\000\013\000\022\000\000\000\
\\053\000\000\000\
\\054\000\000\000\
\\055\000\000\000\
\\056\000\000\000\
\\057\000\000\000\
\\058\000\000\000\
\\059\000\012\000\023\000\000\000\
\\060\000\000\000\
\\061\000\000\000\
\\062\000\000\000\
\\063\000\000\000\
\\064\000\000\000\
\\065\000\010\000\030\000\000\000\
\\066\000\000\000\
\\067\000\009\000\037\000\000\000\
\\068\000\000\000\
\\069\000\010\000\035\000\000\000\
\\070\000\000\000\
\\071\000\009\000\043\000\000\000\
\\072\000\000\000\
\"
val actionRowNumbers =
"\019\000\001\000\020\000\030\000\
\\010\000\011\000\012\000\013\000\
\\014\000\007\000\025\000\024\000\
\\023\000\022\000\021\000\026\000\
\\028\000\031\000\002\000\018\000\
\\014\000\015\000\016\000\027\000\
\\029\000\006\000\034\000\017\000\
\\016\000\038\000\033\000\036\000\
\\003\000\008\000\004\000\016\000\
\\032\000\040\000\035\000\036\000\
\\005\000\009\000\037\000\039\000\
\\040\000\041\000\000\000"
val gotoT =
"\
\\001\000\046\000\010\000\001\000\000\000\
\\003\000\002\000\000\000\
\\000\000\
\\011\000\009\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\015\000\013\000\014\000\000\000\
\\012\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\023\000\000\000\
\\000\000\
\\008\000\025\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\029\000\000\000\
\\008\000\031\000\000\000\
\\005\000\032\000\000\000\
\\000\000\
\\009\000\034\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\039\000\000\000\
\\000\000\
\\006\000\040\000\000\000\
\\000\000\
\\009\000\042\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\045\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 47
val numrules = 24
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | RAW of  (string list)
 | ID of  (string) | INT of  (int) | PPERCENT of  (string list)
 | postlude of  (unit) | prelude of  (unit) | raw of  (unit)
 | bindinglist of  ( ( string * string option )  list)
 | rule of  (A.rule_ast) | rules of  (A.rule_ast list)
 | decls of  (A.decl_ast list) | patterntail of  (A.pattern_ast list)
 | pattern of  (A.pattern_ast) | rulename of  (string)
 | costtail of  (int list) | cost of  (int list)
 | binding of  ( ( string * string option ) ) | decl of  (A.decl_ast)
 | spec of  (A.spec_ast) | full of  (A.spec_ast)
end
type svalue = MlyValue.svalue
type result = A.spec_ast
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn _ => false
val showTerminal =
fn (T 0) => "K_EOF"
  | (T 1) => "K_TERM"
  | (T 2) => "K_START"
  | (T 3) => "K_TERMPREFIX"
  | (T 4) => "K_RULEPREFIX"
  | (T 5) => "K_SIG"
  | (T 6) => "K_COLON"
  | (T 7) => "K_SEMICOLON"
  | (T 8) => "K_COMMA"
  | (T 9) => "K_LPAREN"
  | (T 10) => "K_RPAREN"
  | (T 11) => "K_EQUAL"
  | (T 12) => "K_PIPE"
  | (T 13) => "PPERCENT"
  | (T 14) => "INT"
  | (T 15) => "ID"
  | (T 16) => "RAW"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PPERCENT PPERCENT2, _, PPERCENT2right)) :: 
( _, ( MlyValue.rules rules, _, _)) :: ( _, ( MlyValue.PPERCENT 
PPERCENT1, _, _)) :: ( _, ( MlyValue.decls decls, decls1left, _)) :: 
rest671)) => let val  result = MlyValue.full (
A.SPEC{head=PPERCENT1,
						decls=rev decls,
						rules=rev rules,
						tail=PPERCENT2}
)
 in ( LrTable.NT 0, ( result, decls1left, PPERCENT2right), rest671)

end
|  ( 1, ( rest671)) => let val  result = MlyValue.decls ([])
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.decl decl, _, decl1right)) :: ( _, ( 
MlyValue.decls decls, decls1left, _)) :: rest671)) => let val  result
 = MlyValue.decls (decl :: decls)
 in ( LrTable.NT 9, ( result, decls1left, decl1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.bindinglist bindinglist, _, 
bindinglist1right)) :: ( _, ( _, K_TERM1left, _)) :: rest671)) => let
 val  result = MlyValue.decl (A.TERM (rev bindinglist))
 in ( LrTable.NT 2, ( result, K_TERM1left, bindinglist1right), rest671
)
end
|  ( 4, ( ( _, ( MlyValue.ID ID, _, ID1right)) :: ( _, ( _, 
K_START1left, _)) :: rest671)) => let val  result = MlyValue.decl (
A.START ID)
 in ( LrTable.NT 2, ( result, K_START1left, ID1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ID ID, _, ID1right)) :: ( _, ( _, 
K_TERMPREFIX1left, _)) :: rest671)) => let val  result = MlyValue.decl
 (A.TERMPREFIX ID)
 in ( LrTable.NT 2, ( result, K_TERMPREFIX1left, ID1right), rest671)

end
|  ( 6, ( ( _, ( MlyValue.ID ID, _, ID1right)) :: ( _, ( _, 
K_RULEPREFIX1left, _)) :: rest671)) => let val  result = MlyValue.decl
 (A.RULEPREFIX ID)
 in ( LrTable.NT 2, ( result, K_RULEPREFIX1left, ID1right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.ID ID, _, ID1right)) :: ( _, ( _, K_SIG1left
, _)) :: rest671)) => let val  result = MlyValue.decl (A.SIG ID)
 in ( LrTable.NT 2, ( result, K_SIG1left, ID1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.binding binding, binding1left, binding1right
)) :: rest671)) => let val  result = MlyValue.bindinglist ([binding])
 in ( LrTable.NT 12, ( result, binding1left, binding1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.binding binding, _, binding1right)) :: _ :: 
( _, ( MlyValue.bindinglist bindinglist, bindinglist1left, _)) :: 
rest671)) => let val  result = MlyValue.bindinglist (
binding :: bindinglist)
 in ( LrTable.NT 12, ( result, bindinglist1left, binding1right), 
rest671)
end
|  ( 10, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.binding ((ID, NONE))
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.binding ((ID1, SOME ID2))
 in ( LrTable.NT 3, ( result, ID1left, ID2right), rest671)
end
|  ( 12, ( rest671)) => let val  result = MlyValue.rules ([])
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 13, ( ( _, ( MlyValue.rule rule, _, rule1right)) :: ( _, ( 
MlyValue.rules rules, rules1left, _)) :: rest671)) => let val  result
 = MlyValue.rules (rule :: rules)
 in ( LrTable.NT 10, ( result, rules1left, rule1right), rest671)
end
|  ( 14, ( ( _, ( _, _, K_SEMICOLON1right)) :: ( _, ( MlyValue.cost 
cost, _, _)) :: ( _, ( MlyValue.rulename rulename, _, _)) :: _ :: ( _,
 ( MlyValue.pattern pattern, _, _)) :: _ :: ( _, ( MlyValue.ID ID, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.rule (
A.RULE(ID, pattern, rulename, cost))
 in ( LrTable.NT 11, ( result, ID1left, K_SEMICOLON1right), rest671)

end
|  ( 15, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.rulename (ID)
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.pattern (A.PAT(ID, []))
 in ( LrTable.NT 7, ( result, ID1left, ID1right), rest671)
end
|  ( 17, ( ( _, ( _, _, K_RPAREN1right)) :: ( _, ( 
MlyValue.patterntail patterntail, _, _)) :: ( _, ( MlyValue.pattern 
pattern, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671
)) => let val  result = MlyValue.pattern (
A.PAT(ID, pattern :: patterntail))
 in ( LrTable.NT 7, ( result, ID1left, K_RPAREN1right), rest671)
end
|  ( 18, ( rest671)) => let val  result = MlyValue.patterntail ([])
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 19, ( ( _, ( MlyValue.patterntail patterntail, _, 
patterntail1right)) :: ( _, ( MlyValue.pattern pattern, _, _)) :: ( _,
 ( _, K_COMMA1left, _)) :: rest671)) => let val  result = 
MlyValue.patterntail (pattern :: patterntail)
 in ( LrTable.NT 8, ( result, K_COMMA1left, patterntail1right), 
rest671)
end
|  ( 20, ( rest671)) => let val  result = MlyValue.cost ([])
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 21, ( ( _, ( _, _, K_RPAREN1right)) :: ( _, ( MlyValue.costtail 
costtail, _, _)) :: ( _, ( MlyValue.INT INT, _, _)) :: ( _, ( _, 
K_LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.cost (
INT :: costtail)
 in ( LrTable.NT 4, ( result, K_LPAREN1left, K_RPAREN1right), rest671)

end
|  ( 22, ( rest671)) => let val  result = MlyValue.costtail ([])
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 23, ( ( _, ( MlyValue.costtail costtail, _, costtail1right)) :: (
 _, ( MlyValue.INT INT, _, _)) :: ( _, ( _, K_COMMA1left, _)) :: 
rest671)) => let val  result = MlyValue.costtail (INT :: costtail)
 in ( LrTable.NT 5, ( result, K_COMMA1left, costtail1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.full x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Burg_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun K_EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun K_TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun K_START (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun K_TERMPREFIX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun K_RULEPREFIX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun K_SIG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun K_COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun K_SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun K_COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun K_LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun K_RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun K_EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun K_PIPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun PPERCENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.PPERCENT i,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.INT i,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.ID i,p1,p2))
fun RAW (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.RAW i,p1,p2))
end
end

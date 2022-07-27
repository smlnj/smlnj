functor MLLexLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : MLLex_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure S = LexSpec

structure RE = RegExp
structure SIS = RE.SymSet

val symTable : RE.re AtomMap.map ref = ref AtomMap.empty

val wildcard = SIS.complement (SIS.singleton 0w10) (* everything but \n *)
fun charToSym c = Word.fromInt (Char.ord c)
fun strToSym s = charToSym (String.sub (s, 0))

fun mkRule (ss, (false, re), act) = ((ss, re), act)
  | mkRule (ss, (true,  re), act) = ((ss, re),
      "if not yylastwasn then REJECT() else (" ^ act ^")")


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\004\000\078\000\031\000\077\000\000\000\
\\001\000\005\000\036\000\007\000\035\000\016\000\034\000\020\000\033\000\
\\021\000\032\000\022\000\031\000\023\000\030\000\026\000\029\000\000\000\
\\001\000\005\000\036\000\007\000\035\000\020\000\033\000\021\000\032\000\
\\022\000\031\000\023\000\030\000\026\000\029\000\000\000\
\\001\000\006\000\076\000\015\000\046\000\000\000\
\\001\000\008\000\073\000\009\000\072\000\016\000\071\000\021\000\055\000\
\\022\000\054\000\000\000\
\\001\000\008\000\083\000\016\000\071\000\021\000\055\000\022\000\054\000\000\000\
\\001\000\009\000\058\000\016\000\057\000\019\000\056\000\021\000\055\000\
\\022\000\054\000\000\000\
\\001\000\009\000\058\000\019\000\056\000\021\000\055\000\022\000\054\000\000\000\
\\001\000\011\000\049\000\000\000\
\\001\000\011\000\064\000\031\000\063\000\000\000\
\\001\000\011\000\086\000\000\000\
\\001\000\015\000\046\000\029\000\079\000\000\000\
\\001\000\016\000\071\000\021\000\055\000\022\000\054\000\000\000\
\\001\000\021\000\055\000\022\000\054\000\000\000\
\\001\000\024\000\038\000\000\000\
\\001\000\025\000\080\000\000\000\
\\001\000\026\000\016\000\030\000\015\000\032\000\014\000\034\000\013\000\
\\035\000\012\000\036\000\011\000\037\000\010\000\038\000\009\000\
\\039\000\008\000\040\000\007\000\000\000\
\\001\000\026\000\019\000\000\000\
\\001\000\027\000\047\000\000\000\
\\001\000\027\000\088\000\000\000\
\\001\000\028\000\017\000\000\000\
\\001\000\028\000\018\000\000\000\
\\001\000\028\000\066\000\000\000\
\\001\000\028\000\089\000\000\000\
\\001\000\029\000\039\000\000\000\
\\001\000\030\000\005\000\000\000\
\\001\000\033\000\021\000\000\000\
\\001\000\033\000\061\000\000\000\
\\001\000\033\000\084\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\002\000\004\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\033\000\021\000\000\000\
\\105\000\000\000\
\\106\000\003\000\037\000\005\000\036\000\007\000\035\000\016\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\026\000\029\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\015\000\046\000\000\000\
\\113\000\015\000\046\000\000\000\
\\114\000\005\000\036\000\007\000\035\000\020\000\033\000\021\000\032\000\
\\022\000\031\000\023\000\030\000\026\000\029\000\000\000\
\\115\000\005\000\036\000\007\000\035\000\020\000\033\000\021\000\032\000\
\\022\000\031\000\023\000\030\000\026\000\029\000\000\000\
\\116\000\012\000\044\000\013\000\043\000\014\000\042\000\025\000\041\000\000\000\
\\117\000\012\000\044\000\013\000\043\000\014\000\042\000\025\000\041\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\019\000\067\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\019\000\082\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\"
val actionRowNumbers =
"\032\000\026\000\031\000\033\000\
\\017\000\021\000\022\000\018\000\
\\038\000\039\000\041\000\040\000\
\\027\000\045\000\015\000\037\000\
\\035\000\036\000\025\000\043\000\
\\062\000\056\000\054\000\052\000\
\\019\000\045\000\030\000\009\000\
\\066\000\065\000\064\000\063\000\
\\003\000\007\000\003\000\028\000\
\\003\000\034\000\044\000\010\000\
\\059\000\058\000\057\000\055\000\
\\003\000\023\000\046\000\067\000\
\\051\000\075\000\005\000\070\000\
\\083\000\082\000\014\000\008\000\
\\073\000\004\000\001\000\049\000\
\\012\000\016\000\060\000\053\000\
\\047\000\013\000\081\000\079\000\
\\077\000\080\000\074\000\071\000\
\\006\000\069\000\068\000\029\000\
\\002\000\042\000\011\000\076\000\
\\013\000\072\000\050\000\020\000\
\\061\000\078\000\024\000\048\000\
\\000\000"
val gotoT =
"\
\\001\000\088\000\002\000\001\000\000\000\
\\000\000\
\\000\000\
\\003\000\004\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\018\000\000\000\
\\005\000\026\000\006\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\038\000\000\000\
\\000\000\
\\000\000\
\\011\000\043\000\012\000\020\000\000\000\
\\000\000\
\\000\000\
\\005\000\046\000\006\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\012\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\048\000\010\000\022\000\011\000\021\000\012\000\020\000\000\000\
\\013\000\051\000\014\000\050\000\017\000\049\000\000\000\
\\009\000\057\000\010\000\022\000\011\000\021\000\012\000\020\000\000\000\
\\007\000\058\000\000\000\
\\009\000\060\000\010\000\022\000\011\000\021\000\012\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\063\000\011\000\021\000\012\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\068\000\016\000\067\000\017\000\066\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\072\000\017\000\049\000\000\000\
\\013\000\073\000\014\000\050\000\017\000\049\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\043\000\012\000\020\000\000\000\
\\000\000\
\\016\000\079\000\017\000\066\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\068\000\016\000\067\000\017\000\066\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\083\000\009\000\023\000\010\000\022\000\011\000\021\000\
\\012\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\085\000\017\000\066\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 89
val numrules = 54
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
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
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
datatype svalue = VOID | ntVOID of unit ->  unit
 | LEXSTATE of unit ->  (string) | ACT of unit ->  (string)
 | ID of unit ->  (string) | REPS of unit ->  (int)
 | UNICHAR of unit ->  (UTF8.wchar) | CHAR of unit ->  (string)
 | DECLS of unit ->  (string) | NonCarat of unit ->  (UTF8.wchar)
 | AChar of unit ->  (UTF8.wchar) | CharRng of unit ->  (SIS.set)
 | CharClass' of unit ->  (SIS.set) | CharClass of unit ->  (SIS.set)
 | InExp of unit ->  (RE.re) | Exp of unit ->  (RE.re)
 | CatExp of unit ->  (RE.re) | OrExp of unit ->  (RE.re)
 | LineBreakExp of unit ->  ( ( bool * RE.re ) )
 | RuleStates of unit ->  (AtomSet.set) | Rule of unit ->  (S.rule)
 | Rules of unit ->  (S.rule list)
 | StartStates of unit ->  (AtomSet.set) | Defs of unit ->  (S.config)
 | Decls of unit ->  (string) | Start of unit ->  (S.spec)
end
type svalue = MlyValue.svalue
type result = S.spec
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
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "DECLS"
  | (T 2) => "LT"
  | (T 3) => "GT"
  | (T 4) => "LP"
  | (T 5) => "RP"
  | (T 6) => "LB"
  | (T 7) => "RB"
  | (T 8) => "RBD"
  | (T 9) => "LCB"
  | (T 10) => "RCB"
  | (T 11) => "QMARK"
  | (T 12) => "STAR"
  | (T 13) => "PLUS"
  | (T 14) => "BAR"
  | (T 15) => "CARAT"
  | (T 16) => "DOLLAR"
  | (T 17) => "SLASH"
  | (T 18) => "DASH"
  | (T 19) => "HIGH_CHAR"
  | (T 20) => "CHAR"
  | (T 21) => "UNICHAR"
  | (T 22) => "DOT"
  | (T 23) => "EQ"
  | (T 24) => "REPS"
  | (T 25) => "ID"
  | (T 26) => "ARROW"
  | (T 27) => "ACT"
  | (T 28) => "SEMI"
  | (T 29) => "LEXMARK"
  | (T 30) => "COMMA"
  | (T 31) => "STATES"
  | (T 32) => "LEXSTATE"
  | (T 33) => "COUNT"
  | (T 34) => "REJECTTOK"
  | (T 35) => "FULL"
  | (T 36) => "UNICODE"
  | (T 37) => "STRUCT"
  | (T 38) => "HEADER"
  | (T 39) => "ARG"
  | (T 40) => "POSARG"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34)
 $$ (T 33) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 26) $$ (T 23)
 $$ (T 22) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14)
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Rules Rules1, _, Rules1right)) :: _ :: ( _,
 ( MlyValue.Defs Defs1, _, _)) :: _ :: ( _, ( MlyValue.Decls Decls1, 
Decls1left, _)) :: rest671)) => let val  result = MlyValue.Start (fn _
 => let val  (Decls as Decls1) = Decls1 ()
 val  (Defs as Defs1) = Defs1 ()
 val  (Rules as Rules1) = Rules1 ()
 in (
S.Spec {decls = Decls,
		         conf = Defs,
			 rules = Rules,
			 eofRules = []}
)
end)
 in ( LrTable.NT 0, ( result, Decls1left, Rules1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.DECLS DECLS1, DECLS1left, DECLS1right)) :: 
rest671)) => let val  result = MlyValue.Decls (fn _ => let val  (DECLS
 as DECLS1) = DECLS1 ()
 in (DECLS)
end)
 in ( LrTable.NT 1, ( result, DECLS1left, DECLS1right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.Decls (fn _ => (""))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.Defs (fn _ => (
S.mkConfig()))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.StartStates 
StartStates1, _, _)) :: _ :: ( _, ( MlyValue.Defs Defs1, Defs1left, _)
) :: rest671)) => let val  result = MlyValue.Defs (fn _ => let val  (
Defs as Defs1) = Defs1 ()
 val  (StartStates as StartStates1) = StartStates1 ()
 in (S.updStartStates (Defs, StartStates))
end)
 in ( LrTable.NT 2, ( result, Defs1left, SEMI1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ACT ACT1, _, ACT1right)) :: _ :: ( _, ( 
MlyValue.Defs Defs1, Defs1left, _)) :: rest671)) => let val  result = 
MlyValue.Defs (fn _ => let val  (Defs as Defs1) = Defs1 ()
 val  (ACT as ACT1) = ACT1 ()
 in (
S.updHeader (Defs,
		   String.substring (ACT, 1, String.size ACT - 2))
)
end)
 in ( LrTable.NT 2, ( result, Defs1left, ACT1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.Defs Defs1, Defs1left, _)) :: rest671)) => let val  result = 
MlyValue.Defs (fn _ => let val  (Defs as Defs1) = Defs1 ()
 val  (ID as ID1) = ID1 ()
 in (S.updStructName (Defs, ID))
end)
 in ( LrTable.NT 2, ( result, Defs1left, ID1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ACT ACT1, _, ACT1right)) :: _ :: ( _, ( 
MlyValue.Defs Defs1, Defs1left, _)) :: rest671)) => let val  result = 
MlyValue.Defs (fn _ => let val  (Defs as Defs1) = Defs1 ()
 val  (ACT as ACT1) = ACT1 ()
 in (S.updArg (Defs, ACT))
end)
 in ( LrTable.NT 2, ( result, Defs1left, ACT1right), rest671)
end
|  ( 8, ( ( _, ( _, _, UNICODE1right)) :: ( _, ( MlyValue.Defs Defs1, 
Defs1left, _)) :: rest671)) => let val  result = MlyValue.Defs (fn _
 => let val  (Defs as Defs1) = Defs1 ()
 in (S.updClamp (Defs, S.NO_CLAMP))
end)
 in ( LrTable.NT 2, ( result, Defs1left, UNICODE1right), rest671)
end
|  ( 9, ( ( _, ( _, _, FULL1right)) :: ( _, ( MlyValue.Defs Defs1, 
Defs1left, _)) :: rest671)) => let val  result = MlyValue.Defs (fn _
 => let val  (Defs as Defs1) = Defs1 ()
 in (S.updClamp (Defs, S.CLAMP255))
end)
 in ( LrTable.NT 2, ( result, Defs1left, FULL1right), rest671)
end
|  ( 10, ( ( _, ( _, _, COUNT1right)) :: ( _, ( MlyValue.Defs Defs1, 
Defs1left, _)) :: rest671)) => let val  result = MlyValue.Defs (fn _
 => let val  (Defs as Defs1) = Defs1 ()
 in (Defs)
end)
 in ( LrTable.NT 2, ( result, Defs1left, COUNT1right), rest671)
end
|  ( 11, ( ( _, ( _, _, REJECTTOK1right)) :: ( _, ( MlyValue.Defs 
Defs1, Defs1left, _)) :: rest671)) => let val  result = MlyValue.Defs
 (fn _ => let val  (Defs as Defs1) = Defs1 ()
 in (Defs)
end)
 in ( LrTable.NT 2, ( result, Defs1left, REJECTTOK1right), rest671)

end
|  ( 12, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.OrExp OrExp1,
 _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( MlyValue.Defs
 Defs1, Defs1left, _)) :: rest671)) => let val  result = MlyValue.Defs
 (fn _ => let val  (Defs as Defs1) = Defs1 ()
 val  (ID as ID1) = ID1 ()
 val  (OrExp as OrExp1) = OrExp1 ()
 in (
symTable := AtomMap.insert
		  	       (!symTable, Atom.atom ID, OrExp);
		 Defs
)
end)
 in ( LrTable.NT 2, ( result, Defs1left, SEMI1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.LEXSTATE LEXSTATE1, LEXSTATE1left, 
LEXSTATE1right)) :: rest671)) => let val  result = 
MlyValue.StartStates (fn _ => let val  (LEXSTATE as LEXSTATE1) = 
LEXSTATE1 ()
 in (AtomSet.singleton (Atom.atom LEXSTATE))
end)
 in ( LrTable.NT 3, ( result, LEXSTATE1left, LEXSTATE1right), rest671)

end
|  ( 14, ( ( _, ( MlyValue.StartStates StartStates1, _, 
StartStates1right)) :: ( _, ( MlyValue.LEXSTATE LEXSTATE1, 
LEXSTATE1left, _)) :: rest671)) => let val  result = 
MlyValue.StartStates (fn _ => let val  (LEXSTATE as LEXSTATE1) = 
LEXSTATE1 ()
 val  (StartStates as StartStates1) = StartStates1 ()
 in (AtomSet.add (StartStates, Atom.atom LEXSTATE))
end)
 in ( LrTable.NT 3, ( result, LEXSTATE1left, StartStates1right), 
rest671)
end
|  ( 15, ( rest671)) => let val  result = MlyValue.Rules (fn _ => ([])
)
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 16, ( ( _, ( MlyValue.Rules Rules1, _, Rules1right)) :: ( _, ( 
MlyValue.Rule Rule1, Rule1left, _)) :: rest671)) => let val  result = 
MlyValue.Rules (fn _ => let val  (Rule as Rule1) = Rule1 ()
 val  (Rules as Rules1) = Rules1 ()
 in (Rule :: Rules)
end)
 in ( LrTable.NT 4, ( result, Rule1left, Rules1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ACT ACT1, _, ACT1right)) :: _ :: ( _, ( 
MlyValue.LineBreakExp LineBreakExp1, LineBreakExp1left, _)) :: rest671
)) => let val  result = MlyValue.Rule (fn _ => let val  (LineBreakExp
 as LineBreakExp1) = LineBreakExp1 ()
 val  (ACT as ACT1) = ACT1 ()
 in (mkRule (NONE, LineBreakExp, ACT))
end)
 in ( LrTable.NT 5, ( result, LineBreakExp1left, ACT1right), rest671)

end
|  ( 18, ( ( _, ( MlyValue.ACT ACT1, _, ACT1right)) :: _ :: ( _, ( 
MlyValue.LineBreakExp LineBreakExp1, _, _)) :: _ :: ( _, ( 
MlyValue.RuleStates RuleStates1, _, _)) :: ( _, ( _, LT1left, _)) :: 
rest671)) => let val  result = MlyValue.Rule (fn _ => let val  (
RuleStates as RuleStates1) = RuleStates1 ()
 val  (LineBreakExp as LineBreakExp1) = LineBreakExp1 ()
 val  (ACT as ACT1) = ACT1 ()
 in (mkRule (SOME RuleStates, LineBreakExp, ACT))
end)
 in ( LrTable.NT 5, ( result, LT1left, ACT1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.LEXSTATE LEXSTATE1, LEXSTATE1left, 
LEXSTATE1right)) :: rest671)) => let val  result = MlyValue.RuleStates
 (fn _ => let val  (LEXSTATE as LEXSTATE1) = LEXSTATE1 ()
 in (AtomSet.singleton (Atom.atom LEXSTATE))
end)
 in ( LrTable.NT 6, ( result, LEXSTATE1left, LEXSTATE1right), rest671)

end
|  ( 20, ( ( _, ( MlyValue.LEXSTATE LEXSTATE1, _, LEXSTATE1right)) ::
 _ :: ( _, ( MlyValue.RuleStates RuleStates1, RuleStates1left, _)) :: 
rest671)) => let val  result = MlyValue.RuleStates (fn _ => let val  (
RuleStates as RuleStates1) = RuleStates1 ()
 val  (LEXSTATE as LEXSTATE1) = LEXSTATE1 ()
 in (AtomSet.add (RuleStates, Atom.atom LEXSTATE))
end)
 in ( LrTable.NT 6, ( result, RuleStates1left, LEXSTATE1right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.OrExp OrExp1, _, OrExp1right)) :: ( _, ( _,
 CARAT1left, _)) :: rest671)) => let val  result = 
MlyValue.LineBreakExp (fn _ => let val  (OrExp as OrExp1) = OrExp1 ()
 in (true, OrExp)
end)
 in ( LrTable.NT 7, ( result, CARAT1left, OrExp1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.OrExp OrExp1, OrExp1left, OrExp1right)) :: 
rest671)) => let val  result = MlyValue.LineBreakExp (fn _ => let val 
 (OrExp as OrExp1) = OrExp1 ()
 in (false, OrExp)
end)
 in ( LrTable.NT 7, ( result, OrExp1left, OrExp1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.CatExp CatExp1, _, CatExp1right)) :: _ :: (
 _, ( MlyValue.OrExp OrExp1, OrExp1left, _)) :: rest671)) => let val  
result = MlyValue.OrExp (fn _ => let val  (OrExp as OrExp1) = OrExp1
 ()
 val  (CatExp as CatExp1) = CatExp1 ()
 in (RE.mkOr (OrExp, CatExp))
end)
 in ( LrTable.NT 8, ( result, OrExp1left, CatExp1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.CatExp CatExp1, CatExp1left, CatExp1right))
 :: rest671)) => let val  result = MlyValue.OrExp (fn _ => let val  (
CatExp as CatExp1) = CatExp1 ()
 in (CatExp)
end)
 in ( LrTable.NT 8, ( result, CatExp1left, CatExp1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.Exp Exp1, _, Exp1right)) :: ( _, ( 
MlyValue.CatExp CatExp1, CatExp1left, _)) :: rest671)) => let val  
result = MlyValue.CatExp (fn _ => let val  (CatExp as CatExp1) = 
CatExp1 ()
 val  (Exp as Exp1) = Exp1 ()
 in (RE.mkConcat (CatExp, Exp))
end)
 in ( LrTable.NT 9, ( result, CatExp1left, Exp1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.Exp Exp1, Exp1left, Exp1right)) :: rest671)
) => let val  result = MlyValue.CatExp (fn _ => let val  (Exp as Exp1)
 = Exp1 ()
 in (Exp)
end)
 in ( LrTable.NT 9, ( result, Exp1left, Exp1right), rest671)
end
|  ( 27, ( ( _, ( _, _, QMARK1right)) :: ( _, ( MlyValue.Exp Exp1, 
Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp (fn _ =>
 let val  (Exp as Exp1) = Exp1 ()
 in (RE.mkOpt Exp)
end)
 in ( LrTable.NT 10, ( result, Exp1left, QMARK1right), rest671)
end
|  ( 28, ( ( _, ( _, _, STAR1right)) :: ( _, ( MlyValue.Exp Exp1, 
Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp (fn _ =>
 let val  (Exp as Exp1) = Exp1 ()
 in (RE.mkClosure Exp)
end)
 in ( LrTable.NT 10, ( result, Exp1left, STAR1right), rest671)
end
|  ( 29, ( ( _, ( _, _, PLUS1right)) :: ( _, ( MlyValue.Exp Exp1, 
Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp (fn _ =>
 let val  (Exp as Exp1) = Exp1 ()
 in (RE.mkAtLeast (Exp, 1))
end)
 in ( LrTable.NT 10, ( result, Exp1left, PLUS1right), rest671)
end
|  ( 30, ( ( _, ( _, _, RCB1right)) :: ( _, ( MlyValue.REPS REPS1, _,
 _)) :: ( _, ( MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let
 val  result = MlyValue.Exp (fn _ => let val  (Exp as Exp1) = Exp1 ()
 val  (REPS as REPS1) = REPS1 ()
 in (RE.mkRep (Exp, REPS, REPS))
end)
 in ( LrTable.NT 10, ( result, Exp1left, RCB1right), rest671)
end
|  ( 31, ( ( _, ( _, _, RCB1right)) :: ( _, ( MlyValue.REPS REPS2, _,
 _)) :: _ :: ( _, ( MlyValue.REPS REPS1, _, _)) :: ( _, ( MlyValue.Exp
 Exp1, Exp1left, _)) :: rest671)) => let val  result = MlyValue.Exp
 (fn _ => let val  (Exp as Exp1) = Exp1 ()
 val  REPS1 = REPS1 ()
 val  REPS2 = REPS2 ()
 in (RE.mkRep (Exp, REPS1, REPS2))
end)
 in ( LrTable.NT 10, ( result, Exp1left, RCB1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.InExp InExp1, InExp1left, InExp1right)) :: 
rest671)) => let val  result = MlyValue.Exp (fn _ => let val  (InExp
 as InExp1) = InExp1 ()
 in (InExp)
end)
 in ( LrTable.NT 10, ( result, InExp1left, InExp1right), rest671)
end
|  ( 33, ( ( _, ( _, HIGH_CHAR1left, HIGH_CHAR1right)) :: rest671)) =>
 let val  result = MlyValue.InExp (fn _ => (
RE.mkSymSet (SIS.interval (0w128, 0w255))))
 in ( LrTable.NT 11, ( result, HIGH_CHAR1left, HIGH_CHAR1right), 
rest671)
end
|  ( 34, ( ( _, ( MlyValue.CHAR CHAR1, CHAR1left, CHAR1right)) :: 
rest671)) => let val  result = MlyValue.InExp (fn _ => let val  (CHAR
 as CHAR1) = CHAR1 ()
 in (RE.mkSymSet (SIS.singleton (strToSym CHAR)))
end)
 in ( LrTable.NT 11, ( result, CHAR1left, CHAR1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.UNICHAR UNICHAR1, UNICHAR1left, 
UNICHAR1right)) :: rest671)) => let val  result = MlyValue.InExp (fn _
 => let val  (UNICHAR as UNICHAR1) = UNICHAR1 ()
 in (RE.mkSymSet (SIS.singleton UNICHAR))
end)
 in ( LrTable.NT 11, ( result, UNICHAR1left, UNICHAR1right), rest671)

end
|  ( 36, ( ( _, ( _, DOT1left, DOT1right)) :: rest671)) => let val  
result = MlyValue.InExp (fn _ => (RE.mkSymSet wildcard))
 in ( LrTable.NT 11, ( result, DOT1left, DOT1right), rest671)
end
|  ( 37, ( ( _, ( _, _, RCB1right)) :: ( _, ( MlyValue.ID ID1, ID1left
, _)) :: rest671)) => let val  result = MlyValue.InExp (fn _ => let
 val  (ID as ID1) = ID1 ()
 in (
case AtomMap.find (!symTable, Atom.atom ID)
		  of SOME re => re
		   | NONE => raise Fail ("'" ^ ID ^ "' not defined")
)
end)
 in ( LrTable.NT 11, ( result, ID1left, RCB1right), rest671)
end
|  ( 38, ( ( _, ( _, _, RP1right)) :: ( _, ( MlyValue.OrExp OrExp1, _,
 _)) :: ( _, ( _, LP1left, _)) :: rest671)) => let val  result = 
MlyValue.InExp (fn _ => let val  (OrExp as OrExp1) = OrExp1 ()
 in (OrExp)
end)
 in ( LrTable.NT 11, ( result, LP1left, RP1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.CharClass CharClass1, _, CharClass1right))
 :: _ :: ( _, ( _, LB1left, _)) :: rest671)) => let val  result = 
MlyValue.InExp (fn _ => let val  (CharClass as CharClass1) = 
CharClass1 ()
 in (RE.mkSymSet (SIS.complement CharClass))
end)
 in ( LrTable.NT 11, ( result, LB1left, CharClass1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.CharClass CharClass1, _, CharClass1right))
 :: ( _, ( _, LB1left, _)) :: rest671)) => let val  result = 
MlyValue.InExp (fn _ => let val  (CharClass as CharClass1) = 
CharClass1 ()
 in (RE.mkSymSet CharClass)
end)
 in ( LrTable.NT 11, ( result, LB1left, CharClass1right), rest671)
end
|  ( 41, ( ( _, ( _, _, RB1right)) :: ( _, ( MlyValue.CharClass' 
CharClass'1, CharClass'1left, _)) :: rest671)) => let val  result = 
MlyValue.CharClass (fn _ => let val  (CharClass' as CharClass'1) = 
CharClass'1 ()
 in (CharClass')
end)
 in ( LrTable.NT 12, ( result, CharClass'1left, RB1right), rest671)

end
|  ( 42, ( ( _, ( _, _, RB1right)) :: ( _, ( MlyValue.CharClass' 
CharClass'1, _, _)) :: ( _, ( _, DASH1left, _)) :: rest671)) => let
 val  result = MlyValue.CharClass (fn _ => let val  (CharClass' as 
CharClass'1) = CharClass'1 ()
 in (SIS.add (CharClass', charToSym #"-"))
end)
 in ( LrTable.NT 12, ( result, DASH1left, RB1right), rest671)
end
|  ( 43, ( ( _, ( _, RBD1left, RBD1right)) :: rest671)) => let val  
result = MlyValue.CharClass (fn _ => (SIS.singleton (charToSym #"-")))
 in ( LrTable.NT 12, ( result, RBD1left, RBD1right), rest671)
end
|  ( 44, ( ( _, ( _, _, RBD1right)) :: ( _, ( MlyValue.CharClass' 
CharClass'1, CharClass'1left, _)) :: rest671)) => let val  result = 
MlyValue.CharClass (fn _ => let val  (CharClass' as CharClass'1) = 
CharClass'1 ()
 in (SIS.add (CharClass', charToSym #"-"))
end)
 in ( LrTable.NT 12, ( result, CharClass'1left, RBD1right), rest671)

end
|  ( 45, ( ( _, ( MlyValue.NonCarat NonCarat1, NonCarat1left, 
NonCarat1right)) :: rest671)) => let val  result = MlyValue.CharClass'
 (fn _ => let val  (NonCarat as NonCarat1) = NonCarat1 ()
 in (SIS.singleton NonCarat)
end)
 in ( LrTable.NT 13, ( result, NonCarat1left, NonCarat1right), rest671
)
end
|  ( 46, ( ( _, ( MlyValue.AChar AChar1, _, AChar1right)) :: _ :: ( _,
 ( MlyValue.NonCarat NonCarat1, NonCarat1left, _)) :: rest671)) => let
 val  result = MlyValue.CharClass' (fn _ => let val  (NonCarat as 
NonCarat1) = NonCarat1 ()
 val  (AChar as AChar1) = AChar1 ()
 in (SIS.interval (NonCarat, AChar))
end)
 in ( LrTable.NT 13, ( result, NonCarat1left, AChar1right), rest671)

end
|  ( 47, ( ( _, ( MlyValue.CharRng CharRng1, _, CharRng1right)) :: ( _
, ( MlyValue.CharClass' CharClass'1, CharClass'1left, _)) :: rest671))
 => let val  result = MlyValue.CharClass' (fn _ => let val  (
CharClass' as CharClass'1) = CharClass'1 ()
 val  (CharRng as CharRng1) = CharRng1 ()
 in (SIS.union (CharRng, CharClass'))
end)
 in ( LrTable.NT 13, ( result, CharClass'1left, CharRng1right), 
rest671)
end
|  ( 48, ( ( _, ( MlyValue.AChar AChar2, _, AChar2right)) :: _ :: ( _,
 ( MlyValue.AChar AChar1, AChar1left, _)) :: rest671)) => let val  
result = MlyValue.CharRng (fn _ => let val  AChar1 = AChar1 ()
 val  AChar2 = AChar2 ()
 in (SIS.interval (AChar1, AChar2))
end)
 in ( LrTable.NT 14, ( result, AChar1left, AChar2right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.AChar AChar1, AChar1left, AChar1right)) :: 
rest671)) => let val  result = MlyValue.CharRng (fn _ => let val  (
AChar as AChar1) = AChar1 ()
 in (SIS.singleton AChar)
end)
 in ( LrTable.NT 14, ( result, AChar1left, AChar1right), rest671)
end
|  ( 50, ( ( _, ( _, CARAT1left, CARAT1right)) :: rest671)) => let
 val  result = MlyValue.AChar (fn _ => (charToSym #"^"))
 in ( LrTable.NT 15, ( result, CARAT1left, CARAT1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.NonCarat NonCarat1, NonCarat1left, 
NonCarat1right)) :: rest671)) => let val  result = MlyValue.AChar (fn
 _ => let val  (NonCarat as NonCarat1) = NonCarat1 ()
 in (NonCarat)
end)
 in ( LrTable.NT 15, ( result, NonCarat1left, NonCarat1right), rest671
)
end
|  ( 52, ( ( _, ( MlyValue.CHAR CHAR1, CHAR1left, CHAR1right)) :: 
rest671)) => let val  result = MlyValue.NonCarat (fn _ => let val  (
CHAR as CHAR1) = CHAR1 ()
 in (strToSym CHAR)
end)
 in ( LrTable.NT 16, ( result, CHAR1left, CHAR1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.UNICHAR UNICHAR1, UNICHAR1left, 
UNICHAR1right)) :: rest671)) => let val  result = MlyValue.NonCarat
 (fn _ => let val  (UNICHAR as UNICHAR1) = UNICHAR1 ()
 in (UNICHAR)
end)
 in ( LrTable.NT 16, ( result, UNICHAR1left, UNICHAR1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : MLLex_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun DECLS (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.DECLS (fn () => i),p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun LP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun RP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun LB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun RB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RBD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LCB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RCB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun QMARK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun STAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun CARAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun DOLLAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun SLASH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DASH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun HIGH_CHAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun CHAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.CHAR (fn () => i),p1,p2))
fun UNICHAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.UNICHAR (fn () => i),p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun REPS (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.REPS (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ACT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.ACT (fn () => i),p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun LEXMARK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun STATES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun LEXSTATE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.LEXSTATE (fn () => i),p1,p2))
fun COUNT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun REJECTTOK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun FULL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun UNICODE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun STRUCT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun HEADER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun ARG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun POSARG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
end
end

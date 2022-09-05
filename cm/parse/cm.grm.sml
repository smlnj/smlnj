functor CMLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : CM_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* -*- sml-yacc -*-
 *
 * cm.grm
 *
 * ML-Yacc grammar for CM description files
 *
 * (C) 1999,2001 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@research.bell-labs.com)
 *)

structure S = CMSemant


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\228\000\003\000\228\000\007\000\228\000\008\000\228\000\
\\010\000\228\000\011\000\228\000\013\000\228\000\014\000\228\000\
\\015\000\228\000\016\000\228\000\017\000\228\000\018\000\228\000\
\\019\000\228\000\020\000\228\000\021\000\228\000\028\000\228\000\
\\029\000\228\000\035\000\228\000\000\000\
\\001\000\002\000\010\000\000\000\
\\001\000\002\000\010\000\007\000\009\000\008\000\008\000\010\000\007\000\000\000\
\\001\000\002\000\010\000\008\000\008\000\010\000\007\000\000\000\
\\001\000\002\000\010\000\011\000\065\000\000\000\
\\001\000\002\000\063\000\003\000\062\000\000\000\
\\001\000\002\000\063\000\003\000\062\000\010\000\124\000\000\000\
\\001\000\002\000\063\000\003\000\062\000\034\000\068\000\000\000\
\\001\000\004\000\054\000\006\000\053\000\010\000\052\000\022\000\051\000\
\\023\000\050\000\027\000\049\000\030\000\048\000\031\000\047\000\
\\032\000\046\000\000\000\
\\001\000\004\000\054\000\006\000\053\000\010\000\087\000\023\000\050\000\
\\027\000\049\000\000\000\
\\001\000\004\000\054\000\018\000\022\000\019\000\021\000\020\000\020\000\
\\021\000\019\000\000\000\
\\001\000\005\000\038\000\000\000\
\\001\000\005\000\039\000\000\000\
\\001\000\005\000\040\000\000\000\
\\001\000\005\000\041\000\000\000\
\\001\000\007\000\027\000\008\000\026\000\009\000\037\000\010\000\036\000\
\\013\000\024\000\017\000\023\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\021\000\019\000\035\000\018\000\000\000\
\\001\000\007\000\027\000\008\000\026\000\009\000\135\000\010\000\036\000\
\\013\000\024\000\017\000\023\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\021\000\019\000\035\000\018\000\000\000\
\\001\000\007\000\027\000\008\000\026\000\010\000\025\000\013\000\024\000\
\\017\000\023\000\018\000\022\000\019\000\021\000\020\000\020\000\
\\021\000\019\000\035\000\018\000\000\000\
\\001\000\007\000\027\000\008\000\026\000\010\000\036\000\011\000\093\000\
\\013\000\024\000\017\000\023\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\021\000\019\000\035\000\018\000\000\000\
\\001\000\007\000\027\000\008\000\026\000\010\000\036\000\013\000\024\000\
\\014\000\108\000\015\000\107\000\016\000\106\000\017\000\023\000\
\\018\000\022\000\019\000\021\000\020\000\020\000\021\000\019\000\
\\035\000\018\000\000\000\
\\001\000\007\000\027\000\008\000\026\000\010\000\036\000\013\000\024\000\
\\016\000\144\000\017\000\023\000\018\000\022\000\019\000\021\000\
\\020\000\020\000\021\000\019\000\035\000\018\000\000\000\
\\001\000\007\000\027\000\008\000\026\000\010\000\036\000\013\000\024\000\
\\017\000\023\000\018\000\022\000\019\000\021\000\020\000\020\000\
\\021\000\019\000\035\000\018\000\000\000\
\\001\000\007\000\027\000\008\000\026\000\010\000\036\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\035\000\018\000\000\000\
\\001\000\009\000\059\000\000\000\
\\001\000\009\000\139\000\000\000\
\\001\000\010\000\032\000\000\000\
\\001\000\010\000\058\000\000\000\
\\001\000\010\000\089\000\000\000\
\\001\000\011\000\094\000\000\000\
\\001\000\011\000\097\000\000\000\
\\001\000\011\000\100\000\000\000\
\\001\000\011\000\119\000\025\000\080\000\028\000\079\000\029\000\078\000\000\000\
\\001\000\011\000\120\000\023\000\084\000\024\000\083\000\000\000\
\\001\000\011\000\120\000\023\000\084\000\024\000\083\000\025\000\082\000\
\\026\000\081\000\000\000\
\\001\000\011\000\133\000\000\000\
\\001\000\011\000\134\000\000\000\
\\001\000\011\000\136\000\000\000\
\\001\000\011\000\147\000\000\000\
\\001\000\014\000\143\000\015\000\142\000\016\000\141\000\000\000\
\\001\000\016\000\155\000\000\000\
\\001\000\023\000\084\000\024\000\083\000\025\000\082\000\026\000\081\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\007\000\027\000\008\000\026\000\010\000\030\000\013\000\024\000\
\\017\000\023\000\018\000\022\000\019\000\021\000\020\000\020\000\
\\021\000\019\000\035\000\018\000\000\000\
\\175\000\007\000\027\000\008\000\026\000\010\000\036\000\013\000\024\000\
\\017\000\023\000\018\000\022\000\019\000\021\000\020\000\020\000\
\\021\000\019\000\035\000\018\000\000\000\
\\176\000\007\000\027\000\008\000\026\000\010\000\036\000\013\000\024\000\
\\017\000\023\000\018\000\022\000\019\000\021\000\020\000\020\000\
\\021\000\019\000\035\000\018\000\000\000\
\\177\000\000\000\
\\177\000\002\000\057\000\000\000\
\\177\000\002\000\063\000\003\000\062\000\000\000\
\\178\000\000\000\
\\179\000\033\000\034\000\034\000\033\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\033\000\034\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\002\000\063\000\003\000\062\000\013\000\075\000\017\000\074\000\000\000\
\\195\000\000\000\
\\196\000\002\000\063\000\003\000\062\000\000\000\
\\196\000\002\000\063\000\003\000\062\000\012\000\149\000\000\000\
\\197\000\000\000\
\\198\000\000\000\
\\199\000\000\000\
\\200\000\000\000\
\\201\000\010\000\124\000\000\000\
\\202\000\000\000\
\\203\000\012\000\102\000\000\000\
\\204\000\000\000\
\\205\000\000\000\
\\206\000\000\000\
\\207\000\000\000\
\\208\000\000\000\
\\209\000\000\000\
\\210\000\000\000\
\\211\000\000\000\
\\212\000\000\000\
\\213\000\000\000\
\\214\000\000\000\
\\215\000\025\000\080\000\028\000\079\000\029\000\078\000\000\000\
\\216\000\000\000\
\\217\000\000\000\
\\218\000\000\000\
\\219\000\024\000\083\000\000\000\
\\220\000\000\000\
\\221\000\000\000\
\\222\000\000\000\
\\223\000\000\000\
\\224\000\000\000\
\\225\000\000\000\
\\226\000\025\000\080\000\000\000\
\\227\000\025\000\080\000\028\000\079\000\000\000\
\\229\000\000\000\
\\230\000\023\000\084\000\024\000\083\000\000\000\
\\231\000\023\000\084\000\024\000\083\000\000\000\
\\232\000\000\000\
\\233\000\000\000\
\\234\000\000\000\
\\235\000\000\000\
\\236\000\000\000\
\\237\000\000\000\
\\238\000\000\000\
\\239\000\000\000\
\\240\000\000\000\
\\241\000\000\000\
\\242\000\000\000\
\"
val actionRowNumbers =
"\003\000\003\000\018\000\057\000\
\\042\000\048\000\052\000\050\000\
\\099\000\053\000\051\000\026\000\
\\064\000\069\000\055\000\016\000\
\\068\000\012\000\013\000\014\000\
\\015\000\066\000\009\000\061\000\
\\027\000\067\000\024\000\059\000\
\\062\000\005\000\008\000\023\000\
\\023\000\056\000\060\000\079\000\
\\122\000\121\000\120\000\119\000\
\\103\000\060\000\101\000\041\000\
\\118\000\117\000\009\000\010\000\
\\010\000\028\000\009\000\102\000\
\\100\000\019\000\029\000\047\000\
\\006\000\079\000\030\000\125\000\
\\124\000\123\000\049\000\004\000\
\\031\000\126\000\127\000\072\000\
\\071\000\089\000\079\000\046\000\
\\093\000\009\000\065\000\020\000\
\\009\000\009\000\009\000\010\000\
\\010\000\010\000\010\000\114\000\
\\107\000\010\000\108\000\011\000\
\\032\000\034\000\063\000\070\000\
\\022\000\087\000\044\000\058\000\
\\004\000\054\000\073\000\087\000\
\\002\000\080\000\079\000\075\000\
\\076\000\060\000\009\000\113\000\
\\112\000\001\000\115\000\116\000\
\\106\000\105\000\033\000\035\000\
\\036\000\111\000\104\000\017\000\
\\037\000\088\000\081\000\025\000\
\\091\000\090\000\094\000\092\000\
\\039\000\021\000\060\000\110\000\
\\109\000\079\000\074\000\038\000\
\\082\000\079\000\095\000\096\000\
\\079\000\009\000\077\000\078\000\
\\045\000\086\000\083\000\007\000\
\\043\000\040\000\079\000\081\000\
\\081\000\097\000\098\000\084\000\
\\085\000\000\000"
val gotoT =
"\
\\001\000\157\000\002\000\004\000\003\000\003\000\004\000\002\000\
\\024\000\001\000\000\000\
\\003\000\010\000\004\000\009\000\024\000\001\000\000\000\
\\007\000\015\000\010\000\014\000\020\000\013\000\021\000\012\000\
\\032\000\011\000\000\000\
\\007\000\027\000\008\000\026\000\010\000\014\000\020\000\013\000\
\\021\000\012\000\032\000\011\000\000\000\
\\000\000\
\\006\000\029\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\033\000\020\000\013\000\021\000\012\000\032\000\011\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\043\000\018\000\042\000\019\000\041\000\023\000\040\000\000\000\
\\005\000\054\000\009\000\053\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\033\000\020\000\013\000\021\000\012\000\032\000\011\000\000\000\
\\009\000\053\000\022\000\059\000\030\000\058\000\000\000\
\\024\000\062\000\000\000\
\\022\000\059\000\030\000\065\000\031\000\064\000\000\000\
\\020\000\013\000\021\000\067\000\032\000\011\000\000\000\
\\020\000\013\000\021\000\068\000\032\000\011\000\000\000\
\\000\000\
\\009\000\053\000\000\000\
\\013\000\071\000\014\000\070\000\022\000\069\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\075\000\011\000\074\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\043\000\018\000\083\000\023\000\040\000\000\000\
\\017\000\084\000\023\000\040\000\000\000\
\\017\000\086\000\023\000\040\000\000\000\
\\000\000\
\\017\000\089\000\018\000\088\000\023\000\040\000\000\000\
\\000\000\
\\000\000\
\\010\000\090\000\020\000\013\000\021\000\012\000\032\000\011\000\000\000\
\\000\000\
\\000\000\
\\022\000\093\000\000\000\
\\013\000\094\000\014\000\070\000\022\000\069\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\097\000\024\000\096\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\026\000\099\000\000\000\
\\013\000\101\000\014\000\070\000\022\000\069\000\000\000\
\\000\000\
\\000\000\
\\017\000\043\000\018\000\042\000\019\000\102\000\023\000\040\000\000\000\
\\000\000\
\\010\000\090\000\012\000\103\000\020\000\013\000\021\000\012\000\
\\032\000\011\000\000\000\
\\017\000\043\000\018\000\107\000\023\000\040\000\000\000\
\\017\000\043\000\018\000\108\000\023\000\040\000\000\000\
\\017\000\043\000\018\000\109\000\023\000\040\000\000\000\
\\017\000\110\000\023\000\040\000\000\000\
\\017\000\111\000\023\000\040\000\000\000\
\\017\000\112\000\023\000\040\000\000\000\
\\017\000\113\000\023\000\040\000\000\000\
\\000\000\
\\000\000\
\\017\000\114\000\023\000\040\000\000\000\
\\000\000\
\\020\000\116\000\023\000\115\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\119\000\010\000\014\000\020\000\013\000\021\000\012\000\
\\032\000\011\000\000\000\
\\028\000\121\000\029\000\120\000\000\000\
\\000\000\
\\007\000\027\000\008\000\123\000\010\000\014\000\020\000\013\000\
\\021\000\012\000\032\000\011\000\000\000\
\\004\000\009\000\024\000\096\000\000\000\
\\000\000\
\\000\000\
\\028\000\121\000\029\000\124\000\000\000\
\\024\000\126\000\025\000\125\000\000\000\
\\000\000\
\\013\000\128\000\014\000\070\000\015\000\127\000\022\000\069\000\000\000\
\\000\000\
\\000\000\
\\009\000\129\000\000\000\
\\017\000\043\000\018\000\042\000\019\000\130\000\023\000\040\000\000\000\
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
\\000\000\
\\000\000\
\\010\000\033\000\020\000\013\000\021\000\012\000\032\000\011\000\000\000\
\\000\000\
\\000\000\
\\022\000\136\000\027\000\135\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\138\000\000\000\
\\010\000\090\000\020\000\013\000\021\000\012\000\032\000\011\000\000\000\
\\009\000\075\000\011\000\143\000\000\000\
\\000\000\
\\000\000\
\\013\000\144\000\014\000\070\000\022\000\069\000\000\000\
\\000\000\
\\000\000\
\\022\000\136\000\027\000\146\000\000\000\
\\013\000\148\000\014\000\070\000\022\000\069\000\000\000\
\\000\000\
\\000\000\
\\013\000\149\000\014\000\070\000\022\000\069\000\000\000\
\\017\000\043\000\018\000\042\000\019\000\150\000\023\000\040\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\022\000\152\000\028\000\151\000\000\000\
\\000\000\
\\000\000\
\\013\000\128\000\014\000\070\000\015\000\154\000\022\000\069\000\000\000\
\\022\000\136\000\027\000\155\000\000\000\
\\022\000\136\000\027\000\156\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 158
val numrules = 83
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
type arg = { grouppath:SrcPath.file,context:S.context,obsolete:pos*pos -> unit,error:pos*pos -> string -> unit,doMember:{ name:string,mkpath:unit -> SrcPath.prefile } *pos*pos*S.cm_class option*S.toolopt list option -> S.members,curlib:SrcPath.file option,gp:GeneralParams.info,ig:S.group } 
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | INEQSYM of unit ->  (CMSemant.ineqsym)
 | EQSYM of unit ->  (CMSemant.eqsym)
 | MULSYM of unit ->  (CMSemant.mulsym)
 | ADDSYM of unit ->  (CMSemant.addsym) | ERROR of unit ->  (string)
 | NUMBER of unit ->  (int) | ML_ID of unit ->  (string)
 | CM_ID of unit ->  (string) | FILE_NATIVE of unit ->  (string)
 | FILE_STANDARD of unit ->  (string)
 | filecat of unit ->  ( ( unit -> SrcPath.file option ) *S.complainer -> S.exports)
 | srcfiles of unit ->  (unit -> SrcPath.file option)
 | srcfile of unit ->  (unit -> SrcPath.file)
 | opttoolopts of unit ->  (S.toolopt list option)
 | ptoolopts of unit ->  (S.toolopt list)
 | toolopts of unit ->  (S.toolopt list)
 | optclass of unit ->  (S.cm_class option)
 | class of unit ->  (S.cm_class) | word of unit ->  (S.cm_symbol)
 | sym of unit ->  (S.cm_symbol)
 | pathname of unit ->  ({ name:string,mkpath:unit -> SrcPath.prefile } )
 | ml_symbolset of unit ->  (S.exports)
 | ml_symbol of unit ->  (S.ml_symbol) | exp of unit ->  (S.exp)
 | boolexp of unit ->  (S.exp) | aexp of unit ->  (S.aexp)
 | else_members of unit ->  (S.members)
 | guarded_members of unit ->  (S.members*S.members)
 | member of unit ->  (S.members) | members of unit ->  (S.members)
 | else_exports of unit ->  (S.exports)
 | guarded_exports of unit ->  (S.exports*S.exports)
 | export of unit ->  (S.exports) | exports of unit ->  (S.exports)
 | opt_exports of unit ->  (S.exports)
 | mand_exports of unit ->  (S.exports)
 | wrapspec of unit ->  (S.privilegespec -> S.privilegespec)
 | version of unit ->  (S.cm_version)
 | lprivspec of unit ->  (S.privilegespec)
 | gprivspec of unit ->  (S.privilegespec)
 | group of unit ->  (S.group) | description of unit ->  (S.group)
end
type svalue = MlyValue.svalue
type result = S.group
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 6) => true | (T 7) => true | (T 8) => true | (T 12) => true | 
(T 13) => true | (T 14) => true | (T 15) => true | (T 21) => true | 
(T 17) => true | (T 18) => true | (T 19) => true | (T 20) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 9))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "FILE_STANDARD"
  | (T 2) => "FILE_NATIVE"
  | (T 3) => "CM_ID"
  | (T 4) => "ML_ID"
  | (T 5) => "NUMBER"
  | (T 6) => "GROUP"
  | (T 7) => "LIBRARY"
  | (T 8) => "IS"
  | (T 9) => "LPAREN"
  | (T 10) => "RPAREN"
  | (T 11) => "COLON"
  | (T 12) => "IF"
  | (T 13) => "ELIF"
  | (T 14) => "ELSE"
  | (T 15) => "ENDIF"
  | (T 16) => "ERROR"
  | (T 17) => "STRUCTURE"
  | (T 18) => "SIGNATURE"
  | (T 19) => "FUNCTOR"
  | (T 20) => "FUNSIG"
  | (T 21) => "DEFINED"
  | (T 22) => "ADDSYM"
  | (T 23) => "MULSYM"
  | (T 24) => "EQSYM"
  | (T 25) => "INEQSYM"
  | (T 26) => "TILDE"
  | (T 27) => "ANDALSO"
  | (T 28) => "ORELSE"
  | (T 29) => "NOT"
  | (T 30) => "TRUE"
  | (T 31) => "FALSE"
  | (T 32) => "STAR"
  | (T 33) => "DASH"
  | (T 34) => "SOURCE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    ({ grouppath, context, obsolete, error, doMember, curlib, gp, ig }):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.group group1, group1left, group1right)) :: 
rest671)) => let val  result = MlyValue.description (fn _ => let val 
 (group as group1) = group1 ()
 in (group)
end)
 in ( LrTable.NT 0, ( result, group1left, group1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.members members1, _, members1right)) :: _ ::
 ( _, ( MlyValue.opt_exports opt_exports1, _, _)) :: ( _, ( _, _, 
RPARENright)) :: ( _, ( MlyValue.srcfile srcfile1, _, _)) :: ( _, ( _,
 LPARENleft, _)) :: ( _, ( MlyValue.gprivspec gprivspec1, 
gprivspec1left, _)) :: rest671)) => let val  result = MlyValue.group
 (fn _ => let val  (gprivspec as gprivspec1) = gprivspec1 ()
 val  srcfile1 = srcfile1 ()
 val  (opt_exports as opt_exports1) = opt_exports1 ()
 val  (members as members1) = members1 ()
 in (
obsolete (LPARENleft,
							   RPARENright);
						 S.group
						 { path = grouppath,
						   privileges = gprivspec,
						   exports = opt_exports,
						   members = members,
						   gp = gp,
						   curlib = curlib,
						   initgroup = ig }
)
end)
 in ( LrTable.NT 1, ( result, gprivspec1left, members1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.members members1, _, members1right)) :: _ ::
 ( _, ( MlyValue.opt_exports opt_exports1, _, _)) :: ( _, ( 
MlyValue.gprivspec gprivspec1, gprivspec1left, _)) :: rest671)) => let
 val  result = MlyValue.group (fn _ => let val  (gprivspec as 
gprivspec1) = gprivspec1 ()
 val  (opt_exports as opt_exports1) = opt_exports1 ()
 val  (members as members1) = members1 ()
 in (
S.group
						 { path = grouppath,
						   privileges = gprivspec,
						   exports = opt_exports,
						   members = members,
						   gp = gp,
						   curlib = curlib,
						   initgroup = ig }
)
end)
 in ( LrTable.NT 1, ( result, gprivspec1left, members1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.members members1, _, members1right)) :: _ ::
 ( _, ( MlyValue.mand_exports mand_exports1, _, _)) :: _ :: ( _, ( 
MlyValue.version version1, _, _)) :: _ :: ( _, ( MlyValue.lprivspec 
lprivspec1, lprivspec1left, _)) :: rest671)) => let val  result = 
MlyValue.group (fn _ => let val  (lprivspec as lprivspec1) = 
lprivspec1 ()
 val  (version as version1) = version1 ()
 val  (mand_exports as mand_exports1) = mand_exports1 ()
 val  (members as members1) = members1 ()
 in (
S.library
						 { path = grouppath,
						   privileges = lprivspec,
						   exports = mand_exports,
						   version = SOME version,
						   members = members,
						   gp = gp,
						   initgroup = ig }
)
end)
 in ( LrTable.NT 1, ( result, lprivspec1left, members1right), rest671)

end
|  ( 4, ( ( _, ( MlyValue.members members1, _, members1right)) :: _ ::
 ( _, ( MlyValue.mand_exports mand_exports1, _, _)) :: ( _, ( 
MlyValue.lprivspec lprivspec1, lprivspec1left, _)) :: rest671)) => let
 val  result = MlyValue.group (fn _ => let val  (lprivspec as 
lprivspec1) = lprivspec1 ()
 val  (mand_exports as mand_exports1) = mand_exports1 ()
 val  (members as members1) = members1 ()
 in (
S.library
						 { path = grouppath,
						   privileges = lprivspec,
						   exports = mand_exports,
						   version = NONE,
						   members = members,
						   gp = gp,
						   initgroup = ig }
)
end)
 in ( LrTable.NT 1, ( result, lprivspec1left, members1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.FILE_STANDARD FILE_STANDARD1, (
FILE_STANDARDleft as FILE_STANDARD1left), (FILE_STANDARDright as 
FILE_STANDARD1right))) :: rest671)) => let val  result = 
MlyValue.version (fn _ => let val  (FILE_STANDARD as FILE_STANDARD1) =
 FILE_STANDARD1 ()
 in (
S.cm_version
						(FILE_STANDARD,
						 error (FILE_STANDARDleft,
							FILE_STANDARDright))
)
end)
 in ( LrTable.NT 4, ( result, FILE_STANDARD1left, FILE_STANDARD1right)
, rest671)
end
|  ( 6, ( rest671)) => let val  result = MlyValue.wrapspec (fn _ => (
fn p => p))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 7, ( ( _, ( MlyValue.word word1, wordleft, (wordright as 
word1right))) :: ( _, ( MlyValue.wrapspec wrapspec1, wrapspec1left, _)
) :: rest671)) => let val  result = MlyValue.wrapspec (fn _ => let
 val  (wrapspec as wrapspec1) = wrapspec1 ()
 val  (word as word1) = word1 ()
 in (
fn p =>
						 S.wrap (wrapspec p, word,
							  error (wordleft,
								 wordright))
)
end)
 in ( LrTable.NT 5, ( result, wrapspec1left, word1right), rest671)
end
|  ( 8, ( ( _, ( _, GROUP1left, GROUP1right)) :: rest671)) => let val 
 result = MlyValue.gprivspec (fn _ => (S.initialPrivilegeSpec))
 in ( LrTable.NT 2, ( result, GROUP1left, GROUP1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.gprivspec gprivspec1, _, gprivspec1right))
 :: ( _, ( MlyValue.word word1, (wordleft as word1left), wordright))
 :: rest671)) => let val  result = MlyValue.gprivspec (fn _ => let
 val  (word as word1) = word1 ()
 val  (gprivspec as gprivspec1) = gprivspec1 ()
 in (
S.require (gprivspec, word,
							    error (wordleft,
								   wordright))
)
end)
 in ( LrTable.NT 2, ( result, word1left, gprivspec1right), rest671)

end
|  ( 10, ( ( _, ( _, LIBRARY1left, LIBRARY1right)) :: rest671)) => let
 val  result = MlyValue.lprivspec (fn _ => (S.initialPrivilegeSpec))
 in ( LrTable.NT 3, ( result, LIBRARY1left, LIBRARY1right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.lprivspec lprivspec1, _, lprivspec1right))
 :: ( _, ( MlyValue.word word1, (wordleft as word1left), wordright))
 :: rest671)) => let val  result = MlyValue.lprivspec (fn _ => let
 val  (word as word1) = word1 ()
 val  (lprivspec as lprivspec1) = lprivspec1 ()
 in (
S.require (lprivspec, word,
							    error (wordleft,
								   wordright))
)
end)
 in ( LrTable.NT 3, ( result, word1left, lprivspec1right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.lprivspec lprivspec1, _, lprivspec1right))
 :: _ :: ( _, ( MlyValue.wrapspec wrapspec1, _, _)) :: ( _, ( _, 
LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.lprivspec
 (fn _ => let val  (wrapspec as wrapspec1) = wrapspec1 ()
 val  (lprivspec as lprivspec1) = lprivspec1 ()
 in (wrapspec lprivspec)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, lprivspec1right), rest671)

end
|  ( 13, ( ( _, ( MlyValue.export export1, export1left, export1right))
 :: rest671)) => let val  result = MlyValue.mand_exports (fn _ => let
 val  (export as export1) = export1 ()
 in (export)
end)
 in ( LrTable.NT 6, ( result, export1left, export1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.export export1, _, export1right)) :: ( _, (
 MlyValue.mand_exports mand_exports1, mand_exports1left, _)) :: 
rest671)) => let val  result = MlyValue.mand_exports (fn _ => let val 
 (mand_exports as mand_exports1) = mand_exports1 ()
 val  (export as export1) = export1 ()
 in (S.union (mand_exports,export))
end)
 in ( LrTable.NT 6, ( result, mand_exports1left, export1right), 
rest671)
end
|  ( 15, ( rest671)) => let val  result = MlyValue.opt_exports (fn _
 => (S.default_group_exports))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 16, ( ( _, ( MlyValue.mand_exports mand_exports1, 
mand_exports1left, mand_exports1right)) :: rest671)) => let val  
result = MlyValue.opt_exports (fn _ => let val  (mand_exports as 
mand_exports1) = mand_exports1 ()
 in (mand_exports)
end)
 in ( LrTable.NT 7, ( result, mand_exports1left, mand_exports1right), 
rest671)
end
|  ( 17, ( rest671)) => let val  result = MlyValue.exports (fn _ => (
S.emptyExports))
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 18, ( ( _, ( MlyValue.export export1, _, export1right)) :: ( _, (
 MlyValue.exports exports1, exports1left, _)) :: rest671)) => let val 
 result = MlyValue.exports (fn _ => let val  (exports as exports1) = 
exports1 ()
 val  (export as export1) = export1 ()
 in (S.union (exports, export))
end)
 in ( LrTable.NT 8, ( result, exports1left, export1right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.ml_symbolset ml_symbolset1, 
ml_symbolset1left, ml_symbolset1right)) :: rest671)) => let val  
result = MlyValue.export (fn _ => let val  (ml_symbolset as 
ml_symbolset1) = ml_symbolset1 ()
 in (ml_symbolset)
end)
 in ( LrTable.NT 9, ( result, ml_symbolset1left, ml_symbolset1right), 
rest671)
end
|  ( 20, ( ( _, ( MlyValue.guarded_exports guarded_exports1, _, 
guarded_exports1right)) :: ( _, ( MlyValue.exp exp1, expleft, expright
)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.export (fn _ => let val  (exp as exp1) = exp1 ()
 val  (guarded_exports as guarded_exports1) = guarded_exports1 ()
 in (
S.guarded_exports
						  (exp, guarded_exports,
						   error (expleft, expright))
)
end)
 in ( LrTable.NT 9, ( result, IF1left, guarded_exports1right), rest671
)
end
|  ( 21, ( ( _, ( MlyValue.ERROR ERROR1, (ERRORleft as ERROR1left), (
ERRORright as ERROR1right))) :: rest671)) => let val  result = 
MlyValue.export (fn _ => let val  (ERROR as ERROR1) = ERROR1 ()
 in (
S.error_export
						 (fn () =>
						  error (ERRORleft, ERRORright)
						        ERROR)
)
end)
 in ( LrTable.NT 9, ( result, ERROR1left, ERROR1right), rest671)
end
|  ( 22, ( ( _, ( _, GROUP1left, GROUP1right)) :: rest671)) => let
 val  result = MlyValue.filecat (fn _ => (S.exportgroup))
 in ( LrTable.NT 31, ( result, GROUP1left, GROUP1right), rest671)
end
|  ( 23, ( ( _, ( _, SOURCE1left, SOURCE1right)) :: rest671)) => let
 val  result = MlyValue.filecat (fn _ => (S.exportsource))
 in ( LrTable.NT 31, ( result, SOURCE1left, SOURCE1right), rest671)

end
|  ( 24, ( ( _, ( MlyValue.ml_symbol ml_symbol1, (ml_symbolleft as 
ml_symbol1left), (ml_symbolright as ml_symbol1right))) :: rest671)) =>
 let val  result = MlyValue.ml_symbolset (fn _ => let val  (ml_symbol
 as ml_symbol1) = ml_symbol1 ()
 in (
S.export
						     (ml_symbol,
						      error (ml_symbolleft,
							     ml_symbolright))
)
end)
 in ( LrTable.NT 20, ( result, ml_symbol1left, ml_symbol1right), 
rest671)
end
|  ( 25, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exports 
exports1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.ml_symbolset (fn _ => let val  (exports as 
exports1) = exports1 ()
 in (exports)
end)
 in ( LrTable.NT 20, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.ml_symbolset ml_symbolset2, _, 
ml_symbolset2right)) :: _ :: ( _, ( MlyValue.ml_symbolset 
ml_symbolset1, ml_symbolset1left, _)) :: rest671)) => let val  result
 = MlyValue.ml_symbolset (fn _ => let val  ml_symbolset1 = 
ml_symbolset1 ()
 val  ml_symbolset2 = ml_symbolset2 ()
 in (
S.intersection
						     (ml_symbolset1,
						      ml_symbolset2))

end)
 in ( LrTable.NT 20, ( result, ml_symbolset1left, ml_symbolset2right),
 rest671)
end
|  ( 27, ( ( _, ( MlyValue.ml_symbolset ml_symbolset2, _, 
ml_symbolset2right)) :: _ :: ( _, ( MlyValue.ml_symbolset 
ml_symbolset1, ml_symbolset1left, _)) :: rest671)) => let val  result
 = MlyValue.ml_symbolset (fn _ => let val  ml_symbolset1 = 
ml_symbolset1 ()
 val  ml_symbolset2 = ml_symbolset2 ()
 in (
S.difference
						     (ml_symbolset1,
						      ml_symbolset2))

end)
 in ( LrTable.NT 20, ( result, ml_symbolset1left, ml_symbolset2right),
 rest671)
end
|  ( 28, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.srcfiles 
srcfiles1, srcfilesleft, srcfilesright)) :: _ :: ( _, ( 
MlyValue.filecat filecat1, filecat1left, _)) :: rest671)) => let val  
result = MlyValue.ml_symbolset (fn _ => let val  (filecat as filecat1)
 = filecat1 ()
 val  (srcfiles as srcfiles1) = srcfiles1 ()
 in (
filecat
						     (srcfiles,
						      error (srcfilesleft,
							     srcfilesright))
)
end)
 in ( LrTable.NT 20, ( result, filecat1left, RPAREN1right), rest671)

end
|  ( 29, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.opttoolopts 
opttoolopts1, _, _)) :: ( _, ( MlyValue.pathname pathname1, 
pathnameleft, pathnameright)) :: _ :: ( _, ( _, LIBRARY1left, _)) :: 
rest671)) => let val  result = MlyValue.ml_symbolset (fn _ => let val 
 (pathname as pathname1) = pathname1 ()
 val  (opttoolopts as opttoolopts1) = opttoolopts1 ()
 in (
S.exportlibrary
						     (fn () => SrcPath.file
							 (#mkpath pathname ()),
						      error (pathnameleft,
							     pathnameright),
						      { hasoptions =
							   isSome opttoolopts,
							elab = fn () =>
							   doMember
							       (pathname,
								pathnameleft,
								pathnameright,
								NONE,
								opttoolopts),
							curlib = curlib })
)
end)
 in ( LrTable.NT 20, ( result, LIBRARY1left, RPAREN1right), rest671)

end
|  ( 30, ( ( _, ( MlyValue.else_exports else_exports1, _, 
else_exports1right)) :: ( _, ( MlyValue.exports exports1, exports1left
, _)) :: rest671)) => let val  result = MlyValue.guarded_exports (fn _
 => let val  (exports as exports1) = exports1 ()
 val  (else_exports as else_exports1) = else_exports1 ()
 in ((exports, else_exports))
end)
 in ( LrTable.NT 10, ( result, exports1left, else_exports1right), 
rest671)
end
|  ( 31, ( ( _, ( _, ENDIF1left, ENDIF1right)) :: rest671)) => let
 val  result = MlyValue.else_exports (fn _ => (S.emptyExports))
 in ( LrTable.NT 11, ( result, ENDIF1left, ENDIF1right), rest671)
end
|  ( 32, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.exports 
exports1, _, _)) :: ( _, ( _, ELSE1left, _)) :: rest671)) => let val  
result = MlyValue.else_exports (fn _ => let val  (exports as exports1)
 = exports1 ()
 in (exports)
end)
 in ( LrTable.NT 11, ( result, ELSE1left, ENDIF1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.guarded_exports guarded_exports1, _, 
guarded_exports1right)) :: ( _, ( MlyValue.exp exp1, expleft, expright
)) :: ( _, ( _, ELIF1left, _)) :: rest671)) => let val  result = 
MlyValue.else_exports (fn _ => let val  (exp as exp1) = exp1 ()
 val  (guarded_exports as guarded_exports1) = guarded_exports1 ()
 in (
S.guarded_exports
						 (exp, guarded_exports,
						  error (expleft, expright))
)
end)
 in ( LrTable.NT 11, ( result, ELIF1left, guarded_exports1right), 
rest671)
end
|  ( 34, ( rest671)) => let val  result = MlyValue.members (fn _ => (
S.emptyMembers))
 in ( LrTable.NT 12, ( result, defaultPos, defaultPos), rest671)
end
|  ( 35, ( ( _, ( MlyValue.members members1, _, members1right)) :: ( _
, ( MlyValue.member member1, member1left, _)) :: rest671)) => let val 
 result = MlyValue.members (fn _ => let val  (member as member1) = 
member1 ()
 val  (members as members1) = members1 ()
 in (S.members (member, members))
end)
 in ( LrTable.NT 12, ( result, member1left, members1right), rest671)

end
|  ( 36, ( rest671)) => let val  result = MlyValue.toolopts (fn _ => (
[]))
 in ( LrTable.NT 26, ( result, defaultPos, defaultPos), rest671)
end
|  ( 37, ( ( _, ( MlyValue.toolopts toolopts1, _, toolopts1right)) :: 
( _, ( MlyValue.pathname pathname1, pathname1left, _)) :: rest671)) =>
 let val  result = MlyValue.toolopts (fn _ => let val  (pathname as 
pathname1) = pathname1 ()
 val  (toolopts as toolopts1) = toolopts1 ()
 in (S.string pathname :: toolopts)
end)
 in ( LrTable.NT 26, ( result, pathname1left, toolopts1right), rest671
)
end
|  ( 38, ( ( _, ( MlyValue.toolopts toolopts1, _, toolopts1right)) :: 
( _, ( MlyValue.ptoolopts ptoolopts1, _, _)) :: _ :: ( _, ( 
MlyValue.pathname pathname1, pathname1left, _)) :: rest671)) => let
 val  result = MlyValue.toolopts (fn _ => let val  (pathname as 
pathname1) = pathname1 ()
 val  (ptoolopts as ptoolopts1) = ptoolopts1 ()
 val  (toolopts as toolopts1) = toolopts1 ()
 in (
S.subopts
						     { name = #name pathname,
						       opts = ptoolopts }
						 :: toolopts
)
end)
 in ( LrTable.NT 26, ( result, pathname1left, toolopts1right), rest671
)
end
|  ( 39, ( ( _, ( MlyValue.toolopts toolopts1, _, toolopts1right)) :: 
( _, ( MlyValue.pathname pathname2, _, _)) :: _ :: ( _, ( 
MlyValue.pathname pathname1, pathname1left, _)) :: rest671)) => let
 val  result = MlyValue.toolopts (fn _ => let val  pathname1 = 
pathname1 ()
 val  pathname2 = pathname2 ()
 val  (toolopts as toolopts1) = toolopts1 ()
 in (
S.subopts
						     { name = #name pathname1,
						       opts = [S.string
								   pathname2] }
						 :: toolopts
)
end)
 in ( LrTable.NT 26, ( result, pathname1left, toolopts1right), rest671
)
end
|  ( 40, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.toolopts 
toolopts1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.ptoolopts (fn _ => let val  (toolopts as 
toolopts1) = toolopts1 ()
 in (toolopts)
end)
 in ( LrTable.NT 27, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 41, ( rest671)) => let val  result = MlyValue.opttoolopts (fn _
 => (NONE))
 in ( LrTable.NT 28, ( result, defaultPos, defaultPos), rest671)
end
|  ( 42, ( ( _, ( MlyValue.ptoolopts ptoolopts1, ptoolopts1left, 
ptoolopts1right)) :: rest671)) => let val  result = 
MlyValue.opttoolopts (fn _ => let val  (ptoolopts as ptoolopts1) = 
ptoolopts1 ()
 in (SOME ptoolopts)
end)
 in ( LrTable.NT 28, ( result, ptoolopts1left, ptoolopts1right), 
rest671)
end
|  ( 43, ( rest671)) => let val  result = MlyValue.optclass (fn _ => (
NONE))
 in ( LrTable.NT 25, ( result, defaultPos, defaultPos), rest671)
end
|  ( 44, ( ( _, ( MlyValue.class class1, _, class1right)) :: ( _, ( _,
 COLON1left, _)) :: rest671)) => let val  result = MlyValue.optclass
 (fn _ => let val  (class as class1) = class1 ()
 in (SOME class)
end)
 in ( LrTable.NT 25, ( result, COLON1left, class1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.opttoolopts opttoolopts1, _, 
opttoolopts1right)) :: ( _, ( MlyValue.optclass optclass1, _, _)) :: (
 _, ( MlyValue.pathname pathname1, (pathnameleft as pathname1left), 
pathnameright)) :: rest671)) => let val  result = MlyValue.member (fn
 _ => let val  (pathname as pathname1) = pathname1 ()
 val  (optclass as optclass1) = optclass1 ()
 val  (opttoolopts as opttoolopts1) = opttoolopts1 ()
 in (
doMember (pathname,
							   pathnameleft,
							   pathnameright,
							   optclass,
							   opttoolopts)
)
end)
 in ( LrTable.NT 13, ( result, pathname1left, opttoolopts1right), 
rest671)
end
|  ( 46, ( ( _, ( MlyValue.guarded_members guarded_members1, _, 
guarded_members1right)) :: ( _, ( MlyValue.exp exp1, expleft, expright
)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.member (fn _ => let val  (exp as exp1) = exp1 ()
 val  (guarded_members as guarded_members1) = guarded_members1 ()
 in (
S.guarded_members
						 (exp, guarded_members,
						  error (expleft, expright))
)
end)
 in ( LrTable.NT 13, ( result, IF1left, guarded_members1right), 
rest671)
end
|  ( 47, ( ( _, ( MlyValue.ERROR ERROR1, (ERRORleft as ERROR1left), (
ERRORright as ERROR1right))) :: rest671)) => let val  result = 
MlyValue.member (fn _ => let val  (ERROR as ERROR1) = ERROR1 ()
 in (
S.error_member
						 (fn () =>
						  error (ERRORleft, ERRORright)
						        ERROR)
)
end)
 in ( LrTable.NT 13, ( result, ERROR1left, ERROR1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.word word1, word1left, word1right)) :: 
rest671)) => let val  result = MlyValue.class (fn _ => let val  (word
 as word1) = word1 ()
 in (S.class word)
end)
 in ( LrTable.NT 24, ( result, word1left, word1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.else_members else_members1, _, 
else_members1right)) :: ( _, ( MlyValue.members members1, members1left
, _)) :: rest671)) => let val  result = MlyValue.guarded_members (fn _
 => let val  (members as members1) = members1 ()
 val  (else_members as else_members1) = else_members1 ()
 in ((members, else_members))
end)
 in ( LrTable.NT 14, ( result, members1left, else_members1right), 
rest671)
end
|  ( 50, ( ( _, ( _, ENDIF1left, ENDIF1right)) :: rest671)) => let
 val  result = MlyValue.else_members (fn _ => (S.emptyMembers))
 in ( LrTable.NT 15, ( result, ENDIF1left, ENDIF1right), rest671)
end
|  ( 51, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.members 
members1, _, _)) :: ( _, ( _, ELSE1left, _)) :: rest671)) => let val  
result = MlyValue.else_members (fn _ => let val  (members as members1)
 = members1 ()
 in (members)
end)
 in ( LrTable.NT 15, ( result, ELSE1left, ENDIF1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.guarded_members guarded_members1, _, 
guarded_members1right)) :: ( _, ( MlyValue.exp exp1, expleft, expright
)) :: ( _, ( _, ELIF1left, _)) :: rest671)) => let val  result = 
MlyValue.else_members (fn _ => let val  (exp as exp1) = exp1 ()
 val  (guarded_members as guarded_members1) = guarded_members1 ()
 in (
S.guarded_members
						 (exp, guarded_members,
						  error (expleft, expright))
)
end)
 in ( LrTable.NT 15, ( result, ELIF1left, guarded_members1right), 
rest671)
end
|  ( 53, ( ( _, ( MlyValue.FILE_STANDARD FILE_STANDARD1, 
FILE_STANDARD1left, FILE_STANDARD1right)) :: rest671)) => let val  
result = MlyValue.word (fn _ => let val  (FILE_STANDARD as 
FILE_STANDARD1) = FILE_STANDARD1 ()
 in (S.cm_symbol FILE_STANDARD)
end)
 in ( LrTable.NT 23, ( result, FILE_STANDARD1left, FILE_STANDARD1right
), rest671)
end
|  ( 54, ( ( _, ( MlyValue.CM_ID CM_ID1, CM_ID1left, CM_ID1right)) :: 
rest671)) => let val  result = MlyValue.sym (fn _ => let val  (CM_ID
 as CM_ID1) = CM_ID1 ()
 in (S.cm_symbol CM_ID)
end)
 in ( LrTable.NT 22, ( result, CM_ID1left, CM_ID1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.boolexp boolexp1, boolexp1left, 
boolexp1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (boolexp as boolexp1) = boolexp1 ()
 in (boolexp)
end)
 in ( LrTable.NT 18, ( result, boolexp1left, boolexp1right), rest671)

end
|  ( 56, ( ( _, ( MlyValue.NUMBER NUMBER1, NUMBER1left, NUMBER1right))
 :: rest671)) => let val  result = MlyValue.aexp (fn _ => let val  (
NUMBER as NUMBER1) = NUMBER1 ()
 in (S.number NUMBER)
end)
 in ( LrTable.NT 16, ( result, NUMBER1left, NUMBER1right), rest671)

end
|  ( 57, ( ( _, ( MlyValue.sym sym1, sym1left, sym1right)) :: rest671)
) => let val  result = MlyValue.aexp (fn _ => let val  (sym as sym1) =
 sym1 ()
 in (S.variable gp sym)
end)
 in ( LrTable.NT 16, ( result, sym1left, sym1right), rest671)
end
|  ( 58, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.aexp aexp1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.aexp (fn _ => let val  (aexp as aexp1) = aexp1 ()
 in (aexp)
end)
 in ( LrTable.NT 16, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 59, ( ( _, ( MlyValue.aexp aexp2, _, aexp2right)) :: ( _, ( 
MlyValue.ADDSYM ADDSYM1, _, _)) :: ( _, ( MlyValue.aexp aexp1, 
aexp1left, _)) :: rest671)) => let val  result = MlyValue.aexp (fn _
 => let val  aexp1 = aexp1 ()
 val  (ADDSYM as ADDSYM1) = ADDSYM1 ()
 val  aexp2 = aexp2 ()
 in (S.add (aexp1, ADDSYM, aexp2))
end)
 in ( LrTable.NT 16, ( result, aexp1left, aexp2right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.aexp aexp2, _, aexp2right)) :: ( _, ( 
MlyValue.MULSYM MULSYM1, _, _)) :: ( _, ( MlyValue.aexp aexp1, 
aexp1left, _)) :: rest671)) => let val  result = MlyValue.aexp (fn _
 => let val  aexp1 = aexp1 ()
 val  (MULSYM as MULSYM1) = MULSYM1 ()
 val  aexp2 = aexp2 ()
 in (S.mul (aexp1, MULSYM, aexp2))
end)
 in ( LrTable.NT 16, ( result, aexp1left, aexp2right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.aexp aexp1, _, aexp1right)) :: ( _, ( _, 
TILDE1left, _)) :: rest671)) => let val  result = MlyValue.aexp (fn _
 => let val  (aexp as aexp1) = aexp1 ()
 in (S.negate aexp)
end)
 in ( LrTable.NT 16, ( result, TILDE1left, aexp1right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.aexp aexp1, _, aexp1right)) :: ( _, ( 
MlyValue.ADDSYM ADDSYM1, (ADDSYMleft as ADDSYM1left), ADDSYMright)) ::
 rest671)) => let val  result = MlyValue.aexp (fn _ => let val  (
ADDSYM as ADDSYM1) = ADDSYM1 ()
 val  (aexp as aexp1) = aexp1 ()
 in (
obsolete (ADDSYMleft,
							   ADDSYMright);
						 S.sign (ADDSYM, aexp)
)
end)
 in ( LrTable.NT 16, ( result, ADDSYM1left, aexp1right), rest671)
end
|  ( 63, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ml_symbol 
ml_symbol1, _, _)) :: _ :: ( _, ( _, DEFINED1left, _)) :: rest671)) =>
 let val  result = MlyValue.boolexp (fn _ => let val  (ml_symbol as 
ml_symbol1) = ml_symbol1 ()
 in (S.ml_defined ml_symbol)
end)
 in ( LrTable.NT 17, ( result, DEFINED1left, RPAREN1right), rest671)

end
|  ( 64, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.sym sym1, _,
 _)) :: _ :: ( _, ( _, DEFINED1left, _)) :: rest671)) => let val  
result = MlyValue.boolexp (fn _ => let val  (sym as sym1) = sym1 ()
 in (S.cm_defined gp sym)
end)
 in ( LrTable.NT 17, ( result, DEFINED1left, RPAREN1right), rest671)

end
|  ( 65, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.boolexp 
boolexp1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.boolexp (fn _ => let val  (boolexp as boolexp1
) = boolexp1 ()
 in (boolexp)
end)
 in ( LrTable.NT 17, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 66, ( ( _, ( MlyValue.boolexp boolexp2, _, boolexp2right)) :: _
 :: ( _, ( MlyValue.boolexp boolexp1, boolexp1left, _)) :: rest671))
 => let val  result = MlyValue.boolexp (fn _ => let val  boolexp1 = 
boolexp1 ()
 val  boolexp2 = boolexp2 ()
 in (S.conj (boolexp1, boolexp2))
end)
 in ( LrTable.NT 17, ( result, boolexp1left, boolexp2right), rest671)

end
|  ( 67, ( ( _, ( MlyValue.boolexp boolexp2, _, boolexp2right)) :: _
 :: ( _, ( MlyValue.boolexp boolexp1, boolexp1left, _)) :: rest671))
 => let val  result = MlyValue.boolexp (fn _ => let val  boolexp1 = 
boolexp1 ()
 val  boolexp2 = boolexp2 ()
 in (S.disj (boolexp1, boolexp2))
end)
 in ( LrTable.NT 17, ( result, boolexp1left, boolexp2right), rest671)

end
|  ( 68, ( ( _, ( MlyValue.boolexp boolexp2, _, boolexp2right)) :: ( _
, ( MlyValue.EQSYM EQSYM1, _, _)) :: ( _, ( MlyValue.boolexp boolexp1,
 boolexp1left, _)) :: rest671)) => let val  result = MlyValue.boolexp
 (fn _ => let val  boolexp1 = boolexp1 ()
 val  (EQSYM as EQSYM1) = EQSYM1 ()
 val  boolexp2 = boolexp2 ()
 in (S.beq (boolexp1, EQSYM,
							boolexp2))
end)
 in ( LrTable.NT 17, ( result, boolexp1left, boolexp2right), rest671)

end
|  ( 69, ( ( _, ( MlyValue.boolexp boolexp1, _, boolexp1right)) :: ( _
, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.boolexp (fn _ => let val  (boolexp as boolexp1) = boolexp1 ()
 in (S.not boolexp)
end)
 in ( LrTable.NT 17, ( result, NOT1left, boolexp1right), rest671)
end
|  ( 70, ( ( _, ( MlyValue.aexp aexp2, _, aexp2right)) :: ( _, ( 
MlyValue.INEQSYM INEQSYM1, _, _)) :: ( _, ( MlyValue.aexp aexp1, 
aexp1left, _)) :: rest671)) => let val  result = MlyValue.boolexp (fn
 _ => let val  aexp1 = aexp1 ()
 val  (INEQSYM as INEQSYM1) = INEQSYM1 ()
 val  aexp2 = aexp2 ()
 in (S.ineq (aexp1, INEQSYM,
							 aexp2))
end)
 in ( LrTable.NT 17, ( result, aexp1left, aexp2right), rest671)
end
|  ( 71, ( ( _, ( MlyValue.aexp aexp2, _, aexp2right)) :: ( _, ( 
MlyValue.EQSYM EQSYM1, _, _)) :: ( _, ( MlyValue.aexp aexp1, aexp1left
, _)) :: rest671)) => let val  result = MlyValue.boolexp (fn _ => let
 val  aexp1 = aexp1 ()
 val  (EQSYM as EQSYM1) = EQSYM1 ()
 val  aexp2 = aexp2 ()
 in (S.eq (aexp1, EQSYM, aexp2))
end)
 in ( LrTable.NT 17, ( result, aexp1left, aexp2right), rest671)
end
|  ( 72, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.boolexp (fn _ => (S.boolean true))
 in ( LrTable.NT 17, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 73, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.boolexp (fn _ => (S.boolean false))
 in ( LrTable.NT 17, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 74, ( ( _, ( MlyValue.ML_ID ML_ID1, _, ML_ID1right)) :: ( _, ( _,
 STRUCTURE1left, _)) :: rest671)) => let val  result = 
MlyValue.ml_symbol (fn _ => let val  (ML_ID as ML_ID1) = ML_ID1 ()
 in (S.ml_structure ML_ID)
end)
 in ( LrTable.NT 19, ( result, STRUCTURE1left, ML_ID1right), rest671)

end
|  ( 75, ( ( _, ( MlyValue.ML_ID ML_ID1, _, ML_ID1right)) :: ( _, ( _,
 SIGNATURE1left, _)) :: rest671)) => let val  result = 
MlyValue.ml_symbol (fn _ => let val  (ML_ID as ML_ID1) = ML_ID1 ()
 in (S.ml_signature ML_ID)
end)
 in ( LrTable.NT 19, ( result, SIGNATURE1left, ML_ID1right), rest671)

end
|  ( 76, ( ( _, ( MlyValue.ML_ID ML_ID1, _, ML_ID1right)) :: ( _, ( _,
 FUNCTOR1left, _)) :: rest671)) => let val  result = 
MlyValue.ml_symbol (fn _ => let val  (ML_ID as ML_ID1) = ML_ID1 ()
 in (S.ml_functor ML_ID)
end)
 in ( LrTable.NT 19, ( result, FUNCTOR1left, ML_ID1right), rest671)

end
|  ( 77, ( ( _, ( MlyValue.ML_ID ML_ID1, _, ML_ID1right)) :: ( _, ( _,
 FUNSIG1left, _)) :: rest671)) => let val  result = MlyValue.ml_symbol
 (fn _ => let val  (ML_ID as ML_ID1) = ML_ID1 ()
 in (S.ml_funsig ML_ID)
end)
 in ( LrTable.NT 19, ( result, FUNSIG1left, ML_ID1right), rest671)
end
|  ( 78, ( ( _, ( MlyValue.FILE_STANDARD FILE_STANDARD1, (
FILE_STANDARDleft as FILE_STANDARD1left), (FILE_STANDARDright as 
FILE_STANDARD1right))) :: rest671)) => let val  result = 
MlyValue.pathname (fn _ => let val  (FILE_STANDARD as FILE_STANDARD1)
 = FILE_STANDARD1 ()
 in (
{ name = FILE_STANDARD,
						   mkpath = fn () =>
						     S.file_standard gp
						      (FILE_STANDARD,
						       context, error
						       (FILE_STANDARDleft,
							FILE_STANDARDright)) }
)
end)
 in ( LrTable.NT 21, ( result, FILE_STANDARD1left, FILE_STANDARD1right
), rest671)
end
|  ( 79, ( ( _, ( MlyValue.FILE_NATIVE FILE_NATIVE1, (FILE_NATIVEleft
 as FILE_NATIVE1left), (FILE_NATIVEright as FILE_NATIVE1right))) :: 
rest671)) => let val  result = MlyValue.pathname (fn _ => let val  (
FILE_NATIVE as FILE_NATIVE1) = FILE_NATIVE1 ()
 in (
{ name = FILE_NATIVE,
						   mkpath = fn () =>
						     S.file_native
						       (FILE_NATIVE,
							context, error
							(FILE_NATIVEleft,
							 FILE_NATIVEright)) }
)
end)
 in ( LrTable.NT 21, ( result, FILE_NATIVE1left, FILE_NATIVE1right), 
rest671)
end
|  ( 80, ( ( _, ( MlyValue.pathname pathname1, pathname1left, 
pathname1right)) :: rest671)) => let val  result = MlyValue.srcfile
 (fn _ => let val  (pathname as pathname1) = pathname1 ()
 in (fn () => SrcPath.file
							 (#mkpath pathname ()))
end)
 in ( LrTable.NT 29, ( result, pathname1left, pathname1right), rest671
)
end
|  ( 81, ( ( _, ( MlyValue.srcfile srcfile1, srcfile1left, 
srcfile1right)) :: rest671)) => let val  result = MlyValue.srcfiles
 (fn _ => let val  (srcfile as srcfile1) = srcfile1 ()
 in (SOME o srcfile)
end)
 in ( LrTable.NT 30, ( result, srcfile1left, srcfile1right), rest671)

end
|  ( 82, ( ( _, ( _, DASH1left, DASH1right)) :: rest671)) => let val  
result = MlyValue.srcfiles (fn _ => (fn () => NONE))
 in ( LrTable.NT 30, ( result, DASH1left, DASH1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.description x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : CM_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun FILE_STANDARD (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.FILE_STANDARD (fn () => i),p1,p2))
fun FILE_NATIVE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.FILE_NATIVE (fn () => i),p1,p2))
fun CM_ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.CM_ID (fn () => i),p1,p2))
fun ML_ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.ML_ID (fn () => i),p1,p2))
fun NUMBER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.NUMBER (fn () => i),p1,p2))
fun GROUP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LIBRARY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun IS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun ELIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun ERROR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.ERROR (fn () => i),p1,p2))
fun STRUCTURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun SIGNATURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNSIG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun DEFINED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun ADDSYM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.ADDSYM (fn () => i),p1,p2))
fun MULSYM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.MULSYM (fn () => i),p1,p2))
fun EQSYM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.EQSYM (fn () => i),p1,p2))
fun INEQSYM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.INEQSYM (fn () => i),p1,p2))
fun TILDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ANDALSO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ORELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun STAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun DASH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun SOURCE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
end
end

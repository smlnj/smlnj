structure PP :> PP =
struct

   val TABSPACE = 3

   datatype tok = STRING | NUM | SYM | TOK | SPACE | NEWLINE
   datatype pps = PP of { buf   : string list ref,
                          tabs  : indent list ref,
                          modes : mode list ref,
                          col   : col ref,
                          tok   : tok ref,
                          width : col ref
                        }
   withtype indent = int
   and      col    = int
   and      mode   = string
   and      pp     = pps -> unit

   infix ++

   fun (f ++ g) S = (f S; g S)

   fun nop S = ()
 
   fun emit(PP{buf, col, tok, ...},s,t) =
       (buf := s :: !buf; col := !col + size s; tok := t)

   fun spaceIf p (pps as PP{tok, ...}) = 
       if p (!tok) then emit(pps," ",SPACE) else ()

   val sp        = spaceIf(fn (SPACE | NEWLINE) => false | _ => true)
   val space     = spaceIf(fn (SPACE | NEWLINE | SYM) => false | _ => true)
   fun $ s pps   = (space pps; emit(pps, s, TOK))
   fun $$ s pps  = emit(pps, s, SYM)
   val bool      = $ o Bool.toString
   fun string s pps = emit(pps,"\""^String.toString s^"\"",STRING)
   fun char c pps   = emit(pps,"#\""^Char.toString c^"\"",STRING)
   fun num n pps = (space pps; emit(pps,n,NUM))
   val int       = num o Int.toString 
   val int32     = num o Int32.toString 
   val real      = num o Real.toString 
   val intinf    = num o IntInf.toString 
   val word      = num o (fn w => "0wx"^Word.toString w) 
   val word32    = num o (fn w => "0wx"^Word32.toString w)
   fun tab' offset (pps as PP{tabs, col, ...}) =
       let val at = (case !tabs of i::_ => i |  _ => 0) + offset
           val n = at - !col
       in if n <= 0 then () else emit(pps,StringCvt.padLeft #" " n "",SPACE)
       end
   val tab = tab' 0
   fun indent (PP{tabs, ...}) =
        case !tabs of
           [] => tabs := [TABSPACE]
        | t::_ => tabs := (t+TABSPACE) :: !tabs
   fun settab (PP{tabs, col, ...}) = tabs := !col :: !tabs
   fun unindent (PP{tabs as ref(_::t), ...}) = tabs := t
     | unindent _ = raise Fail "unindent"
   fun setmode m (PP{modes, ...}) = modes := m :: !modes
   fun unsetmode (PP{modes as ref(_::m), ...}) = modes := m
     | unsetmode _ = raise Fail "unsetmode"
   fun select f (pps as PP{modes=ref(m::_), ...}) = f m pps
     | select _ _ = raise Fail "select" 
   fun nl (PP{buf, col, tok, ...}) = 
         (buf := "\n" :: !buf; col := 0; tok := NEWLINE)
   fun nl' (offset,indent) (pps as PP{col, width, ...}) =
       if !col >= !width - offset 
       then (nl pps; tab' indent pps)
       else ()
   fun textWidth w (PP{width, ...}) = width := w

   fun seq (l,sep,r) pps = 
   let fun f [] = nop
         | f [a] = a
         | f(a::b) = a ++ sep ++ f b 
   in  l ++ f pps ++ r end
   fun concat pps = foldr op++ nop pps
   fun block pp = indent ++ pp ++ unindent
   fun line pp  = tab ++ pp ++ nl
   fun paren pp = $$ "(" ++ pp ++ $$ ")"
   fun group(l,r) pp = settab ++ $$ l ++ settab ++ pp ++ 
                       unindent ++ tab ++ $$ r ++ unindent
   fun text pp = 
   let val buf = ref []
       val pps = PP{buf=buf, tabs=ref [], modes=ref ["pretty"], 
                    col=ref 0, tok=ref NEWLINE, width=ref 80}
   in  pp pps;
       String.concat(rev(! buf))
   end

   val !  = $
   val !! = $$

end

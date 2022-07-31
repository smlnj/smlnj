(*
 *  A really stupid but (hopefully) working precedence parser 
 *
 *  --Allen Leung (leunga@cs.nyu.edu)
 *) 

signature PRECEDENCE_PARSER =
sig

   type precedence_stack

   datatype fixity = INFIX of int 
                   | INFIXR of int 
                   | NONFIX 
   datatype 'a token  = ID  of string
                      | EXP of 'a

   exception PrecedenceError

   val empty : precedence_stack 
   val declare : precedence_stack * string * fixity -> precedence_stack
   val parse   : { stack         : precedence_stack,
                   app           : 'a * 'a -> 'a,
                   tuple         : 'a list -> 'a,
                   id            : string -> 'a,
                   error         : string -> unit,
                   toString      : 'a -> string,
                   kind          : string
                 } -> 'a token list -> 'a
end

structure PrecedenceParser : PRECEDENCE_PARSER =
struct


   datatype fixity = INFIX of int 
                   | INFIXR of int 
                   | NONFIX 
   datatype 'a token  = ID of string
                      | EXP of 'a

   type precedence_stack = (string * fixity) list

   val empty = []
   fun declare(stack,id,fixity) = (id,fixity)::stack

   exception PrecedenceError

   fun parse {stack,tuple,app,id,toString,error,kind} tokens =
   let fun fixity x =
       let fun f [] = NONFIX
             | f ((y,fix)::S) = if x = y then fix else f S
       in  f stack end

       val toks = map (fn ID x => (id x,fixity x)
                        | EXP e => (e,NONFIX)) tokens

       fun err(msg) =
             (error(msg^" in "^kind^": "^
                   List.foldr (fn ((x,_),"") => toString x
                                | ((x,_),s) => toString x^" "^s) ""
                               toks);
              raise PrecedenceError)
       fun err'(msg, x) = err(msg^" "^toString x)

       (* 
        * Parse with precedence. 
        *)
       fun scan(p, tokens) =
           case tokens of
             (f,NONFIX)::(x,NONFIX)::rest =>
                 scan(p, (app(f,x), NONFIX)::rest) (* application *)
           | [(x,NONFIX)] => (x, [])
           | (x,INFIX _)::_ => err'("dangling infix symbol", x)
           | (x,INFIXR _)::_ => err'("dangling infixr symbol", x)
           | (left,NONFIX)::(rest as (f,INFIX q)::rest') =>
                if p >= q then 
                  (left, rest)
                else
                   let val (right, rest) = scan(q,rest')
                   in  scan(p,(app(f,tuple[left,right]),NONFIX)::rest)
                   end
           | (left,NONFIX)::(rest as (f,INFIXR q)::rest') =>
                if p > q then
                   (left, rest)
                else
                   let val (right, rest) = scan(q,rest')
                   in  scan(p,(app(f,tuple[left,right]),NONFIX)::rest)
                   end
           | _ => err("parse error")

       fun scanAll [(x,INFIX _)] = x
         | scanAll [(x,INFIXR _)] = x
         | scanAll tokens = #1(scan(~1,tokens))
             
   in  scanAll toks end

end

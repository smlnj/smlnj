(* Copyright 1996 by AT&T Bell Laboratories *)
(* precedence.sml *)

signature PRECEDENCE =
sig
  val parse: {apply: 'a * 'a -> 'a, pair: 'a * 'a -> 'a} -> 
                'a Ast.fixitem list * StaticEnv.staticEnv * 
                (Ast.region->ErrorMsg.complainer) -> 'a

end (* signature PRECEDENCE *)


structure Precedence : PRECEDENCE = 
struct    

local structure EM = ErrorMsg 
      structure F = Fixity

in 

datatype 'a precStack 
  = INf of Symbol.symbol * int * 'a * 'a precStack
  | NONf of 'a * 'a precStack
  | NILf

fun parse {apply,pair} =
  let fun ensureNONf((e,F.NONfix,_,err),p) = NONf(e,p)
        | ensureNONf((e,F.INfix _,SOME sym,err),p) = 
	   (err EM.COMPLAIN
	      ("expression or pattern begins with infix identifier \"" 
	       ^ Symbol.name sym ^ "\"") EM.nullErrorBody;
	       NONf(e,p))
	| ensureNONf _ = EM.impossible "precedence:ensureNONf"

      fun start token = ensureNONf(token,NILf)

      (* parse an expression *)
      fun parse(NONf(e,r), (e',F.NONfix,_,err)) = NONf(apply(e,e'),r)
        | parse(p as INf _, token) = ensureNONf(token,p)
        | parse(p as NONf(e1,INf(_,bp,e2,NONf(e3,r))), 
                (e4, f as F.INfix(lbp,rbp),SOME sym,err))=
	    if lbp > bp then INf(sym,rbp,e4,p)
            else (if lbp = bp
		  then err EM.WARN "mixed left- and right-associative \
				      \operators of same precedence"
			         EM.nullErrorBody
		  else ();
	          parse(NONf(apply(e2,pair (e3,e1)),r),(e4,f,SOME sym,err)))

        | parse(p as NONf _, (e',F.INfix(lbp,rbp),SOME sym,_)) = 
            INf(sym,rbp,e',p)
        | parse _ = EM.impossible "Precedence.parse"
     
      (* clean up the stack *)
      fun finish (NONf(e1,INf(_,_,e2,NONf(e3,r))),err) = 
		     finish(NONf(apply(e2,pair (e3,e1)),r),err)
        | finish (NONf(e1,NILf),_) = e1
        | finish (INf(sym,_,e1,NONf(e2,p)),err) = 
		     (err EM.COMPLAIN 
		      ("expression or pattern ends with infix identifier \"" 
		       ^ Symbol.name sym ^ "\"") EM.nullErrorBody;
		      finish(NONf(apply(e2,e1),p),err))
        | finish (NILf,err) = EM.impossible "Corelang.finish NILf"
        | finish _ = EM.impossible "Corelang.finish"

   in fn (items as item1 :: items',env,error) =>
        let fun getfix{item,region,fixity} =
	      (item,  case fixity of NONE => F.NONfix 
                                   | SOME sym => Lookup.lookFix(env,sym),
               fixity, error region)

            fun endloc[{region=(_,x),item,fixity}] = error(x,x)
              | endloc(_::a) = endloc a
	      | endloc _ = EM.impossible "precedence:endloc"
	      
            fun loop(state, a::rest) = loop(parse(state,getfix a),rest)
              | loop(state,nil) = finish(state, endloc items)

         in loop(start(getfix item1), items')
        end
       | _ => EM.impossible "precedence:parse"
  end

end (* local *)
end (* structure Precedence *)

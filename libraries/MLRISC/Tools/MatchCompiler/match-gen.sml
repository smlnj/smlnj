(*
 * Interface with the match compiler to generate ML code.
 *)
functor MatchGen
    (structure AstPP       : MDL_AST_PRETTY_PRINTER
     structure AstUtil     : MDL_AST_UTIL
     structure AstRewriter : MDL_AST_REWRITER
       sharing AstPP.Ast = AstUtil.Ast = AstRewriter.Ast
    ) : MATCH_GEN =
struct
   structure Ast = AstPP.Ast
   structure A   = Ast
   structure R   = AstRewriter

   val NO = R.noRewrite
   val rw = R.rewrite
   val ++ = PP.++
  
   infix ++

   val i2s = Int.toString

   structure Guard =
     struct
        type guard = int * A.exp
        fun toString(_,e) = PP.text(AstPP.exp e)
        fun compare((i,_),(j,_)) = Int.compare(i,j) 
        val counter = ref 0
        fun guard e = (!counter,e) before counter := !counter + 1
        fun logicalAnd((_,x),(_,y)) = guard(AstUtil.ANDALSO(x,y))
     end

   structure Exp =
     struct
        type exp = A.exp
        val toString = PP.text o AstPP.exp
     end

   structure Literal =
     struct
        type literal = A.literal
        val toString = PP.text o AstPP.literal
        val compare  = AstUtil.compareLiteral
        val bools = SOME{known=[A.BOOLlit false, A.BOOLlit true],others=false}
        fun variants(A.BOOLlit _) = bools
          | variants _ = NONE 
        structure Map = RedBlackMapFn(type ord_key = literal
                                      val compare = compare)
     end
   structure LitMap = Literal.Map

   datatype conrep = CONREP of A.id list * A.consbind * A.datatypebind 
                   | EXN of A.id list * A.id * A.ty option 

   structure Con =
     struct
        type con = conrep 

        fun toString(CONREP(path,A.CONSbind{id, ...},_)) = 
             PP.text(AstPP.ident(A.IDENT(path,id)))
          | toString(EXN(path,id,ty)) =
             PP.text(AstPP.ident(A.IDENT(path,id)))

        fun compare(CONREP(_,A.CONSbind{id=x,...},_),
                    CONREP(_,A.CONSbind{id=y,...},_)) = String.compare(x,y)
          | compare(EXN(_,x,_),EXN(_,y,_)) = String.compare(x,y)
          | compare(CONREP _, EXN _) = LESS
          | compare(EXN _, CONREP _) = GREATER

        fun variants(CONREP(path,_,dt as A.DATATYPEbind{cbs, ...})) =
             {known=map (fn c => CONREP(path,c,dt)) cbs, others=false}
          | variants(EXN _) = {known=[], others=true}

        fun arity(CONREP(_,A.CONSbind{ty=NONE, ...},_)) = 0
          | arity(CONREP(_,A.CONSbind{ty=SOME ty, ...},_)) = 1
          | arity(EXN(_,_,NONE)) = 0
          | arity(EXN(_,_,SOME _)) = 1
     end

   structure Var =
     struct
        type var = A.id
        val compare = String.compare 
        fun toString x = x
        structure Map = RedBlackMapFn(type ord_key = var 
                                      val compare = compare)
        structure Set = RedBlackSetFn(type ord_key = var 
                                       val compare = compare)
     end

   structure Action =
     struct
        type action = A.exp
        val toString = PP.text o AstPP.exp
        fun freeVars e =
        let val fvs = ref Var.Set.empty
            fun exp _ (e as A.IDexp(A.IDENT([],x))) = 
                 (fvs := Var.Set.add(!fvs,x); e)
              | exp _ e = e
        in  #exp(R.rewrite{pat=NO,exp=exp,decl=NO,sexp=NO,ty=NO}) e;
            Var.Set.listItems(!fvs)
        end 
     end

   structure MC  =
     MatchCompiler(structure Guard   = Guard
                   structure Exp     = Exp
                   structure Literal = Literal
                   structure Con     = Con
                   structure Var     = Var
                   structure Action  = Action
                  )

   fun ID x = A.IDexp(A.IDENT([],x))
   fun STATE x = "state_"^(i2s x)

   exception MatchCompiler = MC.MatchCompiler

   structure Env =
   struct
      datatype env = ENV of {cons:conrep Var.Map.map, sigs:env Var.Map.map}
      fun insertCons(ENV{cons,sigs}, id, conrep) =
          ENV{cons=Var.Map.insert(cons, id, conrep), sigs=sigs}
      fun insertSig(ENV{cons,sigs}, id, env) =
          ENV{cons=cons,sigs=Var.Map.insert(sigs, id, env)}
      fun lookupSig(ENV{sigs,...}, id) = Var.Map.find(sigs,id)
      fun lookupCons(ENV{cons, ...}, id) = Var.Map.find(cons,id)
      val empty = ENV{cons=Var.Map.empty,sigs=Var.Map.empty}
   end
   type compiled_type_info = Env.env 

   (* enter all datatypes definitions into a list *)
   fun compileTypes ds =
   let fun dbind(t as A.DATATYPEbind{cbs, ...}, env) = 
             List.foldr (fn (c as A.CONSbind{id,...},env) =>
                    Env.insertCons(env, id, CONREP([],c,t)))
                       env cbs
         | dbind(_, env) = env
       and dbinds(dbs,env) = List.foldr dbind env dbs
       and ebind(A.EXCEPTIONbind(id,ty), env) =
             Env.insertCons(env, id, EXN([], id, ty))
         | ebind(_, env) = env
       and ebinds(ebs,env) = List.foldr ebind env ebs
       and decl(A.DATATYPEdecl(dbs, _), env) = dbinds(dbs, env)
         | decl(A.EXCEPTIONdecl ebs, env) = ebinds(ebs, env)
         | decl(A.MARKdecl(_,d), env) = decl(d, env)
         | decl(A.SIGNATUREdecl(id,A.DECLsig ds),env) = decls(ds, env)
         | decl(A.STRUCTUREdecl(id,_,_,A.DECLsexp ds),env) = nested(id,ds,env)
         | decl(A.SEQdecl ds, env) = decls(ds, env)
         | decl(_,env) = env
       and decls(ds,env) = List.foldr decl env ds 
       and nested(id,ds,env) = 
           let val env' = decls(ds,Env.empty) 
           in  Env.insertSig(env,id,env')
           end
   in  decls(ds,Env.empty)
   end

   fun prClause(p, g) = 
       PP.text(AstPP.pat p ++ PP.sp ++ 
               (case g of NONE => PP.! "=> ..."
                        | SOME e => PP.! "where ... => ..."))

   fun compile env clauses =
   let (* rename all rules *)

       fun hasCon x = isSome(Env.lookupCons(env, x))

       fun lookup(env,path,[],x) = 
           (case Env.lookupCons(env, x) of
              SOME(CONREP(_,c,t)) => CONREP(path,c,t)
            | SOME(EXN(_,id,t)) => EXN(path,id,t)
            | NONE => raise MatchCompiler("undefined constructor "^x)
           )
         | lookup(env,path,p::ps,x) = 
           (case Env.lookupSig(env, p) of
              SOME env => lookup(env,path,ps,x)
            | NONE => raise MatchCompiler("undefined structure "^p^" in "^
                                      PP.text(AstPP.ident(A.IDENT(path,x))))
           )
       fun lookupCon (A.IDENT(p,x)) = lookup(env,p,p,x)

       (* Rewrite list patterns *)
       fun transListPat p = 
       let fun Cons(x,y) = A.CONSpat(A.IDENT([],"::"), SOME(A.TUPLEpat[x,y]))
           val Nil = A.CONSpat(A.IDENT([],"nil"),NONE)

           fun listify([], SOME p) = p
             | listify([], NONE) = Nil
             | listify(p::ps, t) = Cons(p, listify(ps, t))
           fun pat _ (A.LISTpat(ps, t)) = listify(ps, t)
             | pat _ p = p
       in  #pat(R.rewrite{pat=pat,exp=NO,decl=NO,sexp=NO,ty=NO}) p
       end 

       val rule_no = ref 0

       fun renameRule(c as A.CLAUSE([pat],guard,e)) = 
       let val (e,cont) = case e of
                              A.CONTexp(e,x) => (e,SOME x)
                            | _ => (e, NONE)
       in  MC.rename
               (fn {idPat, asPat, consPat, wildPat, 
                    tuplePat, recordPat, litPat, 
                    orPat, andPat, notPat, wherePat, nestedPat, ...} =>
                   fn A.IDpat id => 
                       if hasCon id then consPat(lookupCon(A.IDENT([],id)),[])
                       else idPat id
                    | A.ASpat(id,p) => asPat(id,p)
                    | A.WILDpat         => wildPat()
                    | A.CONSpat(c,NONE) => consPat(lookupCon c,[])
                    | A.CONSpat(c,SOME(p)) => consPat(lookupCon c,[p])
                    | A.TUPLEpat ps => tuplePat ps
                    | A.RECORDpat(lps,_) => recordPat lps
                    | A.LITpat lit => litPat lit
                    | A.ORpat ps => orPat ps
                    | A.ANDpat ps => andPat ps
                    | A.NOTpat p => notPat p
                    | A.WHEREpat(p,e) => wherePat(p,Guard.guard e)
                    | A.NESTEDpat(p,e,p') => nestedPat(p,Guard.guard e,p')
                    | p => raise MC.MatchCompiler("illegal pattern "^
                                       PP.text(AstPP.pat p))
               ) {number= !rule_no, 
                  pats=[transListPat pat],
                  guard=Option.map Guard.guard guard,
                  cont=cont,
                  action=e
                 }
               before rule_no := !rule_no + 1
       end handle MC.MatchCompiler msg =>
              raise MC.MatchCompiler(msg^" in "^ prClause(pat,guard))

       val rules = map renameRule clauses
       
       (* compile the rules into a dfa *)
       val dfa = MC.compile{compiled_rules=rules, compress=true}
   in  dfa
   end

   (* Report errors *)
   fun report {warning, error, log, dfa, rules} =  
   let val red = MC.redundant dfa
       val ex  = MC.exhaustive dfa
       val bad = IntListSet.numItems red > 0
       val error = if bad then error else warning
       val message = if ex then 
                        if bad then "redundant matches" 
                        else ""
                     else 
                        if bad then "non-exhaustive and redundant matches" 
                        else "non-exhaustive matches"
       fun dumpRules(i, []) = ()
         | dumpRules(i, r::rules) =
           let val tab = if IntListSet.member(red,i) then "---> " else "     "
               val A.CLAUSE([p], g, _) = r 
               val text = prClause(p, g)
           in  log(tab^text);
               dumpRules(i+1, rules)
           end
   in  if not ex orelse bad then 
          (error message;
           dumpRules(0, rules)
          ) 
       else ()
   end

   exception GenReal and GenIntInf 

   local
      val intInfCompare = A.IDexp(A.IDENT(["IntInf"],"compare"))
      val realEq        = A.IDexp(A.IDENT(["Real"],"=="))
      val eq            = A.IDexp(A.IDENT([],"="))
      val equal         = A.IDexp(A.IDENT([],"EQUAL"))
   in

      fun makeIntInfEq(x,y) = A.APPexp(eq,
                                 A.TUPLEexp[A.APPexp(intInfCompare,
                                               A.TUPLEexp[x,y]),
                                            equal])
      fun makeRealEq(x,y)   = A.APPexp(realEq,A.TUPLEexp[x,y])
   end
  
   val nameCounter = ref 0
   fun newName() = !nameCounter before nameCounter := !nameCounter + 1
   fun init() = nameCounter := 0

   (* Generate ML code *)
   fun codeGen {root, dfa, fail=genFail, literals} =
   let (* make unique name for path variables *)
       val nameTbl = ref MC.Path.Map.empty

       fun genLit (l as A.INTINFlit _) = 
           (case Literal.Map.find(!literals, l) of 
              SOME v => AstUtil.ID v  
            | NONE => let val v = "lit_"^i2s(newName())
                      in  literals := Literal.Map.insert(!literals, l, v);
                          AstUtil.ID v
                      end 
           )
         | genLit l = A.LITexp l

       fun getName path =
           case MC.Path.Map.find(!nameTbl, path) of
             SOME name => name
           | NONE =>
             let val v = "v_"^i2s(newName())
             in  nameTbl := MC.Path.Map.insert(!nameTbl, path, v);
                 v
             end

       (* Now generate the code; we just have to hook things up with the MC *)
       fun genVar path = getName path
       fun genPath path = ID(genVar path)
       fun genBind [] = []
         | genBind bindings =
           [A.VALdecl(map (fn (v,e) => A.VALbind(A.IDpat v,e)) bindings )]
       fun genOk(e) = e
       fun pathToPat(path) = A.IDpat(getName path)
       fun arg NONE = A.WILDpat
         | arg (SOME p) = A.IDpat(getName p)
       fun fromRep(CONREP(path,A.CONSbind{id, ...},_)) = A.IDENT(path,id)
         | fromRep(EXN(path,id,_)) = A.IDENT(path,id)
       fun genConPat(MC.CON con, []) = A.CONSpat(fromRep con,NONE)
         | genConPat(MC.CON con, paths) = 
                A.CONSpat(fromRep con, SOME(A.TUPLEpat(map arg paths)))
         | genConPat(MC.LIT(A.REALlit _), _) = raise GenReal
         | genConPat(MC.LIT(A.INTINFlit _), _) = raise GenIntInf
         | genConPat(MC.LIT lit, _) = A.LITpat lit
       fun genCase(v, cases, default) = 
           A.CASEexp(ID v,
              map (fn (con, paths, e) =>
                    A.CLAUSE([genConPat(con, paths)],NONE,e)) cases @
                  (case default of
                     NONE => []
                  |  SOME default => [A.CLAUSE([A.WILDpat], NONE, default)]
                  )
              )   
           handle GenReal => genLitCmp(makeRealEq,v,cases, default)
             | GenIntInf => genLitCmp(makeIntInfEq, v,cases,default)
       and genLitCmp(eq, v, cases, SOME default) = 
           let val x = ID v 
               fun equal lit = eq(x, genLit lit)
           in  List.foldr(fn ((MC.LIT lit, _, e),rest) =>
                  A.IFexp(equal lit,e,rest)) default cases    
           end
       fun genIf((_,e), y, n) = A.IFexp(e, y, n)
       fun genGoto(f, args) = A.APPexp(ID(STATE f), A.TUPLEexp(map ID args)) 
       fun genFun(f, args, body) = 
           A.FUNdecl[A.FUNbind(STATE f,
                       [A.CLAUSE([A.TUPLEpat(map A.IDpat args)],NONE,body)])
                     ]
       fun genLet([], e) = e
         | genLet(d, e) = A.LETexp(d,[e])
       fun genVal(v, e) = A.VALdecl[A.VALbind(A.IDpat v, e)]
       fun genProj(path, bindings) =
       let val pat = case bindings of
                       [] => A.WILDpat
                     | (p, MC.INT _)::ps  => 
                       A.TUPLEpat(map (fn (p,_) => arg p) bindings)
                     | (p, MC.LABEL _)::ps =>
                       A.RECORDpat(map (fn (p,MC.LABEL l) => 
                                        (l, arg p)) bindings, true)
       in  A.VALdecl[A.VALbind(pat,ID(getName path))]
       end

       fun genCont(k, f, vars) = 
           A.FUNdecl[A.FUNbind(k,[A.CLAUSE([A.TUPLEpat []], NONE,
                        A.APPexp(ID(STATE f),
                                 A.TUPLEexp(map ID vars)))])]
                           
   in  MC.codeGen 
         {genFail = genFail,
          genOk   = genOk,
          genPath = genPath,
          genBind = genBind,
          genCase = genCase, 
          genIf   = genIf,
          genGoto = genGoto,
          genCont = genCont,
          genFun  = genFun,
          genLet  = genLet,
          genVar  = genVar,
          genVal  = genVal,
          genProj = genProj
         } (root, dfa)
   end

   fun complexPat p =
   let val complex = ref false
       fun pat _ (p as A.WHEREpat _) = (complex := true; p)
         | pat _ (p as A.NESTEDpat _) = (complex := true; p)
         | pat _ (p as A.ANDpat _) = (complex := true; p)
         | pat _ (p as A.NOTpat _) = (complex := true; p)
         | pat _ (p as A.ORpat _) = (complex := true; p)
         | pat _ (p as A.LITpat(A.REALlit _)) = (complex := true; p)
         | pat _ (p as A.LITpat(A.INTINFlit _)) = (complex := true; p)
         | pat _ p = p
       val _ = #pat(rw{exp=NO,ty=NO,pat=pat,decl=NO,sexp=NO}) p
   in  !complex end

   (* Are clauses conditional *)
   val isComplex =
       List.exists (fn A.CLAUSE(p,g,_) => isSome g orelse
                                          List.exists complexPat p)
end

(*
 * Machine description environment.
 *)
functor MDLEnv(TypeUtils : MDL_TYPE_UTILS) : MDL_ENV =
struct

   structure Ast   = TypeUtils.Ast
   structure AstPP = TypeUtils.AstPP
   structure Error = MDLError

   datatype env = 
      ENV of
        { TE : Ast.ty Env.env,                (* type environment *)
          VE : (Ast.exp * Ast.ty) Env.env,    (* value environment *)
          EE : (Ast.decl list * env) Env.env, (* structure environment *)  
          DE : Ast.decl list,                 (* declarations environment *)
          SE : Ast.decl list                  (* signature environment *)
        }

   open Ast

   infix ++
   infix $$
   infix ==>

   val op $$ = Env.union
   val op ==> = Env.bind
   val O = Env.empty

   val empty = ENV{TE=O, VE=O, EE=O, DE=[], SE=[]}
   fun (ENV{TE=TE1, VE=VE1, EE=EE1, DE=DE1, SE=SE1}) ++ 
       (ENV{TE=TE2, VE=VE2, EE=EE2, DE=DE2, SE=SE2}) =
        ENV{TE=TE1 $$ TE2, VE=VE1 $$ VE2, EE=EE1 $$ EE2, DE=DE1@DE2, SE=SE1@SE2}

   fun mkDECL d     = ENV{TE=O, VE=O, EE=O, DE=[d], SE=[]}
   fun mkSIG  d     = ENV{TE=O, VE=O, EE=O, DE=[], SE=[d]}
   fun mkVALs vbs   = ENV{TE=O, VE=vbs, EE=O, DE=[], SE=[]}

   fun VALbind(id,e,t)= mkVALs(id ==> (e,t))
   fun TYPEbind(id,t) = ENV{TE=id ==> t, VE=O, EE=O, DE=[], SE=[]}
   fun STRbind(id,args,E) = ENV{TE=O, VE=O, EE= id ==> (args,E), DE=[], SE=[]}

   (* Create a new free variable; instantiation and generalization *)
   fun var(ENV _) = TypeUtils.newVar 0 (* XXX *)
   fun inst(ENV _) t = TypeUtils.inst 0 t  (* XXX *)
   fun gen(ENV _) t  = TypeUtils.gen 0 t   (* XXX *)
   fun lambda(ENV _) t  = TypeUtils.lambda 0 t   (* XXX *)

   (* Extract components *)
   fun DE(ENV{DE, ...}) = DE 
   fun SE(ENV{SE, ...}) = SE   
   fun datatypeDefinitions(ENV{DE,...}) =  
   let fun collect(DATATYPEdecl(dbs, _), dbs') = dbs @ dbs'
         | collect(MARKdecl(_, d), dbs') = collect(d, dbs')
         | collect(_, dbs') = dbs'
   in  List.foldr collect [] DE
   end

   (* Lookup components from the environment *) 
   fun lookupTy (E as ENV{TE,EE,...}) (IDENT([],id)) =
       (Env.look TE id
        handle _ => (Error.error("undefined type '"^id^"'"); var E))
     | lookupTy (ENV{EE,...}) (IDENT(s::ss,id)) =
        lookupTy (lookupStr' EE (IDENT(ss,s))) (IDENT([],id))

   and lookupVal' err (E as ENV{VE,EE,...}) (IDENT([],id)) =
        (inst E (Env.look VE id)
        handle _ => (err id; (LITexp(INTlit 0), var E)))
     | lookupVal' err (ENV{EE,...}) (IDENT(s::ss,id)) =
        lookupVal' err (lookupStr' EE (IDENT(ss,s))) (IDENT([],id))

   and lookupVal E x = lookupVal' 
        (fn x => Error.error("undefined value '"^x^"'")) E x

   and lookupStr (ENV{EE,...}) id = lookupStr' EE id

   and lookupStr' EE (IDENT([],id)) =
       (#2(Env.look EE id)
        handle _ => 
          (Error.error("undefined structure '"^id^"'"); empty))
     | lookupStr' EE (IDENT(s::ss,id)) =
        lookupStr (lookupStr' EE (IDENT(ss,s))) (IDENT([],id))

   (* Interators *)
   fun foldVal f x (ENV{VE, ...}) = 
       Env.fold (fn (id,(e,ty),l) => f(id,e,ty,l)) x VE

   (* 
    * Elaborate a declaration in an environment.
    * Return an environment.
    *)
   fun elab E d = 
   let (* elaborate a declaration *)
       val mkDECL = fn(l,d) => mkDECL(MARKdecl(l,d))
       val mkSIG = fn(l,d) => mkSIG(MARKdecl(l,d))
       fun D E l (d as DATATYPEdecl(dbs,tys)) = mkDECL(l,d) ++ mkSIG(l,d)
         | D E l (d as INSTRUCTIONdecl cbs) = mkDECL(l,d)
         | D E l (d as FUNdecl _) = mkDECL(l,d)
         | D E l (d as RTLdecl _) = mkDECL(l,d)
         | D E l (d as RTLSIGdecl _) = mkDECL(l,d)
         | D E l (d as VALdecl _) = mkDECL(l,d)
         | D E l (d as TYPESIGdecl _) = mkSIG(l,d)
         | D E l (d as VALSIGdecl _) = mkSIG(l,d)
         | D E l (d as LOCALdecl(d1,d2)) = mkDECL(l,d)
           (* let val E' = Ds E l d1 in Ds (E ++ E') l d2 end *)
         | D E l (d as SEQdecl ds) = Ds E l ds
         | D E l (d as OPENdecl ids) = mkDECL(l,d) ++ openStrs E ids
         | D E l (d as STRUCTUREdecl(id,args,_,DECLsexp ds)) = 
           let val E' = Ds E l ds
           in  STRbind(id,args,E') ++ mkDECL(l,d) end
         | D E l (STRUCTURESIGdecl _) = empty
         | D E l (d as INFIXdecl _) = mkDECL(l,d)
         | D E l (d as INFIXRdecl _) = mkDECL(l,d)
         | D E l (d as NONFIXdecl _) = mkDECL(l,d)
         | D E _ (MARKdecl(l,d)) = (Error.setLoc l; D E l d)
         | D E l d = Error.fail("illegal declaration: "^
                        (PP.text(AstPP.decl d)))

       and Ds E l [] = empty
         | Ds E l (d::ds) = let val E' = D E l d
                          in  E' ++ Ds (E ++ E') l ds end

           (* open up a list of structures *)
       and openStrs E ids = 
           List.foldr (fn (id,E') => lookupStr E id ++ E') empty ids
        
   in  D E SourceMapping.dummyLoc d
   end

   (*
    * Treat a type expression as a pattern and
    * compute its set of bindings.  Duplicated names are assigned 
    * unique suffixes.
    *)
   fun bindingsInType ty = 
   let val names = Env.envir "names"
       fun count id = let val (n,total) = Env.lookup names id
                      in  total := !total + 1 end
                      handle _ => Env.update names (id,(ref 0,ref 1))
       fun getName id = let val (n,total) = Env.lookup names id
                        in  if !total = 1 then id else
                            (n := !n + 1; id^Int.toString(!n))
                        end
       fun f(IDty(IDENT(_,id))) = count id
         | f(APPty(_,[ty])) = f ty
         | f(CELLty id) = count id
         | f(TUPLEty tys) = app f tys
         | f(RECORDty ltys) = app (fn (id,_) => count id) ltys
         | f _ = ()
   in  f ty; (!names,getName) end

   (* Lookup from nested environment *)
   fun declOf(ENV{EE, ...}) id =
       let val (_, ENV{DE,...}) = Env.look EE id 
       in  SEQdecl DE
       end handle _ => $ []

   fun fctArgOf(ENV{EE, ...}) id = 
       let val (args, _) = Env.look EE id
       in  SEQdecl args
       end handle _ => $ []

   fun typeOf(ENV{EE, ...}) id = 
       let val (_, ENV{SE,...}) = Env.look EE id 
       in  SEQdecl SE
       end handle _ => $ []

end

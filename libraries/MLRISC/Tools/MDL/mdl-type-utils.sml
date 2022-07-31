(*
 * Utilities for manipulating types
 *)
functor MDLTypeUtils(AstPP : MDL_AST_PRETTY_PRINTER) : MDL_TYPE_UTILS =
struct

   structure Error = MDLError
   structure AstPP = AstPP
   structure Ast   = AstPP.Ast

   open Ast

   type level = int

   val counter = ref 0
   fun genVar k level = (counter := !counter + 1;
                         VARty(k,!counter,ref level,ref NONE))
   val newIVar = genVar INTkind
   val newVar = genVar TYPEkind

   exception OccursCheck 
   exception Unify 

   fun init() = counter := 0
  
   fun bug msg = MLRiscErrorMsg.error("MDTyping",msg)

   fun pr ty = PP.text(AstPP.ty ty)

   fun deref(VARty(_,_,_,ref(SOME t))) = deref t
     | deref t = t

   fun tupleTy [t] = t
     | tupleTy ts  = TUPLEty ts

   fun copy(VARty(_,_,_,ref(SOME t))) = copy t
     | copy(t as VARty _) = t
     | copy(t as TYVARty _) = t
     | copy(t as CELLty _) = t
     | copy(t as IDty _) = t
     | copy(t as INTVARty _) = t
     | copy(POLYty _) = bug "copy:poly"
     | copy(TUPLEty ts) = TUPLEty(map copy ts)
     | copy(RECORDty ts) = RECORDty(map (fn (l,t) => (l,copy t)) ts)
     | copy(FUNty(a,b)) = FUNty(copy a,copy b)
     | copy(APPty(f,tys)) = APPty(f,map copy tys)
     | copy(LAMBDAty _) = bug "copy:lambda"

   val iboundvars = List.filter (fn VARty(INTkind,_,_,_) => true | _ => false)

   fun inst lvl (e, POLYty(tvs,ty)) = 
       let val tvs' = map (fn VARty(k,_,_,x) => 
                           let val v = genVar k lvl
                           in  x := SOME v; v end) tvs
           val ty = copy ty 
           val _ = app (fn VARty(_,_,_,x) => x := NONE) tvs
           val ivars = iboundvars tvs'
       in  case ivars of 
             [] => (e, ty)
           | _ => (APPexp(e, TUPLEexp(map TYPEexp ivars)), ty)
       end
     | inst lvl (e, t) = (e, t)

   fun gen lvl (e, ty) =
   let val mark = !counter
       val bvs = ref []
       val trail = ref []
       fun f(VARty(_,_,_,ref(SOME t))) = f t
         | f(t as VARty(k,i,ref l,r)) =
               if i > mark orelse l < lvl then t 
               else let val v = genVar k 0
                    in  r := SOME v; 
                        bvs := (v,t) :: !bvs; trail := r :: !trail; v 
                    end
         | f(t as TYVARty _) = t
         | f(t as CELLty _) = t
         | f(t as IDty _) = t
         | f(t as INTVARty _) = t
         | f(FUNty(a,b)) = FUNty(f a,f b)
         | f(TUPLEty ts) = TUPLEty(map f ts)
         | f(RECORDty lts) = RECORDty(map (fn (l,t) => (l,f t)) lts)
         | f(APPty(a,ts)) = APPty(a,map f ts)
         | f(POLYty _) = bug "gen:poly"
         | f(LAMBDAty _) = bug "gen:lambda"
       val t = f ty
       fun arityRaise(bvs, e) =
           case iboundvars bvs of
             []  => e
           | bvs => let val xs = 
                            map (fn VARty(_,n,_,_) => "T"^Int.toString n) bvs
                        val args = map IDpat xs
                    in  case e of
                          LAMBDAexp cs =>
                           LAMBDAexp(map (fn CLAUSE(cs,g,e) =>
                                           CLAUSE(TUPLEpat args::cs,g,e)) cs)
                        | _ => LAMBDAexp[CLAUSE([TUPLEpat args], NONE, e)]
                    end
   in  app (fn r => r := NONE) (!trail);
       case !bvs of
          []  => (e, ty)
       |  bvs => let val bvs = rev bvs (* boundvars are listed in reverse *)
                 in  (arityRaise(map #2 bvs, e), POLYty(map #1 bvs,t)) end
   end

   fun lambda level ty =
        case gen level (LITexp(INTlit 0), ty) of
          (_, POLYty(bvs,t)) => LAMBDAty(bvs,t)
        | (_, t) => t

   fun unify(msg,x,y) =
   let fun errorOccursCheck(t1,t2) =
           Error.error("occurs check failed in unifying "^pr t1^" and "
                       ^pr t2^msg())
       fun errorUnify(t1,t2) =
           Error.error("can't unify "^pr t1^" and "^pr t2^msg())

       fun f(VARty(_,_,_,ref(SOME x)),y) = f(x,y)
         | f(x,VARty(_,_,_,ref(SOME y))) = f(x,y)
         | f(x as VARty(k1,_,m,u),y as VARty(k2,_,n,v)) = 
               if u = v then ()
               else if k1 = INTkind then 
                       (v := SOME x; m := Int.max(!m,!n))
                    else
                       (u := SOME y; n := Int.max(!m,!n))
         | f(VARty x,e) = upd x e
         | f(e,VARty x) = upd x e
         | f(IDty x,IDty y) = if x = y then () else raise Unify
         | f(TYVARty x,TYVARty y) = if x = y then () else raise Unify
         | f(TUPLEty x,TUPLEty y) = g(x,y)
         | f(TUPLEty[x],y) = f(x,y)
         | f(x,TUPLEty[y]) = f(x,y)
         | f(RECORDty x,RECORDty y) = h(x,y)
         | f(CELLty x,CELLty y) = if x = y then () else raise Unify
         | f(FUNty(a,b),FUNty(c,d)) = (f(a,c); f(b,d))
         | f(APPty(a,b),APPty(c,d)) = if a = c then g(b,d) else raise Unify
         | f(INTVARty i,INTVARty j) = if i = j then () else raise Unify
         | f _ = raise Unify

       and g([],[]) = ()
         | g(a::b,c::d) = (f(a,c); g(b,d))
         | g _ = raise Unify

       and h(ltys1,ltys2) =
           let val sort = ListMergeSort.sort (fn ((a,_),(b,_)) => a > b) 
               val ltys1 = sort ltys1
               val ltys2 = sort ltys2
               fun merge((x,t)::m,(y,u)::n) =
                   if x = y then (f(t,u); merge(m,n))
                   else raise Unify
                 | merge([],[]) = ()
                 | merge _ = raise Unify
           in merge(ltys1,ltys2) end

       and upd (t1 as (k,name,lvl,v)) t2 =
           let fun g(VARty(_,_,_,ref(SOME t))) = g t
                 | g(VARty(k',n,l,y)) = 
                     if y = v then raise OccursCheck 
                     else (l := Int.max(!lvl,!l))
                 | g(TUPLEty ts) = app g ts
                 | g(RECORDty lts) = app (fn (_,t) => g t) lts
                 | g(CELLty _) = ()
                 | g(TYVARty t) = ()
                 | g(FUNty(a,b)) = (g a; g b)
                 | g(IDty _) = ()
                 | g(INTVARty _) = ()
                 | g(APPty(_,b)) = app g b
                 | g(POLYty _) = bug "unify:poly"
                 | g(LAMBDAty _) = bug "unify:lambda"
           in  g t2 handle Unify => errorUnify(VARty t1,t2)
                         | OccursCheck => errorOccursCheck(VARty t1,t2);
               v := SOME t2
           end

   in  f(x,y) handle Unify => errorUnify(x,y)
   end

   fun apply (msg,VARty(_,_,_,ref(SOME t)),args) = apply (msg,t,args)
     | apply (msg,f as LAMBDAty(tvs,body),args) = 
        let val arity1 = length tvs
            val arity2 = length args
        in  if arity1 <> arity2 then
               Error.error(
                 "arity mismatch between "^pr f^" and "^pr(TUPLEty args)^msg)
            else ();
            ListPair.app (fn (x,y) =>
               case (deref x,deref y) of
                 (VARty(TYPEkind,_,_,x),y) => x := SOME y
               | (x,VARty(TYPEkind,_,_,y)) => y := SOME x
               | (VARty(INTkind,_,_,x),y as INTVARty _) => x := SOME y
               | (VARty(INTkind,_,_,x),y as VARty(INTkind,_,_,_)) => x := SOME y
               | (VARty(INTkind,_,_,x),y) =>
                 Error.error(
                   "kind mismatch in application between "^pr f^
                   " and "^pr(TUPLEty args)^msg)
             ) (tvs,args);
            copy body before app (fn VARty(_,_,_,x) => x := NONE) tvs
        end
     | apply (msg,t,args) =
        (Error.error("type "^pr t^" is not a type constructor"^msg); newVar 0)
 
   fun poly([],t) = t
     | poly(tvs,t) = POLYty(tvs,t)
         
   fun newType(DATATYPEbind{id,tyvars,...}) = 
       let val ty = IDty(IDENT([],id))
       in  case tyvars of
              [] => ([],ty)
           | tyvars => let val vs = map (fn _ => newVar 0) tyvars
                       in  (vs,ty) end
       end
end



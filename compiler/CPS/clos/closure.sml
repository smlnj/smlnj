(* closure.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(****************************************************************************
 *                                                                          *
 *  ASSUMPTIONS: (1) Five possible combinations of bindings in the same     *
 *                   FIX : known,escape,cont,known-cont,known+escape;       *
 *                                                                          *
 *               (2) Continuation function is never recursive; there is     *
 *                   at most ONE continuation function definition per FIX;  *
 *                                                                          *
 *               (3) The outermost function is always a non-recursive       *
 *                   escaping function.                                     *
 *                                                                          *
 ****************************************************************************)

signature CLOSURE = sig
  val closeCPS : CPS.function -> CPS.function
end (* signature CLOSURE *)

functor Closure(MachSpec : MACH_SPEC) : CLOSURE = struct

local
  open CPS
  structure U = CPSUtil
  structure LV = LambdaVar
  structure SL = LV.SortedList
  structure CGoptions = Control.CG
  structure SProf = StaticProf(MachSpec)

  val saveLvarNames = Control.ElabData.saveLvarNames
  val dupLvar = LV.dupLvar
  val mkLvar = LV.mkLvar

  val OFFp0 = OFFp 0

  val dumcs = NONE              (* dummy callee-save reg contents *)
  val zip = ListPair.zip
  val pr = Control.Print.say
  fun inc (ri as ref i) = ri := i+1

(* static profiling *)
  fun profL n = if !CGoptions.staticprof andalso (n > 0) then SProf.incln n else ()

(* tagged int value *)
fun tagInt i = NUM{ival = IntInf.fromInt i, ty = {sz = Target.defaultIntSz, tag = true}}

(* there is a bug in "CLOSURE SHARING VIA THINNING" where the code is
 * sensitive to the sorting of equal items.  Specifically, the code works
 * using the old (pre 110.78) version of ListMergeSort.sort, which reverses
 * the original order of equal items, whereas it breaks on Twelf (bug #140)
 * when using the new implementation, which is stable.
 *
 * Therefore, we include the old sorting code here for now and use it in that
 * one place in the thinning code.
 *)
  fun oldSort (op > : 'a * 'a -> bool) ls = let
	fun merge([],ys) = ys
	  | merge(xs,[]) = xs
	  | merge(x::xs,y::ys) =
	      if x > y then y::merge(x::xs,ys) else x::merge(xs,y::ys)
	fun mergepairs(ls as [l], k) = ls
	  | mergepairs(l1::l2::ls,k) =
	      if k mod 2 = 1 then l1::l2::ls
	      else mergepairs(merge(l1,l2)::ls, k div 2)
	  | mergepairs _ = raise LibBase.Impossible "ListSort.sort"
	fun nextrun(run,[])    = (rev run,[])
	  | nextrun(run,x::xs) = if x > hd run then nextrun(x::run,xs)
				 else (rev run,x::xs)
	fun samsorting([], ls, k)    = hd(mergepairs(ls,0))
	  | samsorting(x::xs, ls, k) = let
	      val (run,tail) = nextrun([x],xs)
	      in samsorting(tail, mergepairs(run::ls,k+1), k+1)
	      end
	in
	  case ls of [] => [] | _ => samsorting(ls, [], 0)
	end

in


fun bug s = ErrorMsg.impossible ("Closure: " ^ s)

(****************************************************************************
 *                    MISC UTILITY FUNCTIONS                                *
 ****************************************************************************)

fun partition f l =
  foldr (fn (e,(a,b)) => if f e then (e::a,b) else (a,e::b)) ([], []) l

fun sublist test =
  let fun subl arg =
        let fun s(a::r,l) = if test a then s(r,a::l) else s(r,l)
              | s(nil,l) = rev l
         in s(arg,nil)
        end
   in subl
  end

fun formap f =
  let fun iter(nil,_) = nil
	| iter(hd::tl,i) = f(hd,i)::iter(tl,i+1)
   in iter o (fn l => (l,0))
  end

(* clean reverses the order of the argument list *)
fun clean l =
  let fun vars(l, VAR x :: rest) = vars(x::l, rest)
        | vars(l, _::rest) = vars(l,rest)
        | vars(l, nil) = l
   in vars(nil,l)
  end

fun uniqvar l = SL.uniq(clean l)

fun entervar(VAR v,l) = SL.enter(v,l)
  | entervar(_,l) = l

fun member3 l (v:LV.lvar) = let
      fun h [] = false
        | h ((a,_,_)::r) = (case LV.compare(a, v)
	     of LESS => h r
	      | EQUAL => true
	      | GREATER => false
	    (* end case *))
      in
	h l
      end

fun mergeV (l1 : (lvar*int*int) list, l2) = let
      fun h (l1 as ((u1 as (x1,a1,b1))::r1), l2 as ((u2 as (x2,a2,b2))::r2)) = (
	    case LV.compare(x1, x2)
	     of LESS => u1 :: h(r1,l2)
	      | EQUAL => (x1,Int.min(a1,a2),Int.max(b1,b2)) :: h(r1,r2)
	      | GREATER => u2 :: h(l1,r2)
	    (* end case *))
        | h (l1,[]) = l1
        | h ([],l2) = l2
      in
	h (l1, l2)
      end

fun addV(vl,m,n,l) = mergeV (map (fn x => (x,m,n)) vl, l)

fun uniqV z =
  let fun h([],l) = l
        | h(a::r,l) = h(r,mergeV([a],l))
   in h(z,[])
  end

fun removeV (vl : lvar list, l) = let
      fun h (l1 as (x1::r1), l2 as ((u2 as (x2,_,_))::r2)) = (
	    case LV.compare (x1, x2)
	     of LESS => h(r1, l2)
	      | EQUAL => h(r1, r2)
	      | GREATER => u2 :: h(l1, r2)
	    (* end case *))
	| h ([], l2) = l2
	| h (l1, []) = []
      in
	h(vl, l)
      end

fun accumV([],_) = ([],1000000,0,0)
  | accumV(vl,free) =
     let fun h((v,m,n),(z,i,j,k)) =
           if SL.member vl v then (v::z,Int.min(m,i),Int.max(n,j),k+1)
           else (z,i,j,k)
      in foldr h ([],1000000,0,0) free
     end

fun partBindings fl =
  let fun h((fe as (ESCAPE,_,_,_,_))::r,el,kl,rl,cl,jl) =
                                              h(r,fe::el,kl,rl,cl,jl)
        | h((fe as (KNOWN,_,_,_,_))::r,el,kl,rl,cl,jl) =
                                              h(r,el,fe::kl,rl,cl,jl)
        | h((fe as (KNOWN_REC,_,_,_,_))::r,el,kl,rl,cl,jl) =
                                              h(r,el,fe::kl,fe::rl,cl,jl)
        | h((fe as (CONT,_,_,_,_))::r,el,kl,rl,cl,jl) =
                                              h(r,el,kl,rl,fe::cl,jl)
        | h((fe as (KNOWN_CONT,_,_,_,_))::r,el,kl,rl,cl,jl) =
                                              h(r,el,kl,rl,fe::cl,fe::jl)
        | h((fe as (KNOWN_TAIL,_,_,_,_))::r,el,kl,rl,cl,jl) =
                                              h(r,el,fe::kl,rl,cl,jl)
        | h(_::r,el,kl,rl,cl,jl) = bug "partBindings in closure phase 231"
        | h([],el,kl,rl,cl,jl) = (el,kl,rl,cl,jl)
   in h(fl,[],[],[],[],[])
  end

val closureLvar =
  let val save = (!saveLvarNames before saveLvarNames := true)
      val closure = LV.namedLvar(Symbol.varSymbol "closure")
   in (saveLvarNames := save; fn () => dupLvar closure)
  end

(* build a list of k dummy cells *)
fun extraDummy(k) =
  let fun ec(k,l) = if k <= 0 then l else ec(k-1,dumcs::l)
   in ec(k,[])
  end

fun extraLvar (k,t) =
  let fun h (n,l,z) = if n < 1 then (rev l,z) else h(n-1,(mkLvar()::l),t::z)
   in h(k,[],[])
  end

(* cut out the first n elements from a list *)
fun cuthead(n,[]) = []
  | cuthead(n,l as (_::r)) = if n <= 0 then l else cuthead(n-1,r)

(* cut out the last n elements from a list *)
fun cuttail(n,l) = rev(cuthead(n,rev l))

(* sort according to each variable's life time etc. *)
fun sortlud0 x = ListMergeSort.sort (fn ((_,_,i : int),(_,_,j)) => (i>j)) x

fun sortlud1 x =
  let fun ludfud1((_,m:int,i:int),(_,n,j)) =
                    (i>j) orelse ((i=j) andalso (m>n))
   in ListMergeSort.sort ludfud1 x
  end

fun sortlud2 (l, vl) = let
      fun h (v, m, i) = if SL.member vl v then (i*1000+m*10) else (i*1000+m*10+1)
      fun ludfud2 ((_,m:int,v), (_,n,w)) = (m>n) orelse ((m=n) andalso LV.<(v,w))
      val nl = map (fn (u as (v,_,_)) => (u,h u,v)) l
      in
	map #1 (ListMergeSort.sort ludfud2 nl)
      end

(* cut out the first n elements, returns both the header and the rest *)
fun partvnum(l,n) =
  let fun h(vl,[],n) = (vl,[])
        | h(vl,s as ((a,_,_)::r),n) =
             if n <= 0 then (vl,s) else h(SL.enter(a,vl),r,n-1)
   in h([],l,n)
  end

(* spill (into sbase) if too many free variables (>n) *)
fun spillFree(free,n,vbase,sbase) =
  let val len = length free
   in if (len < n) then (SL.merge(map #1 free,vbase),sbase)
      else (let val (nfree,nspill) = partvnum(sortlud1 free,n)
             in (SL.merge(nfree,vbase),uniqV(nspill@sbase))
            end)
  end

fun get_vn ([],v) = NONE
  | get_vn ((a,m,n)::r, v : lvar) = (case LV.compare(a, v)
       of LESS => get_vn(r, v)
        | EQUAL => SOME(m, n)
        | GREATER => NONE
      (* end case *))

(* check if x is a subset of y, x and y must be sorted lists *)
fun subset (x,y) = (case SL.difference(x,y) of [] => true | _ => false)

(* check if a CPS type is a small constant size object *)
fun smallObj (FLTt _) = true
  | smallObj (NUMt{sz, tag}) = tag
  | smallObj _ = false

(* check if a record_kind is sharable by a function of fun_kind *)
(* removed quasiStack from MACH_SPEC -- jhr; 2019-06-03
fun sharable((RK_CONT|RK_FCONT),(ESCAPE|KNOWN)) = not (MachSpec.quasiStack)
  | sharable _ = true
*)
fun sharable _ = true

(* given a fun_kind return the appropriate unboxed closure kind *)
(* need runtime support for RK_FCONT (new tags etc.) CURRENTLY NOT SUPPORTED *)
fun unboxedKind (CONT | KNOWN_CONT) = RK_FCONT
  | unboxedKind _ = RK_RAW64BLOCK

(* given a fix kind return the appropriate boxed closure kind *)
fun boxedKind (CONT | KNOWN_CONT) = RK_CONT
  | boxedKind KNOWN = RK_KNOWN
  | boxedKind _ = RK_ESCAPE

fun COMMENT f = if !CGoptions.comment then (f(); ()) else ()

(****************************************************************************
 *                    CLOSURE REPRESENTATIONS                               *
 ****************************************************************************)

type csregs = (value list * value list) option

datatype closureRep = CR of int * closure
withtype closure = {functions : (lvar * lvar) list,
		    values : lvar list,
		    closures : (lvar * closureRep) list,
                    kind : record_kind,
                    core : lvar list,
                    free : lvar list,
		    stamp : lvar}

type knownfunRep = {label : lvar,
                    gpfree : lvar list,
                    fpfree : lvar list,
                    csdef : (value list * value list) option}

type calleeRep = value * value list * value list

datatype object = Value of cty
                | Callee of calleeRep
                | Closure of closureRep
                | Function of knownfunRep

datatype access = Direct
		| Path of lvar * accesspath * (lvar * closureRep) list


(****************************************************************************
 *        UTILITY FUNCTIONS FOR ELIMINATING THE CLOSURE OFFSET              *
 ****************************************************************************)
(** should we adjust the offset *)
fun adjOff (i, off) =
  if i > 0 then 1
  else if off = 0 then 0 else bug "unexpected case in adjOff"

(** should we treat the mutually recursive functions specially *)
fun mutRec [] = false
  | mutRec [_] = false
  | mutRec _ = true

(* if no_offset is false, use the following versions:

   fun adjOff (i, off) = i - off
   fun mutRec _ = false
 *)

(****************************************************************************
 *                         STATIC ENVIRONMENT                               *
 ****************************************************************************)

abstype env = Env of lvar list *                    (* values *)
	             (lvar * closureRep) list *     (* closures *)
                     lvar list *                    (* disposable cells *)
                     object LV.Tbl.hash_table       (* what map *)
with

(****************************************************************************
 * Environment Initializations and Augmentations                            *
 ****************************************************************************)

exception NotBound
fun emptyEnv() = Env([],[],[],LV.Tbl.mkTable(32,NotBound))

(* add a new object to an environment *)
fun augment(m as (v,obj),e as Env(valueL,closureL,dispL,whatMap)) =
  (LV.Tbl.insert whatMap m;
   case obj
    of Value _ => Env(v::valueL,closureL,dispL,whatMap)
     | Closure cr => Env(valueL,(v,cr)::closureL,dispL,whatMap)
     | _ => e)

(* add a simple program variable "v" with type t into env *)
fun augValue(v,t,env) = augment((v,Value t),env)

(* add a list of value variables into env *)
fun faugValue([],[],env) = env
  | faugValue(a::r,t::z,env) = faugValue(r,z,augValue(a,t,env))
  | faugValue _ = bug "faugValue in closure.249"

(* add a callee-save continuation object into env *)
fun augCallee(v,c,csg,csf,env) = augment((v,Callee(c,csg,csf)),env)

(* add a known continuation function object into env *)
fun augKcont(v,l,gfree,ffree,csg,csf,env) =
  let val kobj = Function {label=l,gpfree=gfree,fpfree=ffree,
                           csdef=SOME(csg,csf)}
   in augment((v,kobj),env)
  end

(* add a general known function object into env *)
fun augKnown(v,l,gfree,ffree,env) =
  let val kobj = Function {label=l,gpfree=gfree,fpfree=ffree,csdef=NONE}
   in augment((v,kobj),env)
  end

(* add an escaping function object into env *)
fun augEscFun(v,i,CR(off,x),env) =
  let val clo = Closure(CR(off+i,x))
   in augment((v,clo),env)
  end

(****************************************************************************
 * Environment Printing (for debugging)                                     *
 ****************************************************************************)

val im : int -> string = Int.toString
val vp = pr o LambdaVar.lvarName
fun Vp (v,m,n) = (vp v; pr " fd="; pr (im m); pr " ld=";
                  pr (im n))
fun ifkind (KNOWN_TAIL) = pr " KNOWN_TAIL "
  | ifkind (KNOWN) = pr " KNOWN "
  | ifkind (KNOWN_REC) = pr " KNOWN_REC "
  | ifkind (ESCAPE) = pr " ESCAPE "
  | ifkind (CONT) = pr " CONT "
  | ifkind (KNOWN_CONT) = pr " KNOWN_CONT "
  | ifkind _ = pr " STRANGE_KIND "
fun plist p l = (app (fn v => (pr " "; p v)) l; pr "\n")
val ilist = plist vp
val iVlist = plist Vp
val iKlist = plist ifkind
val sayv = pr o PPCps.value2str
val vallist = plist sayv

fun printEnv(Env(valueL,closureL,dispL,whatMap)) =
  let fun ip (i : int) = pr(Int.toString i)
      val tlist = plist (fn (a,b) => (vp a; pr "/"; sayv(LABEL b)))
      fun fp(v,Function{label,gpfree,fpfree,...}) =
   	      (vp v; pr "/known "; sayv(LABEL label); pr " -";
               ilist (gpfree@fpfree))
        | fp _ = ()
      fun cp (v,Callee(v',gl, fl)) =
  	   (vp v; pr "/callee(G) "; sayv v'; pr " -"; vallist gl;
            vp v; pr "/callee(F) "; sayv v'; pr " -"; vallist fl)
        | cp _ = ()
      fun p(indent,l,seen) =
	let fun c(v,CR(off, {functions,values,closures,stamp,kind,...})) =
	      (indent(); pr "Closure "; vp v; pr "/"; pr(LV.prLvar stamp);
	       pr " @"; ip off;
	       if SL.member seen stamp
	       then pr "(seen)\n"
	       else (pr ":\n";
		     case functions
		       of nil => ()
		        | _ => (indent(); pr "  Funs:"; tlist functions);
		     case values
		       of nil => ()
		        | _ => (indent(); pr "  Vals:"; ilist values);
		     p(fn() => (indent();pr "  "),closures,SL.enter(stamp,seen))))
	 in app c l
	end
  in  pr "Values:"; ilist valueL;
      pr "Closures:\n"; p(fn () => (),closureL,nil);
      pr "Disposable records:\n"; ilist dispL;
      pr "Known function mapping:\n"; LV.Tbl.appi fp whatMap;
      pr "Callee-save continuation mapping:\n";
      LV.Tbl.appi cp whatMap
  end

(****************************************************************************
 * Environment Lookup (whatIs, returning object type)                       *
 ****************************************************************************)

exception Lookup of string * lvar * env
fun whatIs(env as Env(_,_,_,whatMap),v) =
  LV.Tbl.lookup whatMap v handle NotBound => raise Lookup("whatIs", v,env)

(* Add v to the access environment, v must be in whatMap already *)
fun augvar(v,e as Env(valueL,closureL,dispL,whatMap)) =
  case whatIs(e,v)
   of Value _ => Env(v::valueL,closureL,dispL,whatMap)
    | Closure cr => Env(valueL,(v,cr)::closureL,dispL,whatMap)
    | _ => bug "augvar in cps/closure.223"

(****************************************************************************
 * Environment Access (whereIs, returning object access path)               *
 ****************************************************************************)

fun whereIs (env as Env(valueL,closureL,_,whatMap), target) = let
      fun bfs (nil, nil) = raise Lookup("whereIs",target,env)
	| bfs (nil, next) = bfs(next,nil)
	| bfs ((h, ox as (_, CR(off, {functions,values, closures,stamp,...})))::m, next) = let
            fun cls(nil, _, next) = bfs(m,next)
	      | cls((u as (v,cr))::t, i, next) =
		 if target=v then h(SELp(i, OFFp 0), [])
		 else let val nh = fn (p,z) => h(SELp(i, p), u::z)
		       in cls(t, i+1, (nh,u)::next)
		      end
	    fun vls(nil,i) = cls(closures, i, next)
	      | vls(v::t,i) =
		 if target=v then h(SELp(i, OFFp 0), [])
		 else vls(t, i+1)
	    fun fns(nil,i) = vls(values, adjOff(i,off))
	      | fns((v,l)::t,i) =
		 if target=v then
		   if i = off then h(OFFp 0, [])
		   else h(OFFp(i-off),[ox])
		 else fns(t,i+1)
             in if target=stamp
                then if off = 0 then h(OFFp 0, [])
                     else h(OFFp(~off), [ox])
                else fns(functions, 0)
            end
      fun search closures = let
            val s = map (fn x => (fn (p,z) => (#1 x,p,z), x)) closures
	    in
	      Path (bfs(s,nil))
            end
      fun withTgt (v,CR(_,{free,...})) = SL.member free target
      fun lookC ((v,cr)::tl) =
            if target=v then Direct
            else (case cr
                   of CR(_, {functions=[], ...}) => lookC tl
                    | CR(off, {functions, ...}) =>
                        let val (y, _) = List.nth(functions, off)
                         in if (target=y) then Path (v, OFFp 0, [])
                            else lookC tl
                         end)
        | lookC nil = search (sublist withTgt closureL)
      fun lookV (v::tl) =
            if target=v then Direct else lookV tl
        | lookV nil = search closureL
   in case whatIs (env, target)
       of Function _ => Direct
        | Callee _ => Direct
        | Closure _ => lookC closureL
        | Value _ => lookV valueL
  end



(****************************************************************************
 * Environment Filtering (get the set of current reusable closures)         *
 ****************************************************************************)

(* extract all closures at top n levels, containing duplicates. *)
fun extractClosures (l,n,base) =
  let fun g(_,CR(_,{closures,...})) = closures

      fun h(k,[],z) = z
        | h(k,r,z) =
           if k <= 0 then z
           else (let val nl = List.concat (map g r)
                  in h(k-1,nl,nl@z)
                 end)

      fun s([],vl,r) = r
        | s((u as (v,_))::z,vl,r) =
             if SL.member vl v then s(z,vl,r)
             else s(z,SL.enter(v,vl),u::r)

   in s(h(n,l,l@base),[],[])
  end

(* fetch all free variables residing above level n in the closure cr *)
fun fetchFree(v,CR(_,{closures,functions,values,...}),n) =
  if n <= 0 then [v]
  else (let fun g((x,cr),z) = SL.merge(fetchFree(x,cr,n-1),z)
         in foldr g (SL.uniq (v::values@(map #1 functions))) closures
        end)

(* filter out all closures in the current env that are safe to reuse *)
fun fetchClosures(env as Env(_,closureL,_,_),lives,fkind) =
  let val (closlist,lives) =
             foldr (fn (v,(z,l)) => case whatIs(env,v)
                     of (Closure (cr as (CR(_,{free,...})))) =>
                                           ((v,cr)::z,SL.merge(free,l))
                      | _ => (z,l)) ([],lives) lives

      fun reusable (v,CR(_,{core,kind,...})) =
           ((sharable(kind,fkind)) andalso
            ((subset(core,lives)) orelse (SL.member lives v)))

      fun reusable2 (_,CR(_,{kind,...})) = sharable(kind,fkind)

      fun fblock (_,CR(_,{kind=(RK_RAW64BLOCK|RK_FCONT),...})) = true
        | fblock _ = false

      val level = 4 (* should be made adjustable in the future *)
      val closlist = extractClosures(closureL,level,closlist)
      val (fclist,gclist) = partition fblock closlist

   in (sublist reusable gclist, sublist reusable2 fclist)
  end

(* return the immediately enclosing closure, if any.  This is a hack. *)
fun getImmedClosure (Env(_,closureL,_,_)) =
  let fun getc ([z]) = SOME z
        | getc (_::tl) = getc tl
        | getc nil = NONE
   in getc closureL
  end

(****************************************************************************
 * Continuation Frames Book-keeping (in support of quasi-stack frames)      *
 ****************************************************************************)

(* vl is a list of continuation frames that were reused along this path *)
fun recoverFrames(vl,Env(valueL,closureL,dispL,whatMap)) =
  let fun h(a,l) = if SL.member vl a then l else a::l
      val ndispL = foldr h [] dispL
   in Env(valueL,closureL,ndispL,whatMap)
  end

(* save the continuation closure "v" and its descendants *)
fun saveFrames(v,CR(_,{free,kind=(RK_CONT|RK_FCONT),...}),env) =
         recoverFrames(free,env)
  | saveFrames(_,_,env) = env

(* install the set of live frames at the entrance of this continuations *)
fun installFrames(newd,env as Env(valueL,closureL,dispL,whatMap)) =
      Env(valueL,closureL,newd@dispL,whatMap)

(* split the current disposable frame list into two based on the context *)
fun splitEnv(Env(valueL,closureL,dispL,w),inherit) =
  let val (d1,d2) = partition inherit dispL
   in (Env([],[],d1,w),Env(valueL,closureL,d2,w))
  end

(* return the set of disposable frames *)
fun deadFrames(Env(_,_,dispL,_)) = dispL

end (* abstype env *)

type frags = (fun_kind * lvar * lvar list * cty list * cexp * env *
              int * value list * value list * lvar option) list

(****************************************************************************
 *               UTILITY FUNCTIONS FOR CALLEE-SAVE REGISTERS                *
 ****************************************************************************)

(* it doesnot take the looping freevar into account, NEEDS MORE WORK *)
fun fetchCSregs(c,m,n,env) =
 case whatIs(env,c)
  of Callee (_,csg,csf) => (cuthead(m,csg),cuthead(n,csf))
   | Function {csdef=SOME(csg,csf),...} => (cuthead(m,csg),cuthead(n,csf))
   | _ => ([],[])

(* fetch m csgpregs and n csfpgregs from the default continuation c *)
fun fetchCSvars(c,m,n,env) =
 let val (gpregs,fpregs) = fetchCSregs(c,m,n,env)
  in (uniqvar gpregs,uniqvar fpregs)
 end

(* fill the empty csgpregs with the closure *)
fun fillCSregs(csg,c) =
 let fun g([],l) = l
       | g(a::r,l) = g(r,a::l)
     fun h(NONE::r,x,c) = g(x,c::r)
       | h(u::r,x,c) = h(r,u::x,c)
       | h([],x,c) = bug "no empty slot in fillCSregs in closure.sml"
  in h(csg,[],c)
 end

(* fill the empty cs formals with new variables, augment the environment *)
fun fillCSformals(gpbase,fpbase,env,ft) =
  let fun h(SOME v,(e,a,c)) = (augvar(v,e),v::a,(ft v)::c)
        | h(NONE,(e,a,c)) =
            let val v = mkLvar()
             in (augValue(v,U.BOGt,e),v::a,U.BOGt::c)
            end

      fun g(SOME v,(e,a,c)) = (augvar(v,e), v::a, (FLTt 64)::c) (* REAL64: FIXME *)
        | g(NONE,(e,a,c)) =
            let val v = mkLvar()
             in (augValue(v,FLTt 64,e), v::a, FLTt 64::c)  (* REAL64: FIXME *)
            end

   in foldr h (foldr g (env,[],[]) fpbase) gpbase
  end

(* get all free variables in cs regs, augment the environment *)
fun varsCSregs(gpbase,fpbase,env) =
 let fun h(NONE,(e,l)) = (e,l)
       | h(SOME v,(e,l)) = (augvar(v,e),SL.enter(v,l))

     val (env,gfree) = foldr h (env,[]) gpbase
     val (env,ffree) = foldr h (env,[]) fpbase
  in (gfree,ffree,env)
 end

(* get all free variables covered by the cs regs *)
fun freevCSregs(gpbase,env) =
 let fun h(NONE,l) = l
       | h(SOME v,l) = case whatIs(env,v)
          of (Closure (CR(_,{free,kind=(RK_CONT|RK_FCONT),...}))) =>
                 (SL.merge(free,l))
           | _ => l
  in foldr h [] gpbase
 end

(* partnull cuts out the head of csregs till the first empty position *)
fun partnull l =
  let fun h([],r) = bug "partnull. no empty position in closure 343"
        | h(NONE::z,r) = (rev(NONE::r),z)
        | h(u::z,r) = h(z,u::r)
   in h(l,[])
  end

(* create a template of the base callee-save registers (n : extra cs regs) *)
fun mkbase(regs,free,n) =
  let fun h((VAR v),(r,z)) =
             if SL.member free v then ((SOME v)::r,SL.enter(v,z))
             else (dumcs::r,z)
        | h(_,(r,z)) = (dumcs::r,z)
   in foldr h (extraDummy(n),[]) regs
  end

(* modify the base, retain only those variables in free *)
fun modifybase(base,free,n) =
  let fun h(s as (SOME v),(r,z,m)) =
             if SL.member free v then (s::r,SL.rmv(v,z),m)
             else (if m > 0 then (s::r,z,m-1) else (dumcs::r,z,m))
        | h(NONE,(r,z,m)) = (NONE::r,z,m)
   in foldr h ([],free,n) base
  end

(* fill the empty callee-save registers, assuming newv can be put in base *)
fun fillbase(base,newv) =
  let fun g([],s) = s
        | g(a::r,s) = g(r,a::s)
      fun h(s,l,[]) = g(l,s)
        | h(NONE::z,l,a::r) = h(z,(SOME a)::l,r)
        | h((u as (SOME _))::z,l,r) = h(z,u::l,r)
        | h([],l,_) = bug "no enough slots: fillbase 398 in closure.sml"
   in h(base,[],newv)
  end

(****************************************************************************
 *                  VARIABLE ACCESS PATH LOOKUP                             *
 ****************************************************************************)

(** simulating the OFFSET operation by reconstructing the closures *)
fun offset ((z, CR(n,{functions,values,closures,...})), i, u, x, env) =
  let (* invariants: length functions > 1 *)
      val (_,l) = List.nth(functions, n+i)
      val _ = case u of VAR z' => if z = z' then ()
                                  else bug "unexpected case in offset 1"
                      | _ => bug "unexpected case in offset 2"
      val lab = (LABEL l, OFFp0)
      val vl =
        case (closures, values)
         of (([(v,_)], []) | ([], [v])) => [lab, (VAR v, OFFp0)]
          | ([], []) => [lab]
          | _ => bug "unexpected case in offset 3"
      val (hdr, env) = recordEl(RK_ESCAPE, vl, x, env)
   in (hdr, env)
  end

(* if no_offset is false, use this version

   fun offset (_, i, u, x, env) =
     let val hdr = fn ce => OFFSET(i, u, x, ce)
      in (hdr, env)
     end
 *)

(* build the header by partially following an access path *)
and pfollow (p, env, hdr) =
  (case p
    of (v, np as ((OFFp 0) | (SELp(_,OFFp 0))), []) => ((VAR v,np), env, hdr)
     | (v, np as (OFFp i), [c as (_, cr as CR(n,z))]) =>
         let val w = closureLvar()
             val (nh, env) = offset(c, i, VAR v, w, env)
             val env = augment((w, Closure (CR(n+i,z))), env)
          in ((VAR w, OFFp 0), env, hdr o nh)
         end
     | (v, SELp(i,np), (w,cr)::z) =>
         let val env = augment((w,Closure cr),env)
             val nhdr = fn ce => SELECT(i,VAR v,w,U.BOGt,ce)
          in pfollow((w,np,z), env, hdr o nhdr)
         end
     | _ => bug "pfollow on an inconsistent path")

(* build the header by following an access path *)
and follow (rootvar, t) =
  let fun g ((v, OFFp 0, []), env, h) =
            (env, h o (fn ce => OFFSET(0,VAR v,rootvar,ce)))
        | g ((v, OFFp i, [c]), env, h) =
            let val (nh, env) = offset(c, i, VAR v, rootvar, env)
                (* environment is update by the client of "follow" *)
             in (env, h o nh)
            end
        | g ((v, SELp(i,OFFp 0),[]), env, h) =
            (env, h o (fn ce => SELECT(i,VAR v,rootvar,t,ce)))
        | g ((v, SELp(i,p), (w,cr)::z), env, h) =
            let val env = augment((w,Closure cr), env)
             in g((w,p,z), env, h o (fn ce => SELECT(i,VAR v,w,U.BOGt,ce)))
            end
        | g _ = bug "follow on an inconsistent path"
   in g
  end

(****************************************************************************
 * recordEl finds the complete access paths for elements of a record. It    *
 * returns a header for profiling purposes if needed.                       *
 ****************************************************************************)
and recordEl(rk, l, w, env) =
  let fun g(u as (VAR v, OFFp 0),(l,cl,hdr,env)) =
           let val env = case whatIs(env,v)   (* maybe unnecessary *)
                 of Closure cr => saveFrames(v,cr,env)
                  | _ => env

               val (m,cost,nhdr,env) = case whereIs(env,v)
                 of Direct => (u,0,hdr,env)
                  | Path(np as (start,path,_)) =>
                     (let val n = U.lenp path
                          val () = profL n
                          val (u,env,nhdr) =
                            if (!CGoptions.sharepath)
                            then pfollow(np, env, hdr)
                            else ((VAR start,path), env, hdr)
                       in (u,n,nhdr,env)
                      end)
            in (m::l,cost::cl,nhdr,env)
           end
        | g(u as (VAR _, _), _) = bug "unexpected case in recordEl"
        | g(u,(l,cl,hdr,env)) = (u::l,0::cl,hdr,env)

      val (rl,cl,hdr,env) = foldr g (nil,nil,fn ce => ce,env) l
      val nhdr = fn ce => hdr (RECORD(rk, rl, w, ce))
   in (nhdr, env)
  end

(****************************************************************************
 * fixAccess finds the access path to a variable.  A header to select the   *
 * variable from the environment is returned, along with a new environment  *
 * that reflects the actions of the header (this last implements a "lazy    *
 * display").  fixAccess actually causes rebindings -- the variable         *
 * requested is rebound if it is not immediately available in the           *
 * environment, these rebindings are later eliminated by an "unrebind" pass *
 * which basically does the alpha convertions.                              *
 ****************************************************************************)

fun fixAccess(args,env) =
  let fun access(VAR rootvar,(env,header)) =
           let val what = whatIs(env,rootvar)
               val (env,t) = case what
                 of Value x => (env,x)
                  | Closure cr => (saveFrames(rootvar,cr,env),U.BOGt)
                  | _ => bug "Callee or Known in fixAccess closure"

            in case whereIs(env,rootvar)
                   of Direct => (env,header)
                   | Path (p as (_,path,_)) =>
                       let val (env,header) = follow (rootvar,t) (p,env,header)
                         val env = augment((rootvar,what),env)
			 val () = profL (U.lenp path)
                      in (env, header)
                     end
           end
        | access(_,y) = y
   in foldr access (env, fn x => x) args
  end

(****************************************************************************
 * fixArgs is a slightly modified version of fixAccess. It's used to find   *
 * the access path of function arguments in the APP expressions             *
 ****************************************************************************)

fun fixArgs(args,env) =
  let fun access(z as (VAR rootvar),(res,env,h)) =
           let val what = whatIs(env,rootvar)
               val (env,t) = case what
                 of Value x => (env,x)
                  | Closure cr => (saveFrames(rootvar,cr,env),U.BOGt)
                  | _ => (env,U.BOGt)
            in case what
                of Function _ => bug "Known in fixArgs closure.sml"
                 | Callee(l,csg,csf) =>
                    (let val nargs = (l::csg)@csf@res
                         val (env,hdr) = fixAccess(nargs,env)
                      in (nargs,env,h o hdr)
                     end)
                 | _ => (case whereIs(env,rootvar)
                          of Direct => (z::res,env,h)
                           | Path (p as (_,path,_)) =>
                              let val (env,hdr) = follow (rootvar,t) (p,env,h)
                                  val env = augment((rootvar,what),env)
				  val () = profL (U.lenp path)
                               in (z::res, env, hdr)
                              end)
           end
        | access(z,(res,env,h)) = (z::res,env,h)
   in foldr access ([],env,fn x => x) args
  end

(****************************************************************************
 *                       CLOSURE STRATEGIES                                 *
 ****************************************************************************)

(* produce the CPS header and modify the environment for the new closure *)
fun mkClosure(cname,contents,cr,rkind,fkind,env) =
  let val _ = if !CGoptions.staticprof
              then SProf.incfk(fkind,length contents) else ()
      val l = map (fn v => (v, OFFp0)) contents
      val (hdr, env) = recordEl(rkind, l, cname, env)
      val env = augment((cname, Closure cr), env)
   in case fkind of (CONT|KNOWN_CONT) => (hdr, env, [cname])
                  | _ => (hdr, env ,[])
  end

(* build an unboxed closure, currently not disposable even if fkind=cont *)
(* Place int32's after floats for proper alignment *)

fun closureUbGen(cn, free, rk, fk, env) =
  let val nfree = map (fn (v, _, _) => v) free
      val ul = map VAR nfree
      val cr = CR(0,{functions=[],closures=[],values=nfree,
                     core=[],free=SL.enter(cn,nfree),kind=rk,stamp=cn})
   in (mkClosure(cn, ul, cr, rk, fk, env), cr)
  end

(* 64BIT: on 64-bit targets, there is no need to split the data *)
fun closureUnboxed(cn,int32free,otherfree,fk,env) =
  (case (int32free, otherfree)
    of ([], []) => bug "unexpected case in closureUnboxed 333"
     | ([], _) =>
         (let val rk = unboxedKind(fk)
           in #1(closureUbGen(cn, otherfree, rk, fk, env))
          end)
     | (_, []) =>
         (let val rk = RK_RAWBLOCK
           in #1(closureUbGen(cn, int32free, rk, fk, env))
          end)
     | _ =>
         (let val rk1 = unboxedKind(fk)
              val cn1 = closureLvar()
              val ((nh1, env, nf1), cr1) =
                closureUbGen(cn1, otherfree, rk1, fk, env)
              val rk2 = RK_RAWBLOCK
              val cn2 = closureLvar()
              val ((nh2, env, nf2), cr2) =
                closureUbGen(cn2, int32free, rk2, fk, env)
              val rk = boxedKind(fk)
              val nfree = map (fn (v, _, _) => v) (int32free@otherfree)
              val nfs = [cn1, cn2]
              val ncs = [(cn1,cr1), (cn2,cr2)]
              val ul = map VAR nfs
              val cr = CR(0, {functions=[],closures=ncs,values=[],
                              core=[],free=SL.enter(cn,nfs@nfree),
                              kind=rk,stamp=cn})
              val (nh, env, nfs) = mkClosure(cn, ul, cr, rk, fk, env)
           in (nh1 o nh2 o nh, env, nfs)
          end))

(*
 * old code
 *
 * let val nfree = map (fn (v,_,_) => v) (otherfree @ int32free)
 *     val ul = map VAR nfree
 *     val rk = unboxedKind(fk)
 *     val rk = case (int32free,otherfree)
 *               of ([],_) => rk
 *                | (_,[]) => RK_RAWBLOCK
 *                | _ => bug "unimplemented int32 + float (nclosure.1)"
 *     val cr = CR(0,{functions=[],closures=[],values=nfree,
 *                    core=[],free=SL.enter(cn,nfree),kind=rk,stamp=cn})
 *  in mkClosure(cn,ul,cr,rk,fk,env)
 * end
 *)

(* partition a set of free variables into small frames *)
(* removed quasiStack from MACH_SPEC -- jhr; 2019-06-03
fun partFrame(free) =
  if not (MachSpec.quasiStack) then (free,[])
  else
    (let val sz = MachSpec.quasiFrameSz
         fun h([],n,t) = (t,[])
           | h([v],n,t) = (v::t,[])
           | h(z as (v::r),n,t) =
              if n <= 1 then
                (let val (nb,nt) = h(z,sz,[])
                     val cn = closureLvar()
                  in (cn::t,(cn,nb)::nt)
                 end)
              else h(r,n-1,v::t)
      in h(free,sz,[])
     end)
*)
fun partFrame free = (free, [])

(* partition the free variables into closures and non-closures *)
fun partKind(cfree,env) =
  let fun g(v,(vls,cls,fv,cv)) =
        let val obj = whatIs(env,v)
         in case obj
             of Value t => (v::vls,cls,SL.enter(v,fv),
                            if (smallObj t) then cv else SL.enter(v,cv))
              | Closure (cr as CR (_,{free,core,...})) =>
                  (vls,(v,cr)::cls,SL.merge(free,fv),SL.merge(core,cv))
              | _ => bug "unexpected obj in kind in cps/closure.sml"
        end
   in foldr g (nil,nil,nil,nil) cfree
  end

(* closure strategy : flat *)
fun flat(env,cfree,rk,fk) =
  let val (topfv,clist) = case rk
        of (RK_CONT | RK_FCONT) => partFrame(cfree)
         | _ => (cfree,[])

      fun g((cn,free),(env,hdr,nf)) =
        let val (vls,cls,fvs,cvs) = partKind(free,env)
            val cr = CR(0,{functions=[],values=vls,closures=cls,
                           kind=rk,stamp=cn,core=cvs,free=SL.enter(cn,fvs)})
            val ul = (map VAR vls) @ (map (VAR o #1) cls)
            val (nh,env,nf2) = mkClosure(cn,ul,cr,rk,fk,env)
         in (env,hdr o nh,nf2@nf)
        end
      val (env,hdr,frames) = foldr g (env,fn ce => ce,[]) clist
      val (values,closures,fvars,cvars) = partKind(topfv,env)

   in (closures,values,hdr,env,fvars,cvars,frames)
  end

(* closure strategy : linked *)
fun link(env,cfree,rk,fk) =
  case getImmedClosure(env)
   of NONE => flat(env,cfree,rk,fk)
    | SOME (z,CR(_,{free,...})) =>
       let val notIn = sublist (not o (SL.member free)) cfree
        in if (length(notIn) = length(cfree))
           then flat(env,cfree,rk,fk)
           else flat(env,SL.enter(z,cfree),rk,fk)
       end

(* partition a set of free variables into layered groups based on their lud *)
fun partLayer(free,ccl) =
  let fun find(r,(v,all)::z) = if subset(r,all) then SOME v else find(r,z)
        | find(r,[]) = NONE

      (* current limit of a new layer : 3 *)
      fun m([],t,b) = bug "unexpected case in partLayer in closure"
        | m([v],t,b) = (SL.enter(v,t),b)
        | m([v,w],t,b) = (SL.enter(v,SL.enter(w,t)),b)
        | m(r,t,b) = (case find(r,ccl)
            of NONE =>
                 let val nc = closureLvar() in (SL.enter(nc,t),(nc,r)::b) end
             | SOME v => (SL.enter(v,t),b))

      (* process the rest groups in free *)
      fun h([],i:int,r,t,b) = m(r,t,b)
        | h((v,_,j)::z,i,r,t,b) =
            if j = i then h(z,i,SL.enter(v,r),t,b)
            else let val (nt,nb) = m(r,t,b)
                  in h(z,j,[v],nt,nb)
                 end

      (* cut out the top group and then process the rest *)
      fun g((v,_,i)::z,j,t) =
             if i = j then g(z,j,SL.enter(v,t)) else h(z,i,[v],t,[])
        | g([],j,t) = (t,[])

      val (topfv,botclos) =
        case (sortlud0 free)
         of [] => ([],[])
          | (u as ((_,_,j)::_)) => g(u,j,[])
   in (topfv,botclos)
  end (* function partLayer *)

(* closure strategy : layered *)
fun layer(env,cfree,rk,fk,ccl) =
  let val (topfv,clist) = partLayer(cfree,ccl)

      fun g((cn,vfree),(bh,env,nf)) =
        let val (cls,vls,nh1,env,fvs,cvs,nf1) = flat(env,vfree,rk,fk)
            val cr = CR(0,{functions=[],values=vls,closures=cls,
                           kind=rk,stamp=cn,core=cvs,free=SL.enter(cn,fvs)})
            val ul = (map VAR vls) @ (map (VAR o #1) cls)
            val (nh2,env,nf2) = mkClosure(cn,ul,cr,rk,fk,env)
         in (bh o nh1 o nh2, env, nf2@nf1@nf)
        end
      val (hdr,env,frames) = foldr g (fn ce => ce,env,[]) clist
      val (cls,vls,nh,env,fvs,cvs,nfr) = flat(env,topfv,rk,fk)

   in (cls,vls,hdr o nh,env,fvs,cvs,nfr@frames)
  end (* function layer *)

(* build a general closure, CGoptions.closureStrategy matters:
 *
 *	1 = flat without aliasing
 *	2 = flat with aliasing
 *	3 = linked without aliasing
 *	4 = flat with aliasing
 *)
fun closureBoxed(cn, fns, free, fk, ccl, env) =
  let val rk = boxedKind(fk)
      val (cls, vls, hdr, env, fvs, cvs, frames) =
        case !CGoptions.closureStrategy
         of (4|3) => link(env, map #1 free, rk, fk)
          | (2|1) => flat(env, map #1 free, rk, fk)
          | _ => layer(env, free, rk, fk, ccl)

      val (cls, vls, hdr, env, fvs, cvs, frames, labels) =
        if mutRec fns then (* invariants length fns > 1 *)
          let val nlabs = [LABEL (#2 (hd fns))]  (** no sharing **)
           in (case (cls, vls)
                of (([],[_]) | ([_],[]) | ([],[])) =>
                     (cls, vls, hdr, env, fvs, cvs, frames, nlabs)
                 | _ =>
                     let val nv = closureLvar()
                         val ul = (map VAR vls) @ (map (VAR o #1) cls)
                         val nfvs = SL.enter(nv, fvs)
                         val cr = CR(0,{functions=[],values=vls,closures=cls,
                                        kind=rk,stamp=nv,core=cvs,free=nfvs})
                         val (nh, nenv, nf) = mkClosure(nv,ul,cr,rk,fk,env)
                      in ([(nv,cr)], [], hdr o nh, nenv, nfvs,
                          cvs, nf@frames, nlabs)
                     end)
          end
        else (cls, vls, hdr, env, fvs, cvs, frames, map (LABEL o #2) fns)

      val nfvs = foldr SL.enter (SL.enter(cn,fvs)) (map #1 fns)
      val cr = CR(0,{functions=fns,values=vls,closures=cls,
                     kind=rk,stamp=cn,core=cvs,free=nfvs})
      val ul = labels @ (map VAR vls) @ (map (VAR o #1) cls)
      val (nh, nenv, nf) = mkClosure(cn,ul,cr,rk,fk,env)
   in (hdr o nh, nenv, cr, nf@frames)
  end (* function closureBoxed *)

(****************************************************************************
 *                 CLOSURE SHARING VIA THINNING                             *
 ****************************************************************************)

(* check if some free variables are really not necessary *)
fun shortenFree([],[],_) = ([],[])
  | shortenFree(gpfree,fpfree,cclist) =
      let fun g((v,free),l) =
            if member3 gpfree v then SL.merge(SL.rmv(v,free),l) else l
          val all = foldr g [] cclist
       in (removeV(all,gpfree),removeV(all,fpfree))
      end

(* check if ok to share with some closures in the enclosing environment *)
fun thinFree(vfree,vlen,closlist,limit) =
  let fun g(v,(l,m,n)) =
        if member3 vfree v then (v::l,m+1,n) else (l,m,n+1)
      fun h((v,cr as CR(_,{free,...})),x) =
        let val (zl,m,n) = foldr g ([],0,0) free
         in if m < limit then x else (v,zl,m*10000-n)::x
        end
      fun worse ((_,_,i), (_,_,j) : (lvar * lvar list * int)) = (i<j)
      fun m([],s,r,k) = (s,r)
        | m((v,x,_)::y,s,r,k) =
           if k < limit then (s,r)
           else (let val (nx,i,n,len) = accumV(x,r)
                  in if len < limit then m(y,s,r,k)
                     else m(y,addV([v],i,n,s),removeV(nx,r),k-len)
                 end)
(* !!! the following call to sort has issues !!! *)
(*
      val clist = ListMergeSort.sort worse (foldr h [] closlist)
*)
      val clist = oldSort worse (foldr h [] closlist)
   in m(clist,[],vfree,vlen)
  end

fun thinFpFree(free,closlist) = thinFree(free,length free,closlist,1)
fun thinGpFree(free,closlist) =
  let val len = length free
      val (spill,free) =
        if len <= 1 then ([],free)
        else thinFree(free,len,closlist,Int.min(3,len))
   in mergeV(spill,free)
  end

(* check if there is a closure containing all the free variables *)
fun thinAll([],_,_) = []
  | thinAll(free as [v],_,_) = free
  | thinAll(free,cclist,n) =
     let val vfree = map (fn (v,_,_) => v) free
         fun g((v,nfree),(x,y)) =
           if not (subset(vfree,nfree)) then (x,y)
           else (let val len = length(SL.difference(nfree,vfree))
                  in if len < y then (SOME v,len) else (x,y)
                 end)
         val (res,_) = foldr g (NONE,100000) cclist
      in case res of NONE => free
                   | SOME u => [(u,n,n)]
     end

(****************************************************************************
 * Generating the true free variables (freeAnalysis), each knownfunc is     *
 * replaced by its free variables and each continuation by its callee-save  *
 * registers. Finally, if two free variables are functions from the same    *
 * closure, just one of them is sufficient to access both.                  *
 ****************************************************************************)

fun sameClosureOpt(free,env) =
 case !CGoptions.closureStrategy
  of 1 => free (* flat without aliasing *)
   | 3 => free (* linked without aliasing *)
   | _ => (* all others have aliasing *)
       let fun g(v as (z,_,_))  = (v,whatIs(env,z))
           fun uniq ((hd as (v,Closure(CR(_,{stamp=s1,...}))))::tl) =
                let val m' = uniq tl
                    fun h(_,Closure(CR(_,{stamp=s2,...}))) = (s1 = s2)
                      | h _ = false
                 in if List.exists h m' then m' else (hd::m')
                end
             | uniq (hd::tl) = hd::uniq tl
             | uniq nil = nil
        in map #1 (uniq (map g free))
       end

fun freeAnalysis(gfree,ffree,env) =
  let fun g(w as (v,m,n),(x,y)) =
        case whatIs(env,v)
         of Callee(u,csg,csf) =>
              let val gv = addV(entervar(u,uniqvar csg),m,n,x)
                  val fv = addV(uniqvar csf,m,n,y)
               in (gv,fv)
              end
          | Function{gpfree,fpfree,...} =>
              (addV(gpfree,m,n,x),addV(fpfree,m,n,y))
          | _ => (mergeV([w],x),y)

      val  (ngfree,nffree) = foldr g ([],ffree) gfree
   in (sameClosureOpt(ngfree,env),nffree)
  end

(****************************************************************************
 *                  MAIN FUNCTION : closeCPS                                *
 ****************************************************************************)

fun closeCPS(fk,f,vl,cl,ce) = let

(****************************************************************************
 * utility functions that depends on register configurations                *
 ****************************************************************************)

(* get the current register configuration *)
val maxgpregs = MachSpec.numRegs
val maxfpregs = MachSpec.numFloatRegs - 2  (* need 1 or 2 temps *)
val numCSgpregs = MachSpec.numCalleeSaves
val numCSfpregs = MachSpec.numFloatCalleeSaves
val unboxedfloat = MachSpec.unboxedFloats

(* check the validity of the callee-save configurations *)
val (numCSgpregs,numCSfpregs) =
  if (numCSgpregs <= 0) then
    (if (numCSfpregs > 0) then bug "Wrong CS config 434 - closure.sml"
     else (0,0))
  else (if (numCSfpregs >= 0) then (numCSgpregs,numCSfpregs)
        else (numCSgpregs,0))

(* Initialize the base environment *)
val baseEnv = emptyEnv()

(* Find out the CPS type of an arbitrary program variable *)
fun get_cty v = (case whatIs(baseEnv,v) of Value t => t | _ => U.BOGt)

(* check if a variable is a float number *)
val isFlt = if unboxedfloat then
              (fn v => (case (get_cty v) of FLTt _ => true | _ => false))
            else (fn _ => false)
fun isFlt3 (v,_,_) = isFlt v

(* check if a variable is an untagged int type *)
fun isUntaggedInt (v,_,_) = (case get_cty v
       of NUMt{tag, ...} => not tag
        | _ => false
      (* end case *))

(* count the number of GP and FP registers needed for a list of lvars *)
fun isFltCty (FLTt _) = unboxedfloat
  | isFltCty _ = false

fun numgp (m,CNTt::z) = numgp(m-numCSgpregs-1,z)
  | numgp (m,x::z) = if isFltCty(x) then numgp(m,z) else numgp(m-1,z)
  | numgp (m,[]) = m

fun numfp (m,CNTt::z) = numfp(m-numCSfpregs,z)
  | numfp (m,x::z) = if isFltCty(x) then numfp(m-1,z) else numfp(m,z)
  | numfp (m,[]) = m

(****************************************************************************
 * adjustArgs checks the formal arguments of a function, replace the        *
 * continuation variable with a set of variables representing its callee-   *
 * save register environment variables.                                     *
 ****************************************************************************)

val adjustArgs =
  let fun adjust1(args,l,env) =
        let fun g((a,t),(al,cl,cg,cf,rt,env)) =
              if (t = CNTt) then
                (let val w = dupLvar a
                     val (csg,clg) = extraLvar(numCSgpregs,U.BOGt)
                     val (csf,clf) = extraLvar(numCSfpregs,FLTt 64)  (* REAL32: FIXME *)
                     val csgv = map VAR csg
                     val csfv = map VAR csf
                     val env = augCallee(a,VAR w,csgv,csfv,env)
                     val nargs = w::(csg@csf)
                     val ncl = CNTt::(clg@clf)
                     val env = faugValue(nargs,ncl,env)
                  in case rt
                      of NONE => (nargs@al,ncl@cl,csgv,csfv,SOME a,env)
                       | SOME _ => bug "closure/adjustArgs: >1 cont"
                 end)
              else (a::al,t::cl,cg,cf,rt,augValue(a,t,env))

         in foldr g (nil,nil,nil,nil,NONE,env) (zip(args,l))
        end

      fun adjust2(args,l,env) =
        let fun g((a,t),(al,cl,cg,cf,rt,env)) =
                  (a::al,t::cl,cg,cf,rt,augValue(a,t,env))
         in foldr g (nil,nil,nil,nil,NONE,env) (zip(args,l))
        end

 in if (numCSgpregs > 0) then adjust1 else adjust2
end

(****************************************************************************
 * FreeClose.freemapClose calculates the set of free variables and their    *
 * live range for each function binding. (check freeclose.sml)              *
 ****************************************************************************)

val ((fk,f,vl,cl,ce),snum,nfreevars,ekfuns) =
  FreeClose.freemapClose(fk,f,vl,cl,ce)

(* old freevars code, now obsolete, but left here for debugging *)
(* val (ofreevars,_,_) = FreeMap.freemapClose ce *)

(***************************************************************************
 * makenv: create the environments for functions in a FIX.                 *
 *    here bcsg and bcsf are the current contents of callee-save registers *
 *    bret is the default return continuations, sn is the stage number of  *
 *    the enclosing function, initEnv has the same "whatIs" table as the   *
 *    the baseEnv, however it has the different "whereIs" table.           *
 ***************************************************************************)
fun makenv(initEnv, bindings, bsn, bcsg, bcsf, bret) = let

(***>
fun checkfree(v) =
  let val free = ofreevars v
      val {fv=nfree,lv=loopv,sz=_} = nfreevars v
      val nfree = map #1 nfree
      val _ = if (free <> nfree)
              then (pr "^^^^ wrong free variable subset ^^^^ \n";
                    pr "OFree in "; vp v; pr ":"; ilist free;
                    pr "NFree in "; vp v; pr ":"; ilist nfree;
                    pr "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ \n")
              else ()
      val _ = case loopv
               of NONE => ()
                | SOME sfree =>
                    (if subset (sfree,nfree) then ()
                     else (pr "****wrong free variable subset*** \n";
                           pr "Free in "; vp v; pr ":"; ilist nfree;
                           pr "SubFree in "; vp v; pr ":";ilist sfree;
                           pr "*************************** \n"))
   in ()
  end
val _ = app checkfree (map #2 bindings)
<***)

(***>
val _ = COMMENT(fn() => (pr "BEGINNING MAKENV.\nFunctions: ";
           ilist (map #2 bindings); pr "Initial environment:\n";
           printEnv initEnv; pr "\n"))
val _ = COMMENT(fn() => (pr "BASE CALLEE SAVE REGISTERS: ";
           vallist bcsg; vallist bcsf; pr "\n"))
<***)

(* partition the function bindings into different fun_kinds *)
val (escapeB,knownB,recB,calleeB,kcontB) = partBindings(bindings)

(* For the "numCSgpregs = 0" case, treat kcontB and calleeB as escapeB *)
val (escapeB,calleeB,kcontB) =
  if (numCSgpregs > 0) then (escapeB,calleeB,kcontB)
  else (escapeB@calleeB,[],[])

val escapeV = SL.uniq(map #2 escapeB)
val knownV = SL.uniq(map #2 knownB)
fun knownlvar3(v,_,_) = SL.member knownV v

(*** check whether the basic closure assumptions are valid or not ***)
val (fixKind,nret) =
  case (escapeB,knownB,calleeB,recB,kcontB)
   of ([],_,[],_,[]) => (KNOWN,bret)
    | ([],[],[v],[],[_]) => (KNOWN_CONT,SOME(#2 v))
    | ([],[],[v],[],[]) => (CONT,SOME(#2 v))
    | (_,_,[],_,[]) => (ESCAPE,bret)
    | _ => (pr "^^^ Assumption No.2 is violated in closure phase  ^^^\n";
            pr "KNOWN bindings: "; ilist (map #2 knownB);
            pr "ESCAPE bindings: "; ilist (map #2 escapeB);
            pr "CONT bindings: "; ilist (map #2 calleeB);
            pr "KNOWN_CONT bindings: "; ilist (map #2 kcontB);
            pr "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ \n";
            bug "Violating basic closure conventions closure.sml")

(****************************************************************************
 * Initial processing of known functions                                    *
 ****************************************************************************)

(***>
val _ = COMMENT(fn() => (pr "Known functions:"; ilist (map #2 knownB);
                         pr "                "; iKlist (map #1 knownB)))
<***)

(*** Get the call graph of all known functions in this FIX. ***)
val knownB =
  map (fn (fe as (_,v,_,_,_)) =>
        let val {fv=vn,lv=lpv,sz=s} = nfreevars v
            val (fns,other) = partition knownlvar3 vn
         in ({v=v,fe=fe,other=other,fsz=s,lpv=lpv},length fns,fns)
        end) knownB

(*** Compute the closure of the call graph of the known functions. ***)
val knownB =
  let fun closeCallGraph g =
        let fun getNeighbors l =
              foldr (fn (({v,fe,other,fsz,lpv},_,nbrs),n) =>
                       if member3 l v then mergeV(nbrs,n) else n) l g
            fun traverse ((x,len,nbrs),(l,change)) =
              let val nbrs' = getNeighbors nbrs
                  val len' = length nbrs'
               in ((x,len',nbrs')::l,change orelse len<>len')
              end
            val (g',change) = foldr traverse (nil,false) g
         in if change then closeCallGraph g' else g'
        end
   in closeCallGraph knownB
  end

(*** Compute the closure of the set of free variables ***)
val knownB =
  let fun gatherNbrs l init =
        foldr (fn (({v,other,...},_,_),free) => case get_vn(l,v)
                 of NONE => free
                  | SOME(m,n) =>
                      mergeV(map (fn (z,i,j) =>
                                        (z,Int.min(i,m),Int.max(n,j))) other ,free))
             init knownB
   in map (fn ({v,fe=(k,_,args,cl,body),other,fsz,lpv},_,fns) =>
             {v=v,kind=k,args=args,cl=cl,body=body,lpv=lpv,fsz=fsz,
              other=gatherNbrs fns other,fns=fns}) knownB
  end

(*** See which known function requires a closure, pass 1. ***)
val (knownB,recFlag) = foldr
  (fn ((x as {v,kind,args,cl,other,fns,fsz,lpv,body}),(zz,flag)) =>
    let val free = removeV(escapeV,other)
        val callc = (length other) <> (length free) (* calls escaping-funs *)

        (* if its arguments doesn't contain a return cont, supply one *)
        val defCont = case (kind,bret)
           of (KNOWN_TAIL,SOME z) =>
                if (member3 free z) then bret else NONE (* issue warnings *)
            | _ => NONE

        (* find out the true set of free variables *)
        val (fpfree,gpfree) = partition isFlt3 free
        val (gpfree,fpfree) = freeAnalysis(gpfree,fpfree,initEnv)

(***>
val _ = COMMENT(fn() => (pr "*** Current Known Free Variables: ";
           iVlist gpfree; pr "\n"))
<***)

        (* some free variables must stay in registers for KNOWN_TAIL *)
        val (rcsg,rcsf) = case defCont
           of NONE => ([],[])
            | SOME k => fetchCSvars(k,#1 fsz,#2 fsz,initEnv)
        val gpfree = removeV(rcsg,gpfree)
        val fpfree = removeV(rcsf,fpfree)

        (* the stage number of the current function *)
        val sn = snum v
        fun deep1(_,_,n) = (n > sn)
        fun deep2(_,m,n) = (m > sn)

(***>
val _ = COMMENT(fn() => (pr "*** Current Stage number and fun kind: ";
           ilist [sn]; ifkind kind; pr "\n"))
<***)

        (* for recursive functions, always spill deeper level free variables *)
        val ((gpspill,gpfree),(fpspill,fpfree),nflag) = case lpv
          of SOME _ =>
               let fun h((v,_,_),l) =
                     case whatIs(initEnv,v)
                      of (Closure (CR(_,{free,...}))) => SL.merge(SL.rmv(v,free),l)
                       | _ => l
                   val gpfree = removeV(foldr h [] gpfree, gpfree)
                   val gpfreePart =
                         if length(gpfree) < numCSgpregs
                         then ([],gpfree)
                         else partition deep1 gpfree
                in (gpfreePart, partition deep1 fpfree,true)
               end
           | NONE => if ekfuns v then ((gpfree,[]),(fpfree,[]),flag)
                     else (partition deep2 gpfree,partition deep2 fpfree,flag)

(***>
val _ = COMMENT(fn() => (pr "*** Current Spilled Known Free Variables: ";
           iVlist gpspill; pr "\n"))
<***)

        (* find out the register limit for this known functions *)
        val (gpnmax,fpnmax) = (maxgpregs,maxfpregs) (* reglimit v *)

        (* check if the set of free variables fit into FP registers *)
        val n = Int.min(numfp(maxfpregs-1,cl),fpnmax) - length(rcsf)
        val (fpfree,fpspill) = spillFree(fpfree,n,rcsf,fpspill)

        (* check if the set of free variables fit into GP registers *)
        val m = Int.min(numgp(maxgpregs-1,cl),gpnmax) - length(rcsg)

        val (gpfree,gpspill) = spillFree(gpfree,m,rcsg,gpspill)

     in ((case (gpspill,fpspill)
           of ([],[]) => (x,gpfree,fpfree,[],[],callc,sn,fns)::zz
(*
            | ([(z,_,_)],[]) =>
                if callc then
                  ((x,gpfree,fpfree,gpspill,[],callc,sn,fns)::zz)
                else ((x,SL.enter(z,gpfree),fpfree,[],[],false,sn,fns)::zz)
 *)
            | _ => ((x,gpfree,fpfree,gpspill,fpspill,true,sn,fns)::zz)),nflag)
    end) ([],false) knownB

(* See which known functions require a closure, pass 2. *)
val (knownB,gpcollected,fpcollected) =
  let fun checkNbrs l init =
        foldr (fn (({v,...},_,_,_,_,callc,_,_),c) =>
                c orelse (callc andalso (member3 l v))) init knownB

      fun g(({kind,v,args,cl,body,fns,fsz,lpv,other},gpfree,fpfree,gpspill,
             fpspill,callc,sn,zfns),(z,gv,fv)) =
        let val callc = checkNbrs zfns callc
            val l = dupLvar v
         in ({kind=kind,sn=sn,v=v,l=l,args=args,cl=cl,body=body,gpfree=gpfree,
              fpfree=fpfree,callc=callc}::z,mergeV(gpspill,gv),
             mergeV(fpspill,fv))
        end
   in foldr g ([],[],[]) knownB
  end

(****************************************************************************
 * Initial processing of escaping functions                                 *
 ****************************************************************************)

(***>
val _ = COMMENT(fn() => (pr "Escaping functions:"; ilist (map #2 escapeB)))
<***)

(* get the set of free variables for escaping functions *)
val (escapeB,escapeFree) =
  let fun g((k,v,a,cl,b),(z,c)) =
       let val free = #fv (nfreevars v)
           val l = dupLvar v
        in ({kind=k,v=v,l=l,args=a,cl=cl,body=b}::z,mergeV(free,c))
       end
   in foldr g ([],[]) escapeB
  end

(* get the true set of free variables for escaping functions *)
val (gpfree,fpfree) =
  let val (fns,other) = partition knownlvar3 (removeV(escapeV,escapeFree))
      val (fpfree,gpfree) = partition isFlt3 other
      val (gpfree,fpfree) =
        foldr (fn ({v,gpfree=gv,fpfree=fv,...},(x,y)) =>
                (case get_vn(fns,v)
                  of NONE => (x,y)
                   | SOME (m,n) => (addV(gv,m,n,x),addV(fv,m,n,y))))
            (gpfree,fpfree) knownB
   in freeAnalysis(gpfree,fpfree,initEnv)
  end

(* here are all free variables that ought to be put in the closure *)
val gpFree = mergeV(gpfree,gpcollected)
val fpFree = mergeV(fpfree,fpcollected)


(***************************************************************************
 * Initial processing of callee-save continuation functions                *
 ***************************************************************************)

(***>
val _ = COMMENT(fn() => (pr "CS continuations:"; ilist (map #2 calleeB);
                         pr "                 "; iKlist (map #1 calleeB)))
<***)

(* get the set of free variables for continuation functions *)
val (calleeB,calleeFree,gpn,fpn,pF) =
  let fun g((k,v,a,cl,b),(z,c,gx,fx,pf)) =
       let val {fv=free,lv=_,sz=(gsz,fsz)} = nfreevars v
           val l = dupLvar v
           val sn = snum v
           val (gpn,fpn,pflag) = case k
             of KNOWN_CONT =>
                 if gsz > 0 then (0,0,false)   (* a temporary gross hack *)
                 else
                  (let val x = numgp(maxgpregs-1,CNTt::cl)
                       val y = numfp(maxfpregs-1,CNTt::cl)
                    in (Int.min(x,gx),Int.min(y,fx),false)
                   end)
              | _ => (0,0,sn = (bsn+1))

        in ({kind=k,sn=sn,v=v,l=l,args=a,cl=cl,body=b}::z,mergeV(free,c),
            Int.min(gpn,gx),Int.min(fpn,fx),pflag)
       end
   in case calleeB
       of [] => ([],[],0,0,true)
        | _ => foldr g ([],[],maxgpregs,maxfpregs,true) calleeB
  end

(* get the true set of free variables for continuation functions *)
val (fpcallee,gpcallee) = partition isFlt3 calleeFree
val (gpcallee,fpcallee) = freeAnalysis(gpcallee,fpcallee,initEnv)

(* get all sharable closures from the enclosing environment *)
val (gpclist,fpclist) =
  let val lives = SL.merge(map #1 gpcallee, map #1 gpFree)
      val lives = case (knownB,escapeB)
                   of ([{gpfree=gv,...}],[]) => SL.merge(gv,lives)
                    | _ => lives
   in fetchClosures(initEnv,lives,fixKind)
  end

(* initializing the callee-save register default *)
val safev = SL.merge(SL.uniq(map #1 gpclist),SL.uniq(map #1 fpclist))
val (gpbase,gpsrc) = mkbase(bcsg,SL.merge(safev,map #1 gpcallee),gpn)
val (fpbase,fpsrc) = mkbase(bcsf,map #1 fpcallee,fpn)

(* thinning the set of free variables based on each's contents *)
val cclist =  (* for user function, be more conservative *)
  case calleeB
   of [] => map (fn (v,cr) => (v,fetchFree(v,cr,2))) (fpclist@gpclist)
    | _ => map (fn (v,CR(_,{free,...})) => (v,free)) (fpclist@gpclist)

val (gpcallee,fpcallee) = shortenFree(gpcallee,fpcallee,cclist)
val (gpFree,fpFree) = if recFlag then (gpFree,fpFree)
                      else shortenFree(gpFree,fpFree,cclist)

(***************************************************************************
 * Targeting callee-save registers for continuation functions              *
 ***************************************************************************)

(* decide variables that are being put into FP callee-save registers *)
val (gpspill,fpspill,fpbase) =
  let val numv = length fpcallee
      val numr = numCSfpregs + fpn
   in if (numv <= numr) then
        (let val fpv = map #1 fpcallee
             val p = if pF then numr-numv else 0
             val (fpbase,fpv,_) = modifybase(fpbase,fpv,p)
             val nbase = fillbase(fpbase,fpv)
          in ([],[],nbase)
         end)
      else (* need spill *)
        (let val (gpfree,fpcallee) = thinFpFree(fpcallee,fpclist)
             val numv = length fpcallee
          in if (numv <= numr) then
               (let val fpv = map #1 fpcallee
                    val p = if pF then numr-numv else 0
                    val (fpbase,fpv,_) = modifybase(fpbase,fpv,p)
                    val nbase = fillbase(fpbase,fpv)
                 in (gpfree,[],nbase)
                end)
             else
               (let val fpfree = sortlud2(fpcallee,fpsrc)
                    val (cand,rest) = partvnum(fpfree,numr)
                    val (nbase,ncand,_) = modifybase(fpbase,cand,0)
                    val nbase = fillbase(nbase,ncand)
                 in (gpfree,uniqV rest,nbase)
                end)
         end)
  end

(* INT32: here is a place to filter out all the variables with INT32 types,
   they have to be put into closure (gpspill), because by default, callee-save
   registers always contain pointer values. *)
val (utgpcallee, gpcallee) = partition isUntaggedInt gpcallee
val (utgpFree, gpFree) = partition isUntaggedInt gpFree

(* collect all the FP free variables and build a closure if necessary *)
val allfpFree = mergeV(fpspill,fpFree)
val (gpspill,gpFree,fpcInfo) = case allfpFree
  of [] => (gpspill,gpFree,NONE)
   | _ => (let val (gpextra,ufree) = thinFpFree(allfpFree,fpclist)
               val (gpextra,fpc) =
                 case ufree
                  of [] => (gpextra,NONE)
                   | ((_,m,n)::r) =>
                       (let fun h((_,x,y),(i,j)) = (Int.min(x,i),Int.max(y,j))
                            val (m,n) =  foldr h (m,n) r
                            val cname = closureLvar()
                            val gpextra = mergeV([(cname,m,n)],gpextra)
                         in (gpextra,SOME(cname,ufree))
                        end)
            in case fixKind
                of (CONT|KNOWN_CONT) => (mergeV(gpextra,gpspill),gpFree,fpc)
                 | _ => (gpspill,mergeV(gpextra,gpFree),fpc)
           end)

(* here are free variables that should be put in GP callee-save registers *)
(* by convention: gpspill must not contain any int32 variables ! *)
val gpcallee = mergeV(gpspill,gpcallee)

val (gpcallee,fpcInfo) = case (utgpcallee, fpcInfo)
  of ([],_) => (gpcallee,fpcInfo)
   | ((_,m,n)::r,NONE) =>
       let fun h((_,x,y),(i,j)) = (Int.min(x,i),Int.max(y,j))
           val (m,n) =  foldr h (m,n) r
           val cname = closureLvar()
        in (mergeV([(cname,m,n)],gpcallee), SOME(cname,utgpcallee))
       end
   | (vs, SOME(cname, ufree)) => (gpcallee, SOME(cname, mergeV(vs, ufree)))
(*
   | (_,SOME (cname,ufree)) => bug "unimplemented int32 + float (nclosure.2)"
*)

(* if gpspill is not null, there must be an empty position in gpbase *)
val (gpspill,gpbase) =
  let val numv = length gpcallee
      val numr = numCSgpregs + gpn
   in if (numv <= numr) then
        (let val gpv = map #1 gpcallee
             val p = if pF then numr-numv else 0
             val (gpbase,gpv,_) = modifybase(gpbase,gpv,p)
             val nbase = fillbase(gpbase,gpv)
          in ([],nbase)
         end)
      else
        (let val gpcallee = thinGpFree(gpcallee,gpclist)
             val numv = length gpcallee
          in if (numv <= numr) then
               (let val gpv = map #1 gpcallee
                    val p = if pF then numr-numv else 0
                    val (gpbase,gpv,_) = modifybase(gpbase,gpv,p)
                    val nbase = fillbase(gpbase,gpv)
                 in ([],nbase)
                end)
             else
               (let val gpfree = sortlud2(gpcallee,gpsrc)
                    val (cand,rest) = partvnum(gpfree,numr-1)
                    val (nbase,ncand,_) = modifybase(gpbase,cand,0)
                    val (nbhd,nbtl) = partnull(nbase)
                    val nbtl = fillbase(nbtl,ncand)
                 in (uniqV rest,nbhd@nbtl)
                end)
         end)
  end

(***************************************************************************
 * Building the closures for all bindings in this FIX                      *
 ***************************************************************************)

(* collect all GP free variables that should be put in closures *)
(* assumption: gpspill does not contain any Int32s; they can should
               not be put into gpcallee anyway. *)
val allgpFree = mergeV(gpspill,gpFree)
val unboxedFree = utgpFree

(* filter out all unboxed-values *)
(* INT32: here is the place to filter out all 32-bit integers,
    put them into unboxedFree, then you have to find a way to put both
    32-bit integers and unboxed float numbers in the same record.
   Currently, I use RK_RAW64BLOCK to denote this kind of record_kind,
   you might want to put all floats ahead of all 32-bit ints. *)
(* val (allgpFree,unboxedFree) = partition isBoxed3 allgpFree *)

val (allgpFree,fpcInfo) =
  case (fpcInfo,unboxedFree)
   of (NONE,[]) => (allgpFree,fpcInfo)
    | (NONE,(_,m,n)::r) =>
         (let val c = closureLvar()
              fun h((_,x,y),(i,j)) = (Int.min(x,i),Int.max(y,j))
              val (m,n) = foldr h (m,n) r
           in (mergeV([(c,m,n)],allgpFree),SOME(c,unboxedFree))
          end)
    | (SOME(c,a),r) => (allgpFree,SOME(c,mergeV(a,r)))

(* actually building the closure for unboxed values *)
val (fphdr,env,nframes) =
      case fpcInfo
        of NONE => (fn ce => ce,initEnv,[])
         | SOME(c,a) => let val (int32a,a) = partition isUntaggedInt a
                         in closureUnboxed(c,int32a,a,fixKind,initEnv)
                        end

(* sharing with the enclosing closures if possible *)
val (allgpFree,ccl) =  (* for recursive function, be more conservative *)
  if recFlag then (thinAll(allgpFree,cclist,bsn),cclist)
  else (thinGpFree(allgpFree,gpclist),[])

(* actually building the closure for all GP (or boxed) values *)
val (closureInfo,closureName,env,gphdr,nframes) =
 case (escapeB,allgpFree)
  of ([],[]) => (NONE,NONE,env,fphdr,nframes)
   | ([],[(v,_,_)]) => (NONE,SOME v,env,fphdr,nframes)
   | _ =>
      (let val fns = map (fn {v,l,...} => (v,l)) escapeB
           val cn = closureLvar()
           val (hdr,env,cr,nf) = closureBoxed(cn,fns,allgpFree,fixKind,ccl,env)
        in (SOME cr,SOME cn,env,fphdr o hdr,nf@nframes)
       end)

(***************************************************************************
 * Final construction of the environment for each known function           *
 ***************************************************************************)

(* add new known functions to the environment (side-efffect) *)
val nenv = case closureName
  of NONE =>
      (foldr (fn ({v,l,gpfree,fpfree,...},env) =>
               augKnown(v,l,gpfree,fpfree,env)) env knownB)
   | SOME cname =>
      (foldr (fn ({v,l,gpfree,fpfree,callc,...},env) =>
               if callc then augKnown(v,l,SL.enter(cname,gpfree),fpfree,env)
               else augKnown(v,l,gpfree,fpfree,env)) env knownB)

val knownFrags : frags =
  let fun g({kind,sn,v,l,args,cl,body,gpfree,fpfree,callc},z) =
        let val env = baseEnv (* empty whereIs map but same whatMap as nenv *)
            val env = foldr augvar env gpfree
            val env = foldr augvar env fpfree
            val (ngpfree,env) =
              case (callc,closureName)
               of (false,_) => (inc CGoptions.knownGen; (gpfree,env))
                | (true,SOME cn) => (inc CGoptions.knownClGen;
                                     (SL.enter(cn,gpfree),augvar(cn,env)))
                | (true,NONE) => bug "unexpected 23324 in closure"

            val (nargs,ncl,ncsg,ncsf,nret,env) = adjustArgs(args,cl,env)
            val nargs = nargs @ ngpfree @ fpfree
            val ncl = ncl @ (map get_cty ngpfree) @ (map get_cty fpfree)

(***>
            val _ = COMMENT(fn () => (pr "\nEnvironment in known ";
                            vp v; pr ":\n"; printEnv env))
<***)
         in case nret
             of NONE => ((KNOWN,l,nargs,ncl,body,env,sn,bcsg,bcsf,bret)::z)
              | SOME _ => ((KNOWN,l,nargs,ncl,body,env,sn,ncsg,ncsf,nret)::z)
        end
   in foldr g [] knownB
  end

(***************************************************************************
 * Final construction of the environment for each escaping function        *
 ***************************************************************************)

(* the whatMap in nenv is side-effected with new escape bindings *)
val escapeFrags : frags =
  case (closureInfo,escapeB)
   of (_,[]) => []
    | (NONE,_) => bug "unexpected 23422 in closure"
    | (SOME cr,_) =>
       (let val env = baseEnv (* empty whereIs map but same whatMap as nenv *)
            fun f ({kind,v,l,args,cl,body},i) =
              let val myCname = v  (* my closure name *)
                  val env = augEscFun(myCname,i,cr,env)
                  val (nargs,ncl,ncsg,ncsf,nret,env) = adjustArgs(args,cl,env)
                  val nargs = mkLvar()::myCname::nargs
                  val ncl = U.BOGt::U.BOGt::ncl
                  val sn = snum v
(***>
                  val _ = COMMENT(fn () => (pr "\nEnvironment in escaping ";
                              vp v; pr ":\n";printEnv env))
<***)
               in inc CGoptions.escapeGen;  (* nret must not be NONE *)
                  case nret
                   of NONE => bug "no cont in escapefun in closure.sml"
                    | SOME _ => (kind,l,nargs,ncl,body,env,sn,ncsg,ncsf,nret)
              end
         in formap f escapeB
        end)

(***************************************************************************
 * Final construction of the environment for each callee-save continuation *
 ***************************************************************************)

(* the whatMap in nenv is side-effected with new callee bindings *)
val (nenv, calleeFrags : frags) =
  case calleeB
   of [] => (nenv, [])
    | _ =>
       (let val gpbase = case closureName
                          of NONE => gpbase
                           | SOME _ => fillCSregs(gpbase,closureName)

            val ncsg = map (fn (SOME v) => VAR v | NONE => tagInt 0) gpbase
            val ncsf = map (fn (SOME v) => VAR v | NONE => VOID) fpbase
            val (benv,nenv) = splitEnv(nenv,SL.member (freevCSregs(gpbase,nenv)))

            fun g({kind,sn,v,l,args,cl,body},z) =
              let val env = installFrames(nframes,benv)
                  val (nk,env,nargs,ncl,csg,csf) =
                    case kind
                     of CONT =>
                          (let val env = augCallee(v,LABEL l,ncsg,ncsf,env)
                               val (env,a,c) =
                                     fillCSformals(gpbase,fpbase,env,get_cty)
                            in (CONT,env,(mkLvar())::(a@args),U.BOGt::(c@cl),
                                ncsg,ncsf)
                           end)
                      | KNOWN_CONT =>
                          (let val (gfv,ffv,env) =
                                         varsCSregs(gpbase,fpbase,env)
                               val csg = cuttail(gpn,ncsg)
                               val csf = cuttail(fpn,ncsf)
                               val env = augKcont(v,l,gfv,ffv,csg,csf,env)
                               val gcl = map get_cty gfv
                               val fcl = map (fn _ => FLTt 64) ffv  (* REAL64: FIXME *)
                            in (KNOWN,env,args@gfv@ffv,cl@gcl@fcl,csg,csf)
                           end)
                      | _ => bug "calleeFrags in closure.sml 748"

                  val env = faugValue(args,cl,env)
(***>
                  val _ = COMMENT(fn () =>
                            (pr "\nEnvironment in callee-save continuation ";
                             vp v; pr ":\n"; printEnv env))
<***)
               in inc CGoptions.calleeGen;
                  (nk,l,nargs,ncl,body,env,sn,csg,csf,bret)::z
              end
         in (nenv,foldr g [] calleeB)
        end)

val frags = escapeFrags@knownFrags@calleeFrags

(***>
val _ = COMMENT(fn () => (pr "\nEnvironment after FIX:\n";
                          printEnv nenv; pr "MAKENV DONE.\n\n"));
<***)

in  (* body of makenv *)
    (gphdr,frags,nenv,nret)
end

(****************************************************************************
 *                         MAIN LOOP (closefix and close)                   *
 ****************************************************************************)

fun closefix(fk,f,vl,cl,ce,env,sn,csg,csf,ret) =
  ((fk,f,vl,cl,close(ce,env,sn,csg,csf,ret))
       handle Lookup(f,v,env) => (pr(concat["LOOKUP FAILS on ", f, " "]); vp v;
				pr "\nin environment:\n";
                                printEnv env;
                                pr "\nin function:\n";
                                PPCps.prcps ce;
                                bug "Lookup failure in cps/closure.sml"))

and close(ce,env,sn,csg,csf,ret) =
  case ce
   of FIX(fl,b) =>
       let val (hdr,frags,nenv,nret) = makenv(env,fl,sn,csg,csf,ret)
        in FIX(map closefix frags, hdr(close(b,nenv,sn,csg,csf,nret)))
       end
    | APP(f,args) =>
       let val obj = (case f of VAR v => whatIs(env,v) | _ => Value U.BOGt)
        in case obj
            of Closure(CR(off,{functions,...})) =>
                let val (env,h) = fixAccess([f],env)
                    val (nargs,env,nh) = fixArgs(args,env)
                    val (_,label) = List.nth(functions,off)
                    val call = APP(LABEL label,LABEL label::f::nargs)
                 in h(nh call)
                end
             | Function{label,gpfree,fpfree,csdef} =>
                let val free = map VAR (gpfree@fpfree)
                    val (nargs,env,h) = fixArgs(args@free,env)
                    val call = APP(LABEL label,nargs)
                 in h call
                end
             | Callee(label,ncsg,ncsf) =>
                let val nargs = ncsg@ncsf@args
                    val (env,h) = fixAccess(label::nargs,env)
                    val call = APP(label,label::nargs)
                 in h call
                end
             | Value t =>
                let val (env,h) = fixAccess([f],env)
                    val (nargs,env,nh) = fixArgs(args,env)
                    val l = mkLvar()
                    val call = SELECT(0,f,l,t,
                                 (APP(VAR l,(VAR l)::f::nargs)))
                 in h(nh call)
                end
        end
    | SWITCH(v,c,l) =>
       let val (env,header) = fixAccess([v],env)
        in header(SWITCH(v,c,map (fn c => close(c,env,sn,csg,csf,ret)) l))
       end
    | RECORD(k as RK_RAW64BLOCK,l,v,c) =>
       let val (env,header) = fixAccess(map #1 l,env)
           val env = augValue(v,U.BOGt,env)
        in header(RECORD(k,l,v,close(c,env,sn,csg,csf,ret)))
       end
    | RECORD(k,l,v,c) =>
       let val (hdr, env) = recordEl(k, l, v, env)
           val nc = hdr (close(c,augValue(v,U.BOGt,env),sn,csg,csf,ret))
        in nc
       end
    | SELECT(i,v,w,t,c) =>
       let val (env,header) = fixAccess([v],env)
           val nc = close(c,augValue(w,t,env),sn,csg,csf,ret)
        in header(SELECT(i,v,w,t,nc))
       end
    | OFFSET(i,v,w,c) => bug "OFFSET in pre-closure in cps/closure.sml"
    | BRANCH(i,args,c,e1,e2) =>
       let val (env,header) = fixAccess(args,env)
           val ne1 = close(e1,env,sn,csg,csf,ret)
           val ne2 = close(e2,env,sn,csg,csf,ret)
        in header(BRANCH(i,args,c,ne1,ne2))
       end
    | SETTER(i,args,e) =>
       let val (env,header) = fixAccess(args,env)
           val ne = close(e,env,sn,csg,csf,ret)
        in header(SETTER(i,args,ne))
       end
    | LOOKER(i,args,w,t,e) =>
       let val (env,header) = fixAccess(args,env)
           val ne = close(e,augValue(w,t,env),sn,csg,csf,ret)
        in header(LOOKER(i,args,w,t,ne))
       end
    | ARITH(i,args,w,t,e) =>
       let val (env,header) = fixAccess(args,env)
           val ne = close(e,augValue(w,t,env),sn,csg,csf,ret)
        in header(ARITH(i,args,w,t,ne))
       end
    | PURE(i,args,w,t,e) =>
       let val (env,header) = fixAccess(args,env)
           val ne = close(e,augValue(w,t,env),sn,csg,csf,ret)
        in header(PURE(i,args,w,t,ne))
       end
    | RCC(k,l,p,args,wtl,e) =>
       let val (env,header) = fixAccess(args,env)
	   val ne = close(e,foldl (fn((w, t), env) =>
				     augValue(w,t,env)) env wtl,
			  sn,csg,csf,ret)
        in header(RCC(k,l,p,args,wtl,ne))
       end

(***************************************************************************
 * Calling the "close" on the CPS expression with proper initializations   *
 ***************************************************************************)
val nfe =
  let val _ = if !CGoptions.staticprof then SProf.initfk() else ()
      val (nvl,ncl,csg,csf,ret,env) = adjustArgs(vl,cl,baseEnv)
      val env = augValue(f,U.BOGt,env)
      val nce = close(ce,env,snum f,csg,csf,ret)
   in (fk,mkLvar(),mkLvar()::f::nvl,U.BOGt::U.BOGt::ncl,nce)
  end

(* temporary hack: measuring static allocation sizes of closures *)
(* previous calls to incfk and initfk are also part of this hack *)
val _ = if !CGoptions.staticprof then SProf.reportfk() else ()

in  (* body of closeCPS *)
    UnRebind.unrebind nfe
end
end (* local of open *)
end (* functor Closure *)



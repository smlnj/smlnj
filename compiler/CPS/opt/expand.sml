(* expand.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature EXPAND =
  sig
    val expand : {
	    function : CPS.function,
	    bodysize : int,
	    unroll : bool,
	    afterClosure : bool,
	    do_headers : bool,
	    click : string -> unit
	  } -> CPS.function

  end (* signature EXPAND *)

structure Expand : EXPAND =
struct

local open CPS
      structure CG = Control.CG
      structure LV = LambdaVar
in

 fun inc (ri as ref i) = (ri := i+1)
 fun dec (ri as ref i) = (ri := i-1)

 fun map1 f (a,b) = (f a, b)

 fun sum f = let fun h [] = 0
		   | h (a::r) = f a + h r
	     in h
	     end

 fun muldiv(a,b,c) =   (* a*b/c, approximately, but guaranteed no overflow *)
     (a*b) div c
     handle Overflow => if a>b then muldiv(a div 2,b,c div 2)
			       else muldiv(a,b div 2,c div 2)

 fun sameName(x,VAR y) = LV.sameName(x,y)
   | sameName(x,LABEL y) = LV.sameName(x,y)
   | sameName _ = ()

 datatype mode = ALL | NO_UNROLL | UNROLL of int | HEADERS

fun expand {function=(fkind,fvar,fargs,ctyl,cexp),unroll,bodysize,click,afterClosure,do_headers} =
  let
   val clicked_any = ref false
   val click = fn z => (click z; clicked_any := true)
   val debug = !CG.debugcps (* false *)
   val debugprint = if debug then Control.Print.say else fn _ => ()
   val debugflush = if debug then Control.Print.flush else fn _ => ()
   val CGinvariant = !CG.invariant
   fun label v = if afterClosure then LABEL v else VAR v
   datatype info = Fun of {escape: int ref, call: int ref, size: int ref,
			   args: lvar list, body: cexp,
			   invariant: bool list ref, (* one for each arg *)
			   unroll_call: int ref, level: int,
			   within: bool ref}
		 | Arg of {escape: int ref, savings: int ref,
			   record: (int * lvar) list ref}
		 | Sel of {savings: int ref}
		 | Rec of {escape: int ref, size: int,
			   vars: (value * accesspath) list}
		 | Real
		 | Const
		 | Other

   fun copyLvar v = LV.dupLvar v

 local exception Expand
       val m : info LV.Tbl.hash_table = LV.Tbl.mkTable(128,Expand)
       val get' = LV.Tbl.lookup m
 in    val note = LV.Tbl.insert m
       fun get i = get' i handle Expand => Other
       fun discard_pass1_info() = LV.Tbl.clear m
 end
   fun getval (VAR v) = get v
     | getval (LABEL v) = get v
(* QUESTION: should we return Const for boxed ints? *)
     | getval (NUM{ty={tag=true, ...}, ...}) = Const
(*     | getval (REAL _) = Real*)
     | getval _ = Other
   fun call(v, args) = case getval v
			of Fun{call,within=ref false,...} => inc call
			 | Fun{call,within=ref true,unroll_call,
			       args=vl,invariant,...} =>
			     let fun g(VAR x :: args, x' :: vl, i::inv) =
				       (i andalso x=x') :: g(args,vl,inv)
				   | g( _ :: args, _ :: vl, i::inv) =
				       false :: g(args,vl,inv)
				   | g _ = nil
			     in  inc call; inc unroll_call;
				 invariant := g(args,vl,!invariant)
			     end
			 | Arg{savings,...} => inc savings
			 | Sel{savings} => inc savings
			 | _ => ()
   fun escape v = case getval v
		   of Fun{escape,...} => inc escape
		    | Arg{escape,...} => inc escape
		    | Rec{escape,...} => inc escape
		    | _ => ()
   fun escapeargs v = case getval v
		       of Fun{escape,...} => inc escape
			| Arg{escape,savings, ...} =>
				   (inc escape; inc savings)
			| Sel{savings} => inc savings
			| Rec{escape,...} => inc escape
			| _ => ()
   fun unescapeargs v = case getval v
			 of Fun{escape,...} => dec escape
			  | Arg{escape,savings, ...} =>
				      (dec escape; dec savings)
			  | Sel{savings} => dec savings
			  | Rec{escape,...} => dec escape
			  | _ => ()
   fun notearg v = (note (v,Arg{escape=ref 0,savings=ref 0, record=ref []}))
   fun noteother v = ()  (* note (v,Other) *)
   fun notereal v = noteother v  (* note (v,Real) *)
   fun enter level (_,f,vl,_,e) =
              (note(f,Fun{escape=ref 0, call=ref 0, size=ref 0,
			  args=vl, body=e, within=ref false,
			  unroll_call = ref 0,
			  invariant = ref(map (fn _ => CGinvariant) vl),
			  level=level});
	       app notearg vl)
   fun noterec(w, vl, size) = note (w,Rec{size=size,escape=ref 0,vars=vl})
   fun notesel(i,v,w) = (note (w, Sel{savings=ref 0});
			 case getval v
			  of Arg{savings,record,...} => (inc savings;
						  record := (i,w)::(!record))
			   | _ => ())

   fun setsize(f,n) =
       case get f of
	   Fun{size,...} => (size := n; n)
	 | _ => raise Fail "Expand: setsize: not a Fun"

   fun incsave(v,k) = case getval v
		    of Arg{savings,...} => savings := !savings + k
		     | Sel{savings} => savings := !savings + k
		     | _ => ()
   fun setsave(v,k) = case getval v
			of Arg{savings,...} => savings := k
			 | Sel{savings} => savings := k
			 | _ => ()
   fun savesofar v = case getval v
		      of Arg{savings,...} => !savings
		       | Sel{savings} => !savings
		       | _ => 0

   fun within f func arg =
        case get f of
	    Fun{within=w,...} => (w := true; func arg before (w := false))
	  | _ => raise Fail "Expand: within: f is not a Fun"

   val rec prim = fn (level,vl,e) =>
       let fun vbl(VAR v) = (case get v of Rec _ => 0 | _ => 1)
	     | vbl _ = 0
	   val nonconst = sum vbl vl
	   val sl = map savesofar vl
	   val afterwards = pass1 level e
	   val zl = map savesofar vl
	   val overhead = length vl + 1
	   val potential = overhead
	   val savings = case nonconst of
			   1 => potential
			 | 2 => potential div 4
			 | _ => 0
	   fun app3 f = let fun loop (a::b,c::d,e::r) = (f(a,c,e); loop(b,d,r))
			      | loop _ = ()
			in  loop
			end
       in  app3(fn (v,s,z)=> setsave(v,s + savings + (z-s))) (vl,sl,zl);
	   overhead+afterwards
       end

   and primreal = fn (level,(_,vl,w,_,e)) =>
       (notereal w;
	app (fn v => incsave(v,1)) vl;
	2*(length vl + 1) + pass1 level e)

 (*******************************************************************)
 (* pass1: gather info on code.                                     *)
 (*******************************************************************)
  and pass1 : int -> cexp -> int= fn level =>
    fn RECORD(_,vl,w,e) =>
        let val len = length vl
        in  app (escape o #1) vl;
	    noterec(w,vl,len);
	    2 + len + pass1 level e
	end
     | SELECT (i,v,w,_,e) => (notesel(i,v,w); 1 + pass1 level e)
     | OFFSET (i,v,w,e) => (noteother w; 1 + pass1 level e)
     | APP(f,vl) => (call(f,vl);
		     app escapeargs vl;
		     1 + ((length vl + 1) div 2))
     | FIX(l, e) =>
	  (app (enter level) l;
	   sum (fn (_,f,_,_,e) => setsize(f, within f (pass1 (level+1)) e)) l
                                             + length l + pass1 level e)
     | SWITCH(v,_,el) => let val len = length el
			     val jumps = 4 + len
			     val branches = sum (pass1 level) el
			 in  incsave(v, muldiv(branches,len-1,len) + jumps);
			     jumps+branches
			 end
     | BRANCH(_,vl,c,e1,e2) =>
       let fun vbl(VAR v) = (case get v of Rec _ => 0 | _ => 1)
	     | vbl _ = 0
	   val nonconst = sum vbl vl
	   val sl = map savesofar vl
	   val branches = pass1 level e1 + pass1 level e2
	   val zl = map savesofar vl
	   val overhead = length vl
	   val potential = overhead + branches div 2
	   val savings = case nonconst of
			   1 => potential
			 | 2 => potential div 4
			 | _ => 0
	   fun app3 f = let fun loop (a::b,c::d,e::r) = (f(a,c,e); loop(b,d,r))
			      | loop _ = ()
			in  loop
			end
       in  app3(fn (v,s,z)=> setsave(v,s + savings + (z-s) div 2)) (vl,sl,zl);
	   overhead+branches
       end
     | LOOKER(_,vl,w,_,e) => (noteother w; prim(level,vl,e))
     | SETTER(_,vl,e) => prim(level,vl,e)
     | ARITH(args as (P.REAL_TO_INT _, _,_,_,_)) => primreal (level,args)
     | ARITH(_,vl,w,_,e) => (noteother w; prim(level,vl,e))
(* REAL32: FIXME *)
     | PURE(P.PURE_ARITH{kind=P.FLOAT 64,...},[v],w,_,e) =>
	 (notereal w; incsave(v,1); 4+(pass1 level e))
(* REAL32: FIXME *)
     | PURE(P.INT_TO_REAL{to=64,...},vl,w,_,e) => (notereal w; prim(level,vl,e))
     | PURE(_,vl,w,_,e) => (noteother w; prim(level,vl,e))
     | RCC(k,l,p,vl,wtl,e) =>
         (app (noteother o #1) wtl; prim(level,vl,e)) (* ? *)


   (*********************************************************************)
   (* substiture(args,wl,e,alpha) : substitute args for wl in e.        *)
   (* If alpha=true, also rename all bindings.                          *)
   (*********************************************************************)
   fun substitute(args,wl,e,alpha) =
    let
	exception Alpha
        val vm: value LV.Tbl.hash_table = LV.Tbl.mkTable(16, Alpha)
        fun look (v,default) = getOpt (LV.Tbl.find vm v, default)
        val enter = LV.Tbl.insert vm
	fun use(v0 as VAR v) = look(v,v0)
	  | use(v0 as LABEL v) = look(v,v0)
	  | use x = x
	fun def v = if alpha
	             then let val w = copyLvar v
			  in  enter (v, VAR w); w
			  end
		     else v
	fun defl v = if alpha
	             then let val w = copyLvar v
			  in  enter (v, label w);
			       w
			  end
		     else v
	fun bind(a::args,w::wl) =
	       (sameName(w,a); enter (w,a); bind(args,wl))
	  | bind _ = ()

	val rec g =
       fn RECORD(k,vl,w,ce) => RECORD(k,map (map1 use) vl, def w, g ce)
	| SELECT(i,v,w,t,ce) => SELECT(i, use v, def w, t, g ce)
	| OFFSET(i,v,w,ce) => OFFSET(i, use v, def w, g ce)
	| APP(v,vl) => APP(use v, map use vl)
	| FIX(l,ce) =>
	  let (* Careful: order of evaluation is important here. *)
	      fun h1(fk,f,vl,cl,e) = (fk,defl f,vl,cl,e)
	      fun h2(fk,f',vl,cl,e) =
		  let val vl' = map def vl
		      val e'= g e
		  in  (fk,f',vl',cl,e')
		  end
	  in  FIX(map h2(map h1 l), g ce)
	  end
	| SWITCH(v,c,l) => SWITCH(use v, def c, map g l)
	| LOOKER(i,vl,w,t,e) => LOOKER(i, map use vl, def w, t, g e)
	| ARITH(i,vl,w,t,e) => ARITH(i, map use vl, def w, t, g e)
	| PURE(i,vl,w,t,e) => PURE(i, map use vl, def w, t, g e)
	| SETTER(i,vl,e) => SETTER(i, map use vl, g e)
	| RCC(k,l,p,vl,wtl,e) =>
	    RCC(k, l, p, map use vl, map (fn (w, t) => (def w, t)) wtl, g e)
	| BRANCH(i,vl,c,e1,e2) => BRANCH(i, map use vl, def c, g e1, g e2)

    in  bind(args,wl); g e
    end

   fun whatsave(acc, size, (v:value)::vl, a::al) =
       if acc>=size
       then acc
       else
       (case get a of
	  Arg{escape=ref esc,savings=ref save,record=ref rl} =>
	  let val (this, nvl: value list, nal) =
	       case getval v
		of Fun{escape=ref 1,...} =>
			(if esc>0 then save else 6+save,vl,al)
		 | Fun _ => (save,vl,al)
		 | Rec{escape=ref ex,vars,size} =>
		      let exception Chase
			  fun chasepath(v,OFFp 0) = v
			    | chasepath(v, SELp(i,p)) =
			       (case getval v
				 of Rec{vars,...} =>
					chasepath(chasepath(List.nth(vars,i)),p)
				  | _ => raise Chase)
			    | chasepath _ = raise Chase
			  fun loop([],nvl,nal) =
			      (if ex>1 orelse esc>0
			       then save
			       else save+size+2,nvl,nal)
			    | loop((i,w)::rl,nvl,nal) =
			       loop(rl,
				  chasepath(List.nth(vars,i))::nvl,
				    w::nal)
		      in  loop(rl,vl,al)
			  handle Chase => (0,vl,al)
			       | Subscript => (0,vl,al)
		      end
		(* | Real => (save,vl,al)*)
		 | Const => (save,vl,al)
		 | _ => (0,vl,al)
	  in  whatsave(acc+this - muldiv(acc,this,size), size, nvl,nal)
	  end
	| Sel{savings=ref save} =>
	  let val this =
	      case v
	       of VAR v' => (case get v' of
			      Fun _ => save
			    | Rec _ => save
			    | _ => 0)
		| _ => save
	  in  whatsave(acc + this - muldiv(acc,this,size),size, vl,al)
	  end
	| _ => raise Fail "Expand: whatsave: not Arg nor Sel")
     | whatsave(acc,size,_,_) = acc


    (*************************************************************
     * should_expand: should a function application be inlined?  *
     *************************************************************)
    fun should_expand(d,  (* path length from entry to current function *)
		      u,  (* unroll level *)
		      e as APP(v,vl),
		      Fun{escape,call,unroll_call,size=ref size,args,body,
			  level,within=ref within,...}) =
    if !call + !escape = 1 then false else
      let val stupidloop =  (* prevent infinite loops  at compile time *)
	    case (v,body)
	     of (VAR vv, APP(VAR v',_)) => vv=v'
	      | (LABEL vv, APP(LABEL v',_)) => vv=v'
	      | _ => false
	val calls = case u of UNROLL _ => !unroll_call | _ => !call
	val small_fun_size = case u of UNROLL _ => 0 | _ => 50
	val savings = whatsave(0,size,vl,args)
	val predicted =
	    let val real_increase = size-savings-(1+length vl)
	    in  real_increase * calls -
		(* don't subtract off the original body if
		   the original body is huge (because we might
		   have guessed wrong and the consequences are
		   too nasty for big functions); or if we're
		   in unroll mode *)
		(if size < small_fun_size then size else 0)
	    end
	val depth = 2 and max = 2

    in  if false andalso debug
	    then (PPCps.prcps e;
		  debugprint(Int.toString predicted); debugprint "   ";
		  debugprint(Int.toString bodysize); debugprint "\n")
	else ();

       not stupidloop
       andalso case u
	    of UNROLL lev =>
		 (* Unroll if: the loop body doesn't make function
		    calls orelse "unroll_recur" is turned on; andalso
		    we are within the definition of the function;
		    andalso it looks like things won't grow too much.
		  *)
		   (!CG.unroll_recur orelse level >= lev)
		   andalso within andalso predicted <= bodysize
	     | NO_UNROLL =>
		   !unroll_call = 0 andalso
		   not within andalso
		   (predicted <= bodysize
		     orelse (!escape=0 andalso calls = 1))
	     | HEADERS => false  (* shouldn't get here *)
	     | ALL =>
		   (predicted <= bodysize
		     orelse (!escape=0 andalso calls = 1))
  end
 | should_expand _ = raise Fail "Expand: should_expand: unexpected argument"

   datatype decision = YES of {formals: lvar list, body: cexp}
                     | NO of int  (* how many no's in a row *)
   (* There is really no point in making decisions a ref.  This should
      be changed one day. *)
   val decisions : decision list ref = ref nil
   fun decide_yes x = decisions := YES x :: !decisions
   fun decide_no () = decisions :=
       (case !decisions
         of NO n :: rest => NO(n+1) :: rest
          | d => NO 1 :: d)


   (*********************************************************************)
   (* pass2: mark function applications to be inlined.                  *)
   (*********************************************************************)
   fun pass2(d, (* path length from start of current function *)
	     u, (* unroll-info *)
	     e (* expression to traverse *)
	     ) = case e

    of RECORD(k,vl,w,ce) => pass2(d+2+length vl,u,ce)
     | SELECT(i,v,w,t,ce) => pass2(d+1,u,ce)
     | OFFSET(i,v,w,ce) => pass2(d+1,u,ce)
     | APP(v,vl) =>
	 (case getval v
	   of info as Fun{args,body,...} =>
	       if should_expand(d,u,e,info)
		   then decide_yes{formals=args,body=body}
		    else decide_no()
	    | _ => decide_no())
     | FIX(l,ce) =>
	   let fun fundef (NO_INLINE_INTO,_,_,_,_) = ()
		 | fundef (fk,f,vl,cl,e) =
		   (case get f of
			Fun{level,within,escape=ref escape,...} =>
			let val u' = case u of UNROLL _ => UNROLL level
					     | _ => u
			    fun conform((VAR x)::r,z::l) =
				(x=z) andalso conform(r,l)
			      | conform(_::r,_::l) = false
			      | conform([],[]) = true
			      | conform _ = false
			in
			    within := true;
			    pass2(0,u',e)
			    before within := false
			end
		      | _ => ())	(* cannot happen *)
	   in  app fundef l;
	       pass2(d+length l,u,ce)
	   end
     | SWITCH(v,c,l) => app (fn e => pass2(d+2,u,e)) l
     | (LOOKER(_,_,_,_,e) |
	ARITH(_,_,_,_,e) |
	PURE(_,_,_,_,e) |
	SETTER(_,_,e) |
	RCC(_,_,_,_,_,e)) => pass2(d+2,u,e)
     | BRANCH(i,vl,c,e1,e2) => (pass2(d+2,u,e1);
				pass2(d+2,u,e2))


   val rec gamma =
    fn RECORD(k,vl,w,ce) => RECORD(k,vl, w, gamma ce)
     | SELECT(i,v,w,t,ce) => SELECT(i, v, w, t, gamma ce)
     | OFFSET(i,v,w,ce) => OFFSET(i, v, w, gamma ce)
     | e as APP(v,vl) => e
     | FIX(l,ce) =>
	   let fun fundef (z as (NO_INLINE_INTO,_,_,_,_)) = z
		 | fundef (z as (fk,f,vl,cl,e)) =
		   (case get f of
			Fun{escape=ref escape,call,unroll_call,
			    invariant=ref inv,...} =>
			if escape = 0 andalso !unroll_call > 0
			   andalso (!call - !unroll_call > 1
				    orelse List.exists (fn t=>t) inv)
			then let val f' = copyLvar f
				 val vl' = map copyLvar vl
				 fun drop(false::r,a::s) = a::drop(r,s)
				   | drop(true::r,_::s) = drop(r,s)
				   | drop _ = nil
				 val newformals=
				     label f' :: map VAR (drop(inv,vl'))
				 val e' =substitute(newformals,
						    f :: drop(inv,vl),
						    gamma e,
						    false)
			     in
				 click "!"; debugprint(LV.prLvar f);
				 enter 0 (fk,f',vl',cl,e');
				 (fk,f,vl,cl,FIX([(fk,f',vl',cl,e')],
						 APP(label f', map VAR vl)))
			     end
			else (fk,f,vl,cl,gamma e)
		      | _ => z)		(* cannot happen *)
           in  FIX(map fundef l, gamma ce)
           end
     | SWITCH(v,c,l) => SWITCH(v, c, map gamma l)
     | LOOKER(i,vl,w,t,e) => LOOKER(i, vl, w, t, gamma e)
     | ARITH(i,vl,w,t,e) => ARITH(i, vl, w, t, gamma e)
     | PURE(i,vl,w,t,e) => PURE(i, vl, w, t, gamma e)
     | SETTER(i,vl,e) => SETTER(i, vl, gamma e)
     | RCC(k,l,p,vl,wtl,e) => RCC(k, l, p, vl, wtl, gamma e)
     | BRANCH(i,vl,c,e1,e2) => BRANCH(i, vl, c,gamma e1, gamma e2)


   val rec beta =
    fn RECORD(k,vl,w,ce) => RECORD(k,vl,w,beta ce)
     | SELECT(i,v,w,t,ce) => SELECT(i,v,w,t,beta ce)
     | OFFSET(i,v,w,ce) => OFFSET(i,v,w,beta ce)
     | e as APP(v,vl) =>
	   (case !decisions
	     of YES{formals,body}::rest =>
		 (click "^";
                  case v of VAR vv => debugprint(LV.prLvar vv) | _ => ();
		  debugflush();
		  decisions := rest;
		  substitute(vl,formals,body,true))
              | NO 1::rest => (decisions := rest; e)
              | NO n :: rest => (decisions := NO(n-1)::rest; e)
	      | [] => e (* cannot happen *))
     | FIX(l,ce) =>
	   let fun fundef (z as (NO_INLINE_INTO,_,_,_,_)) = z
		 | fundef (fk,f,vl,cl,e) = (fk,f,vl,cl,beta e)
	   in  FIX(map fundef l, beta ce)
	   end
     | SWITCH(v,c,l) => SWITCH(v,c,map beta l)
     | LOOKER(i,vl,w,t,e) => LOOKER(i,vl,w,t,beta e)
     | ARITH(i,vl,w,t,e) => ARITH(i,vl,w,t,beta e)
     | PURE(i,vl,w,t,e) => PURE(i,vl,w,t,beta e)
     | SETTER(i,vl,e) => SETTER(i,vl,beta e)
     | RCC(k,l,p,vl,wtl,e) => RCC(k,l,p,vl,wtl,beta e)
     | BRANCH(i,vl,c,e1,e2) => BRANCH(i,vl,c,beta e1,beta e2)



   fun pass2_beta(mode,e) =
       (pass2(0,mode,e);
	discard_pass1_info();
	debugprint "Expand: finishing pass2\n"; debugflush();
	case !decisions
         of [NO _] => (debugprint "No expansions to do.\n"; debugflush();
		       e)
	  | _ => (decisions := rev(!decisions);
		  debugprint "Beta: ";
		  beta e
		  before
		  (debugprint "\n"; debugflush())))

   val gamma = fn c =>
       (debugprint "Gamma: ";
	gamma c
	before
	(debugprint "\n"; debugflush()))

  in  (* body of expand *)
      notearg fvar;
      app notearg fargs;
(***>
      if !CG.printit then PPCps.prcps cexp
	  else ();
<***)
      debugprint("Expand: pass1: ");
      debugprint(Int.toString(pass1 0 cexp));
      debugprint "\n";
      debugflush();

      if unroll
	 then let val _ = (debugprint(" (unroll)\n"); debugflush());
		  val e' = pass2_beta(UNROLL 0,cexp)
	      in  if !clicked_any
		      then expand{function=(fkind,fvar,fargs,ctyl,e'),
				  bodysize=bodysize,click=click,unroll=unroll,
				  afterClosure=afterClosure,
				  do_headers=do_headers}
		  else ((*debugprint("\nExpand\n");
		         debugflush();
			 (fkind,fvar,fargs,ctyl,pass2_beta(ALL,cexp)) *)
			(fkind,fvar,fargs,ctyl,e'))
	      end
      else if !CG.unroll
	 then let val _ = (debugprint(" (headers)\n"); debugflush())
		  val e' = if do_headers then gamma cexp else cexp
	      in  if !clicked_any
		  then expand{function=(fkind,fvar,fargs,ctyl,e'),
                              bodysize=bodysize,click=click,
                              unroll=unroll,afterClosure=afterClosure,
                              do_headers=false}
		  else (debugprint(" (non-unroll 1)\n"); debugflush();
			(fkind,fvar,fargs,ctyl,pass2_beta(NO_UNROLL,e')))
	      end
      else (debugprint(" (non-unroll 2)\n"); debugflush();
	    (fkind,fvar,fargs,ctyl,pass2_beta(ALL,cexp)))
  end

end (* toplevel local *)
end (* functor Expand *)


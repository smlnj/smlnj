(* ppcps.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PPCPS =
  sig

    val value2str : CPS.value -> string

    val vpathToString : CPS.value * CPS.accesspath -> string

    val rkToString : CPS.record_kind -> string

    val printcps : (CPS.function * Lty.lty LambdaVar.Tbl.hash_table) -> unit
    val printcps0: CPS.function -> unit
    val prcps : CPS.cexp -> unit

  (* string conversions for various CPS.P types *)
    val numkindToString : CPS.P.numkind -> string
    val arithopToString : CPS.P.arithop -> string
    val pureopToString : CPS.P.pureop -> string
    val cmpopToString : CPS.P.cmpop -> string
    val fcmpopToString : CPS.P.fcmpop -> string
    val branchToString : CPS.P.branch -> string
    val setterToString : CPS.P.setter -> string
    val lookerToString : CPS.P.looker -> string
    val arithToString : CPS.P.arith -> string
    val pureToString : CPS.P.pure -> string

  end (* signature PPCPS *)

structure PPCps : PPCPS =
  struct

    structure LV = LambdaVar
    structure U = CPSUtil
    (* uses Lty and LtyBasic *)

    open CPS

    val say = Control.Print.say

    val sayt = say o U.ctyToString

    fun value2str (VAR v) = LV.lvarName v
      | value2str (LABEL v) = "(L)" ^ LV.lvarName v
      | value2str (NUM{ival, ty={sz=0, ...}}) = "(II)" ^ IntInf.toString ival
      | value2str (NUM{ival, ty={sz, tag=true}}) = concat[
	    "(I", Int.toString sz, "t)", IntInf.toString ival
	  ]
      | value2str (NUM{ival, ty={sz, ...}}) = concat[
	    "(I", Int.toString sz, ")", IntInf.toString ival
	  ]
      | value2str (REAL{rval, ty}) = concat[
	    "(R", Int.toString ty, ")", RealLit.toString rval
	  ]
      | value2str (STRING s) = concat["\"", String.toString s, "\""]
      | value2str (VOID) = "(void)"

    fun numkindToString (P.INT bits) = "i" ^ Int.toString bits
      | numkindToString (P.UINT bits) = "u" ^ Int.toString bits
      | numkindToString (P.FLOAT bits) = "f" ^ Int.toString bits

    val arithopToString = ArithOps.arithopToString

    val pureopToString = ArithOps.pureopToString

    val cmpopToString = ArithOps.cmpopToString

    fun fcmpopToString P.F_EQ   = "="
      | fcmpopToString P.F_ULG = "?<>"
      | fcmpopToString P.F_GT = ">"
      | fcmpopToString P.F_GE = ">="
      | fcmpopToString P.F_LT = "<"
      | fcmpopToString P.F_LE = "<="
      | fcmpopToString P.F_LG = "<>"
      | fcmpopToString P.F_LEG = "<="
      | fcmpopToString P.F_UGT = "?>"
      | fcmpopToString P.F_UGE = "?>="
      | fcmpopToString P.F_ULT = "?<"
      | fcmpopToString P.F_ULE = "?<="
      | fcmpopToString P.F_UE = "?="
      | fcmpopToString P.F_UN = "?"

    fun branchToString (P.CMP{oper, kind}) = numkindToString kind ^ cmpopToString oper
      | branchToString (P.FCMP{oper, size}) = numkindToString (P.FLOAT size) ^ fcmpopToString oper
      | branchToString (P.FSGN sz) = numkindToString (P.FLOAT sz) ^ "sgn"
      | branchToString P.BOXED = "boxed"
      | branchToString P.UNBOXED = "unboxed"
      | branchToString P.PEQL = "peql"
      | branchToString P.PNEQ = "pneq"
      | branchToString (P.STREQL s) = concat["streql(\"", String.toString s, "\")"]

    fun setterToString P.UNBOXEDUPDATE = "unboxedupdate"
      | setterToString P.UPDATE = "update"
      | setterToString (P.NUMUPDATE{kind}) = ("numupdate" ^ numkindToString kind)
      | setterToString P.UNBOXEDASSIGN = "unboxedassign"
      | setterToString P.ASSIGN = "assign"
      | setterToString P.SETHDLR = "sethdlr"
      | setterToString P.SETVAR = "setvar"
      | setterToString P.SETSPECIAL = "setspecial"
      | setterToString (P.RAWSTORE{kind}) = ("rawstore" ^ numkindToString kind)
      | setterToString (P.RAWUPDATE cty) = ("rawupdate" ^ U.ctyToString cty)

    fun lookerToString P.DEREF = "!"
      | lookerToString P.GETHDLR = "gethdlr"
      | lookerToString P.SUBSCRIPT = "subscript"
      | lookerToString (P.NUMSUBSCRIPT{kind}) = ("numsubscript" ^ numkindToString kind)
      | lookerToString P.GETVAR = "getvar"
      | lookerToString P.GETSPECIAL = "getspecial"
      | lookerToString (P.RAWLOAD{kind}) = ("rawload" ^ numkindToString kind)

    val cvtParam = Int.toString
    fun cvtParams (prefix, from, to) = concat [prefix, cvtParam from, "_", cvtParam to]

    fun arithToString (P.IARITH{oper, sz}) = arithopToString oper ^ cvtParam sz
      | arithToString (P.TEST{from, to}) = cvtParams ("test_", from, to)
      | arithToString (P.TESTU{from, to}) = cvtParams ("testu_", from, to)
      | arithToString (P.TEST_INF i) = "test_inf_" ^ cvtParam i
      | arithToString (P.REAL_TO_INT{floor, from, to}) = concat[
	    if floor then "floor_" else "round_", cvtParam from, "to", cvtParam to
	  ]

    fun pureToString P.LENGTH = "length"
      | pureToString (P.PURE_ARITH{oper,kind}) = pureopToString oper ^ numkindToString kind
      | pureToString P.OBJLENGTH = "objlength"
      | pureToString P.MAKEREF = "makeref"
      | pureToString (P.EXTEND{from, to}) = cvtParams ("extend_", from, to)
      | pureToString (P.COPY{from, to}) = cvtParams ("copy_", from, to)
      | pureToString (P.TRUNC{from, to}) = cvtParams ("trunc_", from, to)
      | pureToString (P.TRUNC_INF i) = "trunc_inf_" ^ cvtParam i
      | pureToString (P.COPY_INF i) = concat ["copy_", cvtParam i, "_inf"]
      | pureToString (P.EXTEND_INF i) =  concat ["extend_", cvtParam i, "_inf"]
      | pureToString (P.INT_TO_REAL{from, to}) =
	  concat ["real", cvtParam from, "_", cvtParam to]
      | pureToString P.SUBSCRIPTV = "subscriptv"
      | pureToString (P.PURE_NUMSUBSCRIPT{kind}) = "numsubscriptv" ^ numkindToString kind
      | pureToString P.GETTAG = "gettag"
      | pureToString P.MKSPECIAL = "mkspecial"
      | pureToString P.CAST = "cast"
      | pureToString P.GETCON = "getcon"
      | pureToString P.GETEXN = "getexn"
      | pureToString P.BOX = "box"
      | pureToString P.UNBOX = "unbox"
      | pureToString (P.WRAP kind) = "wrap_" ^ numkindToString kind
      | pureToString (P.UNWRAP kind) = "unwrap_" ^ numkindToString kind
      | pureToString P.GETSEQDATA = "getseqdata"
      | pureToString P.RECSUBSCRIPT = "recsubscript"
      | pureToString P.RAW64SUBSCRIPT = "raw64subscript"
      | pureToString P.NEWARRAY0 = "newarray0"
      | pureToString (P.RAWRECORD rk) = "rawrecord_"^getOpt(Option.map rkToString rk, "notag")

    and rkToString rk = (case rk
	   of RK_VECTOR => "RK_VECTOR"
	    | RK_RECORD => "RK_RECORD"
	    | RK_ESCAPE => "RK_ESCAPE"
	    | RK_CONT => "RK_CONT"
	    | RK_FCONT => "RK_FCONT"
	    | RK_KNOWN => "RK_KNOWN"
	    | RK_RAW64BLOCK => "RK_RAW64BLOCK"
	    | RK_RAWBLOCK => "RK_RAWBLOCK"
	  (* end case *))

    fun vpathToString (v, p) = let
	  fun toList (OFFp 0) = []
	    | toList (OFFp n) = ["+", Int.toString n] (* assumes n > 0 *)
	    | toList (SELp(i, p)) = "." :: toList p
	  in
	    String.concat(value2str v :: toList p)
	  end

    fun show0 say = let
	  fun sayc (#"\n") = say "\\n"
	    | sayc c = say(String.str c)

	  fun sayv v = say(value2str v)

	  fun sayvlist [v] = sayv v
	    | sayvlist nil = ()
	    | sayvlist (v::vl) = (sayv v; say ","; sayvlist vl)

	  fun sayrk (RK_RECORD, n) = ()
	    | sayrk (RK_VECTOR, n) = ()
	    | sayrk (k, n) = (say (rkToString k); say " "; say (Int.toString n); say ",")

	  fun sayparam ([v],[ct]) = (sayv v; sayt ct)
	    | sayparam (nil,nil) = ()
	    | sayparam (v::vl,ct::cl) = (sayv v; sayt ct; say ","; sayparam(vl,cl))
	    | sayparam _ = ErrorMsg.impossible "sayparam in ppcps.sml"

	  fun sayvp (v,path) = say(vpathToString(v, path))
	  fun saylist f [x] = f x | saylist f nil = ()
	    | saylist f (x::r) = (f x; say ","; saylist f r)
	  fun indent n = let
		fun space 0 = () | space k = (say " "; space(k-1))
		fun nl() = say "\n"
		fun f (RECORD(k,vl,v,c)) = (
			space n;
			case k of RK_VECTOR => say "#{" | _ => say "{";
			sayrk(k,length vl);
			saylist sayvp vl; say "} -> ";
			sayv(VAR v);
			nl(); f c)
		  | f (SELECT(i,v,w,t,c)) = (
			space n; sayv v; say "."; say(Int.toString i); say " -> ";
			 sayv(VAR w); sayt(t); nl(); f c)
		  | f (OFFSET(i,v,w,c)) = (
			space n; sayv v; say "+"; say(Int.toString i); say " -> ";
			sayv(VAR w); nl(); f c)
		  | f (APP(w,vl)) = (
			space n; sayv w; say "("; sayvlist vl; say ")\n")
		  | f (FIX(bl,c)) = let
			fun g (_,v,wl,cl,d) = (
				space n; sayv(VAR v); say "(";
				sayparam (map VAR wl,cl);
				say ") =\n";
				indent (n+3) d)
			in
			  app g bl; f c
			end
		  | f (SWITCH(v,c,cl)) = let
			fun g (i,c::cl) = (
			      space(n+1); say(Int.toString(i:int));
			      say " =>\n"; indent (n+3) c; g(i+1,cl))
			  | g (_,nil) = ()
			in
			  space n; say "case "; sayv v; say "  [";
			  say(LV.prLvar c);
			  say "] of\n";
			  g(0,cl)
			end
		  | f (LOOKER(i,vl,w,t,e)) = (
			space n; say(lookerToString i); say "("; sayvlist vl;
			say ") -> "; sayv(VAR w); sayt(t); nl(); f e)
		  | f (ARITH(i,vl,w,t,e)) = (
			space n; say(arithToString i); say "("; sayvlist vl;
			say ") -> "; sayv(VAR w); sayt(t); nl(); f e)
		  | f (PURE(i,vl,w,t,e)) = (
			space n; say(pureToString i); say "("; sayvlist vl;
			say ") -> "; sayv(VAR w); sayt(t); nl(); f e)
		  | f (SETTER(i,vl,e)) = (
			space n; say(setterToString i); say "("; sayvlist vl;
			say ")"; nl(); f e)
		  | f (BRANCH(i,vl,c,e1,e2)) = (
			space n; say "if "; say(branchToString i);
			say "("; sayvlist vl; say ") [";
			sayv(VAR c); say "] then\n";
			indent (n+3) e1;
			space n; say "else\n";
			indent (n+3) e2)
		  | f (RCC(k,l,p,vl,wtl,e)) = (
			space n;
			if k then say "reentrant " else ();
			if l = "" then () else (say l; say " ");
			say "rcc("; sayvlist vl; say ") -> ";
			app (fn (w, t) => (sayv (VAR w); sayt(t))) wtl;
			nl(); f e)
		in
		  f
		end
	  in
	    indent
	  end (* show0 *)

    fun printcps ((fk,f,vl,cl,e),m) = let
	  fun ptv(v,t) = (say(LV.lvarName v); say " type ===>>>";
			  say(LtyBasic.lt_print t); say "\n")
	  val _ = if (!Control.CG.debugRep)
		  then (say "************************************************\n";
			LV.Tbl.appi ptv m;
			say "************************************************\n")
		  else ()
	  fun sayv(v) = say(LV.lvarName v)
	  fun sayparam ([v],[ct]) = (sayv v; sayt ct)
	    | sayparam (nil,nil) = ()
	    | sayparam (v::vl,ct::cl) = (sayv v; sayt ct; say ","; sayparam(vl,cl))
	    | sayparam _ = ErrorMsg.impossible "sayparam in ppcps.sml 3435"
	  in
	    case fk
	     of CONT => say "std_cont "
	      | KNOWN => say "known "
	      | KNOWN_REC => say "known_rec "
	      | KNOWN_CHECK => say "known_chk "
	      | KNOWN_TAIL => say "known_tail "
	      | KNOWN_CONT => say "known_cont "
	      | ESCAPE => say "std "
	      | NO_INLINE_INTO => ()
	    (* end case *);
	    say(LV.lvarName f); say "("; sayparam(vl,cl); say ") =\n";
	    show0 say 3 e
	  end

    exception NULLTABLE
    val nulltable : Lty.lty LV.Tbl.hash_table =
	  LV.Tbl.mkTable(8, NULLTABLE)

    fun printcps0 f = printcps(f,nulltable)

    fun prcps(ce) = show0 (Control.Print.say) 1 ce

  end (* structure PPCps *)

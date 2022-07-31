(* chkplexp.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature CHKPLEXP =
sig

exception ChkPlexp (* PLambda type check error *)

val checkLtyTop : PLambda.lexp * int -> bool
val checkLty : PLambda.lexp * LtyBasic.ltyEnv * int -> bool
val newlam_ref : PLambda.lexp ref
val fname_ref : string ref

end (* signature CHKPLEXP *)

structure ChkPlexp : CHKPLEXP =
struct

local
  structure DI = DebIndex
  structure LV = LambdaVar
  structure DA = Access
  structure LT = Lty
  structure LK = LtyKernel
  structure LD = LtyDef
  structure LB = LtyBasic
  structure LE = LtyExtern  (* == PLambdaType *)
  structure LKC = LtyKindChk
  open PLambda
in

exception ChkPlexp (* PLambda type check error *)

val debugging = ref true

(*** a hack of printing diagnostic output into a separate file ***)
val newlam_ref : PLambda.lexp ref = ref (RECORD[])
val fname_ref : string ref = ref "yyy"

fun bug s = ErrorMsg.impossible ("CheckLty: "^s)
val say = Control.Print.say

structure PP = PrettyPrint
structure PU = PPUtil
val with_pp = PP.with_default_pp
val pd = ref 20

val anyerror = ref false
val clickerror = fn () => (anyerror := true)

(****************************************************************************
 *                         BASIC UTILITY FUNCTIONS                          *
 ****************************************************************************)
fun debugmsg msg =
    if false (* !debugging *)
    then (say "[ChkPlexp]: "; say msg; say "\n")
    else ()

fun app2(f, [], []) = ()
  | app2(f, a::r, b::z) = (f(a, b); app2(f, r, z))
  | app2(f, _, _) = bug "unexpected list arguments in function app2"

fun simplify(le,0) = STRING "<dummy>"
  | simplify(le,n) =
      let fun h le = simplify(le, n-1)
       in case le
           of FN(v, t, e) => FN(v, t, h e)
            | APP(e1, e2) => APP(h e1, h e2)
            | LET(v, e1, e2) => LET(v, h e1, h e2)
            | TFN(ks, e) => TFN(ks, h e)
            | TAPP(e, ts) => TAPP(h e, ts)
            | CON(l, x, e) => CON(l, x, h e)
            | FIX(lv, lt, le, b) => FIX(lv, lt, map h le, h b)
            | SWITCH(e, l, dc, opp) =>
               (let fun g(c, x) = (c, h x)
                    fun f x = case x of SOME y => SOME(h y) | NONE => NONE
                 in SWITCH(h e, l, map g dc, f opp)
                end)
            | RECORD e => RECORD (map h e)
            | SRECORD e => SRECORD (map h e)
            | VECTOR(e, x) => VECTOR (map h e, x)
            | SELECT(i, e) => SELECT(i, h e)
            | HANDLE(e1, e2) => HANDLE(h e1, h e2)
            | WRAP(t, b, e) => WRAP(t, b, h e)
            | UNWRAP(t, b, e) => UNWRAP(t, b, h e)
            | _ => le
      end (* end of simplify *)

(** utility functions for printing *)
fun tkPrint tk =
    with_pp (fn stm =>
     (PPLty.ppTKind (!pd) stm tk;
      PP.newline stm))

fun tcPrint tc =
    with_pp (fn stm =>
     (PPLty.ppTyc (!pd) stm tc;
      PP.newline stm))

fun ltPrint lt =
    with_pp (fn stm =>
     (PPLty.ppLty (!pd) stm lt;
      PP.newline stm))

fun lePrint le =
    with_pp(fn ppstrm => PPLexp.ppLexp 20 ppstrm (simplify(le, 3)))

(*** a hack for type checking ***)
fun laterPhase i = (i > 20)

(****************************************************************************
 *           MAIN FUNCTION --- val checkLty : PLambda.lexp -> bool          *
 ****************************************************************************)
fun checkLty (lexp, venv : LB.ltyEnv, phase) =
let

val ltEquiv = LK.lt_eqv
val ltString = if laterPhase(phase) then LB.ltc_void else LB.ltc_string
val ltExn = if laterPhase(phase) then LB.ltc_void else LB.ltc_exn
fun ltEtag lt = if laterPhase(phase) then LB.ltc_void
                else LB.ltc_etag lt
fun ltVector t = if laterPhase(phase) then LB.ltc_void
                 else LD.ltc_tyc(LB.tcc_vector t)

(** lazily selecting a field from a record/structure type *)
exception LtySelect
fun ltSel (lt, i) =
  (LE.lt_select(lt, i, "chkplexp#127")) handle _ => raise LtySelect

(** build a function or functor type from a pair of arbitrary ltys *)
fun ltFun (x, y) =
  if (LD.ltp_tyc x) andalso (LD.ltp_tyc y) then LD.ltc_parrow(x, y)
  else LD.ltc_pfct(x, y)

fun ltTup ts = LD.ltc_tyc(LD.tcc_tuple (map LD.ltd_tyc ts)) handle LD.DeconExn => bug "ltTup"

(** lazily finding out the arg and res of an lty *)
exception LtyArrow
fun ltArrow lt =
  (if LD.ltp_tyc lt then LD.ltd_parrow lt
   else LD.ltd_pfct lt) handle _ => raise LtyArrow

val lt_inst_chk = LE.lt_inst_chk_gen()
(* kind checker for ltys *)
val ltyChk = fn _ => fn _ => 1 (* LtyKindChk.ltKindCheckGen () *)
(* kind checker for tycs *)
val tycChk = fn _ => fn _ => 1 (* LtyKindChk.tcKindCheckGen () *)

fun ltAppChk (lt, ts, kenv) : LT.lty =
  (case lt_inst_chk(lt, ts, kenv)
    of [b] => b
     | _ => bug "unexpected arg in ltAppChk")

(** utility functions for type checking *)
fun ltTyApp le s (lt, ts, kenv) =
      ((ltAppChk(lt, ts, kenv))
       handle zz =>
       (clickerror ();
        say (s ^ "  **** Kind conflicting in lexp =====> \n    ");
        case zz of LE.LtyAppChk => say "      exception LtyAppChk raised! \n"
                 | LKC.KindChk _ =>  say "      exception KindChk raised! \n"
                 | _ => say "   other weird exception raised! \n";
        say "\n \n"; lePrint le; say "\n For Types: \n";
        ltPrint lt; say "\n and   \n    ";
        app (fn x => (tcPrint x; say "\n")) ts;   say "\n \n";
        say "***************************************************** \n";
        bug "fatal typing error in ltTyApp"))

fun ltMatch le msg (t1, t2) =
  (if ltEquiv(t1,t2) then ()
   else (clickerror();
         with_pp(fn s =>
           (PU.pps s ("ERROR(checkLty): ltEquiv fails in ltMatch: "^msg); PP.newline s;
            PU.pps s "le:"; PP.newline s; PPLexp.ppLexp 6 s le;
            PU.pps s "t1:"; PP.newline s; PPLty.ppLty 20 s t1; PP.newline s;
            PU.pps s "t2:"; PP.newline s; PPLty.ppLty 20 s t2; PP.newline s;
            PU.pps s"***************************************************";
            PP.newline s)); raise Fail "ltMatch"))
  handle LK.TeUnbound =>
  (clickerror();
   with_pp(fn s =>
     (PU.pps s ("ERROR(checkLty): exception teUnbound2 in ltMatch"^msg); PP.newline s;
      PU.pps s "le:"; PP.newline s; PPLexp.ppLexp 6 s le;
      PU.pps s "t1:"; PP.newline s; PPLty.ppLty 10 s t1; PP.newline s;
      PU.pps s "t2:"; PP.newline s; PPLty.ppLty 10 s t2; PP.newline s;
      PU.pps s"***************************************************";
      PP.newline s)))

fun ltFnApp le s (t1, t2) =
  let val (a1, b1) =
        ((ltArrow t1) handle zz =>
            (clickerror ();
             say (s ^ "  **** Applying Non-Arrow Type in lexp =====> \n    ");
             case zz of LtyArrow => say "exception LtyArrow raised. \n"
                      | LK.TeUnbound => say "exception TeUnbound raised. \n"
                      | _ => say "other weird exceptions raised\n";
             say "\n \n";  lePrint le; say "\n For Types \n";
             ltPrint t1; say "\n and   \n    "; ltPrint t2; say "\n \n";
             say "***************************************************** \n";
             bug "fatal typing error in ltFnApp"))

   in ltMatch le (s^":ltFnApp") (a1, t2); b1
  end

fun ltFnAppR le s (t1, t2) =  (*** used for DECON lexps ***)
  let val (a1, b1) =
        ((ltArrow t1) handle zz =>
            (clickerror ();
             say (s ^ "  **** Rev-Apply Non-Arrow Type in lexp =====> \n    ");
             case zz of LtyArrow => say "exception LtyArrow raised. \n"
                      | LK.TeUnbound => say "exception TeUnbound raised. \n"
                      | _ => say "other weird exceptions raised\n";
             say "\n \n";  lePrint le; say "\n For Types \n";
             ltPrint t1; say "\n and   \n    "; ltPrint t2; say "\n \n";
             say "***************************************************** \n";
             bug "fatal typing error in ltFnApp"))

   in ltMatch le s (b1, t2); a1
  end

fun ltSelect le s (lt, i) =
  ((ltSel(lt, i))
     handle zz =>
       (clickerror ();
        say (s ^ "  **** Select from a wrong-type lexp  =====> \n    ");
        case zz of LtyArrow => say "exception LtyArrow raised. \n"
                 | LK.TeUnbound => say "exception TeUnbound raised. \n"
                 | _ => say "other weird exceptions raised\n";
        say "\n \n";  lePrint le; say "\n \n";
        say "Selecting "; say (Int.toString i);
        say "-th component from the type: \n     "; ltPrint lt; say "\n \n ";
        say "***************************************************** \n";
        bug "fatal typing error in ltSelect"))

(** ltConChk currently does not check the case for DATAcon *)
(** Of course, we could easily check for monomorphic DATAcons *)
fun ltConChk le s (DATAcon ((_,rep,lt), ts, v), root, kenv, venv, d) =
      let val t1 = ltTyApp le "DECON" (lt, ts, kenv)
          val t = ltFnAppR le "DECON" (t1, root)
       in LB.ltInsert(venv, v, t, d)
      end
  | ltConChk le s (c, root, kenv, venv, d) = let
      val nt = (case c
	     of INTcon{ty = 0, ...} => bug "INTcon: IntInf"
	      | INTcon{ty, ...} => LB.ltc_num ty
	      | WORDcon{ty, ...} => LB.ltc_num ty
	      | STRINGcon _ => ltString
	      | _ => raise Fail "help"
	    (* end case *))
      in
	((ltMatch le s (nt, root)) handle Fail _ => say "ConChk ltEquiv\n"); venv
      end

(** check : tkindEnv * LB.ltyEnv * DI.depth -> lexp -> lty *)
fun check (kenv, venv, d) =
  let fun ltyChkMsg msg lexp kenv lty =
		  ltyChk kenv lty
		  handle LKC.KindChk kndchkmsg =>
			 (say ("*** Kind check failure during \
			       \ PLambda type check: ");
			  say (msg);
			  say ("***\n Term: ");
			  with_pp(fn s => PPLexp.ppLexp 20 s lexp);
			  say ("\n Kind check error: ");
			  say kndchkmsg;
			  say ("\n");
			  raise ChkPlexp)
      fun loop le =
	  let fun ltyChkMsgLexp msg kenv lty =
		    ltyChkMsg msg lexp kenv lty
	      fun ltyChkenv msg lty = 1 (* ltyChkMsgLexp msg kenv lty *)
	  in
	      (case le
		of VAR v =>
		   (case LB.ltLookup(venv, v, d)
		      of NONE =>
			   (say ("ChkPlexp.check: unbound lvar: " ^ LV.lvarName(v)
				 ^ "\n");
			    bug "unexpected lambda code in checkLty")
		       | SOME lty => lty)
		 | INT{ty = 0, ...} => bug "unexpected IntInf in checkLty"
		 | INT{ty, ...} => LB.ltc_num ty
		 | WORD{ty, ...} => LB.ltc_num ty
(* REAL32: will need extra cases *)
		 | REAL _ => LB.ltc_real
		 | STRING _ => ltString
		 | PRIM(p, t, ts) =>
		   (* kind check t and ts *)
		       (ltyChkenv " PRIM " t;
			map (tycChk kenv) ts;
			debugmsg "PRIM";
			ltTyApp le "PRIM" (t, ts, kenv))

		 | FN(v, t, e1) =>
		   let val _ = ltyChkenv "FN bound var" t
					 (* kind check bound variable type *)
                       val venv' = LB.ltInsert(venv, v, t, d)
                       val res = check (kenv, venv', d) e1
                       val _ = ltyChkenv "FN rng" res
		       val _ = debugmsg "FN"
		       val fnlty = ltFun(t, res) (* handle both functions and functors *)
		       val  _ = ltyChkenv "FNlty " fnlty
		       val _ = debugmsg "FN 2"
		   in fnlty
		   end

		 | FIX(vs, ts, es, eb) =>
		   let val _ = map (ltyChkenv "FIX bound var") ts
				   (* kind check bound variable types *)
		       fun h (env, v::r, x::z) = h(LB.ltInsert(env, v, x, d), r, z)
			 | h (env, [], []) = env
			 | h _ = bug "unexpected FIX bindings in checkLty."
                       val venv' = h(venv, vs, ts)

                       val nts = map (check (kenv, venv', d)) es
                       val _ = map (ltyChkenv "FIX body types") nts
                       val _ = app2(ltMatch le "FIX1", ts, nts)
		       val _ = debugmsg "FIX"
		   in check (kenv, venv', d) eb
		   end

		 | APP(e1, e2) =>
		   let val top = loop e1
                       val targ = loop e2
		       val _ = ltyChkenv "APP operator " top
		       val _ = ltyChkenv "APP argument " targ
		       val _ = debugmsg "APP"
		   in
                       ltFnApp le "APP" (top, targ)
		   end

		 | LET(v, e1, e2) =>
		   let val t1 = loop e1
                       val _ = ltyChkenv "LET definien" t1
                       val venv' = LB.ltInsert(venv, v, t1, d)
		       val bodyLty = check (kenv, venv', d) e2
		       val _ = ltyChkenv "LET body" bodyLty
		       val _ = debugmsg "LET"
		   in bodyLty
		   end

		 | TFN(ks, e) =>
		   let val kenv' = LT.tkInsert(kenv, ks)
                       val lt = check (kenv', venv, DI.next d) e
                       val _ = ltyChkMsgLexp "TFN body" (ks::kenv) lt
		       val _ = debugmsg "TFN"
		   in LD.ltc_poly(ks, [lt])
		   end

		 | TAPP(e, ts) =>
		   let val lt = loop e
                       val _ = map ((ltyChkenv "TAPP args") o LD.ltc_tyc) ts
				   (* kind check type args *)
                       val _ = ltyChkenv "TAPP type function " lt
		       val _ = debugmsg "TAPP"
		   in  ltTyApp le "TAPP" (lt, ts, kenv)
		   end

		 | GENOP(dict, p, t, ts) =>
		   ((* should type check dict also *)
			(map ((ltyChkenv "GENOP args ") o LD.ltc_tyc) ts;
			 ltTyApp le "GENOP" (t, ts, kenv)))

		 | CON((_, rep, lt), ts, e) =>
		   let val t1 = ltTyApp le "CON" (lt, ts, kenv)
                       val _ = ltyChkenv "CON 1 " t1
                       val t2 = loop e
                       val _ = ltyChkenv "CON 2 " t2
		       val _ = debugmsg "CON"
		   in ltFnApp le "CON-A" (t1, t2)
		   end
		       (*
		 | DECON((_, rep, lt), ts, e) =>
		   let val t1 = ltTyApp le "DECON" (lt, ts, kenv)
                       val t2 = loop e
		   in ltFnAppR le "DECON" (t1, t2)
		   end
			*)
		 | RECORD el =>
		   let val elemsltys = map loop el
		       val _ = map (ltyChkenv "RECORD elem ") elemsltys
		       val _ = debugmsg "RECORD"
		   in ltTup elemsltys
		   end
		 | SRECORD el =>
		   let val elemsltys = map loop el
		       val _ = map (ltyChkenv "SRECORD elem ") elemsltys
		   in LD.ltc_str elemsltys
		   end
		 | VECTOR (el, t)  =>
		   let val ts = map loop el
		   in ltyChkenv "VECTOR index " (LD.ltc_tyc t);
                      map (ltyChkenv "VECTOR vector ") ts;
                      app (fn x => ltMatch le "VECTOR" (x, LD.ltc_tyc t)) ts;
                      debugmsg "VECTOR ";
		      ltVector t
		   end

		 | SELECT(i,e) =>
		    let val lty = loop e
			val _ = ltyChkenv " SELECT " lty
			val _ = debugmsg "SELECT"
		    in
			ltSelect le "SEL" (lty, i)
		    end
		 | SWITCH(e, _, cl, opp) =>
		   let val root = loop e
		       val _ = ltyChkenv " SWITCH root " root
                       fun h (c, x) =
			   let val venv' = ltConChk le "SWT1" (c, root, kenv, venv, d)
			   in check (kenv, venv', d) x
			   end
                       val ts = map h cl
                       val _ = map (ltyChkenv "SWITCH branch ") ts
		       val _ = debugmsg "SWITCH"
		   in (case ts
			of [] => bug "empty switch in checkLty"
			 | a::r =>
                           (app (fn x => ltMatch le "SWT2" (x, a)) r;
                            case opp
                             of NONE => a
                              | SOME be => (ltMatch le "SWT3" (loop be, a); a)))
		   end

		 | ETAG(e, t) =>
		   let val z = loop e   (* what do we check on e ? *)
                       val _ = ltyChkenv "ETAG 1 " z
		       val _ = ltMatch le "ETAG1" (z, LB.ltc_string)
                       val _ = ltyChkenv "ETAG " t
		       val _ = debugmsg "ETAG"
		   in ltEtag t
		   end

		 | RAISE(e,t) =>
		   let val exlty = loop e
		       val _ = ltyChkenv "RAISE " exlty
		       val _ = debugmsg "RAISE"
		   in
		       (ltMatch le "RAISE" (exlty, ltExn); t)
		   end
		 | HANDLE(e1,e2) =>
		   let val t1 = loop e1
		       val _ = ltyChkenv "HANDLE exception " t1
                       val t2 = loop e2
		       val _  = ltyChkenv "HANDLE handler " t2
		       val arg = ltFnAppR le "HANDLE" (loop e2, t1)
		       val _  = ltyChkenv "HANDLE arg " arg
		       val _ = debugmsg "HANDLE"
		   in t1 (* [GK] Is this right?? *)
		   end

		       (** these two cases should never happen before wrapping *)
		 | WRAP(t, b, e) =>
		   (ltMatch le "WRAP" (loop e, LD.ltc_tyc t);
		    if laterPhase(phase) then LB.ltc_void
		    else LD.ltc_tyc(if b then LD.tcc_box t else t))

		 | UNWRAP(t, b, e) =>
		   let val ntc = if laterPhase(phase) then LB.tcc_void
				 else (if b then LD.tcc_box t else t)
                       val nt = LD.ltc_tyc ntc
                       val _ = ltyChkenv "UNWRAP " nt
		   in (ltMatch le "UNWRAP" (loop e, nt); LD.ltc_tyc t)
		   end)
	  end (* loop *)
  in (* wrap loop with kind check of result *)
     fn x => let val y = loop x in ltyChkMsg "RESULT " x kenv y; y end
 end (* end-of-fn-check *)

in
  anyerror := false;
  check (LT.initTkEnv, venv, DI.top) lexp;
  !anyerror
end (* end of function checkLty *)

fun checkLtyTop(lexp, phase) = checkLty(lexp, LB.initLtyEnv, phase)

end (* toplevel local *)
end (* structure CheckLty *)

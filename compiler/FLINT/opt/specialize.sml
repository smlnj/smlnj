(* specialize.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * minimal type derivation, type specialization,  and lifting of
 * structure access (not supported yet) and type application
 *)

signature SPECIALIZE =
  sig
    val specialize : FLINT.prog -> FLINT.prog
  end (* signature SPECIALIZE *)

structure Specialize : SPECIALIZE =
struct

local
  structure DI = DebIndex
  structure LT = Lty
  structure FR = FunRecMeta
  structure LK = LtyKernel
  structure LD = LtyDef
  structure LB = LtyBasic
  structure LE = LtyExtern
  structure LV = LambdaVar
  structure PT = PrimTyc
  structure PF = PFlatten
  structure LVMap = LambdaVar.Map
  structure PP = PrettyPrint
  structure PL = PLambda
  open FLINT
in

val debugging = FLINT_Control.spdebugging
fun bug s = ErrorMsg.impossible ("SpecializeNvar: " ^ s)

val say = Control_Print.say
fun pp_fflag (fflag : LT.fflag) =
    PP.with_default_pp (fn ppstrm => PPLty.ppFflag ppstrm fflag)
fun pp_lvar (lvar : LambdaVar.lvar) =
    PP.with_default_pp (fn ppstrm => PP.string ppstrm
			  ("f = "^(LambdaVar.prLvar lvar)))
fun pp_lty (lty : LT.lty) =
    PP.with_default_pp (fn ppstrm => PPLty.ppLty 20 ppstrm lty)

fun mkv _ = LV.mkLvar()
val ident = fn le : FLINT.lexp => le

val tk_eqv = LK.tk_eqv
val tc_eqv = LK.tc_eqv

(* checking the equivalence of two tyc sequences *)
fun tcs_eqv (xs, ys) =
  let fun teq(a::r, b::s) = if tc_eqv(a, b) then teq(r, s) else false
        | teq([],[]) = true
        | teq _ = bug "unexpected cases in tcs_eqv"
   in teq(xs, ys)
  end

(* accounting functions; how many functions have been specialized *)
fun mk_click () =
  let val x = ref 0
      fun click () = (x := (!x) + 1)
      fun num_click () = !x
   in (click, num_click)
  end

(****************************************************************************
 *                  UTILITY FUNCTIONS FOR KIND AND TYPE BOUNDS              *
 ****************************************************************************)

(****************************************************************************
 *                  TYPES FOR INFO ENVIRONMENTS                             *
 ****************************************************************************)

(*
 * Bnd is a lattice on the type hierarchy, used to infer minimum type bounds;
 * Right now, we only deal with first-order kinds. All higher-order kinds
 * will be assigned KTOP.
 *)

datatype bnd
  = KBOX
  | KTOP
  | TBND of LT.tyc

type bnds = bnd list

(*
 * We maintain a table mapping each lvar to its definition depth,
 * its type, and a list of its uses, indexed by its specific type
 * instances.
 *)

exception ITABLE
exception DTABLE

datatype dinfo
  = ESCAPE
  | NOCSTR
  | CSTR of bnds

type depth = DI.depth
type info = (LT.tyc list * LV.lvar list) list
type itable = info LambdaVar.Tbl.hash_table   (* lvar --> (tyc list * lvar list) list *)
type dtable = (depth * dinfo) LambdaVar.Tbl.hash_table
type kenv = (itable * (LT.tvar * LT.tkind) list) list

datatype infoEnv = IENV of kenv * dtable

(* kBnd: kenv -> LE.tyc -> bnd *)
(* THE FOLLOWING FUNCTION IS NOT FULLY DEFINED! (how?)
 * kBnd will only return KBOX or KTOP, never TBND. *)
fun kBnd kenv tc =
    if LD.tcp_var tc then
	let val (i,j) = LD.tcd_var tc
	    val (_,ks) =
		List.nth(kenv, i-1)
		handle Subscript =>
		       bug ("kBnd: db tyvar depth: " ^ Int.toString (i-1))
	    val (_,k) =
		List.nth(ks, j)
		handle Subscript =>
		       bug ("kBnd: db tyvar index: " ^ Int.toString j)
	in if tk_eqv(LD.tkc_box, k) then KBOX else KTOP
	end
    else if LD.tcp_nvar tc then KTOP	(* FIXME: check the kenv for KBOX ??? *)
    else if LD.tcp_prim tc then
	let val p = LD.tcd_prim tc
	in if PT.unboxed p then KTOP else KBOX
	end
    else KBOX

(* kmBnd : kenv -> (LE.tyc * bnd) -> bnd *)
(* kmBnd will only return KTOP of KBOX, never TBND *)
fun kmBnd kenv (tc, KTOP) = KTOP
  | kmBnd kenv (tc, KBOX) = kBnd kenv tc
  | kmBnd kenv (tc, TBND _) = bug "unexpected cases in kmBnd"

(* tBnd: kenv -> LE.tyc -> bnd *)
(* tBnd only returns TBND *)
fun tBnd kenv tc = TBND tc

fun tmBnd kenv (tc, KTOP) = KTOP
  | tmBnd kenv (tc, KBOX) = kBnd kenv tc
  | tmBnd kenv (tc, x as TBND t) =
      if tc_eqv(tc, t) then x else kmBnd kenv (tc, kBnd kenv t)


datatype spkind
  = FULL
  | PART of bool list (* filter indicator; which one is gone *)

datatype spinfo
  = NOSP
  | NARROW of (LT.tvar * LT.tkind) list
  | PARTSP of {ntvks: (LT.tvar * LT.tkind) list, nts: LT.tyc list,
               masks: bool list}
  | FULLSP of LT.tyc list * LV.lvar list

(*
 * Given a list of default kinds, and a list of bnd information, a depth,
 * and the (tyc list * lvar list) list info in the itable, returns the
 * the spinfo.
 *)
fun bndGen(oks, bnds, d, info) =
  let (** pass 1 **)
      fun g ((TBND _)::bs, r, z) = g(bs, false::r, z)
        | g (_::bs, r, _) = g(bs, true::r, false)
        | g ([], r, z) = if z then FULL else PART (rev r)
      val spk = g(bnds, [], true)

      val adj = case spk of FULL => (fn tc => tc)
                          | _ => (fn tc => LB.tc_adj(tc, d, DI.next d)
				           handle LK.TCENV => bug "bndGen")
        (* if not full-specializations, we push depth one-level down *)

      (** pass 2 **)
      val n = length oks

      (* invariants: n = length bnds = length (the-resulting-ts) *)
      fun h([], [], i, [], ts, _) =
            (case info of [(_, xs)] => FULLSP(rev ts, xs)
                       | _ => bug "unexpected case in bndGen 3")
        | h([], [], i, ks, ts, b) =
            if b then NOSP else
             if i = n then NARROW (rev ks)
             else (case spk
                    of PART masks =>
                        PARTSP {ntvks=rev ks, nts=rev ts, masks=masks}
                     | _ => bug "unexpected case 1 in bndGen")
        | h(ok::oks, (TBND tc)::bs, i, ks, ts, b) =
             h(oks, bs, i, ks, (adj tc)::ts, false)
        | h((ok as (tv,_))::oks, KTOP::bs, i, ks, ts, b) =
             h(oks, bs, i+1, ok::ks, (LD.tcc_nvar tv)::ts, b)
        | h((tv,ok)::oks, KBOX::bs, i, ks, ts, b) =
             let (* val nk = if tk_eqv(LD.tkc_box, ok) then ok else LD.tkc_box *)
                 val (nk, b) =
                   if tk_eqv(LD.tkc_mono, ok) then (LD.tkc_box, false) else (ok, b)
              in h(oks, bs, i+1, (tv,nk)::ks, (LD.tcc_nvar tv)::ts, b)
             end
        | h _ = bug "unexpected cases 2 in bndGen"

   in h(oks, bnds, 0, [], [], true)
  end


(****************************************************************************
 *              UTILITY FUNCTIONS FOR TYPE SPECIALIZATIONS                  *
 ****************************************************************************)
(** initializing a new info environment : unit -> infoEnv *)
fun initInfoEnv () =
  let val itable : itable = LambdaVar.Tbl.mkTable (32, ITABLE)
      val dtable : dtable = LambdaVar.Tbl.mkTable (32, DTABLE)
   in IENV ([(itable,[])], dtable)
  end

(** register a definition of sth interesting into the info environment *)
fun entDtable (IENV(_, dtable), v, ddinfo) =
    LambdaVar.Tbl.insert dtable (v, ddinfo)

(** mark an lvar in the dtable as escape *)
fun escDtable (IENV(_, dtable), v) =
    case LambdaVar.Tbl.find dtable v of
	SOME (_, ESCAPE) => ()
      | SOME (d, _) => LambdaVar.Tbl.insert dtable (v, (d, ESCAPE))
      | NONE => ()

(*
 * Register a dtable entry; modify the least upper bound of a particular
 * type binding; notice I am only moving kind info upwards, not type
 * info, I could move type info upwards though.
 *)
fun regDtable (IENV(kenv, dtable), v, infos) =
  let val (dd, dinfo) =
        ((LambdaVar.Tbl.lookup dtable v) handle _ =>
                bug "unexpected cases in regDtable")
   in (case dinfo
        of ESCAPE => ()
         | _ =>
             let fun h ((ts, _), ESCAPE) = ESCAPE
                   | h ((ts, _), NOCSTR) = CSTR (map (kBnd kenv) ts)
                   | h ((ts, _), CSTR bnds) =
                       let val nbnds = ListPair.map (kmBnd kenv) (ts, bnds)
                        in CSTR nbnds
                       end
                 val ndinfo = foldr h dinfo infos
              in LambdaVar.Tbl.insert dtable (v, (dd, ndinfo))
             end)
  end (* function regDtable *)

(*
 * Calculate the least upper bound of all type instances;
 * this should take v out of the current dtable !
 *)
fun sumDtable(IENV(kenv, dtable), v, infos) =
  let val (dd, dinfo) =
        ((LambdaVar.Tbl.lookup dtable v) handle _ =>
                bug "unexpected cases in sumDtable")
   in (case dinfo
        of ESCAPE => (dd, ESCAPE)
         | _ =>
             (let fun h ((ts, _), ESCAPE) = ESCAPE
                    | h ((ts, _), NOCSTR) = CSTR (map (tBnd kenv) ts)
                    | h ((ts, _), CSTR bnds) =
                        let val nbnds = ListPair.map (tmBnd kenv) (ts, bnds)
                         in CSTR nbnds
                        end
                  val ndinfo = foldr h dinfo infos
               in (dd, ndinfo)
              end))
  end

(** find out the set of nvars in a list of tycs *)
fun tcs_nvars tcs = LambdaVar.SortedList.foldmerge (map LK.tc_nvars tcs)

(** look and add a new type instance into the itable *)
fun lookItable (IENV (itabs,dtab), d, v, ts, getlty, nv_depth) =
  let val (dd, _) =
        ((LambdaVar.Tbl.lookup dtab v)
	 handle _ => bug "unexpected cases in lookItable")

      val nd = List.foldr Int.max dd (map nv_depth (tcs_nvars ts))

      val (itab,_) = ((List.nth(itabs, d-nd)) handle _ =>
                      bug "unexpected itables in lookItable")

      val nts = map (fn t => (LB.tc_adj(t, d, nd) handle LK.TCENV => bug "lookItable")) ts
      val xi = getOpt (LambdaVar.Tbl.find itab v, [])

      fun h ((ots,xs)::r) = if tcs_eqv(ots, nts) then (map VAR xs) else h r
        | h [] = let val oldt = getlty (VAR v)     (*** old type is ok ***)
                     val bb = LE.lt_inst(oldt, ts)
                     val nvs =  map mkv  bb
                     val _ = LambdaVar.Tbl.insert itab (v, (nts, nvs)::xi)
                  in map VAR nvs
                 end
   in h xi
  end

(** push a new layer of type abstraction : infoEnv -> infoEnv *)
fun pushItable (IENV(itables, dtable), tvks) =
  let val nt : itable = LambdaVar.Tbl.mkTable(32, ITABLE)
   in (IENV((nt,tvks)::itables, dtable))
  end

(*
 * Pop off a layer when exiting a type abstaction, adjust the dtable properly,
 * and generate the proper headers: infoEnv -> (lexp -> lexp)
 *)
fun popItable (IENV([], _)) =
      bug "unexpected empty information env in popItable"
  | popItable (ienv as IENV((nt,_)::_, _)) =
      let val infos = LambdaVar.Tbl.listItemsi nt
          fun h ((v,info), hdr) =
            let val _ = regDtable(ienv, v, info)
                fun g ((ts, xs), e) = LET(xs, TAPP(VAR v, ts), e)
             in fn e => foldr g (hdr e) info
            end
       in foldr h ident infos
      end

(* Check out a escaped variable from the info env, build the header properly *)
fun chkOutEsc (IENV([], _), v) =
      bug "unexpected empty information env in chkOut"
  | chkOutEsc (ienv as IENV((nt,_)::_, _), v) =
      let val info = getOpt (LambdaVar.Tbl.find nt v, [])
          fun g ((ts, xs), e) = LET(xs, TAPP(VAR v, ts), e)
          val hdr = fn e => foldr g e info
      in
	  (* remove this v so it won't be considered again *)
	  ignore (LambdaVar.Tbl.remove nt v) handle _ => ();
	  hdr
      end

fun chkOutEscs (ienv, vs) =
  foldr (fn (v,h) => (chkOutEsc(ienv, v)) o h) ident vs

(*
 * Check out a regular variable from the info env, build the header
 * properly, of course, adjust the corresponding dtable entry.
 *)
fun chkOutNorm (IENV([], _), v, oks, d) =
      bug "unexpected empty information env in chkOut"

  | chkOutNorm (ienv as IENV((nt,_)::_, dtable), v, oks, d) =
      let val info = getOpt (LambdaVar.Tbl.find nt v, [])
          val (_, dinfo) = sumDtable(ienv, v, info)
          val spinfo =
            (case dinfo
              of ESCAPE => NOSP
               | NOCSTR => (* must be a dead function, let's double check *)
                   (case info of [] => NOSP
                               | _ => bug "unexpected cases in chkOutNorm")
               | CSTR bnds => bndGen(oks, bnds, d, info))

          fun mkhdr((ts, xs), e) =
            (case spinfo
              of FULLSP _ => e
               | PARTSP {masks, ...} =>
                   let fun h([], [], z) = rev z
                         | h(a::r, b::s, z) =
                             if b then h(r, s, a::z) else h(r, s, z)
                         | h _ = bug "unexpected cases in tapp"
                    in LET(xs, TAPP(VAR v, h(ts, masks, [])), e)
                   end
               | _ => LET(xs, TAPP(VAR v, ts), e))
          val hdr = fn e => foldr mkhdr e info
	  (* don't consider it again... *)
	  val _ = LambdaVar.Tbl.remove nt v handle _ => []
       in (hdr, spinfo)
      end

(****************************************************************************
 *                         MAIN FUNCTION                                    *
 ****************************************************************************)

(***** the substitution intmapf: named variable -> tyc *********)
type smap = (LT.tvar * LT.tyc) list
val initsmap = []

fun mergesmaps (s1:smap as h1::t1, s2:smap as h2::t2) = (
      case LambdaVar.compare (#1 h1, #1 h2)
       of LESS => h1 :: (mergesmaps (t1, s2))
        | GREATER => h2 :: (mergesmaps (s1, t2))
        | EQUAL => h1 :: (mergesmaps (t1, t2)) (* drop h2 *)
      (* end case *))
  | mergesmaps (s1, []) = s1
  | mergesmaps ([], s2) = s2

fun addsmap (tvks, ts, smap) =
    let
        fun select ((tvar,tkind),tyc) = (tvar,tyc)
        val tvtcs = ListPair.map select (tvks, ts)
        fun cmp ((tvar1,_), (tvar2,_)) = LambdaVar.>(tvar1, tvar2)
        val tvtcs = ListMergeSort.sort cmp tvtcs
    in
        mergesmaps (tvtcs, smap)
    end
(***** end of the substitution intmapf hack *********************)

(***** the nvar-depth intmapf: named variable -> DI.depth *********)
type nmap = DI.depth LVMap.map
val initnmap = LVMap.empty
fun addnmap (tvks, d, nmap) =
  let fun h ((tv,_)::xs, nmap) =
           h(xs, LVMap.insert(nmap, tv, d))
        | h ([], nmap) = nmap
   in h(tvks, nmap)
  end
fun looknmap nmap nvar =
    case LVMap.find(nmap, nvar) of SOME d => d | NONE => DI.top
     (*  bug "unexpected case in looknmap") *)
(***** end of the substitution intmapf hack *********************)

fun specialize fdec =
let

val (click, num_click) = mk_click ()

(* In pass1, we calculate the old type of each variables in the FLINT
 * expression. The reason we can't merge this with the main pass is
 * that the main pass traverse the code in different order.
 * There must be a simpler way, but I didn't find one yet (ZHONG).
 *)
val getlty = Recover.recover (fdec, false)

(* transform: infoEnv * DI.depth * lty cvt * tyc cvt
              * smap * bool -> (lexp -> lexp)
 *            where type 'a cvt = DI.depth -> 'a -> 'a
 * The 2nd argument is the depth of the resulting expression.
 * The 3rd and 4th arguments are used to encode the type translations.
 * The 5th argument is the substitution map.
 * The 6th argument is a flag that indicates whether we need to
 * flatten the return results of the current function.
 *)
val tc_nvar_subst = LE.tc_nvar_subst_gen()
val lt_nvar_subst = LE.lt_nvar_subst_gen()

fun transform (ienv, d, nmap, smap, did_flat) =
  let val tcf = tc_nvar_subst smap
      val ltf = lt_nvar_subst smap
      val nv_depth = looknmap nmap

      (* we chkin and chkout polymorphic values only *)
      fun chkin v = entDtable (ienv, v, (d, ESCAPE))
      fun chkout v = chkOutEsc (ienv, v)
      fun chkins vs = app chkin vs
      fun chkouts vs = chkOutEscs (ienv, vs)

      (* lpvar : value -> value *)
      fun lpvar (u as (VAR v)) = (escDtable(ienv, v); u)
        | lpvar u = u

      (* lpvars : value list -> value list *)
      fun lpvars vs = map lpvar vs

      (* lpprim : primop -> primop *)
      fun lpprim (d, po, lt, ts) = (d, po, ltf lt, map tcf ts)

      (* lpdc : dcon -> dcon *)
      fun lpdc (s, rep, lt) = (s, rep, ltf lt)

      (* lplet : lvar * lexp -> (lexp -> lexp) *)
      fun lplet (v, e, cont) =
        let val _ = chkin v
            val ne = loop e
         in cont ((chkout v) ne)
        end

      (* lplets : lvar list * lexp -> (lexp -> lexp) *)
      and lplets (vs, e, cont) =
        let val _ = chkins vs
            val ne = loop e
         in cont ((chkouts vs) ne)
        end

      (* lpcon : con * lexp -> con * lexp *)
      and lpcon (PL.DATAcon (dc, ts, v), e) =
            (PL.DATAcon (lpdc dc, map tcf ts, v), lplet(v, e, fn x => x))
        | lpcon (c, e) = (c, loop e)

      (* lpfd : fundec -> fundec *** requires REWORK *** *)
      and lpfd (fk as {cconv=FR.CC_FCT, ...}, f, vts, be) =
           (fk, f, map (fn (v,t) => (v, ltf t)) vts,
                   lplets (map #1 vts, be, fn e => e))
        | lpfd (fk as {cconv=FR.CC_FUN fflag,isrec,known,inline}, f, vts, be) =
           let (** first get the original arg and res types of f *)
	       val f_lty = getlty (VAR f)
               val (fflag_f, atys, rtys) = LD.ltd_arrow f_lty
		   handle LD.DeconExn => bug "lpfd"
               (** just a sanity check; should turn it off later **)
               val _ =
		   if !debugging
		   then (pp_lvar f; print "fflag = "; pp_fflag fflag;
			 print "fflag(f) = "; pp_fflag fflag_f;
	                 print "lpfd: f_lty = "; pp_lty f_lty)
		   else ()
               val (b1,b2) =
                 (* if LE.ff_eqv (fflag, fflag_f) then *) LB.ffd_fspec fflag
                 (* else bug "unexpected code in lpfd" *)

               (** get the newly specialized types **)
               val (natys, nrtys) = (map ltf atys, map ltf rtys)

               (** do we need flatten the arguments and the results *)
               val ((arg_raw, arg_ltys, _), unflatten) =
                 PF.v_unflatten (natys, b1)

               val (body_raw, body_ltys, ndid_flat) = PF.t_flatten (nrtys, b2)

               (** process the function body *)
               val nbe =
                 if ndid_flat = did_flat then loop be
                 else transform (ienv, d, nmap, smap, ndid_flat) be

               val (arg_lvs, nnbe) = unflatten (map #1 vts, nbe)

               (** fix the isrec information *)
               val nisrec = case isrec of NONE => NONE
                                        | SOME _ => SOME(body_ltys, FR.LK_UNKNOWN)
               val nfixed = LB.ffc_fspec(fflag, (arg_raw, body_raw))
               val nfk = {isrec=nisrec, cconv=FR.CC_FUN nfixed,
			  known=known, inline=inline}

            in (nfk, f, ListPair.zip(arg_lvs, arg_ltys), nnbe)
           end

      (* lptf : tfundec * lexp -> lexp *** Invariant: ne2 has been processed *)
      and lptf ((tfk, v, tvks, e1), ne2) =
        let val nienv = pushItable(ienv, tvks)
            val nd = DI.next d
            val nnmap = addnmap(tvks, nd, nmap)
            val ne1 = transform (nienv, nd, nnmap, smap, false) e1
            val hdr = popItable nienv
         in TFN((tfk, v, tvks, hdr ne1), ne2)
        end

      (* loop : lexp -> lexp *)
      and loop le =
        (case le
          of RET vs =>
               if did_flat then
                 let val vts = map (ltf o getlty) vs
                     val ((_,_,ndid_flat),flatten) = PF.v_flatten(vts, false)
                  in if ndid_flat then
                       let val (nvs, hdr) = flatten vs
                        in hdr(RET nvs)
                       end
                     else RET(lpvars vs)
                 end
               else RET(lpvars vs)
           | LET(vs, e1, e2) =>
               let (* first get the original types *)
                   val vtys = map (ltf o getlty o VAR) vs
                   (* second get the newly specialized types *)
                   val ((_, _, ndid_flat), unflatten) =
                      PF.v_unflatten(vtys, false)
                          (* treat the let type as always "cooked" *)
                   val _ = chkins vs
                   val ne2 = loop e2
                   val ne2 = (chkouts vs) ne2
                   val (nvs, ne2) = unflatten(vs, ne2)

                   val ne1 =
                    if ndid_flat = did_flat then loop e1
                    else transform (ienv, d, nmap, smap, ndid_flat) e1
                in LET(nvs, ne1, ne2)
               end

           | FIX(fdecs, e) => FIX(map lpfd fdecs, loop e)
           | APP(v, vs) =>
               let val vty = getlty v
                in if LD.ltp_fct vty then APP(lpvar v, lpvars vs)
                   else
                     let (** first get the original arg and res types of v *)
                         val (fflag, atys, rtys) = LD.ltd_arrow vty
			     handle LD.DeconExn => bug "loop"
                         val (b1, b2) = LB.ffd_fspec fflag

                         (** get the newly specialized types **)
                         val (natys, nrtys) = (map ltf atys, map ltf rtys)

                         val (nvs, hdr1) = (#2 (PF.v_flatten (natys, b1))) vs
                         val hdr2 =
                           if did_flat then ident
                           else (let val ((_, _, ndid_flat), unflatten) =
                                       PF.v_unflatten(nrtys, b2)
                                     val fvs = map mkv nrtys
                                  in if ndid_flat then
                                       let val (nvs, xe) =
                                             unflatten(fvs, RET (map VAR fvs))
                                        in fn le => LET(nvs, le, xe)
                                       end
                                     else ident
                                 end)
                      in hdr1 (APP(lpvar v, lpvars nvs))
                     end
               end

           | TFN((tfk, v, tvks, e1), e2) =>
               let val _ = entDtable(ienv, v, (d,NOCSTR))
                   val ne2 = loop e2
                   val ks = map #2 tvks
                   val (hdr2, spinfo) = chkOutNorm(ienv, v, tvks, d)
                   val ne2 = hdr2 ne2
                in (case spinfo
                     of NOSP => lptf((tfk, v, tvks, e1), ne2)
                      | NARROW ntvks => lptf((tfk, v, ntvks, e1), ne2)
                      | PARTSP {ntvks, nts, ...} =>
                          (* assume nts is already shifted one level down *)
                          let val nienv = pushItable(ienv, ntvks)
                              val xd = DI.next d
                              val nnmap = addnmap(ntvks, xd, nmap)
                              val nsmap = addsmap(tvks, nts, smap)
                              val ne1 =
                                transform (nienv, xd, nnmap, nsmap, false) e1
                              val hdr0 = popItable nienv
                           in TFN((tfk, v, ntvks, hdr0 ne1), ne2)
                          end
                      | FULLSP (nts, xs) =>
                          let
                              val nnmap = addnmap(tvks, d, nmap)
                              val nsmap = addsmap(tvks, nts, smap)
                              val ne1 = transform (ienv, d, nnmap, nsmap, false) e1
                           in click(); LET(xs, ne1, ne2)
                          end)
               end  (* case TFN *)

           | TAPP(u as VAR v, ts) =>
               let val nts = map tcf ts
                   val vs = lookItable(ienv, d, v, nts, getlty, nv_depth)
                in if did_flat then
                     let val vts = LE.lt_inst(ltf (getlty u), nts)
                         val ((_,_,ndid_flat),flatten) =
                            PF.v_flatten(vts, false)
                      in if ndid_flat then
                           let val (nvs, hdr) = flatten vs
                            in hdr(RET nvs)
                           end
                         else RET vs
                     end
                   else RET vs
               end

           | SWITCH (v, csig, cases, opp) =>
               SWITCH(lpvar v, csig, map lpcon cases,
                      case opp of NONE => NONE | SOME e => SOME(loop e))
           | CON (dc, ts, u, v, e) =>
               lplet (v, e, fn ne => CON(lpdc dc, map tcf ts, lpvar u, v, ne))

           | RECORD (rk as FR.RK_VECTOR t, vs, v, e) =>
               lplet (v, e, fn ne => RECORD(FR.RK_VECTOR (tcf t),
                                            lpvars vs, v, ne))
           | RECORD(rk, vs, v, e) =>
               lplet (v, e, fn ne => RECORD(rk, lpvars vs, v, ne))
           | SELECT (u, i, v, e) =>
               lplet (v, e, fn ne => SELECT(lpvar u, i, v, ne))

           | RAISE (sv, ts) =>
               let val nts = map ltf ts
                   val nsv = lpvar sv
                in if did_flat then
                     let val (_, nnts, _) = PF.t_flatten(nts, false)
                      in RAISE(nsv, nnts)
                     end
                   else
                     RAISE(nsv, nts)
               end
           | HANDLE (e, v) => HANDLE(loop e, lpvar v)

           | BRANCH (p, vs, e1, e2) =>
               BRANCH(lpprim p, lpvars vs, loop e1, loop e2)
           | PRIMOP (p, vs, v, e) =>
               lplet (v, e, fn ne => PRIMOP(lpprim p, lpvars vs, v, ne))
           | _ => bug "unexpected lexps in loop")
   in loop
  end (* function transform *)

in
(case fdec
  of (fk as {cconv=FR.CC_FCT, ...}, f, vts, e) =>
      let val ienv = initInfoEnv()
          val d = DI.top
          val _ = app (fn (x,_) => entDtable(ienv, x, (d, ESCAPE))) vts
          val ne = transform (ienv, d, initnmap, initsmap, false) e
          val hdr = chkOutEscs (ienv, map #1 vts)
          val nfdec = (fk, f, vts, hdr ne)
       in (* if (num_click()) > 0 then (*  LContract.lcontract *) nfdec
          (* if we did specialize, we run a round of lcontract on the result *)
          else *) nfdec
      end
   | _ => bug "non functor program in specialize")
end (* function specialize *)

end (* toplevel local *)
end (* structure Specialize *)

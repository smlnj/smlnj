(* cpstrans.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module implements a CPS -> CPS transformation that ensures that function
 * (and continuation) arguments will fit in the available machine registers.
 *
 * QUESTION: this pass is currently applied before CPS optimization, but if we
 *   implemented something like useless-variable elimination, we might be able
 *   to avoid spilling in some cases.
 *)

functor CPStrans (MachSpec : MACH_SPEC) : sig

    val cpstrans : CPS.function -> CPS.function

  end = struct

    open CPS
    structure LV = LambdaVar

    fun bug s = ErrorMsg.impossible ("CPStrans: " ^ s)
    val mkv = LV.mkLvar

    val unboxedfloat = MachSpec.unboxedFloats

  (**************************************************************************
   *                    TOP OF THE MAIN FUNCTION                            *
   **************************************************************************)
    fun cpstrans fe = let
        (* variable substitution table *)
	  exception CPSSUBST
	  val M : value LV.Tbl.hash_table = LV.Tbl.mkTable(32,CPSSUBST)
	  val addvl = LV.Tbl.insert M
	  fun mapvl v = ((LV.Tbl.lookup M v) handle CPSSUBST => VAR v)
        (* variable to type hash*)
	  exception CTYMAP
	  val CT : cty LV.Tbl.hash_table = LV.Tbl.mkTable(32,CTYMAP)
	  val addty = LV.Tbl.insert CT
	  val getty = LV.Tbl.lookup CT
	  fun grabty (VAR v) = ((getty v) handle _ => CPSUtil.BOGt)
	    | grabty (NUM{ty, ...}) = NUMt ty
	    | grabty (REAL{ty, ...}) = FLTt ty
	    | grabty _ = CPSUtil.BOGt

	(**************************************************************************
	 *          UTILITY FUNCTIONS THAT DO THE ARGUMENT SPILLING               *
	 **************************************************************************)

	(** the following values must be consistent with the choices made
	 ** in the closure or spilling phases
	 *)
	  val fpnum = Int.min(MachSpec.numFloatRegs-2, MachSpec.numArgRegs)
	  val nregs = MachSpec.numRegs - MachSpec.numCalleeSaves
	  val gpnum = Int.min(nregs - 3, MachSpec.numArgRegs)

        (* analyze a list of arguments to determine if they fix in the available target-machine
         * registers or if we need to spill some of them to the heap.  If we need to spill,
         * we return `SOME(ovs, ots, gvs, gts, fts)`, where
         *    ovs  original arguments
         *    ots  original argument types
         *    gvs  spilled general arguments
         *    gts  spilled argument types
         *    fvs  spilled real arguments
         * If no spilling is necessary, return `NONE`
         *)
	  fun argSpill (args, ctys) = let
                (* helper to determine which arguments to spill.  The parameters are
                 *    xs   list of argument variables
                 *    cts  list of argument types
                 *    ngp  number of available general-purpose registers
                 *    nfp  number of available floating-point registers
                 *    ovs  accumulated original arguments
                 *    ots  accumulated original argument types
                 *    gvs  accumulated spilled general arguments
                 *    gts  accumulated spilled argument types
                 *    fvs  accumulated spilled real arguments
                 * The accumulators are all in reverse order.
                 * REAL32: need to track types of spilled real arguments too
                 *)
(* QUESTION: if we spill floating-point arguments, that should increases the pressure
 * on the integer arguments by one, but this code does not seem to account for that.
 *)
		fun h ([], [], ngp, nfp, ovs, ots, [], [], []) = NONE
		  | h ([], [], ngp, nfp, ovs, ots, [x], [_], []) = NONE
		  | h ([], [], ngp, nfp, ovs, ots, gvs, gts, fvs) =
		      SOME(rev ovs, rev ots, rev gvs, rev gts, rev fvs)
		  | h(x::xs, ct::cts, ngp, nfp, ovs, ots, gvs, gts, fvs) = (case ct
		       of FLTt 64 => if nfp > 0
			    then h (xs, cts, ngp, nfp-1, x::ovs, ct::ots, gvs, gts, fvs)
			    else h (xs, cts, ngp, nfp, ovs, ots, gvs, gts, x::fvs)
			| FLTt n => raise Fail(concat[ (* REAL32: FIXME *)
			      "argSpill: FLTt ", Int.toString n, " is unsupported"
			    ])
			| _ => if ngp > 0
			    then h (xs, cts, ngp-1, nfp, x::ovs, ct::ots, gvs, gts, fvs)
			    else h (xs, cts, ngp, nfp, ovs, ots, x::gvs, ct::gts, fvs))
		  | h _ = bug "unexpected case in argSpill"
		val n = length args
		in
		  if (n > fpnum) orelse (n > gpnum)
		    then h (args, ctys, gpnum, fpnum, [], [], [], [], [])
		    else NONE
		end (* function argSpill *)
        (* spill code for the arguments of a function application *)
	  fun spillIn (origargs, origctys, spgvars, spgctys, spfvars) = let
		val (fhdr, spgvars, spgctys) = (case spfvars
		       of [] => (Fn.id, spgvars, spgctys)
			| _ => let
                          (* we have floating-point arguments to spill *)
			    val v = mkv()
			    val vs = map (fn x => (x, OFFp 0)) spfvars
			    val ct = PTRt(FPT (length vs))
			    fun fh e = RECORD(RK_RAW64BLOCK, vs, v, e)
			    in
			      (fh, (VAR v)::spgvars, ct::spgctys)
			    end
		      (* end case *))
		val (spgv, ghdr) = (case spgvars
		       of [] => (NONE, fhdr)
			| [x] => (SOME x, fhdr)
			| _ => let
                          (* we have general-purpose arguments to spill *)
			    val v = mkv()
			    val vs = map (fn x => (x, OFFp 0)) spgvars
                            fun gh e = fhdr (RECORD(RK_RECORD, vs, v, e))
			    in
			      (SOME (VAR v), gh)
			    end
		      (* end case *))
		in
		  case spgv
		   of SOME x => SOME(origargs@[x], ghdr)
		    | NONE => NONE
		  (* end case *)
		end (* spillIn *)
        (* spill code for the parameters of a function *)
	  fun spillOut (origargs, origctys, spgvars, spgctys, spfvars) = let
		val (spfv, fhdr, spgvars, spgctys) = (case spfvars
		       of [] => (NONE, Fn.id, spgvars, spgctys)
			| _ => let
                          (* we have spilled floating-point arguments *)
			    val v = mkv()
			    val v' = VAR v
                            fun fh e = List.foldri
                                  (fn (i, sv, e) => SELECT(i, v', sv, FLTt 64, e)) (* REAL32: FIXME *)
                                    e spfvars
			    val ct = PTRt(FPT(List.length spfvars))
			    in
			      (SOME v, fh, v::spgvars, ct::spgctys)
			    end
		      (* end case *))
		val (spgv, ghdr) = (case (spgvars, spgctys)
		       of ([], _) => (NONE, fhdr)
			| ([x], t::_) => (SOME(x, t), fhdr)
			| _ => let
                          (* we have spilled general-purpose arguments *)
			    val v = mkv()
			    val v' = VAR v
                            fun gh e = ListPair.foldri
                                  (fn (i, sv, st, e) => SELECT(i, v', sv, st, e))
                                    (fhdr e)
                                      (spgvars, spgctys)
			    val ct = PTRt(RPT(List.length spgvars))
			    in
			      (SOME (v, ct), gh)
			    end
		      (* end case *))
		in
		  case spgv
		   of SOME(x, t) => SOME (origargs@[x], origctys@[t], ghdr)
		    | NONE => NONE
		  (* end case *)
		end (* spillOut *)

	(* mkArgIn : value list -> (cexp -> cexp * value list) option
         * process the arguments of an application.
         *)
	  fun mkArgIn (args : value list) =
		Option.mapPartial spillIn (argSpill (args, List.map grabty args))

	(* mkArgOut : lvar list -> (lvar list * cty list * cexp -> cexp) option
         * process the parameters of a function on entry
         *)
	  fun mkArgOut args =
		Option.mapPartial spillOut (argSpill (args, List.map getty args))

	(**************************************************************************
	 *              MAIN FUNCTIONS THAT TRANSLATE CPS CODE                    *
	 **************************************************************************)
	  fun cexptrans ce = (case ce
		 of RECORD(k,vl,w,ce) => RECORD(k,map rectrans vl,w,cexptrans ce)
		  | SELECT(i,v,w,t,ce) => let
		      val _ = addty(w,t)
		      val v' = vtrans v
		      val ce' = cexptrans ce
		      in
			SELECT(i, v', w, getty w, ce')
		      end
		  | OFFSET(i,v,w,ce) => OFFSET(i, vtrans v, w, cexptrans ce)
		  | APP(v,vl) => (case mkArgIn vl
		       of SOME (nvl, hdr) => cexptrans(hdr(APP(v, nvl)))
			| NONE =>  APP(vtrans v, map vtrans vl)
		      (* end case *))
		  | FIX(l,ce) => FIX(map functrans l, cexptrans ce)
		  | SWITCH(v,c,l) => SWITCH(vtrans v,c,map cexptrans l)
		  | LOOKER(p,vl,w,t,ce) => let
		      val _ = addty(w,t)
		      val vl' = map vtrans vl
		      val ce' = cexptrans ce
		      in
			LOOKER(p, vl', w, getty w, ce')
		      end
		  | SETTER(p,vl,ce) => SETTER(p, map vtrans vl, cexptrans ce)
		  | ARITH(p,vl,w,t,ce) => (
		      addty(w,t);
		      ARITH(p, map vtrans vl, w, t, cexptrans ce))
		  | RCC(k,l,p,vl,wtl,ce) => (
		      List.app addty wtl;
		      RCC(k, l, p, map vtrans vl, wtl, cexptrans ce))
		  | PURE(P.BOX, [u], w, t, ce) => (addvl(w,vtrans u); cexptrans ce)
		  | PURE(P.UNBOX, [u], w, t, ce) => (
		      case u of VAR z => addty(z, t) | _ => ();
		      addvl(w,vtrans u); cexptrans ce)
		  | PURE(p as P.WRAP(P.INT sz), [u], w, t, ce) =>
		      if (sz <= Target.defaultIntSz)
			then (  (* remove wrapping of tagged ints *)
			  addvl(w, vtrans u);
			  cexptrans ce)
			else (
			  addty(w,t);
			  PURE(p, [vtrans u], w, t, cexptrans ce))
		  | PURE(p as P.UNWRAP(P.INT sz), [u], w, t, ce) =>
		      if (sz <= Target.defaultIntSz)
			then (  (* remove unwrapping of tagged ints *)
			  addvl(w,vtrans u);
			  cexptrans ce)
			else (
			  addty(w,t);
			  PURE(p, [vtrans u], w, t, cexptrans ce))
		  | PURE(p as P.WRAP(P.FLOAT _), [u], w, t, ce) =>
		      if unboxedfloat
			then (addty(w,t); PURE(p, [vtrans u], w, t, cexptrans ce))
			else (addvl(w,vtrans u); cexptrans ce)
		  | PURE(p as P.UNWRAP(P.FLOAT _), [u], w, t, ce) =>
		      if unboxedfloat
			then (addty(w,t); PURE(p, [vtrans u], w, t, cexptrans ce))
			else (addvl(w,vtrans u); cexptrans ce)
		  | PURE(P.GETCON,[u],w,t,ce) => (
		      addty (w, t);
		      SELECT(0,vtrans u,w,t,cexptrans ce))
		  | PURE(P.GETEXN,[u],w,t,ce) => (
		      addty (w, t);
		      SELECT(0,vtrans u,w,t,cexptrans ce))
		  | PURE(p,vl,w,t,ce) => let
		      val _ = addty(w,t)
		      val vl' = map vtrans vl
		      val ce' = cexptrans ce
		      in
			PURE(p, vl', w, getty w, ce')
		      end
		  | BRANCH(p,vl,c,e1,e2) =>
		      BRANCH(p, map vtrans vl, c, cexptrans e1, cexptrans e2)
		(* end case *))

	  and functrans (fk, v, args, cl, ce) = let
		val _ = ListPair.app addty (args,cl)
		val ce' = cexptrans ce
		in
		  case mkArgOut args
		   of SOME (nargs, nctys, fhdr) => (fk, v, nargs, nctys, fhdr ce')
		    | NONE => (fk, v, args, cl, ce')
		  (* end case *)
		end

	  and rectrans(v, acp) = (vtrans v, acp)

	  and vtrans (VAR v) = (mapvl v)
            | vtrans u = u

          in
	    functrans fe
	  end (* cpstrans *)

  end (* structure CPStrans *)

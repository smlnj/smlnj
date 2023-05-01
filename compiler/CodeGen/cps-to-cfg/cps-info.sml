(* cps-info.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Information about a CPS compilation unit.
 *
 * TODO: to get the information about base pointers right after cluster
 *       normalization, we will either need to propagate info from the
 *	 clustering phase or else do a global analysis of the compilation
 *	 unit.
 *)

structure CPSInfo : sig

    type cluster = CPS.function list

  (* the container of various bits of information about the program *)
    type global_info

  (* the container of information local to a CPS fucntion (aka fragment) *)
    type local_info

  (* abstraction of a use count: a TREE variable has just one use, and
   * a BOUND variable has multiple uses.  Unused variables do not appear
   * in the table.
   *)
    datatype use_mode = TREE | BOUND

  (* initialize the global information from a list of clusters *)
    val analyze : cluster list -> global_info

  (* initialize the info container for a cluster; this call returns a function
   * for initializing local, per-function info.
   *)
    val newCluster : global_info -> CPS.function -> local_info

  (* get the cluster attributes *)
    val clusterAttrs : global_info -> CFG.attrs

  (* get the function kind for the given function name *)
    val funKindOf : local_info -> LambdaVar.lvar -> CPS.fun_kind

  (* return true if the lvar is the label of a cluster entry fragment, which is
   * defined to be the first function in a cluster when we are using the LLVM backend.
   *)
    val isEntry : local_info -> LambdaVar.lvar -> bool

  (* add type info for a CPS variable; this information is local to the current
   * function.
   *)
    val addTy : local_info -> LambdaVar.lvar * CPS.cty -> unit

  (* initialize the information about the parameters of a CPS function *)
    val addParams : local_info * LambdaVar.lvar list * CPS.cty list -> unit

    val typeOf : local_info -> LambdaVar.lvar -> CPS.cty

    val modeOf : local_info -> LambdaVar.lvar -> use_mode

    val numVars : local_info -> int

  (* `invokeGC (info, kind, f, params)` returns an expression for invoking the
   * garbage collector, where `params` is the list of live lvars (with types)
   * and `f` is the label of the fragment where the collection occurs.  The
   * actual code returned depends on the `frag_kind`, since some GC routines
   * can be shared across clusters.
   *)
    val invokeGC : local_info * CFG.frag_kind * LambdaVar.lvar * CFG.param list -> CFG.stm

  (* return the invoke-GC clusters for the compilation unit *)
    val getGCCode : global_info -> CFG.cluster list

  end = struct

    structure P = CPS.P
    structure Set = LambdaVar.Set
    structure Tbl = LambdaVar.Tbl

    datatype value = datatype CPS.value
    datatype cexp = datatype CPS.cexp

    type cluster = CPS.function list

    datatype global_info = GInfo of {
	gTbl : CPS.fun_kind Tbl.hash_table,	(* function kinds *)
	entryFns : Set.set,
      (* cache of GC invocation routines *)
	gcCache : InvokeGC.t,
      (* cluster attributes *)
	alignHP : int ref,
	needsBasePtr : bool ref,
	hasTrapArith : bool ref,
	hasRCC : bool ref
      }

    datatype use_mode = TREE | BOUND

    datatype local_info = LInfo of {
	gInfo : global_info,
	modes : use_mode Tbl.hash_table,	(* maps LVars to uses *)
	typs : CPS.cty Tbl.hash_table		(* maps LVars to types *)
      }

  (* pointers to unknown objects *)
    val BOGty = CPSUtil.BOGt

    fun newFunction gInfo (fk, f, params, paramTys, body) = let
	  val GInfo{alignHP, needsBasePtr, hasTrapArith, hasRCC, ...} = gInfo
	  fun align sz = if !alignHP < sz then alignHP := sz else ()
	(* table to track use modes of lvars *)
	  val modes = Tbl.mkTable(32, Fail "modes")
	  val getMode = Tbl.find modes
	  val setMode = Tbl.insert modes
	  fun useVar x = (case getMode x
		 of NONE => setMode (x, TREE)
		  | SOME TREE => setMode (x, BOUND)
		  | _ => ()
		(* end case *))
	  fun useVal (VAR x) = useVar x
	    | useVal _ = ()
	  val useVals = List.app useVal
	(* use a value in an argument position (as opposed to an application position) *)
	  fun useArg (VAR x) = useVar x
	    | useArg (LABEL l) = (needsBasePtr := true)
	    | useArg _ = ()
	  val useArgs = List.app useArg
	(* mark a variable as "BOUND" since it will map to a CFG variable *)
	  fun bindVar x = setMode (x, BOUND)
(* QUESTION: should we track CFG types instead of CPS types? *)
	  val typs = Tbl.mkTable(32, Fail "typs")
	  val recordTy = Tbl.insert typs
	  fun init cexp = (case cexp
		 of RECORD(rk, flds, x, k) => (
		      case rk
		       of CPS.RK_FCONT => align 8
			| CPS.RK_RAW64BLOCK => align 8
			| _ => ()
		      (* end case *);
		      recordTy (x, BOGty);
		      List.app (useArg o #1) flds;
		      init k)
		  | SELECT(_, v, x, ty, k) => (recordTy (x, ty); useVal v; init k)
		  | OFFSET(_, v, x, k) => (recordTy (x, BOGty); useVal v; init k)
		  | APP(f, vs) => (useVal f; useArgs vs)
		  | FIX _ => raise Fail "unexpected FIX"
		  | SWITCH(v, _, ks) => (
		      needsBasePtr := true;
		      useVal v; List.app init ks)
		  | BRANCH(_, vs, _, k1, k2) => (useVals vs; init k1; init k2)
		  | SETTER(_, vs, k) => (useArgs vs; init k)
		  | LOOKER(_, vs, x, ty, k) => (
		    (* we mark `x` as `BOUND` so that its definition is not moved
		     * across `SETTERs`.
		     *)
		      bindVar x;
		      recordTy (x, ty);
		      useVals vs;
		      init k)
		  | ARITH(_, vs, x, ty, k) => (
		      hasTrapArith := true;
		    (* we mark `x` as `BOUND` so that its definition is not moved
		     * across `SETTERs`.
		     *)
		      bindVar x;
		      recordTy (x, ty); useVals vs; init k)
		  | PURE(rator, vs, x, ty, k) => (
		    (* check for pure operations that map to other forms in CFG *)
		      case rator
		       of P.MAKEREF => bindVar x
			| P.NEWARRAY0 => bindVar x
			| P.WRAP(P.INT sz) => (align(sz div 8); bindVar x)
			| P.WRAP(P.FLOAT sz) => (align(sz div 8); bindVar x)
			| P.RAWRECORD(SOME CPS.RK_RAW64BLOCK) => (
			    align 8; bindVar x)
			| P.RAWRECORD _ => bindVar x
			| _ => ()
		      (* end case *);
		      recordTy (x, ty); useVals vs; init k)
		  | RCC(_, _, _, vs, xs, k) => (
		      hasRCC := true;
		      List.app (bindVar o #1) xs;
		      List.app recordTy xs; useVals vs; init k)
		(* end case *))
	  in
	    init body;
	    LInfo{gInfo = gInfo, modes = modes, typs = typs}
	  end

    fun newCluster gInfo = let
	  val GInfo{alignHP, needsBasePtr, hasTrapArith, hasRCC, ...} = gInfo
	  in
	  (* reset cluster attributes *)
	    alignHP := Target.alignInBytes;
	    needsBasePtr := false;
	    hasTrapArith := false;
	    hasRCC := false;
	  (* return creation function for per-function info *)
	    newFunction gInfo
	  end

    fun analyze clusters = let
	(* mapping of CPS function lvars to CPS types *)
	  val kindTbl = Tbl.mkTable(32, Fail "kindTbl")
	  val addKindBinding = Tbl.insert kindTbl
	  fun doFun (fk, f, _, _, _) = addKindBinding(f, fk)
	  fun doCluster (entry::funcs, entries) = (
		doFun entry;
		List.app doFun funcs;
		Set.add(entries, #2 entry))
	  val entryFns = List.foldl doCluster Set.empty clusters
	  in
	    GInfo{
		gTbl = kindTbl,
		entryFns = entryFns,
		gcCache = InvokeGC.new(),
		alignHP = ref(Target.mlValueSz div 8),
		needsBasePtr = ref false,
		hasTrapArith = ref false,
		hasRCC = ref false
	      }
	  end

  (* get the cluster attributes *)
    fun clusterAttrs (GInfo{alignHP, needsBasePtr, hasTrapArith, hasRCC, ...}) = {
	    alignHP = !alignHP,
	    needsBasePtr = !needsBasePtr,
	    hasTrapArith = !hasTrapArith,
	    hasRCC = !hasRCC
	  }

    fun funKindOf (LInfo{gInfo=GInfo{gTbl, ...}, ...}) = Tbl.lookup gTbl

    fun isEntry (LInfo{gInfo=GInfo{entryFns, ...}, ...}) f = Set.member(entryFns, f)

    fun addTy (LInfo{typs, ...}) = Tbl.insert typs

    fun addParams (LInfo{typs, modes, ...}, params, paramTys) = let
	  val recordTy = Tbl.insert typs
	  val setMode = Tbl.insert modes
	  fun doParam (x, cty) = (
		recordTy (x, cty);
		setMode (x, BOUND))
	  in
	    ListPair.appEq doParam (params, paramTys)
	  end

    fun typeOf (LInfo{gInfo=GInfo{gTbl, ...}, typs, ...}) = let
	  val lFind = Tbl.find typs
	  val gFind = Tbl.find gTbl
	  in
	    fn lv => (case lFind lv
		 of NONE => (case gFind lv
		       of NONE => raise Fail("unbound lvar " ^ LambdaVar.lvarName lv)
			| SOME CPS.CONT => CPS.CNTt
			| SOME _ => CPS.FUNt
		      (* end case *))
		  | SOME ty => ty
		(* end case *))
	  end

    fun modeOf (LInfo{modes, ...}) = let val find = Tbl.find modes
	  in
	    fn lv => (case find lv
		 of SOME m => m
		  | NONE => TREE (* an unused variable that somehow snuck through!! *)
		(* end case *))
	  end

    fun numVars (LInfo{modes, ...}) = Tbl.numItems modes

  (* GC support *)
    fun invokeGC (LInfo{gInfo=GInfo{gcCache, ...}, ...}, kind, f, params) = (
	  case kind
	   of CFG.STD_FUN => InvokeGC.invokeStdFunGC (gcCache, params)
	    | CFG.STD_CONT => InvokeGC.invokeStdContGC (gcCache, params)
	    | CFG.INTERNAL => InvokeGC.invokeInternalGC (f, params)
	    | _ => raise Match
	  (* end case *))

    fun getGCCode (GInfo{gcCache, ...}) = InvokeGC.getCompUnitCode gcCache

  end

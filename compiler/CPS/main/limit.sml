(* limit.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Compute allocation amounts and change the kind of those KNOWN functions
 * that require a check to KNOWN_CHECK.
 *)

structure Limit : sig

  (* `allocChecks clusters` returns the pair `(clusters', maxAllocOf)`, where `clusters`
   * is a revised list of clusters where the function kind of known functions that require
   * a GC check have been changed to `KNOWN_CHECK` and the `maxAllocOf` is a function that
   * maps function labels to the maximum number of words allocated by the function on
   * any execution path.
   *)
    val allocChecks : Cluster.cluster list -> Cluster.cluster list * (CPS.lvar -> int)

  end = struct

    structure C = CPS
    structure P = C.P
    structure LV = LambdaVar
    structure Tbl = LV.Tbl

    val debug = Control.CG.printit
    val say = Control.Print.say
    val error = ErrorMsg.impossible

  (* maximum number of words to allocate per check, which is one less than the slop *)
    val MAX_ALLOC = 1023

  (* `findFunKinds clusters` returns the record `{kindOf, addCheck}`, where `kindOf`
   * is a mapping from function labels to their kind and `addCheck` is a function
   * for marking known functions as `KNOWN_CHECK`.
   *)
    fun findFunKinds clusters = let
	  val m = Tbl.mkTable(32, Fail "FunKinds")
	  val kindOf = Tbl.lookup m
	  val _ = Cluster.app (fn (k, f, _, _, _) => Tbl.insert m (f, k)) clusters
	  in {
	    kindOf = kindOf,
	    addCheck = fn f => (case kindOf f
	       of C.KNOWN => Tbl.insert m (f, C.KNOWN_CHECK)
		| _ => ()
	      (* end case *))
	  } end

  (* size of RAW64BLKOCK or FCONT in ml-value-sized words *)
    val record64Sz = if Target.is64
	  then fn n => n + 1
	  else fn n => n + n + 2

    val seqHdrSz = 3		(* size of sequence header object *)
    val storeListSz = 2		(* size of store list entry *)

  (* extra alignment for 64-bit data *)
    val raw64Pad = if Target.is64 then 0 else 1

  (* path now counts instructions as well as allocations, for polling *)
    fun path kindOf clusters = let
	  exception LimitExn
	(* map from function label to function body *)
	  val b : C.cexp Tbl.hash_table = Tbl.mkTable(32, LimitExn)
	  val _ = Cluster.app (Tbl.insert b o (fn (_, f, _, _, e) => (f, e))) clusters
	  val bodyOf = Tbl.lookup b
	(* map from function label to info *)
	  val infoTbl : {kind : C.fun_kind, alloc : int} Tbl.hash_table = Tbl.mkTable(32, LimitExn)
	  val setInfo = Tbl.insert infoTbl
	  val getInfo = Tbl.find infoTbl
	(* compute required storage in ml-value-sized words *)
	  fun count (allocSzW, exp) = let
		fun continue e = count (allocSzW, e)
		fun inc (n, e) = count (allocSzW+n, e)
		in
		  case exp
		   of C.RECORD(C.RK_RAW64BLOCK, vl, _, e) => inc( record64Sz(length vl), e)
		    | C.RECORD(C.RK_FCONT, vl, _, e) => inc (record64Sz(length vl), e)
		    | C.RECORD(C.RK_VECTOR, vl, _, e) => inc (length vl + (seqHdrSz + 1), e)
		    | C.RECORD(_, vl, _, e) => inc (length vl + 1, e)
		    | C.SELECT(_, _, _, _, e) => continue e
		    | C.OFFSET(_, _, _, e) => continue e
		    | C.APP(C.LABEL lab, _) => (case maxPath lab
			 of {kind=C.KNOWN, alloc} =>
			    if allocSzW+alloc > MAX_ALLOC
			      then (
				setInfo (lab, {kind=C.KNOWN_CHECK, alloc=alloc});
				allocSzW)
			      else allocSzW + alloc
			  | _ => allocSzW
			(* end case *))
		    | C.APP _ => allocSzW
		    | C.FIX _ => error "FIX in Limit.count"
		    | C.SWITCH(_, _, el) => List.foldl (fn (e, n) => Int.max(continue e, n)) 0 el
		    | C.BRANCH(_, _, _, e1, e2) => Int.max(continue e1, continue e2)
		    | C.SETTER(P.ASSIGN, _, e) => inc (storeListSz, e)
		    | C.SETTER(P.UPDATE, _, e) => inc (storeListSz, e)
		    | C.SETTER(_, _, e) => continue e
		    | C.LOOKER(_, _, _, _, e) => continue e
		    | C.ARITH(P.TEST_INF _, _, _, _, _) => error "TEST_INF in Limit.count"
		    | C.ARITH(_, _, _, _, e) => continue e
		    | C.PURE(P.WRAP(P.INT sz), _, _, _, e) =>
			if (sz = Target.mlValueSz)
			  then inc (2, e)
			else if (sz <= Target.defaultIntSz)
			  then error "unexpected tagged int wrap in Limit.count"
			  else inc (1 + sz div Target.mlValueSz, e)
		    | C.PURE(P.WRAP(P.FLOAT sz), _, _, _, e) => inc (record64Sz 1, e)
		    | C.PURE(P.NEWARRAY0, _, _, _, e) => inc (seqHdrSz + 2, e)
		    | C.PURE(P.MAKEREF, _, _, _, e) => inc (2, e)
		    | C.PURE(P.MKSPECIAL, _, _, _, e) => inc (2, e)
		    | C.PURE(P.RAWRECORD NONE, [C.NUM{ty={tag=true, ...}, ival}], _, _, e) =>
			inc (IntInf.toInt ival, e)
		    | C.PURE(P.RAWRECORD _, [C.NUM{ty={tag=true, ...}, ival}], _, _, e) =>
			inc (IntInf.toInt ival + 1, e)
		    | C.PURE(P.RAWRECORD _, _, _, _, _) => error "bogus RAWRECORD in Limit.count"
		    | C.PURE((P.TRUNC_INF _ | P.EXTEND_INF _ | P.COPY_INF _), _, _, _, e) =>
			error "*_INF in Limit.count"
		    | C.PURE(_, _, _, _, e) => continue e
		    | C.RCC(_, _, _, _, _, e) => continue e
		  (* end case *)
		end
	  and maxPath lab = (case getInfo lab
		 of SOME info => info
		  | NONE => (
		    (* Note that the heap may need to be aligned so g is
		     * called with g(raw64Pad, bod). Be conservative.
		     *)
		      case kindOf lab
		       of C.KNOWN => let
			    val body = bodyOf lab
			    val n = count(raw64Pad, bodyOf lab)
			    val info = if n > MAX_ALLOC
				  then {kind=C.KNOWN_CHECK, alloc=n}
				  else {kind=C.KNOWN, alloc=n}
			    in
			      setInfo (lab, info);
			      info
			    end
		        | kind => let
			  (* initialize info for lab *)
			    val _ = setInfo (lab, {kind=kind, alloc=0})
			  (* compute allocation amount *)
			    val info = {kind = kind, alloc = count (1, bodyOf lab)}
			    in
			      setInfo (lab, info); info
			    end
		      (* end case *))
		(* end case *))
	(* compute maximum path for all functions *)
	  val _ = Cluster.app (fn (_, lab, _, _, _) => ignore(maxPath lab)) clusters
	(* update function kinds *)
	  val look = Tbl.lookup infoTbl
	  val clusters' = Cluster.map
		(fn (fk, lab, args, tys, e) => (#kind(look lab), lab, args, tys, e))
		  clusters
	  in
	    (clusters', fn lab => #alloc(look lab))
	  end

  (* use feedback-vertex analysis to place heap limit checks *)
    fun addChecks clusters = let
	  val {kindOf, addCheck} = findFunKinds clusters
	  fun mkNode ((_, f, vl, _, body), nds) = let
		fun edges (C.RECORD(_,_,_,e)) = edges e
		  | edges (C.SELECT(_,_,_,_,e)) = edges e
		  | edges (C.OFFSET(_,_,_,e)) = edges e
		  | edges (C.SWITCH(_,_,el)) = List.concat (map edges el)
		  | edges (C.SETTER(_,_,e)) = edges e
		  | edges (C.LOOKER(_,_,_,_,e)) = edges e
		  | edges (C.ARITH(_,_,_,_,e)) = edges e
		  | edges (C.PURE(_,_,_,_,e)) = edges e
		  | edges (C.RCC(_,_,_,_,_,e)) = edges e
		  | edges (C.BRANCH(_,_,_,a,b)) = edges a @ edges b
		  | edges (C.APP(C.LABEL w, _)) = (case kindOf w of C.KNOWN => [w] | _ => [])
		  | edges (C.APP _) = []
		  | edges (C.FIX _) = error "FIX in Limit.mkNode"
		in
		  (f, edges body) :: nds
		end
	(* construct the CFG for the whole compilation unit *)
	  val nodes = List.foldr
		(fn (cluster, nds) => List.foldr mkNode nds cluster)
		  [] clusters
	  in
	    if !debug
	      then (say "Starting feedback..."; Control.Print.flush())
	      else ();
	    List.app addCheck (Feedback.feedback nodes);
	    if !debug
	      then (say "Finished\n"; Control.Print.flush())
	      else ();
	    kindOf
	  end

    fun allocChecks clusters = let
	  val kindOf = addChecks clusters
	  val info = path kindOf clusters
	  fun showInfo (fk, f, _, _, _) = let
		val alloc = (#2 info) f
		val s = Int.toString alloc
		in
		  say (StringCvt.padRight #" " 7 (LV.lvarName f));
		  case fk
		   of C.KNOWN => say " K  "
		    | C.KNOWN_CHECK => say " H  "
		    | C.ESCAPE => say " E  "
		    | C.CONT => say " C  "
		    | _ => error "bogus function kind in Limit.showInfo"
		  (* end case *);
		  say s; say "\n"
		end
	  in
	    if !debug
	      then Cluster.app showInfo (#1 info)
	      else ();
	   info
	  end

  end (* structure Limit *)

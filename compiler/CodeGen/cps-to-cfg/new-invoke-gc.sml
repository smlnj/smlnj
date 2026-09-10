(* invoke-gc.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Support for adding garbage-collection invocation code to the CFG.
 *)

structure InvokeGC : sig

  (* cache of shared GC code *)
    type t

  (* allocate a fresh cache for a compilation unit *)
    val new : unit -> t

  (* return code to call a invokeGC cluster for a standard function with
   * the given live parameters.
   *)
    val invokeStdFunGC : t * CFG.param list -> CFG.stm

  (* return code to call a invokeGC cluster for a standard continuation with
   * the given live parameters.
   *)
    val invokeStdContGC : t * CFG.param list -> CFG.stm

  (* return a CFG statment that invokes the GC and then returns control
   * to the given label.
   *)
    val invokeInternalGC : LambdaVar.lvar * CFG.param list -> CFG.stm

  (* return the invoke-GC clusters for the compilation unit *)
    val getCompUnitCode : t -> CFG.cluster list

  end = struct

    structure C = CFG
    structure P = CFG_Prim
    structure LV = LambdaVar
(* FIXME
    structure D = MS.ObjDesc
*)
    structure D = ObjectDesc

    (* a GC signature is a pair `(isCont, tys)`, where `isCont` is true
     * for continuations and `tys` is a list of the CFG types for the
     * parameters at the GC point.
     *)
    type gc_sig = bool * CFG.ty list

    (* finite maps from GC signatures *)
    structure GCInfoMap = RedBlackMapFn (
      struct
	type ord_key = gc_sig
      (* lexical order on the GC root type signature *)
	fun compare ((false, _), (true, _)) = LESS
	  | compare ((true, _), (false, _)) = GREATER
	  | compare ((_, tys1), (_, tys2)) = let
              (* when comparing types, we treat `LABt`, `PTRt`, and `TAGt` as
               * equal, since they all have "uniform" representation.
               *)
(* QUESTION: since we do not pack untagged numbers, it might be safe to ignore the
 * size component of `NUMt` types.
 *)
              fun cmpTy (C.NUMt{sz=s1}, C.NUMt{sz=s2}) = Int.compare(s1, s2)
                | cmpTy (C.FLTt{sz=s1}, C.FLTt{sz=s2}) = Int.compare(s1, s2)
                | cmpTy (C.NUMt _, C.FLTt _) = LESS
                | cmpTy (C.FLTt _, C.NUMt _) = GREATER
                | cmpTy (_, C.NUMt _) = LESS
                | cmpTy (_, C.FLTt _) = LESS
                | cmpTy (C.NUMt _, _) = GREATER
                | cmpTy (C.FLTt _, _) = GREATER
                | cmpTy _ = EQUAL
              in
	        List.collate cmpTy (tys1, tys2)
              end
      end)

    fun error msg = ErrorMsg.impossible(concat("InvokeGC." :: msg))

(* FIXME: once we switch over to the CFG code path, make this structure a functor
 * over the machine spec and set the number of GC roots to 4 + # calleesaves.
 *)
    val numCalleeSaves = 3
    val numGCRoots = 4 + numCalleeSaves


  (***** Code generation for GC invocation *****)

    fun var x = C.VAR{name = x}
    val unit = C.NUM{iv = 1, sz = Target.mlValueSz}
    fun record desc = P.RECORD{desc=desc, mut=false}
    fun rawSelect (nk, sz, n, rr) = C.PURE{
	    oper = P.RAW_SELECT{kind=nk, sz=sz, offset=n},
	    args = [rr]
	  }

  (* index into the list of GC parameters *)
    type index = int

  (* representation of what a GC root holds *)
    datatype root
      = Unit			          (* initialized to "()" *)
      | Ptr of index		          (* pointer value *)
      | Record of {
          roots : index list,             (* pointer parameters *)
          raw : (index * P.numkind) list  (* raw parameters *)
        }

(* DEBUG
fun roots2s roots = concat["[", String.concatWithMap "," root2s roots, "]"]
and root2s Unit = "()"
  | root2s (Ptr i) = "param" ^ Int.toString i
  | root2s (Record{roots, raw}) = let
      fun rawToS (ix, nk) = concat[
	      "param", Int.toString ix, ":", PPCfg.numkindToString(nk, 64)
	    ]
      in
	concat [
            "<|",
            String.concatWithMap "," (fn ix => "param" ^ Int.toString i) roots,
            ";"
            String.concatWithMap "," rawToS flds,
            "|>"
          ]
      end
fun prRoots roots = Control.Print.say (concat["Roots = ", roots2s roots, "\n"])
*)

  (* GC root order: std-link, std-clos, std-cont, callee-saves, std-arg *)
    val stdRootOrder = List.tabulate (numGCRoots, Fn.id)
  (* for STD_CONT, we start with std-cont and then loop around *)
    val contRootOrder = 2 :: List.tabulate (numCalleeSaves+1, fn i => 3 + i) @ [0, 1]

  (* given the list of live variables with types and size, assign the GC root
   * layout.
   *)
    fun assignRoots (isCont, params : CFG.param list) = let
	  fun split (ix, {name, ty} :: params, boxed, raw) = let
		fun doRaw (kind, 64) = split (ix+1, params, boxed, (ix, kind)::raw)
		  | doRaw _ = error ["split: unexpected raw type ", CFGUtil.tyToString ty]
		in
		  case ty
		   of C.NUMt{sz} => doRaw (P.INT, sz)
		    | C.FLTt{sz} => doRaw (P.FLT, sz)
		    | _ => split (ix+1, params, ix::boxed, raw)
		  (* end case *)
		end
	    | split (ix, [], boxed, raw) =
		{n=ix, boxed=List.rev boxed, raw=List.rev raw}
	  val {n, boxed, raw} = split (0, params, [], [])
          val nBoxed = length boxed
          val nRaw = length raw
	(* assign roots; note that to avoid register shuffling, we use a different
	 * order for STD_CONT tests, where the first parameter is STD_CONT.
	 *)
	  val gcRoots = Array.array (numGCRoots, Unit)
          val rootIds = if isCont then contRootOrder else stdRootOrder
          fun assignBoxed (boxed, extra) = let
                fun lp (id::ids, ix::ixs) = (
                      Array.update(gcRoots, id, Ptr ix); lp (ids, ixs))
                  | lp (ids, []) = (case (ids, extra)
                       of (id::_, SOME root) => Array.update(gcRoots, id, root)
                        | (_, NONE) => ()
                        | _ => raise Fail "too many roots"
                      (* end case *))
                  | lp ([], _) = raise Fail "too many roots"
                in
                  lp (rootIds, boxed)
                end
          in
          (* cases:
           *   nRaw = 0 andalso nBoxed <= numGCRoots    then no record
           *   nRaw = 0 andalso nBoxed > numGCRoots     then record
           *   nRaw > 0 andalso nBoxed < numGCRoots     then raw record
           *   nRaw > 0 andalso nBoxed >= numGCRoots    then mixed record
           *)
            if (nRaw = 0)
              then if (nBoxed <= numGCRoots)
                then assignBoxed(boxed, NONE)
                else let
                  val (boxed, excess) = List.splitAt (boxed, numGCRoots-1)
                  in
                    assignBoxed (boxed, SOME(Record{roots=excess, raw=[]}))
                  end
            else if (nBoxed < numGCRoots)
              then assignBoxed (boxed, SOME(Record{roots=[], raw=raw}))
              else let
                val (boxed, excess) = List.splitAt (boxed, numGCRoots-1)
                in
                  assignBoxed (boxed, SOME(Record{roots=excess, raw=raw}))
                end;
            { nParams = n, roots = Array.toList gcRoots }
          end
(* +DEBUG *)
handle ex => (
print(concat["# assignRoots: isCont = ", Bool.toString isCont, ", params = [",
String.concatWithMap "," PPCfg.paramToString params, "]\n"]);
raise ex)
(* -DEBUG *)

    fun fromListMap f l = Vector.fromList (List.map f l)

  (* align an offset to 64-bit alignment *)
    fun align64 offset =
	  Word.toIntX (Word.andb(Word.fromInt offset + 0w7, Word.notb 0w7))

  (* given a list of params that represent the live variables at the GC point,
   * create the wrap/callgc/unwrap code and pass the resulting fresh live
   * values to the continuation `k`.
   *)
    fun callGC (isCont, live, k) = let
	  val {nParams, roots} = assignRoots (isCont, live)
	(* first we construct the code to unpack the live data from the new roots *)
	  val results = Array.array(nParams, unit)
	  fun setResult (i, v) = Array.update(results, i, v)
	  val newRoots = List.tabulate (numGCRoots, fn _ => LV.mkLvar())
	  fun unpack ([], []) = k (Array.toList results)
	    | unpack (_::xs, Unit::rs) = unpack (xs, rs)
	    | unpack (x::xs, Ptr ix::rs) = (
		setResult (ix, var x); unpack (xs, rs))
	    | unpack (x::xs, Record{roots, raw} :: rs) = let
                fun select ix = C.SELECT{idx = ix, arg = var x}
                fun unpackRoots (ix, jx::rs) = (
                      setResult (jx, select ix);
                      unpackRoots (ix+1, rs))
                  | unpackRoots (ix, []) = unpackRaw (ix, raw)
                and unpackRaw (ix, (jx, nk)::rs) = (
                      setResult (jx, rawSelect (nk, 64, 8*ix, var x));
                      unpackRaw (ix+1, rs))
                  | unpackRaw (_, []) = unpack (xs, rs)
                in
                  unpackRoots (0, roots)
                end
	    | unpack _ = raise Match
	  val code = unpack (newRoots, roots)
	(* pack the roots from the live data *)
	  val live = fromListMap (fn {name, ty} => var name) live
	  fun getLive ix = Vector.sub(live, ix)
	  fun pack ([], args) = C.CALLGC(List.rev args, newRoots, code)
	    | pack (Unit :: rs, args) = pack (rs, unit::args)
	    | pack (Ptr ix :: rs, args) = pack (rs, Vector.sub(live, ix)::args)
            | pack (Record{roots, raw=[]} :: rs, args) = let
                fun packRoots ([], n, flds) = let
		      val desc = D.makeDesc (n, D.tag_record)
		      val tpl = LV.mkLvar()
		      in
			C.ALLOC(record desc, List.rev flds, tpl,
			  pack (rs, var tpl :: args))
		      end
                  | packRoots (ix::ixs, n, flds) =
		      packRoots (ixs, n+1, getLive ix :: flds)
                in
                  packRoots (roots, 0, [])
                end
            | pack (Record{roots=[], raw} :: rs, args) = let
		val tpl = LV.mkLvar()
		val nWords = List.length raw
		val desc = D.makeDesc (nWords, D.tag_raw)
		val (tys, flds) = let
		      fun get ((ix, nk), (tys, flds)) =
			    ({kind=nk, sz=64}::tys, getLive ix :: flds)
		      in
			List.foldr get ([], []) raw
		      end
		in
		  C.ALLOC(P.RAW_RECORD{desc=desc, align=64, fields=tys}, flds, tpl,
		    pack (rs, var tpl :: args))
		end
            | pack (Record{roots, raw} :: rs, args) = let
                fun packRoots (ix::ixs, flds) = packRoots (ixs, getLive ix :: flds)
                  | packRoots ([], flds) = packRaw (raw, flds)
                and packRaw ((ix, _)::rr, flds) = packRaw (rr, getLive ix :: flds)
                  | packRaw ([], flds) = let
                      val ptrLen = List.length roots
                      val rawLen = List.length raw
                      val desc = D.makeMixedDesc {ptrLen = ptrLen, rawLen = rawLen}
                      val tpl = LV.mkLvar()
                      in
                        C.ALLOC(record desc, List.rev flds, tpl,
                          pack (rs, var tpl :: args))
		      end
                in
                  packRoots (roots, [])
                end
	  val code = pack (roots, [])
	  in
	    code
	  end

  (***** Cache of shared GC invocation functions *****)

  (* map from GC type signature to the entry label of an invokeGC cluster *)
    datatype t = Cache of LV.lvar GCInfoMap.map ref

    fun new () = Cache(ref GCInfoMap.empty)

  (* unzip a list of parameters into a list of arguments and types *)
    fun unzip ([], args, tys) = (List.rev args, List.rev tys)
      | unzip ({name, ty}::params, args, tys) =
	  unzip (params, C.VAR{name=name}::args, ty::tys)

    fun invokeStdGC (Cache gcMap, isCont, params) = let
	  val (args, tys) = unzip (params, [], [])
	  val gcSig = (isCont, tys)
	  fun jmp lab = if isCont
		then C.THROW(C.LABEL{name=lab}, args, tys)
		else C.APPLY(C.LABEL{name=lab}, args, tys)
	  in
	    case GCInfoMap.find (!gcMap, gcSig)
	     of SOME lab => jmp lab
	      | NONE => let
		  val lab = LV.mkLvar()
		  in
		    gcMap := GCInfoMap.insert (!gcMap, gcSig, lab);
		    jmp lab
		  end
	    (* end case *)
	  end

    fun invokeStdFunGC (cache, params) = invokeStdGC (cache, false, params)

    fun invokeStdContGC (cache, params) = invokeStdGC (cache, true, params)

    fun invokeInternalGC (lab, params) =
	  callGC (false, params, fn live => C.GOTO(lab, live))

    fun getCompUnitCode (Cache gcMap) = let
	  fun mkCluster ((isCont, tys), lab, clusters) = let
		val params = List.map (fn ty => {name = LV.mkLvar(), ty = ty}) tys
		val frag = if isCont
		      then C.Frag{
			  kind = C.STD_CONT, lab = lab, params = params,
			  body = callGC (true, params, fn live =>
			    C.THROW(hd live, live, tys))
			}
		      else C.Frag{
			  kind = C.STD_FUN, lab = lab, params = params,
			  body = callGC (false, params, fn live =>
			    C.APPLY(hd live, live, tys))
			}
		val alignHP = let
		      fun align (C.NUMt{sz}, a) = Int.max(sz div 8, a)
			| align (C.FLTt{sz}, a) = Int.max(sz div 8, a)
			| align (_, a) = a
		      in
			List.foldl align Target.alignInBytes tys
		      end
		in
		  C.Cluster{
		      attrs = {
			  alignHP = alignHP,
			  needsBasePtr = false,
			  hasTrapArith = false,
			  hasRCC = false
			},
		      frags = [frag]
		    } :: clusters
		end
	  in
	    GCInfoMap.foldri mkCluster [] (!gcMap)
	      before gcMap := GCInfoMap.empty
	  end

  end (* InvokeGC *)

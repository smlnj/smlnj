(* invoke-gc.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
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

    structure GCInfoMap = RedBlackMapFn (
      struct
	type ord_key = bool * CFG.ty list
      (* lexical order on the GC root type signature *)
	fun compare ((false, _), (true, _)) = LESS
	  | compare ((true, _), (false, _)) = GREATER
	  | compare ((_, tys1), (_, tys2)) =
	      List.collate CFGUtil.compareTy (tys1, tys2)
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

  (* representation of what a GC root holds *)
    datatype root
      = Unit			(* initialized to "()" *)
      | Boxed of int		(* boxed live value with index *)
      | Record of root list	(* elements are Param or Raw only *)
      | Raw of (int * P.numkind) list * (int * P.numkind) list

(* DEBUG
fun roots2s roots = concat["[", String.concatWithMap "," root2s roots, "]"]
and root2s Unit = "()"
  | root2s (Boxed i) = "param" ^ Int.toString i
  | root2s (Record roots) = roots2s roots
  | root2s (Raw(raw32, raw64)) = let
      fun toS sz (ix, nk) = concat[
	      "param", Int.toString ix, ":", PPCfg.numkindToString(nk, sz)
	    ]
      val flds = List.map (toS 32) raw32 @ List.map (toS 64) raw64
      in
	concat ["<|", String.concatWith "," flds, "|>"]
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
    fun assignRoots (isCont, params) = let
	  fun split (ix, {name, ty} :: params, boxed, raw32, raw64) = let
		fun doRaw (kind, 32) =
		      split (ix+1, params, boxed, (ix, kind)::raw32, raw64)
		  | doRaw (kind, 64) =
		      split (ix+1, params, boxed, raw32, (ix, kind)::raw64)
		  | doRaw _ = error ["split: unexpected raw type ", CFGUtil.tyToString ty]
		in
		  case ty
		   of C.NUMt{sz} => doRaw (P.INT, sz)
		    | C.FLTt{sz} => doRaw (P.FLT, sz)
		    | _ => split (ix+1, params, Boxed ix::boxed, raw32, raw64)
		  (* end case *)
		end
	    | split (ix, [], boxed, raw32, raw64) =
		{n=ix, boxed=List.rev boxed, raw32=List.rev raw32, raw64=List.rev raw64}
	  val {n, boxed, raw32, raw64} = split (0, params, [], [], [])
	  val nBoxed = length boxed
	  val (rawObj, nBoxed) = (case (raw32, raw64)
		 of ([], []) => (Unit, nBoxed)
		  | _ => (Raw(raw32, raw64), nBoxed+1)
		(* end case *))
	  val nExcess = nBoxed - numGCRoots
	  val boxed = if (nExcess > 0)
		then let
		  val (boxed, excess) = List.splitAt (boxed, numGCRoots-1)
		  in
		    case rawObj
		     of Unit => boxed @ [Record excess]
		      | _ => boxed @ [Record(rawObj::excess)]
		    (* end case *)
		  end
		else (case rawObj
		   of Unit => boxed
		    | _ => boxed @ [rawObj]
		  (* end case *))
	(* assign roots; note that to avoid register shuffling, we use a different
	 * order for STD_CONT tests, where the first parameter is STD_CONT.
	 *)
	  val gcRoots = Array.array (numGCRoots, Unit)
	  fun assign (root::roots, ix::ixs) = (
		Array.update(gcRoots, ix, root);
		assign (roots, ixs))
	    | assign _ = Array.toList gcRoots
	  in {
	    nParams = n,
	    roots = assign (boxed, if isCont then contRootOrder else stdRootOrder)
	  } end

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
	    | unpack (x::xs, Boxed ix::rs) = (
		setResult (ix, var x); unpack (xs, rs))
	    | unpack (x::xs, Record flds :: rs) =
		unpackRecord (var x, flds, fn () => unpack(xs, rs))
	    | unpack (x::xs, Raw(raw32, raw64)::rs) =
		unpackRaw (var x, raw32, raw64, fn () => unpack(xs, rs))
	    | unpack _ = raise Match
	(* unpack the record of extra pointers/tagged values *)
	  and unpackRecord (tplV, flds, k) = let
		fun select ix = C.SELECT{idx = ix, arg = tplV}
		fun unpackFlds (ix, Boxed jx :: flds) = (
		      setResult (jx, select ix); unpackFlds (ix+1, flds))
		  | unpackFlds (ix, Raw(raw32, raw64) :: flds) =
		      unpackRaw (select ix, raw32, raw64, fn () =>
			unpackFlds(ix+1, flds))
		  | unpackFlds (_, []) = k()
		  | unpackFlds _ = error ["callGC.unpackRecord: bogus field"]
		in
		  unpackFlds (0, flds)
		end
	(* unpack the record of raw data *)
	  and unpackRaw (rExp, raw32, raw64, k) = let
		fun unpack tplV = let
		      fun unpack32 (offset, []) = unpack64 (align64 offset, raw64)
			| unpack32 (offset, (ix, nk)::r) = (
			    setResult (ix, rawSelect (nk, 32, offset, tplV));
			    unpack32 (offset+4, r))
		      and unpack64 (_, []) = k()
			| unpack64 (offset, (ix, nk)::r) = (
			    setResult (ix, rawSelect (nk, 64, offset, tplV));
			    unpack64 (offset+8, r))
		      in
			unpack32 (0, raw32)
		      end
		in
		  case rExp
		   of C.VAR _ => unpack rExp
		    | _ => let
			val tpl = LV.mkLvar()
			in
			  C.LET(rExp, {name=tpl, ty=C.PTRt}, unpack (var tpl))
			end
		  (* end case *)
		end
	  val code = unpack (newRoots, roots)
	(* pack the roots from the live data *)
	  val live = fromListMap (fn {name, ty} => var name) live
	  fun getLive ix = Vector.sub(live, ix)
	  fun pack ([], args) = C.CALLGC(List.rev args, newRoots, code)
	    | pack (Unit :: rs, args) = pack (rs, unit::args)
	    | pack (Boxed ix :: rs, args) = pack (rs, Vector.sub(live, ix)::args)
	    | pack (Record flds :: rs, args) =
		packRecord (flds, fn arg => pack (rs, arg::args))
	    | pack (Raw(raw32, raw64) :: rs, args) =
		packRaw (raw32, raw64, fn arg => pack (rs, arg::args))
	(* pack the record of extra pointers/tagged values and pass it to `k` *)
	  and packRecord (flds, k) = let
		fun packFlds ([], n, args) = let
		      val desc = D.makeDesc' (n, D.tag_record)
		      val tpl = LV.mkLvar()
		      in
			C.ALLOC(record desc, List.rev args, tpl,
			  k (var tpl))
		      end
		  | packFlds (Boxed ix :: flds, n, args) =
		      packFlds (flds, n+1, getLive ix :: args)
		  | packFlds (Raw(raw32, raw64) :: flds, n, args) =
		      packRaw (raw32, raw64, fn arg => packFlds (flds, n+1, arg::args))
		  | packFlds _ = error ["callGC.packRecord: bogus field"]
		in
		  packFlds (flds, 0, [])
		end
	(* pack the record of raw data and pass it to `k` *)
	  and packRaw (raw32, raw64, k) = let
		val tpl = LV.mkLvar()
		val n32 = List.length raw32
		val n64 = List.length raw64
		val (nWords, tag) = if Target.is64
		        then (align64 (4*n32) div 8 + n64, D.tag_raw)
		      else if (n64 = 0)
			then (n32, D.tag_raw)
			else (2 * (align64 (4*n32) div 8 + n64), D.tag_raw64)
		val desc = D.makeDesc' (nWords, tag)
		val (flds, args) = let
		      fun get sz ((ix, nk), (flds, args)) =
			    ({kind=nk, sz=sz}::flds, getLive ix :: args)
		      in
			List.foldr (get 32) (List.foldr (get 64) ([], []) raw64) raw32
		      end
		val align = if (n64 > 0) then 8 else 4
		in
		  C.ALLOC(P.RAW_RECORD{desc=desc, align=align, fields=flds}, args, tpl,
		    k (var tpl))
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

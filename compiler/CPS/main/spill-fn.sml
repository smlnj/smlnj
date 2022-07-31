(* spill.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is a complete rewrite of the old Spill module.
 * The old module suffers from some serious performance problem but
 * I cannot decipher the old code fully, so instead of patching the problems up,
 * I'm reimplementing it with a different algorithm.  The new code is more
 * modular, smaller when compiled, and substantially faster
 * (O(n log n) time and O(n) space).
 *
 * As far as I can tell, the purpose of this module is to make sure the
 * number of live variables at any program point (the bandwidth)
 * does not exceed a certain limit, which is determined by the
 * size of the spill area.
 *
 * When the bandwidth is too large, we decrease the register pressure by
 * packing live variables into spill records.  How we achieve this is
 * completely different than what we did in the old code.
 *
 * First, there is something that MLRiscGen code generator does
 * that we should be aware of:
 *
 * o MLRiscGen performs code motion!
 *
 *    In particular, it will move floating point computations and
 *    address computations involving only the heap pointer to
 *    their use sites (if there is only a single use).
 *    What this means is that if we have a CPS record construction
 *    statement
 *
 *        RECORD(k,vl,w,e)
 *
 *    we should never count the new record address w as live if w
 *    has only one use (which is often the case).
 *
 *    We should do something similar to floating point, but the transformation
 *    there is much more complex, so I won't deal with that.
 *
 * Secondly, there are now two new cps primops at our disposal:
 *
 *  1. rawrecord of record_kind option
 *     This pure operator allocates some uninitialized storage from the heap.
 *     There are two forms:
 *
 *      rawrecord NONE [INT n]  allocates a tagless record of length n
 *      rawrecord (SOME rk) [INT n] allocates a tagged record of length n
 *                                  and initializes the tag.
 *
 *  2. rawupdate of cty
 *       rawupdate cty (v,i,x)
 *       Assigns to x to the ith component of record v.
 *       The storelist is not updated.
 *
 * We use these new primops for both spilling and incremental record construction.
 *
 *  1. Spilling.
 *
 *     This is implemented with a linear scan algorithm (but generalized
 *     to trees).  The algorithm will create a single spill record at the
 *     beginning of the cps function and use rawupdate to spill to it,
 *     and SELECT or SELp to reload from it.  So both spills and reloads
 *     are fine-grain operations.  In contrast, in the old algorithm
 *     "spills" have to be bundled together in records.
 *
 *     Ideally, we should sink the spill record construction to where
 *     it is needed.  We can even split the spill record into multiple ones
 *     at the places where they are needed.  But CPS is not a good
 *     representation for global code motion, so I'll keep it simple and
 *     am not attempting this.
 *
 *  2. Incremental record construction (aka record splitting).
 *
 *     Records with many values which are simulatenously live
 *     (recall that single use record addresses are not considered to
 *      be live) are constructed with rawrecord and rawupdate.
 *     We allocate space on the heap with rawrecord first, then gradually
 *     fill it in with rawupdate.  This is the technique suggested to me
 *     by Matthias.
 *
 *     Some restrictions on when this is applicable:
 *     1. It is not a VECTOR record.  The code generator currently
 *        does not handle this case. VECTOR record uses double
 *        indirection like arrays.
 *     2. All the record component values are defined in the same "basic block"
 *        as the record constructor.  This is to prevent speculative
 *        record construction.
 *
 * -- Allen
 *)

signature SPILL = sig
  val spill : CPS.function list -> CPS.function list
end (* signature SPILL *)

local

  val DEBUG          = false
  val MAX_BANDWIDTH  = 100 (* Kick in spilling when this many values
                            * are live at the same time
                            *)
  val SPLIT_LARGE_RECORDS = true (* True if record splitting is enabled *)
  val MAX_RECORD_LEN = 16 (* Split record of this size or larger *)

in

functor SpillFn (MachSpec : MACH_SPEC) : SPILL =
struct

  structure CPS = CPS
  structure P   = CPS.P
  structure U   = CPSUtil
  structure LV  = LambdaVar
  structure H   = LV.Tbl     (* For mapping from lvar *)

  val debug_cps_spill = Control_CG.debugSpill
  val debug_cps_spill_info = Control_CG.debugSpillInfo

  infix 6 \/
  infix 7 /\
  infix 5 --

  val error = ErrorMsg.impossible
  val pr    = Control.Print.say
  val i2s   = Int.toString

  val maxgpfree =
        Int.min(MachSpec.spillAreaSz div (2 * MachSpec.valueSize),MAX_BANDWIDTH)
  val maxfpfree =
        Int.min(MachSpec.spillAreaSz div (2 * MachSpec.realSize),MAX_BANDWIDTH)

  (* Pretty printing *)
  fun dump(title, cpsFun) =
      if !debug_cps_spill
      then (pr ("------------ "^title^" the spill phase ---------- \n");
            PPCps.printcps0 cpsFun;
            pr "--------------------------------------\n\n")
      else ()

  (*
   * The following data structure groups together type specific functions.
   *)
  datatype type_info = TYPE_INFO of {
        maxLive  : int,             (* max live values allowed *)
        isVar   : CPS.lvar -> bool, (* is variable a candidate for spilling? *)
        itemSize : int              (* number of words per item *)
      }

  datatype spill_candidate = SPILL_CANDIDATE of {
        lvar : CPS.lvar,
        cty  : CPS.cty,
        rank : int          (* distance to next use *)
      }

  (* Cheap set representation *)
  structure SimpleSet =
  struct
      structure Set = LV.Set
      val op \/ = Set.union
      val op /\ = Set.intersection
      val op -- = Set.difference
      val O     = Set.empty
      val card  = Set.numItems     (* cardinality *)
      fun rmv(S, x) = Set.subtract(S, x)
  end

  (* Spill candidates set representation; this one has to be ranked *)
  structure RankedSet =
  struct
     structure Set = RedBlackSetFn
        (type ord_key = spill_candidate
         fun compare(SPILL_CANDIDATE{rank=r1,lvar=v1,...},
                     SPILL_CANDIDATE{rank=r2,lvar=v2,...}) =
             case Int.compare(r1,r2) of
               EQUAL => LV.compare(v1,v2)
             | ord   => ord
        )
     exception Item of Set.item
     (* as priority queue *)
     fun next S =
        Set.foldr (fn (x,_) => raise Item x) NONE S
        handle Item x => SOME(x, Set.delete(S, x))
     (* Abbreviations for set operations *)
     val op \/ = Set.union
     val op /\ = Set.intersection
     val op -- = Set.difference
     val O     = Set.empty
     val card  = Set.numItems     (* cardinality *)
     fun rmv(S, x) = Set.delete(S, x) handle _ => S
  end

(* map record kind to CPS type that serves as the argument to the RAWUPDATE primopm *)
  fun rkToCty (CPS.RK_FCONT | CPS.RK_RAW64BLOCK) = CPS.FLTt 64  (* REAL32: FIXME *)
    | rkToCty _ = U.BOGt

  fun splittable CPS.RK_VECTOR = false (* not supported in backend (yet) *)
    | splittable _             = true

  (* tagged integers *)
  fun tagInt n = CPS.NUM{
          ival = IntInf.fromInt n,
	  ty = {tag = true, sz = Target.defaultIntSz}
	}

  (*-------------------------------------------------------------------------
   *
   * All CPS functions can be independently processed.
   *
   * Some complexity assumptions:
   *   Hashing is O(1)
   *   N = max{number of lvars, size of cps function}
   *
   *-------------------------------------------------------------------------*)

  (*------------------------------------------------------------------------
   * markFpAndRec
   * =============
   * Mark all floating point variables and return a hash table
   *
   * This is needed because we do spilling of integer and floating
   * point stuff separately.
   *
   * This function takes O(N) time and space
   *-----------------------------------------------------------------------*)
  fun markFpAndRec cpsFun =
  let val (funKind, f, args, argTypes, body) = cpsFun : CPS.function
      open SimpleSet
      exception FloatSet
      val floatSet = H.mkTable(32,FloatSet)
      val addToFloatSet = H.insert floatSet
      fun fp(r, CPS.FLTt _) = addToFloatSet(r,true)
        | fp(r ,_)        = ()
      exception RecordSet
      val recordSet = H.mkTable(32,RecordSet)
      val markrec = H.insert recordSet
      val findrec = H.find recordSet

      (* Mark all record uses *)
      val recUses =
          app (fn (CPS.VAR v,_) =>
                  (case findrec v of
                    NONE => ()                 (* not a record address *)
                  | SOME n => markrec(v, n+1)
                  )
                | _ => ()
              )

      fun markPure (p, w) = (case p
            (* these pure operators actually allocate storage! *)
	     of P.WRAP(P.INT sz) => if (sz <= Target.defaultIntSz) then () else markrec(w, 0)
	      | (P.WRAP(P.FLOAT _) | P.NEWARRAY0 | P.MAKEREF | P.MKSPECIAL | P.RAWRECORD _) =>
		  markrec(w, 0)
              | _ => ()
	    (* end case *))

      fun markfp e = (case e
	   of CPS.APP _               => ()
	    | CPS.SWITCH(_,_,es)      => app markfp es
	    | CPS.SELECT(_,_,w,t,e)   => (fp(w,t); markfp e)
	    | CPS.RECORD(_,vl,w,e)    => (recUses vl; markrec(w, 0); markfp e)
	    | CPS.OFFSET(_,_,_,e)     => markfp e
	    | CPS.SETTER(_,_,e)       => markfp e
	    | CPS.LOOKER(_,_,w,t,e)   => (fp(w,t); markfp e)
	    | CPS.ARITH(_,_,w,t,e)    => (fp(w,t); markfp e)
	    | CPS.PURE(p,_,w,t,e)     => (markPure(p,w); fp(w,t); markfp e)
	    | CPS.RCC(_,_,_,_,wtl,e)  => (app fp wtl; markfp e)
	    | CPS.BRANCH(_,_,_,e1,e2) => (markfp e1; markfp e2)
	    | CPS.FIX _ => error "FIX in Spill.markfp"
	  (* end case *))

      val () = ListPair.app fp (args, argTypes) (* mark function parameters *)
      val () = markfp body                      (* mark function body *)

      (* Filter out multiple uses of record values because these
       * are not forward propagated by the backend.
       *)
      val () = if DEBUG then
                H.appi (fn (v, n) =>
                   if n >= 2 then pr(LV.lvarName v^" uses="^i2s n^"\n") else ())
                   recordSet
               else ()
      val () = H.filter (fn n => n <= 1) recordSet
  in  (floatSet, recordSet)
  end

  (*--------------------------------------------------------------------------
   * needsSpilling
   * =============
   * This function checks whether we need to perform spilling for
   * the current type, which is either gpr or fpr.
   * Parameterized by type info.  This is supposed to be a cheap check
   * since most of the time this function should return false,
   * so no information is saved.
   *
   * This function takes O(N log N) time and O(N) space.
   *-------------------------------------------------------------------------*)
  fun needsSpilling (TYPE_INFO{maxLive, isVar, ...}) cpsFun =
  let val (funKind, f, args, argTypes, body) = cpsFun : CPS.function
      open SimpleSet
      exception TooMany

      val bandwidth = ref 0

      (* Make sure |S| is not too large.
       * Note: card is a O(1) operation.
       *)
      fun check S =
      let val n = card S
      in  if n > !bandwidth then bandwidth := n else ();
          if n >= maxLive then raise TooMany else S
      end

      (* This function inserts lvars of the current type into set S *)
      fun uses(vs,S) =
      let fun f((CPS.VAR x)::vs,S) =
               f(vs, if isVar x then Set.add(S,x) else S)
            | f(_::vs,S) = f(vs,S)
            | f([],S) = check S
      in  f(vs,S)
      end

      (* Remove w (a definition) from S.  *)
      fun def(w,S) = rmv(S,w)

      (* Union a list of sets S_1, ..., S_n
       * Runs in O(m \log m) time and space
       * where m = \sum_{i=1\ldots n} |S_i|
       *)
      val unions = List.foldr op\/ O

      (*
       * Compute the set of free vars at each program point.
       * Raise exception TooMany if the live set exceeds maxLive.
       * This phase runs in total O(N log N) time and O(N) space.
       *)
      fun freevars e =
        case e of
          CPS.APP(v,args)          => uses(v::args,O)
        | CPS.SWITCH(v,c,l)        => uses([v],unions(map freevars l))
        | CPS.SELECT(_,v,w,t,e)    => uses([v],def(w,freevars e))
        | CPS.RECORD(_,l,w,e)      => uses((map #1 l),def(w,freevars e))
        | CPS.OFFSET(_,v,w,e)      => uses([v],def(w,freevars e))
        | CPS.SETTER(_,vl,e)       => uses(vl,freevars e)
        | CPS.LOOKER(_,vl,w,t,e)   => uses(vl,def(w,freevars e))
        | CPS.ARITH(_,vl,w,t,e)    => uses(vl,def(w,freevars e))
        | CPS.PURE(_,vl,w,t,e)     => uses(vl,def(w,freevars e))
        | CPS.RCC(_,_,_,vl,wtl,e)  => uses(vl, foldl (fn((w,_),s) => def(w,s))
						     (freevars e) wtl)
        | CPS.BRANCH(_,vl,c,e1,e2) => uses(vl,freevars e1 \/ freevars e2)
        | CPS.FIX _ => error "FIX in Spill.freevars"

      val needsSpilling = (freevars body; false) handle TooMany => true
  in  {needsSpilling = needsSpilling,
       bandwidth     = !bandwidth
      }
  end (* needsSpilling *)

  (*--------------------------------------------------------------------------
   * linearScan
   * ==========
   *
   * Perform the actual spilling.
   *
   * The algorithm is derived from linear-scan RA algorithms.
   * But since we are dealing with trees, (and because of immutable
   * data structures), we'll do this in multiple passes rather than
   * a single pass.
   *
   * What spilling means in CPS is transforming:
   *
   *
   *   v <- f(...)  /* definition */
   *   ....
   *   ... <- g(... v ...) /* use */
   *
   * into:
   *
   *   spilled <- rawrecord NONE m  /* create an uninitialized spill record
   *                                   of length m */
   *   ....
   *   v <- f(...) /* definition */
   *   rawupdate(spilled, v_offset, v)
   *   ...
   *   ... <- g(... SELp(spilled,v_offset) ...) /* reload */
   *
   * Important notes:
   *  1. The spill record is never live beyond the
   *     cps function, so we never even have to assign its
   *     record tag.
   *
   *  2. We spill all tagged/untagged values into a spill record,
   *     without segregating them by their types, so we are mixing
   *     32-bit integers, 31-bit tagged ints, and pointers together.
   *     This is safe because of (1).
   *
   * This function takes a total of O(N log N) time and O(N) space.
   *-------------------------------------------------------------------------*)
  fun linearScan (TYPE_INFO{maxLive, isVar, itemSize, ...}) cpsFun =
  let val (funKind, f, args, argTypes, body) = cpsFun : CPS.function
      open RankedSet

      val () = dump("before", cpsFun)

      (* Information about each lvar *)
      datatype lvar_info =
         LVAR_INFO of
         { useCount   :int ref,  (* number of uses in this function *)
           defPoint   :int,      (* level of definition *)
           defBlock   :int,      (* block of definition *)
           cty        :CPS.cty,
           nearestUse :int ref   (* min {level(x) | x in uses(v)} *)
         }
      exception LvarInfo

      val () = if !debug_cps_spill_info
               then pr "CPS Spill: linearScan\n" else ()

      val lvarInfo = H.mkTable(32,LvarInfo)
      val lookupLvar = H.lookup lvarInfo

      fun spillCand v =
      let val LVAR_INFO{nearestUse, useCount, defPoint, cty, ...} = lookupLvar v
          val dist = !nearestUse - defPoint
          val rank = dist (* for now *)
      in  SPILL_CANDIDATE{lvar=v, cty=cty, rank=rank}
      end

      (*----------------------------------------------------------------------
       * Gather information about each lvar
       * We partition the cps function into blocks.
       *     A block is a continuous group of statements without
       *     controlflow or store updates.
       * This phase runs in O(N) time and space.
       *---------------------------------------------------------------------*)
      local
          val infinity = 10000000
          val enterLvar = H.insert lvarInfo
          fun def(v,t,b,n) =
              enterLvar(v, LVAR_INFO{useCount=ref 0,
                                     defPoint=n,
                                     defBlock=b,
                                     cty=t,
                                     nearestUse=ref infinity
                                    }
                       )

          fun use(CPS.VAR v, n) =
              if isVar v then
              let val LVAR_INFO{useCount, nearestUse, ...} = lookupLvar v
              in  useCount := !useCount + 1;
                  nearestUse := Int.min(!nearestUse, n)
              end
              else ()
            | use _ = ()
          fun uses([], n) = ()
            | uses(v::vs, n) = (use(v, n); uses(vs, n))

          fun gather(e, b, n) =
          let fun gathers([], b, n)    = ()
                | gathers(e::es,b,n) = (gather(e,b,n); gathers(es,b,n))
              fun f0(vl, e) = (uses(vl, n); gather(e, b+1, n+1))
              fun f1(v, w, t, e) = (use(v, n); def(w,t,b,n); gather(e,b,n+1))
              fun fx(vl,w,t,e,b) = (uses(vl, n); def(w,t,b,n); gather(e,b,n+1))
          in  case e of
                CPS.APP(v,args)        => uses(v::args, n)
              | CPS.SWITCH(v,c,l)      => (use(v, n); gathers(l, b+1, n+1))
              | CPS.SELECT(_,v,w,t,e)  => f1(v, w, t, e)
              | CPS.OFFSET(_,v,w,e)    => f1(v, w, U.BOGt, e)
              | CPS.RECORD(_,l,w,e)    => fx(map #1 l, w, U.BOGt, e, b)
              | CPS.SETTER(_,vl,e)     => f0(vl, e)
              | CPS.LOOKER(_,vl,w,t,e) => fx(vl, w, t, e, b)
              | CPS.ARITH(_,vl,w,t,e)  => fx(vl, w, t, e, b)
              | CPS.PURE(_,vl,w,t,e)   => fx(vl, w, t, e, b)
              | CPS.RCC(_,_,_,vl,wtl,e)=>
		  let val b = b+1
		  in uses (vl, n);
		     app (fn (w, t) => def (w, t, b, n)) wtl;
		     gather (e, b, n+1)
		  end
              | CPS.BRANCH(_,vl,c,x,y) => (uses(vl, n); gathers([x,y],b+1,n+1))
              | CPS.FIX _ => error "FIX in Spill.gather"
          end
      in  (* Always remember to define the arguments! *)
          val () = ListPair.app (fn (v, t) => def(v, t, 0, 0)) (args, argTypes)
          val () = gather(body, 1, 1)
      end (* gather *)

      val () = if !debug_cps_spill then pr "CPS Spill: gather done\n" else ()

      (*-----------------------------------------------------------------
       *
       * Spill tables and utilities
       *
       *-----------------------------------------------------------------*)

      exception SpillTable
      val spillTable = H.mkTable(32, SpillTable) :
                   (CPS.value * int * CPS.cty) H.hash_table
                           (* lvar -> spillRecord * spill offset * cty *)
      val enterSpill  = H.insert spillTable
      val findSpill   = H.find spillTable
      val isSpilled   = H.inDomain spillTable
      val currentSpillRecord = ref (NONE : (CPS.lvar * CPS.value) option)

      (*
       * Generate a new spill record variable
       *)
      fun genSpillRec() =
          case !currentSpillRecord of
            SOME x => x
          | NONE =>
          let val v = LV.namedLvar (Symbol.varSymbol "spillrec")
              val e = CPS.VAR v
          in  currentSpillRecord := SOME(v,e); (v, e)
          end

     (*
      * This function finds up to m good spill candidates from the live set
      *)
     fun findGoodSpills(0, L, spOff) = (L, spOff)
       | findGoodSpills(m, L, spOff) =
         case next L of
            (* no more spill candidates! *)
            NONE => (L, spOff)
         |  SOME(SPILL_CANDIDATE{lvar, cty, rank, ...}, L) =>
            let val offset = spOff (* should align when we have 64-bit values *)
                val (_,spRecExp) = genSpillRec()
                val ()     = enterSpill(lvar,(spRecExp,offset,cty))
                fun inc(spOff,cty) = spOff + 1 (* should look at cty
                                                * when we have 64-bit values
                                                *)
            in  (* okay; it's actually live and hasn't been spilled! *)
                if !debug_cps_spill then
                  pr("Spilling "^LV.lvarName lvar^" rank="^i2s rank^"\n")
                else ();
                findGoodSpills(m-1, L, inc(spOff, cty))
            end

      (*
       * Can and should the record be split?
       * Split if,
       *  1. we can handle the record type
       *  2. if it has >= MAX_RECORD_LEN live lvars as arguments
       *  3. All its arguments are defined in the same block as the record.
       *)
      fun shouldSplitRecord(rk,vl,b) =
      SPLIT_LARGE_RECORDS andalso
      let fun okPath(CPS.SELp(i,p)) = okPath p
            | okPath(CPS.OFFp 0) = true
            | okPath _ = false
          fun f([], n) = n >= MAX_RECORD_LEN
            | f((CPS.VAR v,p)::vl, n) =
              let val LVAR_INFO{defBlock, ...} = lookupLvar v
              in  defBlock = b andalso okPath p andalso
                     (if isVar v andalso not(isSpilled v)
                      then f(vl, n+1)
                      else f(vl, n)
                     )
              end
            | f((_,CPS.OFFp 0)::vl, n) = f(vl, n)
            | f _ = false
      in  splittable rk andalso f(vl, 0)
      end

      (*
       * Tables for splitting a record
       *)
      exception RecordTable
      datatype split_record_item  =
         SPLIT_RECORD_ITEM of
           { record  : CPS.lvar,
             kind    : CPS.record_kind,
             len     : int,
             offset  : int,
             path    : CPS.accesspath,
             numVars : int ref,
             consts  : (int * CPS.value) list
           }

      val recordAllocTable = H.mkTable(16, RecordTable)
      val enterRecordItem = H.insert recordAllocTable
      val findRecordItem = H.find recordAllocTable
      val splitRecordTable = H.mkTable(16, RecordTable)
      val markSplitRecord = H.insert splitRecordTable
      fun insertRecordItem(v, x) =
          enterRecordItem(v, x::getOpt(findRecordItem v,[]))

      (*
       * Mark record w as being split.
       * Enter the appropriate info to all its arguments.
       *)
      fun splitRecordConstruction(rk, vl, w) =
      let fun f(i, (CPS.VAR v,offp)::vl, vars, consts) =
                f(i+1, vl, (i,v,offp)::vars, consts)
            | f(i, (c,CPS.OFFp 0)::vl, vars, consts) =
               f(i+1, vl, vars, (i,c)::consts)
            | f(_, [], vars, consts) = (vars, consts)
            | f _ = error "CPS Spill.splitRecordConstruction"
          val (vars, consts) = f(0, vl, [], [])
          val n = length vars
          val _ = if n = 0 then
                    error "CPS Spill: splitting constant record" else ()
          val _ = if !debug_cps_spill_info then
                     pr("Splitting record "^LV.lvarName w^" len="^i2s n^"\n")
                  else ()
          val len     = length vl
          val numVars = ref n
          fun enter(i, v, path) =
          let val item = SPLIT_RECORD_ITEM
                         { record  = w,
                           kind    = rk,
                           len     = len,
                           offset  = i,
                           path    = path,
                           numVars = numVars,
                           consts  = consts
                         }
          in  insertRecordItem(v, item)
          end
      in  app enter vars;
          markSplitRecord(w,true)
      end

      (*-----------------------------------------------------------------
       * Linear scan spilling.
       * This function marks all spill/reload sites.
       *
       * Parameters:
       *  e     --- cps expression
       *  b     --- current block
       *  spOff --- current available spill offset
       *
       * Return:
       *  L      --- the set of live lvars in e
       *  spills --- the number of spills
       *
       * This phase takes O(N log N) time and O(N) space
       *-----------------------------------------------------------------*)
      fun scan(e, b, spOff) =
      let
          (* add uses to live set *)
          fun addUses([], L) = L
            | addUses(CPS.VAR v::vs, L) =
              addUses(vs, if isVar v andalso not(isSpilled v) then
                              Set.add(L, spillCand v) else L)

            | addUses(_::vs, L) = addUses(vs, L)

          (* This function kills a definition *)
          fun kill(w, L) = if isVar w then rmv(L, spillCand w) else L

          (* This function find things to spill *)
          fun genSpills(L, spOff) =
          let val toSpills = card L - maxLive
          in  if toSpills > 0 then findGoodSpills(toSpills, L, spOff)
              else (L, spOff)
          end

          (* This function visits a list of continuations and
           * gathers up the info
           *)
          fun scanList es =
          let val b = b + 1
              fun f [] = (O, 0)
                | f [e] = scan(e, b, spOff)
                | f(e::es) =
                  let val (L1, spOff1) = scan(e, b, spOff)
                      val (L2, spOff2) = f es
                  in  (L1 \/ L2, Int.max(spOff1, spOff2))
                  end
          in  f es end

          (* This function scans normal cps operators
           * with one definition and one continuation
           *
           *  w : t <- f vs; e
           *)
          fun scanOp(vs, w, e, b) =
          let val (L,spOff) = scan(e,b,spOff)     (* do continuation *)
              val L         = kill(w, L)          (* remove definition *)
              val L         = addUses(vs, L)      (* add uses *)
              val (L,spOff) = genSpills(L, spOff) (* find spill *)
          in  (L, spOff)
          end

          (* This function scans stmts with multiple continuations *)
          fun scanStmt(vs, es) =
          let val (L,spOff)  = scanList es         (* do continuation *)
              val L          = addUses(vs, L)      (* add uses *)
              val (L, spOff) = genSpills(L,spOff)  (* find spills *)
          in  (L, spOff)
          end

          (* This function scans record constructors *)
          fun scanRec(rk, vl, w, e) =
          let val (L,spOff)  = scan(e,b,spOff) (* do continuation *)
              val (L, spOff) =
                  if shouldSplitRecord(rk, vl, b) then
                     (splitRecordConstruction(rk, vl, w); (L,spOff))
                  else
                     let val L = kill(w, L)
                         val L = addUses(map #1 vl, L)
                     in  genSpills(L, spOff)
                     end
          in  (L, spOff)
          end

          val (L, numSpills) =
           case e of
            CPS.APP(v,args)        => scanStmt(v::args, [])
          | CPS.SWITCH(v,c,es)     => scanStmt([v], es)
          | CPS.SELECT(i,v,w,t,e)  => scanOp([v], w, e, b)
          | CPS.OFFSET(i,v,w,e)    => scanOp([v], w, e, b)
          | CPS.RECORD(rk,l,w,e)   => scanRec(rk, l, w, e)
          | CPS.SETTER(p,vl,e)     => scanStmt(vl,[e])
          | CPS.LOOKER(p,vl,w,t,e) => scanOp(vl, w, e, b)
          | CPS.ARITH(p,vl,w,t,e)  => scanOp(vl, w, e, b)
          | CPS.PURE(p,vl,w,t,e)   => scanOp(vl, w, e, b)
          | CPS.RCC(k,l,p,vl,wtl,e)=>
	    let val b = b+1
		val (L,spOff) = scan(e,b,spOff)
		val L = foldl (fn ((w, _), L) => kill (w, L)) L wtl
		val L = addUses (vl, L)
		val (L, spOff) = genSpills (L, spOff)
	    in (L, spOff)
	    end
          | CPS.BRANCH(p,vl,c,x,y) => scanStmt(vl,[x,y])
          | CPS.FIX _ => error "FIX in Spill.scan"

      in  (L, numSpills)
      end

      (* Scan the body *)
      val (L, numSpills) = scan(body, 1, 0)

      val () = if !debug_cps_spill then
               pr("CPS Spill: scan done. Spilling "^i2s numSpills^"\n")
               else ()
      (*
       * Generate reloads for a list of arguments.
       * Returns:
       *    the rewritten list of arguments
       *    a function for inserting selects.
       *)
      fun emitReloads vs =
      let fun g([], vs', f) = (rev vs', f)
            | g((v as CPS.VAR x)::vs, vs', f) =
              (case findSpill x of
                NONE => g(vs, v::vs', f)
              | SOME(spillRec, off, cty) =>
                let val x'   = LV.dupLvar x
                    val v'   = CPS.VAR x'
                    fun f' e = CPS.SELECT(off,spillRec,x',cty, f e)
                in  g(vs, v'::vs', f')
                end
              )
            | g(v::vs, vs', f) = g(vs, v::vs', f)
      in  g(vs, [], fn e => e)
      end

      (*
       * Generate reloads for record paths
       * Returns:
       *    the rewritten list of record paths
       *)
      fun emitPathReloads vl =
      let fun f([], vl') = rev vl'
            | f((v as CPS.VAR x, p)::vl, vl') =
               (case findSpill x of
                 NONE => f(vl, (v, p)::vl')
               | SOME(spillRec, off, cty) =>
                   f(vl, (spillRec,CPS.SELp(off,p))::vl')
               )
            | f(v::vl, vl') = f(vl, v::vl')
      in  f(vl, [])
      end

      (* This function generate spill code for variable w *)
      fun emitSpill(w, e) = (case findSpill w
             of NONE => e
              | SOME(spillRecord, off, cty) =>
                  CPS.SETTER(P.RAWUPDATE cty, [spillRecord, tagInt off, CPS.VAR w], e)
            (* end case *))

      (*
       * Emit spill record code
       *)
      fun createSpillRecord(0, e) = e
        | createSpillRecord(numSpills, e) =
      let val (spillRecLvar,_) = genSpillRec()
          val m = numSpills * itemSize
          val e = CPS.PURE(P.RAWRECORD NONE,[tagInt m],
                           spillRecLvar,U.BOGt,e)
      in  currentSpillRecord := NONE; (* clear *)
          e
      end

      val recordIsSplit = H.inDomain splitRecordTable
      val findSplitRecordArg = H.find recordAllocTable

      (*
       * proj(v, path, e) ==> w <- v.path ; e[w/v]
       *)
      fun proj(v, CPS.OFFp 0, e) = e v
        | proj(v, CPS.SELp(i,p), e) =
          let val v' = LV.mkLvar()
              val e  = e v'
          in  CPS.SELECT(i, CPS.VAR v, v', U.BOGt, e)
          end
	| proj _ = error "SpillFn: proj"

      (*
       * generate
       *    record.offset <- v.path ; e
       *)
      fun initRecordItem (record, rk, offset, v, path, e) =
            proj(v, path,
               fn x => CPS.SETTER(P.RAWUPDATE(rkToCty rk),
                         [CPS.VAR record, tagInt offset, CPS.VAR x], e))

      (*
       * Generate code to create a record.
       *)
      fun createRecord (record, rk, len, consts, e) = let
(* FIXME: note that `rk` can be RK_RECORD or RK_CONT, which is kind of bogus. *)
          val e = emitSpill(record, e)
          val p = P.RAWUPDATE(rkToCty rk)
          fun init((i, c),e) = CPS.SETTER(p,[CPS.VAR record, tagInt i, c], e)
          val e = foldr init e consts
	  in
	    CPS.PURE(P.RAWRECORD(SOME rk), [tagInt len], record, U.BOGt, e)
	  end

      (*
       * It is the definition of lvar v.
       * Check to see if v is some component of split records.
       * If so, generate code.
       *
       *)
      fun assignToSplitRecord(v, e) =
          case findSplitRecordArg v of
            SOME inits =>
            let fun gen(SPLIT_RECORD_ITEM
                         {record, kind, len, offset,
                          path, numVars, consts,...}, e) =
                let val e = initRecordItem(record, kind, offset, v, path, e)
                    val n = !numVars - 1
                in  numVars := n;
                    if n = 0 then createRecord(record, kind, len, consts, e)
                    else e
                end
            in  foldr gen e inits
            end
          | NONE => e

      (*-----------------------------------------------------------------
       * Rebuild
       *
       * This function rewrites the cps expression and insert spill/reload
       * code.
       *
       * This phase takes O(N) time and O(N) space
       *-----------------------------------------------------------------*)

      fun rebuild e =
      let

          fun rewriteStmt(vs, es, f) =
          let val es      = map rebuild es
              val (vs, g) = emitReloads vs
          in  g(f(vs, es)) end

          fun rewrite(vs, w, e, f) =
          let val e = rebuild e
              val e = emitSpill(w, e)
              val e = assignToSplitRecord(w, e)
              val (vs, g) = emitReloads vs
          in  g(f(vs, w, e))
          end

	  fun rewrite'(vs,wl,e,f) =
	      let val e = rebuild e
		  val e = foldl emitSpill e wl
		  val e = foldl assignToSplitRecord e wl
		  val (vs, g) = emitReloads vs
	      in g (f (vs, wl, e))
	      end

          fun rewriteRec(vl, w, e, f) =
          let val e = rebuild e
              val e = emitSpill(w, e)
              val e = assignToSplitRecord(w, e)
          in  if recordIsSplit w then e
              else f(emitPathReloads vl, w, e)
          end

	  (* wrappers -- make the match compiler shut up *)
	  fun s1 f (v :: vs, es) = f (v, vs, es)
	    | s1 _ _ = error "Spill: s1"

	  fun e1 f ([v], w, e) = f (v, w, e)
	    | e1 _ _ = error "Spill: e1"

	  fun s'1 f (vs, [e]) = f (vs, e)
	    | s'1 _ _ = error "Spill: s'1"

	  fun s'2 f (vs, [x, y]) = f (vs, x, y)
	    | s'2 _ _ = error "Spill: s'2"

          (*
           * Rewrite the expression
           *)
          val e =
          case e of
            CPS.APP(v,args) =>
               rewriteStmt(v::args, [], s1 (fn (v, args,_) => CPS.APP(v,args)))
          | CPS.SWITCH(v,c,es) =>
               rewriteStmt([v], es, s1 (fn (v, _, es) => CPS.SWITCH(v, c, es)))
          | CPS.SELECT(i,v,w,t,e) =>
               rewrite([v], w, e, e1 (fn (v,w,e) => CPS.SELECT(i,v,w,t,e)))
          | CPS.OFFSET(i,v,w,e) =>
               rewrite([v], w, e, e1 (fn (v,w,e) => CPS.OFFSET(i,v,w,e)))
          | CPS.RECORD(k,l,w,e) =>
               rewriteRec(l,w,e,fn (l,w,e) => CPS.RECORD(k, l, w, e))
          | CPS.SETTER(p,vl,e) =>
               rewriteStmt(vl, [e], s'1 (fn (vl,e) => CPS.SETTER(p,vl,e)))
          | CPS.LOOKER(p,vl,w,t,e) =>
               rewrite(vl,w,e, fn (vl,w,e) => CPS.LOOKER(p,vl,w,t,e))
          | CPS.ARITH(p,vl,w,t,e) =>
               rewrite(vl,w,e, fn (vl,w,e) => CPS.ARITH(p,vl,w,t,e))
          | CPS.PURE(p,vl,w,t,e) =>
               rewrite(vl,w,e,fn (vl,w,e) => CPS.PURE(p,vl,w,t,e))
          | CPS.RCC(k,l,p,vl,wtl,e) =>
	      rewrite' (vl, map #1 wtl, e,
			fn (vl, wl, e) => CPS.RCC (k, l, p, vl,
						   ListPair.map (fn (w, (_, t)) => (w, t)) (wl, wtl),
						   e))
          | CPS.BRANCH(p,vl,c,x,y) =>
               rewriteStmt(vl,[x,y],
			   s'2 (fn (vl,x,y) => CPS.BRANCH(p,vl,c,x,y)))
          | CPS.FIX _ => error "FIX in Spill.rebuild"

      in  e
      end (* rebuild *)

          (* insert spill/reload code *)
      val body = rebuild body
      val body = foldr emitSpill body args (* spill code for arguments *)
          (*
           * Insert spill record creation code.
           *)
      val body = createSpillRecord(numSpills, body)

      val () = if !debug_cps_spill_info
               then pr("CPS Spill: linearScan done "^i2s numSpills^" spilled\n")
               else ()

      val cpsFun = (funKind, f, args, argTypes, body)

      val () = dump("after", cpsFun)

  in  cpsFun
  end (* linearScan *)

  (*-------------------------------------------------------------------------
   * spillOne
   * ========
   *
   * This is the driver to process only one CPS function.
   *
   * This routine takes a total of O(N log N) time and O(N) space
   *
   *-------------------------------------------------------------------------*)
  fun spillOne cpsFun =
  let
      (*
       * Perform spilling.
       *)

      fun spillIt type_info cpsFun =
      let val {needsSpilling, bandwidth, ...} = needsSpilling type_info cpsFun
          val () = if !debug_cps_spill_info then
                      pr("CPS Spill bandwidth="^i2s bandwidth^"\n")
                   else ()
      in  if needsSpilling then linearScan type_info cpsFun
          else cpsFun
      end
      (*
       * If we have unboxed floats then we have to distinguish between
       * fpr and gpr registers.
       *)

      val (fpTable,recordTable) = markFpAndRec cpsFun (* collect fp type info *)

      val isMoveableRec = H.inDomain recordTable

      val cpsFun =
      if MachSpec.unboxedFloats then
         let val isFP = H.inDomain fpTable
             fun isGP r = not(isFP r) andalso not(isMoveableRec r)
(* REAL32: FIXME *)
             val fp = TYPE_INFO{
		    isVar = isFP, maxLive = maxfpfree,
		    itemSize = if Target.is64 then 1 else 2
		  }
             val gp = TYPE_INFO{isVar=isGP, maxLive=maxgpfree, itemSize=1}
             val cpsFun = spillIt fp cpsFun (* do fp spills first *)
             val cpsFun = spillIt gp cpsFun (* do gp spills *)
         in  cpsFun
         end
      else
         let fun isGP r = not(isMoveableRec r)
         in  spillIt (TYPE_INFO{isVar=isGP, maxLive=maxgpfree, itemSize=1})
                 cpsFun
         end
  in  cpsFun
  end (* spillOne *)


  (* Main entry point *)
  val spill = map spillOne

end (* SpillFn *)

end (* local *)

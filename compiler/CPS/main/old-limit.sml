(* old-limit.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the old version of the Limit structure; it will go away soon.
 *)

structure OldLimit : sig

  (* returns list of function and mapping from function names to pairs of allocation
   * amount and number of instructions.
   *)
    val nolimit : CPS.function list -> CPS.function list * (CPS.lvar -> (int * int))

  end = struct

    structure CGoptions = Control.CG

    structure LV = LambdaVar
    structure Tbl = LV.Tbl
    open CPS

    val say = Control.Print.say
    val error = ErrorMsg.impossible

    val MAX_ALLOC = 1023  (* maximum number of words to allocate per check *)

    fun findescapes fl = let
	  exception Limit
	  val m : fun_kind Tbl.hash_table = Tbl.mkTable(32,Limit)
	  val _ = app (fn (k,f,_,_,_) => Tbl.insert m (f,k)) fl
	  val escapes = Tbl.lookup m
	  in {
	    escapes = escapes,
	    check = fn f => (case escapes f
	       of KNOWN => Tbl.insert m (f, KNOWN_CHECK)
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
    fun path escapes fl = let
	  exception LimitExn
	  val b : cexp Tbl.hash_table = Tbl.mkTable(32, LimitExn)
	  val _ = app (Tbl.insert b o (fn (_,f,_,_,body) => (f,body))) fl
	  val body = Tbl.lookup b

	  val m : {known: fun_kind, alloc: int, instrs: int} Tbl.hash_table =
		Tbl.mkTable(32, LimitExn)
	  val look = Tbl.lookup m
	(* compute required storage in ml-value-sized words *)
	  fun g (d, RECORD(RK_RAW64BLOCK,vl,_,e)) = g(d + record64Sz(length vl), e)
	    | g (d, RECORD(RK_FCONT,vl,_,e)) = g(d + record64Sz(length vl), e)
	    | g (d, RECORD(RK_VECTOR,vl,_,e)) = g(d + length vl + (seqHdrSz + 1), e)
	    | g (d, RECORD(_,vl,_,e)) = g(d + length vl + 1, e)
	    | g (d, SELECT(_,_,_,_,e)) = g(d, e)
	    | g (d, OFFSET(_,_,_,e)) = g(d, e)
	    | g (d, SWITCH(_,_,el)) = foldr Int.max 0 (map (fn e => g(d,e)) el)
	    | g (d, SETTER(P.ASSIGN, _, e)) = g(d+storeListSz, e)
	    | g (d, SETTER(P.UPDATE,_,e)) = g(d+storeListSz, e)
		(*** should be +0 when unboxedfloat is turned on ***)
(* QUESTION: why are these operations +1, since the allocation is done in WRAP?
	    | g (d, ARITH(P.IARITH _,_,_,_,e)) = g(d+1, e)
	    | g (d, ARITH(P.TESTU _, _, _, _, e)) = g(d+1, e)
	    | g (d, ARITH(P.TEST _, _, _, _, e)) = g(d+1, e)
*)
	    | g (d, ARITH(P.TEST_INF _, _, _, _, e)) = error "TEST_INF in limit"
	    | g (d, ARITH(_,_,_,_,e)) = g(d, e)
(* QUESTION: why are these operations +3, since the allocation is done in WRAP?
	    | g (d, PURE(P.PURE_ARITH{kind=P.FLOAT 64,...},_,_,_,e)) = g(d+3, e)
	    | g (d, PURE(P.INT_TO_REAL{to=64,...},_,_,_,e)) = g(d+3, e)
*)
	    | g (d, PURE(P.WRAP(P.INT sz), _, _, _, e)) =
		if (sz = Target.mlValueSz)
		  then g(d + 2, e)
		else if (sz <= Target.defaultIntSz)
		  then error "unexpected tagged int wrap"
		  else g(d + 1 + sz div Target.mlValueSz, e)
(* REAL32: FIXME *)
	    | g (d, PURE(P.WRAP(P.FLOAT 64), _, _, _, e)) = g (d + record64Sz 1, e)
	    | g (d, PURE(P.NEWARRAY0,_,_,_,e)) = g(d + (seqHdrSz + 2), e)
	    | g (d, PURE(P.MAKEREF, _, _, _, e)) = g(d+2, e)
	    | g (d, PURE(P.MKSPECIAL, _, _, _, e)) = g(d+2, e)
	    | g (d, PURE(P.RAWRECORD tag, [NUM{ty={tag=true, ...}, ival}],_,_,e)) =
		g (d+(IntInf.toInt ival)+(case tag of SOME _ => 1 | NONE => 0), e)
	    | g (d, PURE((P.TRUNC_INF _ | P.EXTEND_INF _ | P.COPY_INF _), _, _, _, e)) =
		error "*_INF in limit"
(* QUESTION: why is this operation +3, since the allocation is done in WRAP?
	    | g (d, LOOKER(P.NUMSUBSCRIPT{kind=P.FLOAT 64},_,_,_,e)) = g(d+3, e)
*)
	    | g (d, SETTER(_,_,e)) = g(d,e)
	    | g (d, LOOKER(_,_,_,_,e)) = g(d,e)
	    | g (d, PURE(_,_,_,_,e)) = g(d,e)
	    | g (d, RCC(_,_,_,_,_,e)) = g(d, e)
	    | g (d, BRANCH(_,_,_,e1,e2)) = Int.max(g(d,e1), g(d,e2))
	    | g (d, APP(LABEL w, _)) = (case maxpath w
		 of {known=KNOWN, alloc=n, instrs=i} =>
		    if d+n > MAX_ALLOC
		      then (
			Tbl.insert m (w,{known=KNOWN_CHECK, alloc=n, instrs=i});
			d)
		      else d+n
		  | _ => d
		(* end case *))
	    | g (d, APP(_, _)) = d
	    | g (d, FIX _) = error "FIX in limit"

	  and h (d, RECORD(_,_,_,e)) = h(d+1, e)
	    | h (d, SELECT(_,_,_,_,e)) = h(d+1, e)
	    | h (d, OFFSET(_,_,_,e)) = h(d+1, e)
	    | h (d, SWITCH(_,_,el)) = foldr Int.max 1 (map (fn e => g(d,e)) el)
	    | h (d, SETTER(_,_,e)) = h(d+1, e)
	    | h (d, ARITH(_,_,_,_,e)) = h(d+1, e)
	    | h (d, PURE(_,_,_,_,e)) = h(d+1, e)
	    | h (d, LOOKER(_,_,_,_,e)) = h(d+1, e)
	    | h (d, RCC(_,_,_,_,_,e)) = h(d+1, e)
	    | h (d, BRANCH(_,_,_,a,b)) = Int.max(h(d,a), h(d,b)) + 1
	    | h (d, APP(LABEL w, _)) =
		(case maxpath w of {known=KNOWN, alloc, instrs=i} => d+i | _ => d)
	    | h (d, APP(_, _)) = d
	    | h (d, FIX _) = error "FIX in limit"

	  and maxpath w = look w
		handle LimitExn => (
	       (* Note that the heap may need to be aligned so g is
		* called with g(raw64Pad, bod). Be conservative.
		*)
		  case escapes w
		   of KNOWN => let
			val bod = body w
			val n = g(raw64Pad, bod)
			val i = h(0, bod)
			val z = if n>MAX_ALLOC
			      then {known=KNOWN_CHECK,alloc=n,instrs=i}
			      else {known=KNOWN,alloc=n,instrs=i}
			in
			  Tbl.insert m (w,z);
			  z
			end
		   | kind => let
			val bod = body w
			val z = (Tbl.insert m (
				w, {known=kind, alloc=0, instrs=0});
				{known=kind, alloc=g(1,bod), instrs=h(0,bod)})
			in
			  Tbl.insert m (w,z); z
			end
		  (* end case *))

	  val _ = app (fn (_, x, _, _, _) => (maxpath x; ())) fl;
	  val nfl = map (fn (fk,v,args,cl,ce) => (#known(look v),v,args,cl,ce)) fl
	  in
	    (nfl, fn x => (let val f = look x in (#alloc f,#instrs f) end))
	  end

    fun nolimit fl = let
	  val {escapes, check} = findescapes fl
	  fun makenode (_,f,vl,_,body) = let
		fun edges (RECORD(_,_,_,e)) = edges e
		  | edges (SELECT(_,_,_,_,e)) = edges e
		  | edges (OFFSET(_,_,_,e)) = edges e
		  | edges (SWITCH(_,_,el)) = List.concat (map edges el)
		  | edges (SETTER(_,_,e)) = edges e
		  | edges (LOOKER(_,_,_,_,e)) = edges e
		  | edges (ARITH(_,_,_,_,e)) = edges e
		  | edges (PURE(_,_,_,_,e)) = edges e
		  | edges (RCC(_,_,_,_,_,e)) = edges e
		  | edges (BRANCH(_,_,_,a,b)) = edges a @ edges b
		  | edges (APP(LABEL w, _)) = (case escapes w of KNOWN => [w] | _ => nil)
		  | edges (APP _) = nil
		  | edges (FIX _) = error "8933 in limit"
		in
		  (f, edges body)
		end
	  in
	    if !CGoptions.printit
	      then (say "Starting feedback..."; Control.Print.flush())
	      else ();
	    List.app check (Feedback.feedback (map makenode fl));
	    if !CGoptions.printit
	      then (say "Finished\n"; Control.Print.flush())
	      else ();
	    path escapes fl
	  end

    val nolimit = fn fl => if !CGoptions.printit
	  then let
	    val info as (newfl, limits) = nolimit fl
	    fun showinfo (k,f,_,_,_) = let
		  val (alloc, instrs) = limits f
		  val s = Int.toString alloc
		  val i = Int.toString instrs
		  val _ = (say (LV.lvarName f); say "\t")
		  val _ = (case k
			 of KNOWN => say "K  "
			  | KNOWN_CHECK => say "H  "
			  | ESCAPE => say "E  "
			  | CONT => say "C  "
			  | _ => error "nolimit 323 in limit.sml"
			(* end case *))
		  in
		    say s; say "\t"; say i; say "\n"
		  end
	    in
	      List.app showinfo newfl;
	      info
	    end
	  else nolimit fl

  end (* structure Limit *)

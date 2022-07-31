(* collect.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Author: monnier@cs.yale.edu
 * revised by DBM, 10/2021
 *)

signature COLLECT =
  sig
    type info

    (* Collect information (type info) about variable and function uses/calls.
     * The info is accumulated in a global hashtable infoTable (see T.clear at line 456) *)
    val collect : FLINT.fundec -> unit

    (* get counts (info) for lvar from global hashtable (infoTable) *)
    val getInfo : LambdaVar.lvar -> info

    (* query functions on info *)
    val escaping  : info -> bool	(* non-call uses *)
    val called    : info -> bool	(* known call uses *)
    val uses      : info -> int		(* total nb (number) of uses *)
    val calls     : info -> int		(* total nb of calls *)
    val dead      : info -> bool	(* uses = 0 ? *)
    val usedLvar  : LambdaVar.lvar -> bool  (* uses (getInfo lvar) > 0 *)

    (* increment the uses (respectively, uses & calls) counts *)
    val usesInc   : info -> unit
    val callsInc  : info -> unit

    (* decrement the uses, respectively uses & calls, counts and return true if !uses = 0 *)
    val usesDec   : info -> bool
    val callsDec  : info -> bool

    (* transfer (lv1, lv2) adds the counts for lv1 to those of lv2 and remaps lv1 to the
     * updated lv2 info in the infoTable so that lv1 and lv2 are mapped to the same
     * (merged) info record *)
    val transfer : LambdaVar.lvar * LambdaVar.lvar -> unit

    (* _register_ an lvar by creating a new lvar table entry initialized to zero info *)
    val new : LambdaVar.lvar -> info

    (* fix up function to keep counts up-to-date when getting rid of code.
     * the arg (undertaker) is called for _free_ variables when they become dead. *)
    val unuselexp : (LambdaVar.lvar -> unit) -> FLINT.lexp -> unit

    (* function to analyze variable usage for an lexp and collect it in the info map *)
    val analyze : FLINT.lexp -> unit

    (* function to copy (and collect info) a lexp *)
    val copylexp : LambdaVar.lvar LambdaVar.Map.map  -> FLINT.lexp -> FLINT.lexp

    (* create description string for an lvar with (uses, calls) usage info from its
     * associated info record (if registered in the infoTable) *)
    val lvarToString : LambdaVar.lvar -> string

    (* create description string for an info record *)
    val infoToString : info -> string

  end

(* Internal vs External references:
 * I started with a version that kept track separately of internal and external
 * uses. This has the advantage that if the extuses count goes to zero, we can
 * consider the function as dead.  Without this, recursive functions can never
 * be recognized as dead during fcontract (they are still eliminated at the
 * beginning, tho).  This looks nice at first, but poses problems:
 * - when you do simple inlining (just moving the body of the procedure), you
 *   may inadvertently turn ext-uses into int-uses.  This only happens when
 *   inlining mutually recursive function, but this can be common (think of
 *   when fcontract undoes a useless uncurrying of a recursive function). This
 *   can be readily overcome by not using the `move body' optimization in
 *   dangerous cases and do the full copy+kill instead.
 * - you have to keep track of what is inside what. The way I did it was to
 *   have an 'inside' ref cell in each fun. That was a bad idea. The problem
 *   stems from the fact that when you detect that a function becomes dead,
 *   you have to somehow reset those `inside' ref cells to reflect the location
 *   of the function before you can uncount its references. In most cases, this
 *   is unnecessary, but it is necessary when undertaking a function mutually
 *   recursive with a function in which you currently are when you detect the
 *   function's death.
 * rather than fix this last point, I decided to give up on keeping internal
 * counts up-to-date. Instead, I just compute them once during collect and
 * never touch them again: this means that they should not be relied on in
 * general. More specifically, they become potentially invalid as soon as
 * the body of the function is changed. This still allows their use in
 * many cases.
 *)

structure Collect :> COLLECT =
struct
local
  structure PL = PLambda
  structure F  = FLINT
  structure T  = LambdaVar.Tbl  (* lvar hashtable *)
  structure FU = FlintUtil
  structure LV = LambdaVar
  structure PU = PrintUtil
  structure PF = PrintFlint
  structure PO = Primop

  val debugging = FLINT_Control.fcdebugging

  fun say s = (Control_Print.say s; Control_Print.flush())
  fun newline () = say "\n"
  fun saynl s = (Control_Print.say s; say "\n"; Control_Print.flush())
  fun says (msgs: string list) = say (PU.interpws msgs)
  fun saysnl (msgs: string list) = saynl (PU.interpws msgs)
  fun dbsay msg = if !debugging then saynl msg else ()
  fun dbsays msgs = if !debugging then saysnl msgs else ()
  fun bug msg = ErrorMsg.impossible ("Collect: "^msg)
  fun buglexp (msg,lexp) = (newline(); PF.printLexp lexp; bug msg)
  fun bugval (msg,value) = (newline(); PF.printValue value; bug msg)

in

(* info: counts of calls, (escaping?) uses, "internal" uses (recursive calls?)
 *  INVARIANTS:
 *  (1) !uses >=0, !calls >= 0, !intuses >= 0  (all values non-negative)
 *  (2) !calls <= !uses *)
type info = {calls: int ref, uses: int ref}

exception InfoTable

(* infoTable : info T.hash_table
 *  fixed, global hash table mapping lvars to associated info records,
 *  must be cleared for each call of collect (by calling T.clear infoTable) *)
val infoTable : info T.hash_table = T.mkTable (128, InfoTable)

(* infoToString : info -> string
 *  string representing an info *)
fun infoToString ({uses, calls}: info) : string =
    concat ["{", Int.toString (!uses), ",", Int.toString (!calls), "}"]

(* lvarToString : LV.lvar -> string
 *  fails (getInfo calls bug) if lvar has not been "registered" in infoTable *)
fun lvarToString (lvar: LV.lvar) =
    (case T.find infoTable lvar
       of SOME info => LV.lvarName lvar ^ infoToString info
        | NONE => LV.lvarName lvar ^ "{?}")  (* unregistered lvar *)

(* new : LV.lvar -> info
 *  registering a new lvar in the hash infoTable *)
fun new lvar =
    let val info = {uses=ref 0, calls=ref 0}
     in T.insert infoTable (lvar, info);
        info
    end

(* getInfo : LV.lvar -> info
 *  lookup lvar in infoTable (T.find), returning info if found or calling bug ow *)
fun getInfo (lvar : LV.lvar) : info =
    case T.find infoTable lvar
      of SOME info => info
       | NONE => bug ("getInfo: " ^ LV.toString lvar)

(* addto : info * info -> unit
 *  adds the uses/calls counts of info1 to those of info2, updating info2 *)
fun addto ({uses=uses1,calls=calls1,...}: info, {uses=uses2,calls=calls2,...}: info) =
    (uses2 := !uses2 + !uses1; calls2 := !calls2 + !calls1)

(* transfer : LV.lvar * LV.lvar -> unit
 *   adds uses/calls counts for lvar1 to those of lvar2 and redirects lvar1 to lvar2's info *)
fun transfer (lvar1, lvar2) =
    let val info1 = getInfo lvar1
        val info2 = getInfo lvar2
     in (addto(info1, info2);
	 T.insert infoTable (lvar1, info2)) (* note the transfer by redirecting the map *)
    end

exception Dec (* raised when decrementing a non-positive ref *)

(* inc : int ref -> unit   -- increment an int ref by 1
 * dec : int ref -> unit   -- decrement, raising Dec if contents < 1 *)
fun inc ri = (ri := !ri + 1)
fun dec ri =
    let val n = !ri
    in if n < 1
       then raise Dec
       else ri := n - 1
    end

(* usesInc : info -> unit
 *  increment uses count *)
fun usesInc ({uses,calls}: info) =
    inc uses

(* callsInc : info -> unit
 *  increment calls count *)
fun callsInc ({uses,calls,...}: info) =
    (inc uses; inc calls)

(* usesDec : info -> bool
 *  bool result indicates whether use was decremented to 0 *)
fun usesDec ({uses, ...}: info) =
    (dec uses;         (* raises Dec if !uses < 1 *)
     !uses = 0)   (* was uses decremented to zero? *)

(* callsDec : info -> bool *)
fun callsDec ({uses, calls, ...}: info) =
    (* notice the calls could be dec'd to negative values because a
     * use might be turned from escaping to known between the census
     * and the unuse.  We can't easily detect such changes, but
     * we can detect it happened when we try to go below zero.
     * [DBM: if dec fails on values < 1, will this cause a problem?] *)
    (dec uses; dec calls;  (* raises Dec if !calls < 1 or !uses < 1 *)
     !uses = 0)   (* was uses decremented to zero? *)

(* uses : info -> int  -- number of uses *)
fun uses ({uses,...}: info) = !uses
(* calls : info -> int  -- number of calls *)
fun calls ({calls,...}: info) = !calls
(* used : info -> bool  -- any uses? *)
fun used ({uses,...}: info) = !uses > 0
(* dead : info -> bool  -- no uses? *)
fun dead ({uses,...}: info) = !uses = 0
(* escaping : info -> bool  -- more uses than calls *)
fun escaping ({uses,calls}: info) = !uses > !calls
(* called : info -> bool  -- some calls *)
fun called ({calls,...}: info) = !calls > 0

(* usedLvar : LV.lvar -> bool *)
fun usedLvar (lvar: LV.lvar) = used (getInfo lvar)

(* kill : LV.lvar -> unit
 *  delete the _last_ reference to a variable, i.e. delete the variable from infoTable
 *  Ideally, we should check that uses = 1, but we may have been a bit
 *  conservative when keeping the counts up to date *)
fun kill (lvar: LV.lvar) : unit =
    ignore (T.remove infoTable lvar)
    handle InfoTable => ()

(* ********************************************************************** *)

fun impurePO (po:F.primop) = PrimopUtil.effect (#2 po)

(* analyze: F.lexp -> unit -- the main collection function *)
val analyze: F.lexp -> unit =
let
    (* call : LV.lvar -> unit
     *  increment uses and calls for the lvar, which is assumed to have been registered *)
    fun call (lvar: LV.lvar) = callsInc (getInfo lvar)

    (* use : F.value -> unit   -- local version of usesInc that applies for F.value *)
    fun use (F.VAR lvar: F.value) = usesInc (getInfo lvar)
      | use _ = ()

    (* cpo : F.primop -> unit
     *   here, the use resembles a call, but it's safer to consider it as a use *)
    fun cpo (NONE: F.dict option, po, lty, tycs) = ()
      | cpo (SOME {default,table}, po, lty, tycs) =
	(use (F.VAR default); app (use o F.VAR o #2) table)

    (* cdcon : PL.dataconstr -> unit *)
    fun cdcon (s, Access.EXN(Access.LVAR lv),lty) = use (F.VAR lv)
      | cdcon _ = ()

    (* cexp: F.lexp -> unit
     * the actual analysis/collection (or "census") function *)
    fun cexp lexp =
	case lexp
	 of F.RET vs => app use vs

	  | F.LET (lvs,le1,le2) =>
	    let val lvsi = map new lvs
	    in cexp le2; cexp le1
	    end

	  | F.FIX (fundecs,fixBody) =>
	    let fun cfun (_, _, args, body) = (* analysis of a fundec *)
		    (app (fn (v,t) => ignore(new v)) args;
		     cexp body)
		fun cfix fundecs0 = (* analysis of the list of fundecs of the FIX *)
		    let val (usedfds,unusedfds) = List.partition (usedLvar o #2) fundecs0
		    in if List.null usedfds then ()
		       else (app cfun usedfds; cfix unusedfds)
		       (* more fundecs may have become used *)
		    end
	    in app (fn (_,fvar,_,_) => ignore(new fvar)) fundecs; (* register function lvars *)
	       cexp fixBody;
	       cfix fundecs
	    end

	  | F.APP (F.VAR f,vs) =>   (* function "value" must be a VAR *)
	    (call f; app use vs)

	  | F.TFN ((tfk,tf,args,body),le) =>
	    let val tfi = new tf
	    in cexp le; if used tfi then cexp body else ()
	    end

	  | F.TAPP (F.VAR tf,tycs) => call tf

	  | F.SWITCH (v,cs,arms,def) =>
	    (use v; Option.map cexp def;
	     app (fn (PL.DATAcon(dc,_,lv),le) => (cdcon dc; new lv; cexp le)
		   | (_,le) => cexp le)
		 arms)

	  | F.CON (dc,_,v,lv,le) =>
	    let val lvinfo = new lv
	    in cdcon dc; cexp le; if used lvinfo then use v else ()
	    end

	  | F.RECORD (_,vs,lv,le) =>
	    let val lvi = new lv
	    in cexp le; if used lvi then app use vs else ()
	    end

	  | F.SELECT (v,_,lv,le) =>
	    let val lvi = new lv
	    in cexp le; if used lvi then use v else ()
	    end

	  | F.RAISE (v,_) => use v
	  | F.HANDLE (le,v) => (use v; cexp le)

	  | F.BRANCH (po,vs,le1,le2) =>
	    (app use vs; cpo po; cexp le1; cexp le2)

	  | F.PRIMOP (po,vs,lv,le) =>
	    let val lvi = new lv
	    in  cexp le;
		if used lvi orelse impurePO po then (cpo po; app use vs) else ()
	    end

	  | le => buglexp("unexpected lexp", le)
in
    cexp
end

(* unuselexp : undertaker:(LambdaVar.lvar -> unit) -> FLINT.lexp -> unit
 *  fix up function to keep counts up-to-date when getting rid of code.
 *  the undertaker arg is called for *free* variables when they become dead.
 *  The code is almost the same for uncounting, except that calling
 *  undertaker should not be done for non-free variables. For that we
 *  artificially increase the usage count of each variable when it's defined
 *  (by calling usesInc) so that its counter never reaches 0
 *  while processing its scope. Once its scope has been processed, we can
 *  completely get rid of the variable and corresponding info (after verifying
 *  that the count is indeed exactly 1 (accomplished by the "kill" calls) *)
fun unuselexp (undertaker: LV.lvar -> unit) =
let
    (* uncall : LV.lvar -> unit *)
    fun uncall (lvar: LV.lvar) =
	if callsDec (getInfo lvar) then undertaker lvar else ()

    (* unuse : F.value -> unit *)
    fun unuse (F.VAR lvar) =
	if usesDec (getInfo lvar) then undertaker lvar else ()
      | unuse _ = ()

    (* useLvar : LV.lvar -> unit *)
    fun useLvar lvar = usesInc (getInfo lvar)

    (* cpo : F.primop -> unit *)
    fun cpo (NONE:F.dict option,po,lty,tycs) = ()
      | cpo (SOME{default,table},po,lty,tycs) =
	(unuse(F.VAR default); app (unuse o F.VAR o #2) table)

    (* cdcon : PL.dataconstr -> unit *)
    fun cdcon (s, Access.EXN(Access.LVAR lv), lty) = unuse(F.VAR lv)
      | cdcon _ = ()

    (* cfun : LV.lvar list * F.lexp -> unit *)
    fun cfun (args, body) = (* fundec *)
	  (app useLvar args; cexp body; app kill args)

    (* cexp : F.lexp -> unit *)
    and cexp (lexp: F.lexp) : unit =
	case lexp
	 of F.RET vs => app unuse vs

	  | F.LET (lvs,le1,le2) =>
	    (app useLvar lvs; cexp le2; cexp le1; app kill lvs)

	  | F.FIX (fundecs, body) =>
	    let val funvars = map #2 fundecs            (* function lvars *)
		val funvarInfos = map getInfo funvars   (* corresponding infos *)
		fun folder (info, fundec, fundecs) =
		    if used info then fundec::fundecs else fundecs
		val usedFundecs = ListPair.foldrEq folder nil (funvarInfos, fundecs)
	     in app usesInc funvarInfos;
		cexp body;
		app (fn (_,_,args,le) => cfun(map #1 args, le)) usedFundecs;
		app kill funvars
	    end

	  | F.APP (F.VAR f, vs) =>
	    (uncall f; app unuse vs)

	  | F.TFN ((tfk,tf,args,body),le) =>
	    let val tfinfo = getInfo tf
	     in if used tfinfo then cexp body else ();
	        usesInc tfinfo; cexp le; kill tf
	    end

	  | F.TAPP (F.VAR tf,tycs) => uncall tf

	  | F.SWITCH (v,cs,arms,default) =>
	    (unuse v; Option.app cexp default;
	     (* here we don't absolutely have to keep track of vars bound within
	      * each arm since these vars can't be eliminated anyway *)
	     app (fn (PL.DATAcon(dc,_,lv),le) =>
		     (cdcon dc;
		      usesInc (getInfo lv);
		      cexp le; kill lv)
		   | (_,le) => cexp le)
		 arms)

	  | F.CON (dc,_,v,lv,le) =>
	      let val lvinfo = getInfo lv
	       in cdcon dc; if used lvinfo then unuse v else ();
	          usesInc lvinfo; cexp le; kill lv
	      end

	  | F.RECORD (_, vs, lv, le) =>
	      let val lvinfo = getInfo lv
	       in if used lvinfo then app unuse vs else ();
		  usesInc lvinfo; cexp le; kill lv
	      end

	  | F.SELECT (v,_,lv,le) =>
	      let val lvinfo = getInfo lv
	       in if used lvinfo then unuse v else ();
                  usesInc lvinfo; cexp le; kill lv
	      end

	  | F.RAISE (v,_) => unuse v
	  | F.HANDLE (le,v) => (unuse v; cexp le)

	  | F.BRANCH (po,vs,le1,le2) =>
	    (app unuse vs; cpo po; cexp le1; cexp le2)

	  | F.PRIMOP (po,vs,lv,le) =>
	      let val lvinfo = getInfo lv
	       in if used lvinfo orelse impurePO po
	          then (cpo po; app unuse vs)
	          else ();
	          usesInc lvinfo; cexp le; kill lv
	      end

	  | le => buglexp("unexpected lexp", le)
in
    cexp
end

(* copylexp : alpha:LV.lvar LV.Map.map -> lexp:F.lexp -> F.lexp
 *  copy the lexp with alpha-conversion using alpha lvar map *)
fun copylexp alpha lexp =
    let val newlexp = FU.copy [] alpha lexp
     in analyze newlexp; newlexp
    end

(* collect : F.fundec -> unit *)
fun collect (fundec as (_,f,_,_) : F.fundec) : unit =
    (T.clear infoTable;	   (* clear the infoTable hashtable to start from a fresh state *)
     PF.lvarToStringRef := lvarToString;
     analyze (F.FIX([fundec], F.RET[F.VAR f])))


end (* top local *)
end (* structure Collect *)

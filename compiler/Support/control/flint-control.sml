(* Admin/control/flint-control.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure FLINTControl :> FLINT_CONTROL =
struct

    val printAllIR    = ref false (* "show IR after each phase" *)
    val printPlambda  = ref false (* "show plambda IR after translate" *)
    val printFlint    = ref false (* "show flint IR after each phase" *)
    val printPhases   = ref false (* "show phases" *)
    val printFctTypes = ref false (* "show function types" *)
    val printDTNames  = ref true  (* "print datatype names in PPLty.fmtTyc" *)
    val printINDltys  = ref false (* "print IND tycs and ltys" *)

    val lkdebugging   = ref false (* "LtyKernel debugging" *)
    val tmdebugging   = ref false (* "TransTypes debugging" *)
    val trdebugging   = ref false (* "Translate debugging" *)

    val nmdebugging   = ref false (* "PLambda normalization debugging" *)
    val lcdebugging   = ref false (* "lcontract phase debugging" *)
    val fcdebugging   = ref false (* "fcontract phase debugging" *)
    val fccounters    = ref false (* "fcontract counter messages" *)
    val spdebugging   = ref false (* "specialize phase debugging" *)
    val ffdebugging   = ref false (* "fixfix phase debugging" *)
    val wrdebugging   = ref false (* "wrap phase debugging" *)
    val redebugging   = ref false (* "reify phase debugging" *)
    val rtdebugging   = ref false (* "runtime types(?) debugging" *)

    val phases = (* "FLINT optimizer phases" *)
	ref ["deb2names",       (* 0 *)
	     "lcontract",       (* 1 *) (* Cruder but quicker than fcontract *)
	     "fixfix",          (* 2 *)
	     "fcontract",       (* 3 *)
	     "specialize",      (* 4 *)
	     "loopify",         (* 5 *)
	     "fixfix",          (* 6 *)
	     "fcontract",       (* 7 *)
	     "wrap",            (* 8 *)
	     "fcontract",       (* 9 *)
	     "reify",           (* 10 *)
	     "fcontract",       (* 11 *)
	     "fixfix",          (* 12 *)
	     "fcontract+eta"])  (* 13 *)

    val currentPhase = ref "nophase" (* "current FlintOpt phase" *)

    fun insertPhase (phaseName: string, pos: int) : unit = 
	let val phases0 = !phases
            fun insert (0, prefix, rest) = List.revAppend (prefix, phaseName :: rest)
	      | insert (n, prefix, nil) = raise Subscript
	      | insert (n, prefix, p::ps) = insert (n-1, p::prefix, ps)
	in phases := insert (pos, nil, phases0)
	end
	    
    val inlineThreshold = ref 16 (* "inline threshold" *)
    val unrollThreshold = ref 20 (* "unroll threshold" *)
    val maxargs = ref 6 (* "max number of arguments" *)
    val dropinvariant = ref true (* "dropinvariant" *)

    val specialize = ref true (* "whether to specialize" *)

    (* val liftLiterals	= ref false *)
    val sharewrap = ref true (* "sharewrap", "whether to share wrappers" *)
    val saytappinfo = ref false (* "whether to show typelifting stats" *)

    (* a spare integer flag for random temporary debugging *)
    val misc = ref 0 (* "misc FLINT debugging" *)

    (* ======== TYPE CHECKING IR ========= *)

    (* PLambda.lexp type checking *)
    val checkPLambda    = ref false (* "typecheck plambda" *)

    (* FLINT internal type-checking controls *)
    val check = ref false (* "whether to typecheck the IR" *)
        (* fails on MLRISC/*/*RegAlloc.sml *)
    val checkDatatypes = ref false (* "typecheck datatypes" *)
	(* loops on the new cm.sml *)
    val checkKinds = ref true (* "check kinding information" *)

    (* exported for use in FLINT/main/flintcomp.sml -- belongs somewhere else *)
(*    val recover : (LambdaVar.lvar -> unit) ref = ref(fn x => ()) *)

end (* structure FLINTControl *)

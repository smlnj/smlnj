(* compiler/FLINT/main/control.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure FlintControl :> FLINTCONTROL =
struct

  val {newBool, newInt, newString, newStrings} = MakeControls.make {name = "FLINT", priority = [1]}

  val printAllIR    = newBool ("print", "show IR after each phase", false)
  val printPlambda  = newBool ("print", "show plambda IR after translate", false)
  val printFlint    = newBool ("print", "show flint IR after each phase", false)
  val printPhases   = newBool ("print-phases", "show phases", false)
  val printFctTypes = newBool ("print-fct-types", "show function types", false)
  val printDTNames  = newBool ("print", "print datatype names in PPLty.fmtTyc", true)
  val printINDltys  = newBool ("print", "print IND tycs and ltys", false)

  val lkdebugging   = newBool ("lkdebugging", "LtyKernel debugging", false)
  val tmdebugging   = newBool ("tmdebugging", "TransTypes debugging", false)
  val trdebugging   = newBool ("trdebugging", "Translate debugging", false)

  val nmdebugging   = newBool ("nmdebugging",
			   "PLambda normalization debugging", false)
  val lcdebugging   = newBool ("lcdebugging",
			   "lcontract phase debugging", false)
  val fcdebugging   = newBool ("fcdebugging",
			   "fcontract phase debugging", false)
  val fccounters    = newBool ("fccounters",
			   "fcontract counter messages", false)
  val spdebugging   = newBool ("spdebugging",
			   "specialize phase debugging", false)
  val ffdebugging   = newBool ("ffdebugging",
			   "fixfix phase debugging", false)
  val wrdebugging   = newBool ("wrdebugging",
			   "wrap phase debugging", false)
  val redebugging   = newBool ("redebugging",
			   "reify phase debugging", false)
  val rtdebugging   = newBool ("rtdebugging",
			   "runtime types(?) debugging", false)

  (* OBS[split eliminated]:
   * `split' should probably be called just after `fixfix' since
   * fcontract might eliminate some uncurry wrappers which are
   * locally unused but could be cross-module inlined. *)
  val phaseNames : string list =
	   ["deb2names",       (* 0 *)
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
	    "fcontract+eta"]  (* 13 *)

  val phases = newStrings ("phases", "FLINT optimizer phases", phaseNames)

  val currentPhase = newString ("currentPhase", "current FlintOpt phase", "nophase")

  fun insertPhase (phaseName: string, pos: int) : unit = 
      let val phases0 = !phases
	  fun insert (0, prefix, rest) = List.revAppend (prefix, phaseName :: rest)
	    | insert (n, prefix, nil) = raise Subscript
	    | insert (n, prefix, p::ps) = insert(n-1, p::prefix, ps)
      in phases := insert(pos, nil, phases0)
      end

  val inlineThreshold = newInt ("inline-theshold", "inline threshold", 16)
  val unrollThreshold = newInt ("unroll-threshold", "unroll threshold", 20)
  val maxargs = newInt ("maxargs", "max number of arguments", 6)
  val dropinvariant = newBool ("dropinvariant", "dropinvariant", true)

  val specialize = newBool ("specialize", "whether to specialize", true)

  (* val liftLiterals	= ref false *)
  val sharewrap = newBool ("sharewrap", "whether to share wrappers", true)
  val saytappinfo = newBool ("saytappinfo", "whether to show typelifting stats", false)

  (* only for temporary debugging *)
  val misc = newInt ("misc FLINT debugging", "misc debugging", 0)

  (* ======== TYPE CHECKING IR ========= *)

  (* PLambda.lexp type checking *)
  val checkPLambda = newBool ("checkPLambda", "typecheck plambda", false)

  (* FLINT internal type-checking controls *)
  val check = newBool ("check", "whether to typecheck the IR", false)
      (* fails on MLRISC/*/*RegAlloc.sml *)
  val checkDatatypes = newBool ("check-datatypes", "typecheck datatypes", false)
      (* loops on the new cm.sml *)
  val checkKinds = newBool ("check-kinds",
			"check kinding information", true)

  (* trackExn and polyEqWarn moved here from Control (TopLevel/main/control.{sig,sml} *)
  val trackExn =
      newBool ("track-exn", "whether to generate code that tracks exceptions", true)
  (* warning message when call of polyEqual compiled: *)
  val polyEqWarn =
      newBool ("poly-eq-warn", "whether to warn about calls of polyEqual", true)

  (* exported for use in FLINT/main/flintcomp.sml *)
  val recover : (LambdaVar.lvar -> unit) ref = ref(fn x => ())

end (* structure FlintControl *)

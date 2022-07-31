(* control.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure FLINT_Control :> FLINTCONTROL =
struct
   local
       val priority = [10, 11, 1]
       val obscurity = 5
       val prefix = "flint"

       val registry = ControlRegistry.new
			  { help = "FLINT optimizer settings" }

       val _ = BasicControl.nest (prefix, registry, priority)

       val flag_cvt = ControlUtil.Cvt.bool
       val int_cvt = ControlUtil.Cvt.int
       val string_cvt = ControlUtil.Cvt.string
       val sl_cvt = ControlUtil.Cvt.stringList

       val nextpri = ref 0

       fun new (cvt_fn, name: string, help: string, default) =
	   let val r = ref default
	       val p = !nextpri
	       val ctl = Controls.control { name = name,
					    pri = [p],
					    obscurity = obscurity,
					    help = help,
					    ctl = r }
	   in nextpri := p + 1;
	      ControlRegistry.register registry
	       { ctl = Controls.stringControl cvt_fn ctl,
		 envName = SOME (ControlUtil.EnvName.toUpper "FLINT_" name) };
	      r
	   end
   in

    val printAllIR    = new (flag_cvt, "print", "show IR after each phase", false)
    val printPlambda  = new (flag_cvt, "print", "show plambda IR after translate", false)
    val printFlint    = new (flag_cvt, "print", "show flint IR after each phase", false)
    val printPhases   = new (flag_cvt, "print-phases", "show phases", false)
    val printFctTypes = new (flag_cvt, "print-fct-types", "show function types", false)
    val printDTNames  = new (flag_cvt, "print", "print datatype names in PPLty.fmtTyc", true)
    val printINDltys  = new (flag_cvt, "print", "print IND tycs and ltys", false)

    val lkdebugging   = new (flag_cvt, "lkdebugging", "LtyKernel debugging", false)
    val tmdebugging   = new (flag_cvt, "tmdebugging", "TransTypes debugging", false)
    val trdebugging   = new (flag_cvt, "trdebugging", "Translate debugging", false)

    val nmdebugging   = new (flag_cvt, "nmdebugging",
			     "PLambda normalization debugging", false)
    val lcdebugging   = new (flag_cvt, "lcdebugging",
			     "lcontract phase debugging", false)
    val fcdebugging   = new (flag_cvt, "fcdebugging",
			     "fcontract phase debugging", false)
    val fccounters    = new (flag_cvt, "fccounters",
			     "fcontract counter messages", false)
    val spdebugging   = new (flag_cvt, "spdebugging",
			     "specialize phase debugging", false)
    val ffdebugging   = new (flag_cvt, "ffdebugging",
			     "fixfix phase debugging", false)
    val wrdebugging   = new (flag_cvt, "wrdebugging",
			     "wrap phase debugging", false)
    val redebugging   = new (flag_cvt, "redebugging",
			     "reify phase debugging", false)
    val rtdebugging   = new (flag_cvt, "rtdebugging",
			     "runtime types(?) debugging", false)

    (* OBS[split eliminated]:
     * `split' should probably be called just after `fixfix' since
     * fcontract might eliminate some uncurry wrappers which are
     * locally unused but could be cross-module inlined. *)
    val phases =
	new (sl_cvt, "phases", "FLINT optimizer phases",
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
	      "fcontract+eta"])  (* 13 *)

    val currentPhase = new (string_cvt, "currentPhase", "current FlintOpt phase", "nophase")

    fun insertPhase (phaseName: string, pos: int) : unit = 
	let val phases0 = !phases
            fun insert (0, prefix, rest) = List.revAppend (prefix, phaseName :: rest)
	      | insert (n, prefix, nil) = raise Subscript
	      | insert (n, prefix, p::ps) = insert(n-1, p::prefix, ps)
	in phases := insert(pos, nil, phases0)
	end
	    
    val inlineThreshold = new (int_cvt, "inline-theshold", "inline threshold", 16)
    (* val splitThreshold  = ref 0 *)
    val unrollThreshold = new (int_cvt, "unroll-threshold", "unroll threshold", 20)
    val maxargs = new (int_cvt, "maxargs", "max number of arguments", 6)
    val dropinvariant = new (flag_cvt, "dropinvariant", "dropinvariant", true)

    val specialize = new (flag_cvt, "specialize", "whether to specialize", true)

    (* val liftLiterals	= ref false *)
    val sharewrap = new (flag_cvt, "sharewrap", "whether to share wrappers", true)
    val saytappinfo = new (flag_cvt, "saytappinfo", "whether to show typelifting stats", false)

    (* only for temporary debugging *)
    val misc = new (int_cvt, "misc FLINT debugging", "misc debugging", 0)

    (* ======== TYPE CHECKING IR ========= *)

    (* PLambda.lexp type checking *)
    val checkPLambda    = new (flag_cvt, "checkPLambda", "typecheck plambda", false)

    (* FLINT internal type-checking controls *)
    val check = new (flag_cvt, "check", "whether to typecheck the IR", false)
        (* fails on MLRISC/*/*RegAlloc.sml *)
    val checkDatatypes = new (flag_cvt, "check-datatypes", "typecheck datatypes", false)
	(* loops on the new cm.sml *)
    val checkKinds = new (flag_cvt, "check-kinds",
			  "check kinding information", true)

    (* exported for use in FLINT/main/flintcomp.sml *)
    val recover : (LambdaVar.lvar -> unit) ref = ref(fn x => ())

   end
end

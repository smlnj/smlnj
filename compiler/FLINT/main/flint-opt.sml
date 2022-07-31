(* flint-opt.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure FLINTOpt : sig

  (* [OBS, deleted] the int option (splitting) gets passed to lambda-split phases (if any).
   * The "split" phase has been eliminated  *)

    val optimize : FLINT.prog * string -> FLINT.prog

end = struct

    structure CTRL = FLINT_Control  (* == Control.FLINT *)
    structure PF = PrintFlint  (* structure PPF = PPFlint *)
    structure LB = LtyBasic
    structure LE = LtyExtern
    structure F  = FLINT

    fun bug s = ErrorMsg.impossible ("FLINTOpt:" ^ s)
    val say = Control_Print.say

    (* flintkind : FLINT "variants" -- four flavors of FLINT lexps
     *   FK_DEBRUIJN : expression-bound type variables use deBruijn representation
     *   FK_NAMED : expression-bound type variables use "named" (lvar-like) representation
     *   FK_WRAP : FK_NAMED representation that has had "wrapping" applied
     *   FK_REIFY : FK_NAMED (and FK_WRAP?) that has had types and type parameters lowered
     *      to value level *)
    datatype flintkind = FK_DEBRUIJN | FK_NAMED | FK_WRAP | FK_REIFY

    (* phase mapping of flintkind:
       lcontract : fk --> fk  (fk <> FK_DEBRUIJN; not applicable to FK_DEBRUIJN)
       fcontract : fk --> fk  (fk <> FK_DEBRUIJN; not applicable to FK_DEBRUIJN)
       fcontract+eta : fk --> fk  (fk <> FK_DEBRUIJN)  -- fcontract+eta = fcontract[etaSplit=true]
       fixfix : fk --> fk
       loopify : fk --> fk
       specialize : FK_NAMED --> FK_NAMED
       wrap : FK_NAMED --> FK_NAMED
       reify : FK_WRAP --> FK_REIFY
       deb2names : FK_DEBRUIJN --> FK_NAMED
       names2deb : fk --> FK_DEBRUIJN  (fk = FK_NAMED, FK_WRAP?, FK_REIFY?)
       typelift : fk --> fk
       split : FK_NAMED --> FK_NAMED
       id, wellformed, recover, print, printsplit, check : fk --> fk
         (check may require FK_DEBRUIJN)

       Progression of flintkinds starting with flintnm:

          PLambda.lexp -->  (flintnm)   --> FK_DEBRUIJN
                       -->  (deb2names) --> FK_NAMED
                       -->* (wrap)      --> FK_WRAP
                       -->* (reify)     --> FK_REIFY
       Questions:
         (1) Does FK_REIFY => FK_WRAP => FK_NAMED?
         (2) What about names2deb (FK_REIFY) or names2deb (FK_WRAP)?
    *)

    type phaseData =
	 (F.prog          (* flint code (fundec == prog) *)
	* flintkind       (* the prog's flint "flavor" *)
	* string)         (* name of previous phase that produced the prog *)
       (* [DBM]: a fi: prog option ("inlineable approximation"), was previously
        * produced by the split phase, which has been eliminated because it was not used. *)

    fun phase x = Stats.doPhase (Stats.makePhase x)

    val deb2names  = phase "FLINT 056 deb2names" TvarCvt.debIndex2names
    val names2deb  = phase "FLINT 057 names2deb" TvarCvt.names2debIndex

    val lcontract  = phase "FLINT 052 lcontract" LContract.lcontract
    val fcontract  = phase "FLINT 052b fcontract" FContract.contract
    val loopify    = phase "FLINT 057 loopify" Loopify.loopify
    val fixfix     = phase "FLINT 056 fixfix" FixFix.fixfix
    val typelift   = phase "FLINT 0535 typelift" Lift.typeLift
    val wformed    = phase "FLINT 0536 wformed" Lift.wellFormed
    val specialize = phase "FLINT 053 specialize" Specialize.specialize
    val wrap       = phase "FLINT 054 wrap" Wrapping.wrapping
    val reify      = phase "FLINT 055 reify" Reify.reify
    val recover    = phase "FLINT 05a recover" Recover.recover

    (* printProg : string * F.prog -> F.prog *)
    (** pretty printing FLINT code *)
    fun printProg (phaseName: string, prog: F.prog) : F.prog =
	if !CTRL.printAllIR
	then (say (concat["\n[After ", phaseName, " ...]\n\n"]);
	      PrintFlint.printProg prog; say "\n";
	      prog)
	else prog

    (* dumpProg : string * F.prog -> unit *)
    (** writing out a term into a error output file named <filename>.FLINT<phase> *)
    fun dumpProg (fileName, prog) =
	let val outS = TextIO.openAppend fileName  (* appends to file if called repeatedly *)
	    val savedOut = !Control.Print.out
            val tempOut = {say = fn s => TextIO.output(outS,s),
			   flush = fn () => TextIO.flushOut outS}
	    fun finish () = (TextIO.closeOut outS; Control.Print.out := savedOut)
         in Control.Print.out := tempOut;
	    PrintFlint.printFundec prog  (* PPF.ppProg prog *)
	      handle x => (finish () handle _ => (); raise x);
	    finish ()
        end (* function dumpProg *)


    (* optimize : F.prog * string -> F.prog * F.prog option *)
    (** optimizing FLINT code *)
    fun optimize (prog: F.prog, sourceName: string) =
	let val _ = (FContract.contractCount := 0)
	    (* keeping track of fcontract rounds, initialized to 0 *)

	    val baseName = OS.Path.base sourceName

          (* dump: F.prog * string -> unit *)
          (* ASSUME: prog uses named type variables (FK_NAMED, FK_WRAP, FK_REIFY) *)
	  fun dump (phase, prog, flintkind) =
	      (case flintkind
		of FK_DEBRUIJN => dumpProg (concat [baseName, "-deb.", phase], prog)
		 | _ => (dumpProg (concat [baseName, "-deb.", phase], names2deb prog);
			 dumpProg (concat [baseName, "-named.", phase], prog)))

          (* check : string * bool * string * F.prog -> unit *)
	  (* checkKind : string = "FLINT" or "iFLINT", the later indicates an "inlineable approximation"
           * check assumes the F.prog arg is using deBruijn-style type variables (is flintkind
	   * FK_DEBRUIJN, or has been transformed by names2deb) *)
	  fun check (phase: string, reified: bool, prog: F.prog) =
	      if ChkFlint.checkTop (prog, reified)
	      then (dumpProg (baseName ^ "." ^ phase, prog);
		    bug ("FLINT typing errors " ^ phase))
	      else ()

	  fun wff (f, s) =
	      if wformed f
	      then ()
	      else print ("\nAfter " ^ s ^ " CODE NOT WELL FORMED\n")

	 (* phase: string      phase name ("lcontract", etc.)
	  * f: prog            flint code (a fundec = prog)
	  * fk: flintkind      the flint "variant" for f
	  * l: string          name of the (previous) phase that produced f
	  *)
	  fun runphase (phase: string, pdata as (f, fk, l): phaseData) : phaseData =
	      (FLINT_Control.currentPhase := phase;
	       case (phase, fk)
		 of (("fcontract" | "lcontract"), FK_DEBRUIJN) =>
		      (say ("\n!! " ^ phase ^ " cannot be applied to the DeBruijn form !!\n");
		       pdata)
		  | ("fcontract", _) =>
		      (fcontract (false, f), fk, phase)   (* etaSplit = false *)
		  | ("fcontract+eta", _) =>
		      (fcontract (true, f), fk, phase)    (* etaSplit = true *)
		  | ("lcontract",_) =>
		      (lcontract f, fk, phase)
		  | ("fixfix", _) =>
		      (fixfix f, fk, phase)
		  | ("loopify", _) =>
		      (loopify f, fk, phase)
		  | ("specialize", FK_NAMED) =>
		      (specialize f, fk, phase)
		  | ("wrap", FK_NAMED) =>
		      let val f_wrapped = wrap f
		       in if !CTRL.wrdebugging
			  then let val f_wrapped_deb = names2deb f_wrapped
				       (* coverted back to FK_DEBRUIJN *)
			        in (dumpProg (sourceName ^ "-named.wrap", f_wrapped);
				    dumpProg (sourceName ^ "-deb.wrap", f_wrapped_deb))
			       end
			  else ();
			  (f_wrapped, FK_WRAP, phase)
		      end
		  | ("reify", FK_WRAP) =>
		      (reify f, FK_REIFY, phase)
		  | ("deb2names", FK_DEBRUIJN) =>
		      (deb2names f, FK_NAMED, phase)
		  | ("names2deb", FK_NAMED) =>
		      (names2deb f, FK_DEBRUIJN, phase)
		  | ("typelift", _) =>
		      let val f' = typelift f
		       in if !CTRL.check then wff(f, phase) else ();
			  (f', fk, phase)
		      end

		  (* pseudo FLINT phases *)
		  | ("dump", _) =>
		      (dump (l, f, fk); pdata)
		  | _ => (case phase
			    of "id" => ()  (* does nothing *)
			     | "wellformed" => wff(f,l)
			     | "recover" =>
			         let val getlty = recover(f, fk = FK_REIFY)
				  in CTRL.recover := (say o LB.lt_print o getlty o F.VAR)
				 end
			     | "print" =>
			         (say("\n[After "^l^"...]\n\n"); PF.printFundec f; say "\n")
			     | "check" => check (l, (fk = FK_REIFY), f)
			     | _ => say(concat["\n!! Unknown or badly scheduled FLINT phase '", phase,
					       "' !!\n"])
			  (* end case *);
			  pdata)
		(* end case *))

	  fun print (pdata as (prog,_,lastphase)) =
	      (printProg (lastphase, prog); pdata)

	  fun check' (pdata as (f,fk,l)) =
	      (if !CTRL.check
	       then check (l, (fk = FK_REIFY), names2deb f)
	       else ();
	       pdata)

	  fun showhist [s] = say(concat["  raised at:\t", s, "\n"])
	    | showhist (s::r) = (showhist r; say (concat["\t\t", s, "\n"]))
	    | showhist [] = ()

	  fun runphase' (arg as (phase, (prog,_,_))) =
	      (if !CTRL.printPhases then say ("Phase " ^ phase ^ "...") else ();
	       ((check' o print o runphase) arg) before
	       (if !CTRL.printPhases then say ("..." ^ phase ^ " Done.\n") else ()))
	      handle x => (say ("\nwhile in " ^ phase ^" phase\n");
			   dumpProg("flint.core", prog);
			   showhist(SMLofNJ.exnHistory x);
			   raise x)

	  val (prog, fk, _) =
	      foldl runphase' (prog, FK_DEBRUIJN, "flintnm") (!CTRL.phases)

        (* run any missing, mandatory, phases: deb2names, wrap, reify *)
	  val (prog, fk) =
	      if fk = FK_DEBRUIJN
	      then (say "\n!!Forgot deb2names!!\n"; (deb2names prog, FK_NAMED))
	      else (prog, fk)
	  val (prog, fk) =
	      if fk = FK_NAMED
	      then (say "\n!!Forgot wrap!!\n"; (wrap prog, FK_WRAP))
	      else (prog, fk)
	  val (prog, fk) =
	      if fk = FK_WRAP
	      then (say "\n!!Forgot reify!!\n"; (reify prog, FK_REIFY))
	      else (prog, fk)

       in prog
      end (* function optimize *)

    val optimize = phase "FLINT 050 flintopt" optimize

  end (* structure FLINTOpt *)

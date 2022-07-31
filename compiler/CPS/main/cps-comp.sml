(* cps-comp.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Translate FLINT to machine code via CPS and CFG.
 *)

signature CPS_COMP =
  sig

    val compile : {
	    source : string,			(* source file name *)
	    prog : FLINT.prog			(* compilation unit in FLINT IR *)
	  } -> {
	    clusters : Cluster.cluster list,	(* compilation unit in CPS IR *)
	    maxAlloc : LambdaVar.lvar -> int,	(* per-function allocation limits *)
(* TODO:
	    code : CFG.comp_unit,		(* compilation unit in CFG IR *)
 *)
	    data : Word8Vector.vector		(* literal data *)
	  }

  (* translate CPS to CFG *)
    val toCFG : {
            source : string,
            clusters : Cluster.cluster list,
            maxAlloc : CPS.lvar -> int
          } -> CFG.comp_unit

  end

functor CPSCompFn (MachSpec : MACH_SPEC) : CPS_COMP = struct

    structure Convert = Convert(MachSpec)
    structure CPStrans = CPStrans(MachSpec)
    structure CPSopt = CPSopt(MachSpec)
    structure Closure = Closure(MachSpec)
    structure Spill = SpillFn(MachSpec)

    structure CPStoCFG = CPStoCFGFn (MachSpec)

    val toCFG = CPStoCFG.translate

    fun bug s = ErrorMsg.impossible ("CPSComp:" ^ s)
    val say = Control.Print.say

    fun phase x = Stats.doPhase (Stats.makePhase x)

    val convert   = phase "CPS 060 convert" Convert.convert
    val cpstrans  = phase "CPS 065 cpstrans" CPStrans.cpstrans
    val cpsopt    = phase "CPS 070 cpsopt" CPSopt.reduce
    val litsplit  = phase "CPS 075 litsplit" Literals.split
    val newlitsplit = phase "CPS 075 litsplit" NewLiterals.split
    val closure   = phase "CPS 080 closure"  Closure.closeCPS
    val globalfix = phase "CPS 090 globalfix" GlobalFix.globalfix
    val spill     = phase "CPS 100 spill" Spill.spill
    val allocChks = phase "CPS 110 limit" Limit.allocChecks

  (** pretty printing for the CPS code *)
    fun prC s e = if !Control.CG.printit
	  then (
	    say (concat["\n[After ", s, " ...]\n\n"]);
	    PPCps.printcps0 e;
	    say "\n"; e)
	  else e

  (* generate the clusters a limit checks for a list of first-order CPS functions *)
    fun clusters funcs = let
	  val clusters = Cluster.cluster (!Control.CG.useLLVM, funcs)
	  in
	  (* add heap-limit checks etc. *)
	    allocChks clusters
	  end

    fun compile {source, prog} = let
	(* convert to CPS *)
	  val function = convert prog
	  val _ = prC "convert" function
	  val function = (prC "cpstrans" o cpstrans) function
	(* optimize CPS *)
	  val function = cpsopt (function, NONE, false)
	  val _ = prC "cpsopt-code" function
	(* split out heap-allocated literals; litProg is the bytecode *)
(* TODO: switch to newLiterals for all targets *)
	  val (function, data) = if !Control.CG.newLiterals orelse Target.is64
		then newlitsplit function
		else litsplit function
	  val _ = prC "lit-split" function
	(* convert CPS to closure-passing style *)
	  val function = prC "closure" (closure function)
	(* flatten to 1st-order CPS *)
	  val funcs = globalfix function
	(* spill excess live variables *)
	  val funcs = spill funcs
(* TODO: move clustering and limit checks to here *)
	(* optional CFG generation to a file for debugging purposes *)
	  val _ = if !Control.CG.dumpCFG orelse !Control.CG.printCFG
		then let
		(* form clusters *)
		  val clusters = Cluster.cluster (true, funcs)
		(* add heap-limit checks etc. *)
		  val (clusters, maxAlloc) = allocChks clusters
		  val cfg = toCFG {
			  source = source, clusters = clusters, maxAlloc = maxAlloc
			}
		  val pklFile = if source = "stdin"
			then "out.pkl"
			else (case OS.Path.splitBaseExt source
			   of {base, ext=SOME "sml"} =>
				OS.Path.joinBaseExt{base=base, ext=SOME "pkl"}
			    | _ => source ^ ".pkl"
			  (* end case *))
		  in
		    if !Control.CG.printCFG
		      then (
			say "***** CFG *****\n";
			PPCfg.prCompUnit cfg;
			say "***** END CFG *****\n")
		      else ();
		    if !Control.CG.dumpCFG
		      then (
			say (concat["## dump CFG pickle to ", pklFile, "\n"]);
			CFGPickler.toFile (pklFile, cfg))
		      else ()
		  end
		else ()
	(* redo the clusters for now, since we haven't integrated the LLVM code gen yet *)
	  val (clusters, maxAlloc) = clusters funcs
	  in
	    {clusters = clusters, maxAlloc = maxAlloc, data = data}
          end (* compile *)

  end (* functor CPSCompFn *)

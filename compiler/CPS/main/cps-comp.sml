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
    val cluster   = phase "CPS 110 clusters" Cluster.cluster
    val normalize = phase "CPS 120 normalize" NormalizeCluster.transform
    val allocChks = phase "CPS 130 alloc-chks" Limit.allocChecks

  (** pretty printing for the CPS code *)
    fun prC s e = if !Control.CG.printit
          then (
            say (concat["\n[", s, " ...]\n\n"]);
            PPCps.printcps0 e;
            say "\n"; e)
          else e

    (* invariant checking for CPS *)
    fun check s e = (
          if !Control.CG.checkCPS
            then CheckCPS.check (s, e)
            else ();
          prC s e)

    (* printing for the first-order CPS IR *)
    fun prFuncs s funcs = if !Control.CG.printit
          then (
            say (concat["\n[", s, " ...]\n\n"]);
            List.app PPCps.printcps0 funcs;
            say "\n"; funcs)
          else funcs

    (* optionally dump the cluster graph to a file *)
    fun dumpClusters (source, suffix, cl) = if !Control.CG.dumpClusters
          then DumpClusterGraph.dumpWithSuffix {
              file = source, suffix=suffix, clusters=cl
            }
          else ()

    (* optionally print the CFG IR *)
    fun printCFG cfg = if !Control.CG.printCFG
          then (
            say "***** CFG *****\n";
            PPCfg.prCompUnit cfg;
            say "***** END CFG *****\n")
          else ()

    (* optionally dump a pickle of the CFG IR to a file *)
    fun dumpCFG (source, cfg) = if !Control.CG.dumpCFG
          then let
            val pklFile = if source = "stdin"
                  then "out.pkl"
                  else (case OS.Path.splitBaseExt source
                     of {base, ext=SOME "sml"} =>
                          OS.Path.joinBaseExt{base=base, ext=SOME "pkl"}
                      | _ => source ^ ".pkl"
                    (* end case *))
            in
              say (concat["## dump CFG pickle to ", pklFile, "\n"]);
              CFGPickler.toFile (pklFile, cfg)
            end
          else ()

    fun compile {source, prog} = let
	  (* convert to CPS *)
	  val function = check "after convert" (convert prog)
	  val function = check "after cpstrans" (cpstrans function)
	  (* optimize CPS *)
	  val function = check "after cpsopt-code" (cpsopt (function, NONE, false))
	  (* split out heap-allocated literals; litProg is the bytecode *)
(* TODO: switch to newLiterals for all targets *)
	  val (function, data) = if !Control.CG.newLiterals orelse Target.is64
		then newlitsplit function
		else litsplit function
	  val _ = check "after lit-split" function
	  (* convert CPS to closure-passing style *)
	  val function = prC "after closure" (closure function)
	  (* flatten to 1st-order CPS *)
	  val funcs = prFuncs "after globalfix" (globalfix function)
	  (* spill excess live variables *)
	  val funcs = prFuncs "after spill" (spill funcs)
          (* form the clusters *)
          val clusters = cluster funcs
          (* if requested, dump the pre-normalized clusters to a file *)
          val () = dumpClusters (source, "before", clusters)
          (* normalize the clusters to single-entry points *)
          val clusters = normalize clusters
          (* if requested, dump the post-normalized clusters to a file *)
          val () = dumpClusters (source, "after", clusters)
          (* add heap-limit checks etc. *)
          val (clusters, maxAlloc) = allocChks clusters
          (* optionally print and/or dump the CFG IR *)
          val _ = if !Control.CG.dumpCFG orelse !Control.CG.printCFG
                  then let
                    val cfg = toCFG {
                            source = source, clusters = clusters, maxAlloc = maxAlloc
                          }
                    in
                      printCFG cfg;
                      dumpCFG (source, cfg)
                    end
                  else ()
          in
            {clusters = clusters, maxAlloc = maxAlloc, data = data}
          end (* compile *)

  end (* functor CPSCompFn *)

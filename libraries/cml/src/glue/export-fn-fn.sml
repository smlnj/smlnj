(* export-fn-fn.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

functor ExportFnFn (G : OS_GLUE) : sig

    val pollK : unit SMLofNJ.Cont.cont
    val pauseK : unit SMLofNJ.Cont.cont

    datatype ('a, 'b) pair = PAIR of ('a * 'b)

    val wrapForExport :
	  (((string * string list) -> OS.Process.status) * Time.time option)
	    -> (string, string list) pair
	      -> OS.Process.status

  end = struct

    structure S = Scheduler
    structure CU = CleanUp
    structure Cont = SMLofNJ.Cont

    val pollK : unit Cont.cont = Cont.isolate (fn _ => (
	  S.atomicBegin();
	  G.pollOS();
	  S.atomicDispatch()))

    val pauseK : unit Cont.cont = Cont.isolate (fn _ => (
	  S.atomicBegin();
	(* first, we poll the OS to schedule any ready threads *)
	  G.pollOS();
	(* check for ready threads orelse pause *)
	  if (not (Q.isEmpty S.rdyQ1) orelse G.pause())
	    then S.atomicDispatch()
	    else (
	      S.atomicEnd();
	      Cont.throw (! S.shutdownHook) (true, OS.Process.failure))))

    datatype ('a, 'b) pair = PAIR of ('a * 'b)
    type cmdt = (string, string list) pair -> OS.Process.status
    val exportFn' : (string * cmdt) -> unit =
	  Unsafe.CInterface.c_function "SMLNJ-RunT" "exportFn"

    fun wrapForExport (f, tq) (PAIR args) = let
	  val _ = (
		SMLofNJ.Internals.initSigTbl ();
		Thread.reset true;
		G.init();
		S.schedulerHook := pollK;
		S.pauseHook := pauseK)
	  fun initialProc () =
		OS.Process.exit(f args handle _ => OS.Process.failure)
	  val (cleanUp, sts) = Cont.callcc (fn doneK => (
		S.shutdownHook := doneK;
		case tq of (SOME tq) => S.startTimer tq | _ => S.restartTimer();
		CU.clean CU.AtInitFn;
		CML.spawn initialProc;
		CML.exit ()))
	  in
	    CU.clean CU.AtExit;
	    G.shutdown();
	    S.stopTimer();
	    Thread.reset false;
	    sts
	  end

  end

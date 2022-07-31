(* run-cml-fn.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

functor RunCMLFn (G : OS_GLUE) : sig

    val doit : ((unit -> unit) * Time.time option) -> OS.Process.status

    val isRunning : unit -> bool

    val shutdown : OS.Process.status -> 'a

    val exportFn :
	  (string * (string * string list -> OS.Process.status) * Time.time option)
	    -> unit

    include CML_CLEANUP

  end = struct

    structure S = Scheduler
    structure Sig = Signals
    structure CU = CleanUp

    open InitCleanup	(* to force CM to link this module in *)

    structure E = ExportFnFn (G);

    open CU

    val runningFlg = Running.isRunning

    fun isRunning () = !runningFlg

    fun shutdown sts = if !runningFlg
	  then SMLofNJ.Cont.throw (! S.shutdownHook) (true, sts)
	  else raise Fail "CML is not running"

  (* a dummy print function, in case the user's program doesn't reference
   * CML's TextIO structure directly.
   *)
    fun dummyPrint _ = raise Fail "print called without loading CML's TextIO"

    val interruptK : unit SMLofNJ.Cont.cont =
	  SMLofNJ.Cont.isolate (fn _ => shutdown OS.Process.failure)

    fun doit (initialProc, tq) = let
	  val saveIntHandler = Sig.inqHandler Sig.sigINT
	  val savePrintFn = !SMLofNJ.Internals.prHook
	  val _ = (
		if !runningFlg
		  then raise Fail "CML is already running"
		  else runningFlg := true;
		Thread.reset true;
		G.init();
		S.schedulerHook := E.pollK;
		S.pauseHook := E.pauseK)
	  val (cleanUp, sts) = SMLofNJ.Cont.callcc (fn doneK => (
		ignore (
		  Sig.setHandler (Sig.sigINT, Sig.HANDLER(fn _ => interruptK)));
		S.shutdownHook := doneK;
		SMLofNJ.Internals.prHook := dummyPrint;
		case tq of (SOME tq) => S.startTimer tq | _ => S.restartTimer();
		CU.clean CU.AtInit;
		CML.spawn initialProc;
		S.dispatch()))
	  in
	    CU.clean CU.AtShutdown;
	    G.shutdown();
	    S.stopTimer();
	    Thread.reset false;
	    runningFlg := false;
	    SMLofNJ.Internals.prHook := savePrintFn;
	    ignore (Sig.setHandler (Sig.sigINT, saveIntHandler));
	    sts
	  end

    type cmdt = (string, string list) E.pair -> OS.Process.status
    val exportFn' : (string * cmdt) -> unit =
	  Unsafe.CInterface.c_function "SMLNJ-RunT" "exportFn"

    fun exportFn (fileName, main, timeQ) = (
	  if !runningFlg
	    then raise Fail "Cannot exportFn while CML is running"
	    else runningFlg := true;
	  Signals.maskSignals Signals.MASKALL;
	(* run the SML/NJ AtExportFn cleaners to eliminate space-leaks *)
	  SMLofNJ.Internals.CleanUp.clean SMLofNJ.Internals.CleanUp.AtExportFn;
	(* strip out any unecessary stuff from the CML Cleanup state. *)
	  CU.exportFnCleanup ();
	(* unlink the SML print function *)
	  SMLofNJ.Internals.prHook := (fn _ => ());
	(* unlink the perv structure *)
	  Unsafe.pStruct := Unsafe.Object.toObject ();
	(* now export the wrapped main function *)
	  exportFn' (fileName, E.wrapForExport (main, timeQ)))

  end

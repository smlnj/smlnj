(* int-sys.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the interactive system;
 * At link-time (i.e., at bootstrap time) this code builds the boot
 * environments, sets default signal handlers, and then dumps a heap.
 * When the heap image restarts, the system goes interactive.
 *
 * (We do not want to go interactive before dumping the heap because it
 * would mean that environments get loaded unnecessarily.)
 *
 * This code refers directly to structure Compiler, because by the time it
 * gets compiled, CM's conditional compilation facility has already
 * made sure that structure Compiler refers to the visible compiler
 * for the current architecture.
 *)

structure InteractiveSystem : sig end =
  struct

    (* first, we have to step back out of the boot directory... *)
    val bootdir = OS.FileSys.getDir ()
    val _ = OS.FileSys.chDir OS.Path.parentArc

    (* environment initializations *)
    val { heapfile, procCmdLine } =
	BootEnv.init bootdir
	handle e as IO.Io { function, name, cause } =>
	       (TextIO.output (TextIO.stdErr,
			       concat ["IO exception: file = ", name,
				       ", function = ", function,
				       ", cause: ",
				       General.exnMessage cause,
				       "\n"]);
		raise e)
	     | e => (TextIO.output (TextIO.stdErr,
				    concat ["exception raised during init phase: ",
					    General.exnMessage e, "\n"]);
		     raise e)

    (* establish default signal handlers *)
    fun handleINT _ = !Unsafe.topLevelCont
    fun handleTERM _ = OS.Process.exit OS.Process.failure
    fun ifSignal (sigName, handler) = (case Signals.fromString sigName
           of SOME s => (Signals.overrideHandler (s, Signals.HANDLER handler); ())
	    | _ => ()
          (* end case *))

    (* function to set up the handers for INT, TERM, and QUIT signals; because we want signals
     * that are ignored in the parent process to continue to be ignored, we need to delay
     * running this to when we start up the process.
     *)
    fun establishSignalHandlers () = (
	  Signals.overrideHandler (Signals.sigINT, Signals.HANDLER handleINT);
	  Signals.overrideHandler (Signals.sigTERM, Signals.HANDLER handleTERM);
	  ifSignal ("QUIT", handleTERM))

    (* install "use" functionality *)
    val _ = UseHook.useHook := (fn f => ignore(Backend.Interact.use f))

    (* add cleanup code that resets the internal timers and stats
     * when resuming from exportML... *)
    local
      structure I = SMLofNJ.Internals
      structure C = I.CleanUp
      fun reset _ = (I.resetTimers (); Stats.reset ())
    in
    val _ = C.addCleaner ("initialize-timers-and-stats", [C.AtInit], reset)
    end (* local *)

    (* initialize control *)
    val _ = ControlRegistry.init BasicControl.topregistry

    (* launch interactive loop *)
    val _ = let
          val f = SMLofNJ.Cont.callcc (fn k => (
                    Backend.Interact.redump_heap_cont := k;
                    heapfile))
	  in
            Control.Print.say "Generating heap image...\n";
	    if SMLofNJ.exportML f
              then (
                establishSignalHandlers ();
	        print SMLNJVersion.banner;
		print "\n";
		getOpt (procCmdLine, fn () => ()) ();
		Backend.Interact.interact ())
	      else (
	        print "This is...\n";
		print SMLNJVersion.banner;
		print "\n";
		OS.Process.exit OS.Process.success)
	  end

  end

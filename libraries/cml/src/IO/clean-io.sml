(* clean-io.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module keeps track of open I/O streams, and handles the proper
 * cleaning of them.  It is a modified version of the SML/NJ module
 * of the same name (in boot/IO/clean-io.sml).  Unlike the SML/NJ version,
 * we only do cleanup at shutdown/exit time (we do not try to support the
 * persistence of CML streams across invocations of RunCML.doit), and only
 * require a single clean-up function (this flushes the standard streams,
 * and closes all others).  These operations should only be called while
 * CML is running, since they use synchronization primitives.
 *
 * NOTE: there is currently a problem with removing the cleaners for streams
 * that get dropped by the application, but the system limit on open files
 * will limit this.
 *
 *)

structure CleanIO :> sig

    type tag

    val osInitHook : (unit -> unit) ref
	(* this function gets invoked as the first action during the IO
	 * initialization.  It is meant to support any OS specific initialization
	 * that might be necessary.
	 *)

    val stdStrmHook : (unit -> unit) ref
	(* this function is defined in TextIOFn, and is called after the osHook.
	 * It is used to rebuild the standard streams.
	 *)

    val addCleaner : (unit -> unit) -> tag

    val rebindCleaner : (tag * (unit -> unit)) -> unit

    val removeCleaner : tag -> unit

  (* for linking the master IO cleaner function into the list of cleanup hooks *)
    val ioCleaner : (string * CleanUp.when list * (CleanUp.when -> unit))

  end = struct

    structure SV = SyncVar

    type tag = unit ref

    type cleaner = {
	tag : tag,		(* unique ID for this cleaner *)
	close : unit -> unit	(* called AtExit and AtShutdown *)
      }

    val osInitHook = ref(fn () => ())
    val stdStrmHook = ref(fn () => ())

    val cleaners = SV.mVarInit ([] : cleaner list)

    fun addCleaner close = let
	  val tag = ref()
	  val cleanerRec = {tag = tag, close = close}
	  in
	    SV.mPut (cleaners, cleanerRec :: SV.mTake cleaners);
	    tag
	  end

    fun getTag ({tag, ...} : cleaner) = tag

    fun rebindCleaner (t, close) = let
	  fun f [] = raise Fail "rebindCleaner: tag not found"
	    | f (x :: r) = let
		val t' = getTag x
		in
		  if (t' = t)
		    then {tag=t, close=close} :: r
		    else x :: f r
		end
	  in
	    SV.mPut (cleaners, f (SV.mTake cleaners))
	  end

    fun removeCleaner t = let
	  fun f [] = []		(* should we raise an exception here? *)
	    | f (x :: r) = if (getTag x = t) then r else x :: f r
	  in
	    SV.mPut (cleaners, f (SV.mTake cleaners))
	  end

    fun doClean () = let
	  fun doit [] = ()
	    | doit ({tag, close}::r) = ((close()) handle _ => (); doit r)
	  in
	    doit (SV.mGet cleaners)
	  end

    structure C = CleanUp

    fun cleanUp (C.AtShutdown | C.AtExit) = doClean ()
      | cleanUp (C.AtInit | C.AtInitFn) = (
	  (!osInitHook)();
	  (!stdStrmHook)())

  (* for linking the master IO cleaner function into the list of cleanup hooks *)
    val ioCleaner = ("IO", C.atAll, cleanUp)

  end (* CleanIO *)


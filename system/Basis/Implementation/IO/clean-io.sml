(* clean-io.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module keeps track of open I/O streams, and handles the proper
 * cleaning of them.
 *
 * NOTE: there is currently a problem with removing the cleaners for streams
 * that get dropped by the application, but the system limit on open files
 * will limit this.
 *)

structure CleanIO :> sig

    type tag

    val osInitHook : (unit -> unit) ref
	(* this function gets invoked as the first action during the IO
	 * initialization.  It is meant to support any OS specific initialization
	 * that might be necessary.
	 *)

    val stdStrmHook : (unit -> unit) ref
	(* this function is defined in TextIOFn, and is called after the osHook,
	 * but before the per-stream init functions.  It is used to rebuild the
	 * standard streams.
	 *)

    val addCleaner : {
	    init : unit -> unit,	(* called AtInit and AtInitFn *)
	    flush : unit -> unit,	(* called AtExportML *)
	    close : unit -> unit	(* called AtExit and AtExportFn *)
	  } -> tag

    val rebindCleaner : (tag * {
	    init : unit -> unit,	(* called AtInit and AtInitFn *)
	    flush : unit -> unit,	(* called AtExportML *)
	    close : unit -> unit	(* called AtExit and AtExportFn *)
	  })-> unit

    val removeCleaner : tag -> unit

  end = struct

    type tag = unit ref

    type cleaner = {
	tag : tag,		(* unique ID for this cleaner *)
	init : unit -> unit,	(* called AtInit and AtInitFn *)
	flush : unit -> unit,	(* called AtExportML *)
	close : unit -> unit	(* called AtExit and AtExportFn *)
      }

    val osInitHook = ref(fn () => ())
    val stdStrmHook = ref(fn () => ())

    val cleaners = ref ([] : cleaner list)

    fun addCleaner {init, flush, close} = let
	  val tag = ref()
	  val cleanerRec = {tag = tag, init = init, flush = flush, close = close}
	  in
	    cleaners := cleanerRec :: !cleaners;
	    tag
	  end

    fun getTag ({tag, ...} : cleaner) = tag

    fun rebindCleaner (t, {init, flush, close}) = let
	  fun f [] = raise Fail "rebindCleaner: tag not found"
	    | f (x :: r) = let
		val t' = getTag x
		in
		  if (t' = t)
		    then {tag=t, init=init, flush=flush, close=close} :: r
		    else x :: f r
		end
	  in
	    cleaners := f (! cleaners)
	  end

    fun removeCleaner t = let
	  fun f [] = []		(* should we raise an exception here? *)
	    | f (x :: r) = if (getTag x = t) then r else x :: f r
	  in
	    cleaners := f (! cleaners)
	  end

    fun doClean selFn = let
	  fun doit [] = ()
	    | doit (x::r) = (((selFn x)()) handle _ => (); doit r)
	  in
	     doit (! cleaners)
	  end

    structure C = CleanUp

    fun cleanUp (C.AtExportML) = doClean #flush
      | cleanUp (C.AtExportFn | C.AtExit) = doClean #close
      | cleanUp (C.AtInit | C.AtInitFn) = (
	  (!osInitHook)();
	  (!stdStrmHook)();
	  doClean #init)

    val _ = C.addCleaner ("IO", C.atAll, cleanUp)

  end (* CleanIO *)

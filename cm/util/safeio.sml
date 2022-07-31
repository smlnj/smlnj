(*
 * Guarding IO against file descriptor leakage...
 *
 *   Copyright (c) 1998 by Lucent Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature SAFEIO = sig

    (* the cleanup function is being told whether it is called because
     * of an interrupt *)
    val perform :
	{ openIt : unit -> 'a,
	  closeIt : 'a -> unit,
	  work : 'a -> 'b,
	  cleanup : bool -> unit } -> 'b
end

structure SafeIO :> SAFEIO = struct

    structure S = Signals

    fun perform { openIt, closeIt, work, cleanup } = let
	val oh = S.inqHandler S.sigINT
	val intMask = S.MASK [S.sigINT]
	val _ = S.maskSignals intMask
	val s = openIt ()
	    handle e => (S.unmaskSignals intMask;
			 cleanup false;
			 raise e)
	fun reset () = (closeIt s; ignore (S.setHandler (S.sigINT, oh)))
	fun handler arg =
	    (reset ();
	     cleanup true;
	     case oh of
		 S.HANDLER h => h arg
	       | _ => OS.Process.exit OS.Process.failure)
    in
	(S.overrideHandler (S.sigINT, S.HANDLER handler);
	 S.unmaskSignals intMask;
	 (work s handle e => (reset (); cleanup false; raise e))
	 before reset ())
    end
end

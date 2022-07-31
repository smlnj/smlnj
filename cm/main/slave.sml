(*
 * This module implements the slave-side of the master-slave protocol used
 * for parallel make.
 *
 *   Copyright (c) 1999 by Lucent Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
local
    structure DG = DependencyGraph
in
  structure Slave = struct

    fun slave { penv, my_archos, parse, sbtrav, make } = let

	val dbr = ref BtNames.dirbaseDefault

	fun shutdown () = OS.Process.exit OS.Process.success
	fun say_ok () = Say.say ["SLAVE: ok\n"]
	fun say_error () = Say.say ["SLAVE: error\n"]
	fun say_pong () = Say.say ["SLAVE: pong\n"]

	fun path (s, penv) = SrcPath.decode penv s
		  
	fun chDir d =
	    OS.FileSys.chDir (SrcPath.osstring (path (d, penv)))

	fun getLineTokens s =
	    Option.map (String.tokens Char.isSpace) (TextIO.inputLine s)

	fun waitForStart () = let
	    fun loop () =
		case getLineTokens TextIO.stdIn of
		    NONE => shutdown ()
		  | SOME ["cd", d] => (chDir d; say_ok (); waitForStart ())
		  | SOME ["cm", archos, f] => do_cm (archos, f)
		  | SOME ["cmb", db, archos, f] =>
		      (dbr := db; do_cmb (archos, f))
		  | SOME ["reset_cmb", archos] => reset_cmb archos
		  | SOME ["ping"] => (say_pong (); waitForStart ())
		  | SOME ["finish"] => (say_ok (); waitForStart ())
		  | SOME ["shutdown"] => shutdown ()
		  | _ => (say_error (); waitForStart ())
	in
	    loop () handle _ => (say_error (); waitForStart ())
	end

	and reset_cmb archos = let
	    val slave = CMBSlave.slave make
	in
	    ignore (slave archos NONE);	(* causes reset *)
	    say_ok ();
	    waitForStart ()
	end

	and do_cmb (archos, f) = let
	    val slave = CMBSlave.slave make
	in
	    case slave archos (SOME (!dbr, f)) of
		NONE => (say_error (); waitForStart ())
	      | SOME (g, trav, cmb_penv) => let
		    val _ = say_ok ()
		    val index = Reachable.snodeMap g
		in
		    workLoop (index, trav, cmb_penv)
		end
	end handle _ => (say_error (); waitForStart ())

	and do_cm (archos, f) =
	    if archos <> my_archos then (say_error (); waitForStart ())
	    else let
		val p = path (f, penv)
	    in
		case parse p of
		    NONE => (say_error (); waitForStart ())
		  | SOME (g, gp) => let
			val _ = say_ok ()
			val index = Reachable.snodeMap g
			val trav = sbtrav ()
			fun trav' sbn = isSome (trav sbn gp)
		    in
			workLoop (index, trav', penv)
		    end
	    end handle _ => (say_error (); waitForStart ())

	and workLoop (index, trav, penv) = let
	    fun loop () =
		case getLineTokens TextIO.stdIn of
		    NONE => shutdown ()
		  | SOME ["cd", d] => (chDir d; say_ok (); loop ())
		  | SOME ["compile", f] =>
		      let val p = path (f, penv)
		      in
			  case SrcPathMap.find (index, p) of
			      NONE => (say_error (); loop ())
			    | SOME sn => let
				  val sbn = DG.SB_SNODE sn
			      in
				  if trav sbn then (say_ok (); loop ())
				  else (say_error (); loop ())
			      end
		      end
		  | SOME ["cm", archos, f] => do_cm (archos, f)
		  | SOME ["cmb", db, archos, f] =>
		      (dbr := db; do_cmb (archos, f))
		  | SOME ["reset_cmb", archos] => reset_cmb archos
		  | SOME ["finish"] => (say_ok (); waitForStart ())
		  | SOME ["ping"] => (say_pong (); loop ())
		  | SOME ["shutdown"] => shutdown ()
		  | _ => (say_error (); loop ())
	in
	    loop () handle _ => (say_error (); workLoop (index, trav, penv))
	end
    in
	ignore (Signals.setHandler (Signals.sigINT, Signals.IGNORE));
	say_ok ();		(* announce readiness *)
	waitForStart () handle _ => ();
	    OS.Process.exit OS.Process.failure
    end
  end
end

(* win-install.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Win32-specific invocation of LibInstall.proc (see libinstall.sml).
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure WinInstall : sig end =
  struct

    structure I = GenericInstall

    fun proc () = let
	  val home = valOf (OS.Process.getEnv "SMLNJ_HOME")
	  fun copy { from, to } = let
		val ins = TextIO.openIn from
		val outs = TextIO.openOut to
		fun loop NONE = (TextIO.closeIn ins; TextIO.closeOut outs)
		  | loop (SOME l) = (TextIO.output (outs, l); next ())
		and next () = loop (TextIO.inputLine ins)
		in
		  next ()
		end
	  in
	    I.proc {
		smlnjroot = home,
		installdir = home,
	 	configcmd = "config.bat " ^ home,
		buildcmd = "build.bat",
		unpack = NONE,
		instcmd = fn target => copy {
		    from = concat [home, "\\config\\", target, ".bat"],
		    to = concat [home, "\\bin\\", target, ".bat"]
		  }
	      }
	  end

  (* doit right away *)
    val _ = proc ()

  end

(* nix-install.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Unix-specific invocation of LibInstall.proc (see libinstall.sml).
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)

structure UnixInstall : sig end =
  struct

    structure I = GenericInstall

    fun proc () = let
	  val home = valOf (OS.Process.getEnv "ROOT")
	  val installdir = getOpt (OS.Process.getEnv "INSTALLDIR", home)
	  val configdir = getOpt (OS.Process.getEnv "CONFIGDIR",
				  OS.Path.concat (home, "config"))
	  val unpackcmd = OS.Path.concat (configdir, "unpack")
	  fun unpack modules = let
	        val cmdline =
		      concat ("\"" :: unpackcmd :: "\" \"" :: home :: "\"" ::
			      foldl (fn (f, l) => " " :: f :: l) [] modules)
	        in
		  OS.Process.system cmdline = OS.Process.success
	        end
	  val bindir = getOpt (OS.Process.getEnv "BINDIR",
			       OS.Path.concat (installdir, "bin"))
	  fun bincmd cmd = OS.Path.concat (bindir, cmd)
	  val runsml = ".run-sml"		(* don't prepend bindir! *)
	(* the config and build commands are standard scripts that take a size argument *)
	  val (configcmd, buildcmd) = let
		val sz = Int.toString(SMLofNJ.SysInfo.getArchSize())
		val configcmd = "./config.sh -" ^ sz
		val buildcmd = "CM_LOCAL_PATHCONFIG=/dev/null ./build.sh -" ^ sz
		in
		  (configcmd, buildcmd)
		end
	  in
	    I.proc {
		smlnjroot = home,
		installdir = installdir,
	 	configcmd = configcmd,
		buildcmd = buildcmd,
		unpack = SOME unpack,
		instcmd = fn target => let
		  val new = bincmd target
		  in
		    if OS.FileSys.access (new, [])
		      then ()
		      else Posix.FileSys.symlink { old = runsml, new = new }
		  end
	      }
	  end

    val _ = proc ()

  end

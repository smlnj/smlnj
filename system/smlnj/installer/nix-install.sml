(* nix-install.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Unix-specific invocation of LibInstall.proc (see libinstall.sml).
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)

structure UnixInstall : sig end =
  struct

    structure I = GenericInstall

    fun getEnv var = (case OS.Process.getEnv var
           of SOME v => v
            | NONE => raise Fail("No definition for " ^ var)
          (* end case *))

    fun getEnvOpt (var, dflt) = (case OS.Process.getEnv var
           of SOME v => v
            | NONE => dflt
          (* end case *))

    fun proc () = let
	  val home = getEnv "SMLNJ_ROOT"
	  val installdir = getEnvOpt ("INSTALLDIR", home)
	  val configdir = getEnvOpt ("CONFIGDIR", OS.Path.concat (home, "config"))
	  val bindir = getEnvOpt ("BINDIR", OS.Path.concat (installdir, "bin"))
	  fun bincmd cmd = OS.Path.concat (bindir, cmd)
	  val runsml = ".run-sml"		(* don't prepend bindir! *)
	(* the config and build commands *)
	  val configcmd = "./config.sh"
	  val buildcmd = "CM_LOCAL_PATHCONFIG=/dev/null ./build.sh"
	  in
	    I.proc {
		smlnjroot = home,
		installdir = installdir,
	 	configcmd = configcmd,
		buildcmd = buildcmd,
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

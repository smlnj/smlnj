(* library-install.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Installer routine for additional libraries.
 *
 * author: Matthias Blume
 *)

structure LibraryInstall : sig end =
  struct

    structure U = InstallerUtil
    structure P = OS.Path
    structure F = OS.FileSys
    structure SI = SMLofNJ.SysInfo

    val say = U.say and fail = U.fail
    val pconc = U.pconcat

    fun usage () =
	say ["sml -m $smlnj/library-install.cm src libdir tgt\n",
	     "\tsrc: .cm-file for library (path name in Unix syntax)\n",
	     "\tlibdir: library directory (path name in native syntax)\n",
	     "\ttgt: .cm-file for destination (Unix-syntax, ",
	     "relative to libdir)\n"]

    (* figure out who and what we are *)
    val { arch_oskind, ... } = U.platformInfo ()

    fun add_anchor (f, a) =
	let val s = TextIO.openAppend f
	in TextIO.output (s, concat [a, " ", a, "\n"]);
	   TextIO.closeOut s
	end

    (* src is still Unix-style, tgt is native: *)
    fun install (usrc, src, libdir, rtgt) =
	(if CM.stabilize false usrc then
	     case #arcs (P.fromString rtgt) of
		 anchor :: _ =>
		   let val pathconfig =
			   case OS.Process.getEnv "CM_PATHCONFIG" of
			       SOME pc => pc
			     | NONE => P.concat (libdir, "pathconfig")
		       val srcdir = P.dir src
		       val srcfile = P.file src
		       val s_src = pconc [srcdir, CM.cm_dir_arc,
					  arch_oskind, srcfile]
		       val tgt = P.concat (libdir, rtgt)
		       val { dir = tgtdir, file = tgtfile } = P.splitDirFile tgt
		       val s_tgtdir = pconc [tgtdir, CM.cm_dir_arc, arch_oskind]
		       val s_tgt = P.concat (s_tgtdir, tgtfile)
		   in U.mkdir s_tgtdir;
		      U.rename { old = s_src, new = s_tgt };
		      add_anchor (pathconfig, anchor);
		      (* TODO: uniqconfig *)
		      OS.Process.success
		   end
	       | [] => (usage (); OS.Process.failure)
	 else OS.Process.failure)
	handle exn => fail ["uncaught exception: ",
			    General.exnMessage exn, "\n"]

    fun doit [src, libdir, tgt] =
	  install (src, P.fromUnixPath src, libdir, P.fromUnixPath tgt)
      | doit [src, libdir] =
	  let val nsrc = P.fromUnixPath src
	      val f = P.file nsrc
	  in install (src, nsrc, libdir, P.concat (f, f))
	  end
      | doit _ = (usage (); OS.Process.failure)

    (* run the installer *)
    val _ = OS.Process.exit (doit (CommandLine.arguments ()))

  end

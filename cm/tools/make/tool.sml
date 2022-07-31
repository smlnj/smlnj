(*
 * A tool for running "make" from CM.
 *
 *   (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure MakeTool = struct
    local
	open Tools

	structure C = StdConfig.MakeTool

	val tool = "Make-Command"	(* the name of this tool *)
	val class = "make"		(* the name of the class *)
	val kw_class = "class"
	val kw_options = "options"

	fun err m = raise ToolError { tool = tool, msg = m }

	fun rule { spec, context, native2pathmaker, defaultClassOf, sysinfo } = let
	    val { name = str, mkpath, opts = too, ... } : spec = spec
	    val (tclass, topts, mopts) =
		case too of
		    NONE => (NONE, NONE, [])
		  | SOME options => let
			val { matches, restoptions } =
			    parseOptions
				{ tool = tool,
				  keywords = [kw_class, kw_options],
				  options = options }
		    in
			(case matches kw_class of
			     SOME [STRING { name, ... }] => SOME name
			   | NONE => NONE
			   | _ => err "invalid class specification",
			 matches kw_options,
			 restoptions)
		    end
	    val p = srcpath (mkpath ())
	    val tname = nativeSpec p	(* for passing to "make" *)
	    val partial_expansion =
		(* The "make" class is odd in that it has only a target
		 * but no sources.  We use "str" and "mkpath", that is,
		 * we retain the distinction between native and standard
		 * paths instead of going native in all cases. *)
		({ smlfiles = [], cmfiles = [], sources = [] },
		 [{ name = str, mkpath = mkpath,
		    class = tclass, opts = topts, derived = true }])
	    fun runcmd () = let
		val cmdname = mkCmdName (#get C.command ())
		val bindir =
		    if #get C.pass_bindir () then
			case #get (CM.Anchor.anchor "bindir") () of
			    NONE => ""
			  | SOME d => " SMLNJ_BINDIR=" ^ d
		    else ""
		val tname =
		    if OS.Path.isAbsolute tname then
			OS.Path.mkRelative
			    { path = tname,
			      relativeTo = OS.FileSys.getDir () }
		    else tname
		val cmd = concat (cmdname :: foldr (fn (x, l) => " " :: x :: l)
				                   [bindir, " ", tname] mopts)
	    in
		vsay ["[", cmd, "]\n"];
		if OS.Process.system cmd = OS.Process.success then ()
		else err cmd
	    end
	    fun rulefn () = (runcmd (); partial_expansion)
	in
	    context rulefn
	end
    in
        val _ = registerClass (class, rule)
	structure Control = C
    end
end

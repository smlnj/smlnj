(*
 * Functor implementing the public interface to CM's tools mechanism.
 *   (This functor must be instantiated after the rest of CM is
 *    already in place because it uses load_plugin.)
 *
 *   (C) 2006 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
functor ToolsFn (val load_plugin' : SrcPath.file -> bool
		 val penv: SrcPath.env) : TOOLS = struct

    open PrivateTools

    val say = Say.say
    val vsay = Say.vsay

    fun boolcontrol (name, doc, default) =
	StdConfig.new (ControlUtil.Cvt.bool, name, doc, default)
    fun stringcontrol (name, doc, default) =
	StdConfig.new (ControlUtil.Cvt.string, name, doc, default)

    val tolerate_tool_failures =
	boolcontrol ("tolerate-tool-failures",
		     "let CM continue if a tool fails \
		     \as long as target files exist",
		     false)

    val force_tools =
	boolcontrol ("force-tools",
		     "force execution of shell-command tools",
		     false)

    fun mkCmdName cmdStdPath =
	(* The result of this function should not be cached. Otherwise
	 * a later addition or change of an anchor will go unnoticed. *)
	case SrcPath.get_anchor (penv, cmdStdPath) of
	    NONE => cmdStdPath
	  | SOME p => OS.Path.joinDirFile { dir = p, file = cmdStdPath }

    fun registerStdShellCmdTool args = let
	val { tool, class, cmdStdPath,
	      extensionStyle, template, dflopts } = args
	val template = getOpt (template, "\"%c\" %u \"%s\"")
	fun err m = raise ToolError { tool = tool, msg = m }
	fun rule { spec, context, native2pathmaker, defaultClassOf, sysinfo } = let
	    val { name, mkpath, opts = oto, derived, ... } : spec = spec
	    val opts = getOpt (oto, dflopts)
	    val sol = let		(* only use STRING options for %o *)
		fun so (SUBOPTS _) = NONE
		  | so (STRING s) = SOME (nativeSpec (srcpath (#mkpath s ())))
	    in
		List.mapPartial so opts
	    end
	    val p = srcpath (mkpath ())
	    val nativename = nativeSpec p
	    val tfiles = extend extensionStyle (nativename, oto)
	    val partial_expansion =
		({ smlfiles = [], cmfiles = [],
		   sources = [(p, { class = class, derived = derived })] },
		 map (fn (f, co, too) => { name = f,
					   mkpath = native2pathmaker f,
					   class = co,
					   opts = too,
					   derived = true })
		     tfiles)
	    fun runcmd targets = let
		val (csp, shelloptions) = cmdStdPath ()
		val cmdname = mkCmdName csp
		val cmd =
		    Subst.substitute
			[{ prefix = "%",
			   substitutions =
			     [Subst.subfor "%c" cmdname,
			      Subst.subfor "%s" nativename,
			      Subst.subfor "%%" "%",
			      Subst.subnsel (1, #"o", fn x => x, " ") sol,
			      Subst.subnsel (1, #"t", #1, " ") tfiles,
			      Subst.subnsel (1, #"u", fn x => x, " ")
					    shelloptions] }]
			template
	    in Say.vsay ["[", cmd, "]\n"];
	       if OS.Process.system cmd = OS.Process.success then ()
	       else if #get tolerate_tool_failures ()
		       andalso targetsExist targets then
		   Say.say ["[*** WARNING: \"", cmd, "\" failed ***]\n\
			    \[*** using potentially outdated targets ***]\n"]
	       else err cmd
	    end
	    fun rulefn () =
		let val targets = map #1 tfiles
		in if #get force_tools ()
		      orelse outdated tool (targets, nativename)
		   then runcmd targets
		   else ();
		   partial_expansion
		end
	in context rulefn
	end
    in registerClass (class, rule)
    end

    local
	val toolclass = "tool"
	val suffixclass = "suffix"
	val empty_expansion =
	    ({ cmfiles = [], smlfiles = [], sources = [] }, [])
	fun toolrule { spec, context, native2pathmaker, defaultClassOf, sysinfo } =
	    let val { name, mkpath, opts, ... } : spec = spec
		fun err m = raise ToolError { tool = toolclass, msg = m }
		val p = srcpath (mkpath ())
	    in
		case opts of
		    NONE => if withPlugin p (fn () => load_plugin' p) then
				empty_expansion
			    else err "tool registration failed"
		  | SOME _ => err "no tool options are recognized"
	    end
	fun suffixrule { spec, context, native2pathmaker, defaultClassOf, sysinfo } =
	    let val { name = s, opts, ... } : spec = spec
		fun err m = raise ToolError { tool = suffixclass, msg = m }
		fun reg c =
		    (registerClassifier (stdSfxClassifier { sfx = s,
							    class = c });
		     empty_expansion)
	    in
		case opts of
		    SOME [STRING c] => reg (#name c)
		  | SOME [SUBOPTS { name = "class", opts = [STRING c] }] =>
		    reg (#name c)
		  | _ => err "invalid options"
	    end
    in
        val _ = registerClass (toolclass, toolrule)
	val _ = registerClass (suffixclass, suffixrule)
    end
end

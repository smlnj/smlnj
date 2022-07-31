(*
 * A tool for running arbitrary shell commands from CM.
 *
 *   (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure ShellTool = struct
    local
	open Tools

	val tool = "Shell-Command"
	val class = "shell"
	val kw_class = "class"
	val kw_options = "options"
	val kw_source = "source"
	val kw_target = "target"
	val kwl = [kw_class, kw_options, kw_source, kw_target]

	fun err msg = raise ToolError { tool = tool, msg = msg }
	fun badspec kw = err (concat ["bad specification for keyword `",
				      kw, "'"])

	fun rule { spec, context, native2pathmaker, defaultClassOf, sysinfo } = let
	    val { name = str, mkpath, opts = too, derived, ... } : spec = spec
	    val { symval, archos } = sysinfo
	    val specpath = srcpath (mkpath ())
	    val specname = nativeSpec specpath
	    val (sname, tname, tclass, topts, cmdline) =
		case too of
		    NONE => err "missing options"
		  | SOME ol => let
			val { matches, restoptions } =
			    parseOptions
			       { tool = tool, keywords = kwl, options = ol }
			fun fmatch kw =
			    case matches kw of
				NONE => NONE
			      | SOME [STRING { name, mkpath }] =>
				  SOME (nativeSpec (srcpath (mkpath ())))
			      | _ => badspec kw
			val tclass =
			    case matches kw_class of
				NONE => NONE
			      | SOME [STRING { name, ... }] => SOME name
			      | _ => badspec kw_class
			val topts = matches kw_options
			fun return (sname, tname) = let
			    fun subst s = let
				fun otherpercents ss =
				    if Substring.size ss = 2 then
					SOME (String.str (Substring.sub (ss, 1)))
				    else NONE
				fun sv ss =
				    SOME (case symval (Substring.string ss) of
					      NONE => ""
					    | SOME i => Int.toString i)
				fun qsv ss = let
				    val sslen = Substring.size ss
				    fun doqsv (var, value) =
					case symval (Substring.string var) of
					    NONE => SOME ""
					  | SOME _ => SOME (Substring.string value)
				    fun findcolon i =
					if i >= sslen then doqsv (ss, ss)
					else if Substring.sub (ss, i) = #":" then
					    doqsv (Substring.slice (ss, 0, SOME i),
						   Substring.slice (ss, i+1,
								    SOME (sslen-i-1)))
					else findcolon (i+1)
				in
				    findcolon 0
				end
			    in
				Subst.substitute [{ prefix = "$?(",
						    substitutions =
						    [Subst.submap (3, #")") qsv] },
						  { prefix = "$(",
						    substitutions =
						    [Subst.submap (2, #")") sv] },
						  { prefix = "%",
						    substitutions =
						    [Subst.subfor "%s" sname,
						     Subst.subfor "%t" tname,
						     Subst.subfor "%a" archos,
						     otherpercents] }]
						 s
			    end
			    fun ad (x, l) = " " :: subst x :: l
			    val cmdline =
				case restoptions of
				    [] => err "no command line specified"
				  | h :: t => concat (subst h :: foldr ad [] t)
			in
			    (sname, tname, tclass, topts, cmdline)
			end
		    in
			case (fmatch kw_source, fmatch kw_target) of
			    (NONE, NONE) => err
			      "either `source:' or `target:' must be specified"
			  | (SOME src, NONE) => return (src, specname)
			  | (NONE, SOME tgt) => return (specname, tgt)
			  | (SOME _, SOME _) => err
			 "only one of `source:' and `target:' can be specified"
		    end
	    val spath = srcpath (native2pathmaker sname ())
	    val partial_expansion =
		({ smlfiles = [], cmfiles = [],
		   (* If str was the target, then "derived" does not really
		    * make much sense.  I guess the best thing is to get
		    * rid of the "source:" option. FIXME!! *)
		   sources = [(spath, { class = class, derived = derived })] },
		 [{ name = tname, mkpath = native2pathmaker tname,
		    class = tclass, opts = topts, derived = true }])
	    fun runcmd () =
		(vsay ["[", cmdline, "]\n"];
		 if OS.Process.system cmdline = OS.Process.success then ()
		 else err cmdline)
	    fun rulefn () =
		(if outdated tool ([tname], sname) then runcmd ()
		 else ();
		 partial_expansion)
	in
	    context rulefn
	end
    in
        val _ = Tools.registerClass (class, rule)
    end
end

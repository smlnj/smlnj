(*
 * A tool for processing filesystem directories that contain other source
 * files.
 *
 *   (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure DirTool = struct
    local
	open Tools

	val tool = "Dir"
	val class = DirToolClassify.class

	fun err m = raise ToolError { tool = tool, msg = m }

	fun rule { spec, context, native2pathmaker, defaultClassOf, sysinfo } = let
	    val { name, mkpath, opts = too, ... } : spec = spec
	    val pre_d = mkpath ()
	    (* We are making up specs for the members of the directory
	     * by gluing the the member name to the directory name.
	     * Since the result must be valid in the directories' context,
	     * we use the name of the directory relative to that context. *)
	    val spec_d = nativePreSpec pre_d
	    (* When no options are given, we read the physical directory
	     * and look for ML files... *)
	    fun noOpts () = let
		fun getfiles s = let
		    fun loop l =
			case OS.FileSys.readDir s of
			    NONE => rev l
			  | SOME f => let
				val df = OS.Path.joinDirFile
					     { dir = spec_d, file = f }
				fun mkpath () = augment pre_d [f]
				val dfs = { name = df, mkpath = mkpath }
			    in
				case defaultClassOf dfs of
				    SOME ("sml") => loop (dfs :: l)
				  | _ => loop l
			    end
		in
		    (loop [] before OS.FileSys.closeDir s)
		    handle e => (OS.FileSys.closeDir s; raise e)
		end
		val fl = getfiles (OS.FileSys.openDir spec_d)
		fun toSpec { name, mkpath } =
		    { name = name, mkpath = mkpath,
		      class = SOME "sml", opts = NONE, derived = false }
	    in
		({ smlfiles = [], cmfiles = [], sources = [] },
		 map toSpec fl)
	    end
	    (* When options are given, we take precisely the files specified
	     * there. *)
	    fun procOpts ol = let
		fun oneFile ({ name, mkpath }, co, oo) = let
		    val f = nativeSpec (srcpath (mkpath ()))
		in
		    case OS.Path.fromString f of
			{ isAbs = false, vol = "", arcs } =>
			{ name = OS.Path.concat (spec_d, f),
			  mkpath = fn () => augment pre_d arcs,
			  class = co,
			  opts = oo,
			  derived = false }
		      | _ => 
			err ("invalid directory entry: " ^ f)
		end
		fun oneOpt (STRING fns) = oneFile (fns, NONE, NONE)
		  | oneOpt (SUBOPTS { name = "member", opts }) =
		    (case opts of
			 [STRING fns] => oneFile (fns, NONE, NONE)
		       | [STRING fns, SUBOPTS { name = "class",
						opts = [STRING c] }] =>
			 oneFile (fns, SOME (#name c), NONE)
		       | [STRING fns, SUBOPTS { name = "options", opts }] =>
			 oneFile (fns, NONE, SOME opts)
		       | [STRING fns,
			  SUBOPTS { name = "class",
				    opts = [STRING c] },
			  SUBOPTS { name = "options", opts }] =>
			 oneFile (fns, SOME (#name c), SOME opts)
		       | _ => err "illegal member specification")
		  | oneOpt (SUBOPTS so) = err ("illegal option: " ^ #name so)
	    in
		({ smlfiles = [], cmfiles = [], sources = [] },
		 map oneOpt ol)
	    end
	in
	    case too of
		SOME ol => procOpts ol
	      | NONE =>
		(* We actually open the directory and read it, so we must
		 * switch to the right context... *)
		context noOpts
	end
    in
        val _ = registerClass (class, rule)
    end
end

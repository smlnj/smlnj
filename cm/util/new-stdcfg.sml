(*
 * CM parameters that are configurable via shell-environment variables.
 *
 *   Copyright (c) 1999 by Lucent Technologies, Bell Laboratories.
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure StdConfig =
struct
local

  val lib_pathconfig =
      OS.Path.toString { isAbs = false, vol = "",
			 arcs = ["lib", "pathconfig"] }
  val usr_lib_smlnj_pathconfig =
      OS.Path.toString { isAbs = true, vol = "",
			 arcs = ["usr", "lib", "smlnj-pathconfig"] }

  val {newBool, newInt, newString, newStrings} = MakeControls.make {name = "CM", priority = [1]}

in

  val verbose = newBool ("verbose", "CM chattiness", true)
  val debug = newBool ("debug", "CM debug mode", false)
  val keep_going = newBool ("keep-going", "whether CM presses on in face of errors", false)
  val warn_obsolete = newBool ("warn-obsolete", "whether CM accepts old-style syntax", true)
  val conserve_memory = newBool ("conserve-memory", "CM memory stinginess", false)
  val generate_index = newBool ("generate-index", "whether CM generates library indices", false)
  val parse_caching = newInt ("parse-caching", "limit on parse trees cached", 100)

  (* two flags used in ToolsFn (cm/tools/main/tools-fn.sml) *)
  val tolerate_tool_failures = newBool ("tolerate-tool-failures",
				       "continue on tool failure if target files exist",
				       false)

  val force_tools = newBool ("force-tools",
			     "force execution of shell-command tools",
			     false)

  val local_pathconfig =
      ref (fn () => Option.map (fn h => OS.Path.concat (h, ".smlnj-pathconfig"))
			       (OS.Process.getEnv "HOME"))

  val pathcfgspec =
      ref (fn () =>
	      getOpt (Option.map (fn h => OS.Path.concat (h, lib_pathconfig))
				 (OS.Process.getEnv "SMLNJ_HOME"),
		      usr_lib_smlnj_pathconfig))

  (* controls for make tool *)
  structure MakeTool =
  struct
    val command = newString ("command", "the shell-command", "make")
    val pass_bindir =
	  newBool ("smlnj-bindir", "whether to pass SMLNJ_BINDIR to command", true)
  end (* structure MakeTool *)

end (* local *)
end (* structure StdConfig *)

(* asdlgen-tool.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ASDLGenTool : sig end =
  struct

  (* given a file `foo.asdl`, we generate four files:
   *
   *	foo.sml			-- contains the generated type definitions
   *    foo-pickle.sig		-- the signature for the memory-pickling module
   *	foo-memory-pickle.sml	-- the implementation of the memory-pickling operations
   *	foo-file-pickle.sml	-- the implementation of the file-pickling operations
   *)
    fun genFiles base = let
	  fun join ext = (base ^ ext, SOME "sml", Fn.id)
	  in
	    List.map join [
		".sml",
		"-pickle.sig",
		"-memory-pickle.sml",
		"-file-pickle.sml"
	      ]
	  end

    val _ = Tools.registerStdShellCmdTool {
	      tool = "ASDLGen",
	      class = "asdlgen",
	      cmdStdPath = fn () => ("asdlgen", ["sml"]),
	      template = NONE,
	      extensionStyle = Tools.RENAME(["asdl"], genFiles),
	      dflopts = []
	    }

  end

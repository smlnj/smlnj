(* cm/paths/dbm/pathconfig.sml *)

    (* Processing a "spec file" (as an instream) "relative to a given base fpath". *)

signature PATHCONFIG_PROCESSING =
sig

  (* process a pathconfig specification file (the instream) in the context of the given fpath;
   * does a sync afterwards! *)
  val processPathConfig : Path.fpath -> TextIO.instream -> unit

end (* signature PATHCONFIG_PROCESSING *)

structure PathConfigProcessing =
struct

local
  structure OSP = OS.Path
  structure P = Path
in

    (* processSpecFile is used to read and process pathconfig files.
     * It processes an instream (of the contents of the pathconfig file) line by line
       "relative" to the localFpath parameter.
     * Each line is interpretted a comment, a blank line, or a "command".
     * Blank lines and comment lines are skipped and cause no action.
     * The "commands" are:
     *   "#" ... -- a comment line, ignored
     *   !standard -- switch to "standard" parsing of fpaths (isnative becomes false)
     *   !native -- switch to "native" parsing of (isnative becomes true)
     *   anchor fpath  -- add anchor |-> parseFpath (fpath) binding to the global
     *     anchor environment (relative to the parent directory of localFpath)
     *   anchor -- delete anchor from the global anchor environment
     *   -  -- reset the global anchor environment to empty (removing all bindings)
     * This determines the expected content of pathconfig files. *)

    (* processPathConfig : fpath -> TextIO.instream -> unit *)
    fun processPathConfig (localFpath: fpath) =
	let val localDirPath : path = parseFpath (OSP.dir (FS.fullPath localFpath))

	    fun set (anchor, path) =
		P.setRelative (anchor, SOME path, localDirPath)

	    (* parseFpath : bool -> P.fpath -> P.path *)
	    fun parseFpath true = P.parseNative
	      | parseFpath false = P.parseStandard

	    (* processLines: TextIO.instream -> ? *)
	    (* BUG! the isnative boolean arg is not actually controlling the parsing
	       of file paths! Because we are just calling parseFpath in all cases. *)
	    fun processLines (instream: TextIO.instream) =
		(* loop processes each and every line of the instream *)
		let fun loop isnative =
			case TextIO.inputLine instream
			  of NONE => ()
			   | SOME line =>
			       if String.sub (line, 0) = #"#" then loop isnative
			       else case String.tokens Char.isSpace line
				      of ["!standard"] => loop false
				       | ["!native"] => loop true
				       | [anchor, fpath] =>
					   (set (anchor, parseFpath isnative fpath);
					    loop isnative)
				       | ["-"] => (resetAnchors (); loop isnative)
				       | [anchor] =>
					   (AE.set_anchor (anchor, NONE);
					    loop isnative)
				       | [] => loop isnative  (* empty, or blank, line *)
				       | _ => (error [localFpath, ": malformed line (ignored)"];
					       loop isnative)
		 in loop true  (* isnative is initially true, hence native parsing is the default *)
		end

	 in processLines
	end (* fun processPathConfig *)

end (* top local *)
end (* structure PathConfigProcessing *)

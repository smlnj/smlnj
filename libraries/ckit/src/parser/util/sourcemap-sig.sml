signature SOURCE_MAP = 
sig

  type charpos = int 
    (* char position in a file *)

  type region = charpos * charpos 
    (* region between two character positions, where it is assumed that
     * the first charpos is less than the second *)

  datatype location
    = LOC of
        {srcFile   : string,
	 beginLine : int,
	 beginCol  : int,
	 endLine   : int,
	 endCol    : int}
      | UNKNOWN
    (* encodes the information used to record locations in input sources.
     * a location designates a region within a (single) source file *)

  type sourcemap 
    (* a data structure maintaining a mapping between character positions
     * in an input source and locations.
     * This handles multiple source files, which can happen if the input
     * has been passed through the C preprocessor.
     *)

  val newmap   : {srcFile : string} -> sourcemap
    (* creates a new sourcemap with an initial source file name srcFile *)

  val newline  : sourcemap -> charpos -> unit
    (* records a line break in the input source *)

  val resynch  : sourcemap -> {pos:charpos, srcFile:string option, line:int} -> unit
    (* switch source file names in response to a directive created by
     * an include *)

  val parseDirective : sourcemap -> charpos * string -> unit
    (* parse a C preprocessor directive to reset src file name and line number *)

  val location : sourcemap -> region -> location
    (* maps a region to a location *)

  val currPos  : sourcemap -> charpos
    (* returns the current character position in the source represented
     * by the sourcemap *)

  val locToString : location -> string
    (* format a location as a string *)

end



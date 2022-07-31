(*
 * This maps character position in the input stream to 
 * the source file location(s).
 *)
signature SOURCE_MAPPING = 
sig

  type charpos = int 

  type region = charpos * charpos 

  datatype location = LOC of {srcFile   : UniqueSymbol.symbol,
                              beginLine : int,
                              beginCol  : int,
                              endLine   : int,
                              endCol    : int
                             }

  type sourcemap 
  type state

  val dummyLoc : location 
  val newmap   : {srcFile : string} -> sourcemap
  val newline  : sourcemap -> charpos -> unit
  val resynch  : sourcemap -> {pos:charpos, srcFile:string, line:int} -> unit

  val state    : sourcemap -> state
  val reset    : sourcemap -> state -> unit

  val parseDirective : sourcemap -> charpos * string -> unit
  val location : sourcemap -> region -> location
  val currPos  : sourcemap -> charpos
  val toString : location -> string
  val directive : location -> string

end


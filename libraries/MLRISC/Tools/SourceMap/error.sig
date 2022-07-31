(*
 * Module for simple error handling with filenames/line numbers 
 *)
signature MDL_ERROR =
sig

   exception Error

   val setLoc       : SourceMapping.location -> unit
   val errorCount   : int ref
   val warningCount : int ref
   val init         : unit -> unit
   val log          : string -> unit
   val fail         : string -> 'a
   val error        : string -> unit
   val errorPos     : SourceMapping.location * string -> unit
   val warning      : string -> unit
   val warningPos   : SourceMapping.location * string -> unit
   val withLoc      : SourceMapping.location -> ('a -> 'b) -> 'a -> 'b
   val status       : unit -> string

   (* attach error messages to a log file too *)
   val printToLog   : string -> unit
   val openLogFile  : string -> unit
   val closeLogFile : unit -> unit
   val logfile      : unit -> string
end

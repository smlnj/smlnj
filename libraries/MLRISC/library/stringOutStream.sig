(*
 * This module allows use to bind a streambuf to an outstream.
 * We can use this to capture all the output to a stream as a single string.  
 *)
signature STRING_OUTSTREAM =
sig
   type streambuf

   val mkStreamBuf   : unit -> streambuf
   val getString     : streambuf -> string
   val setString     : streambuf * string -> unit
   val openStringOut : streambuf -> TextIO.outstream

end

(* mlriscErrormsg.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *)

signature MLRISC_ERROR_MSG =
sig
   exception Error
   val print      : string -> unit
   val impossible : string -> 'a
   val error      : string * string -> 'a
end

structure MLRiscErrorMsg : MLRISC_ERROR_MSG =
struct
  exception Error
  val print = fn s => TextIO.output(TextIO.stdOut, s)
  fun impossible msg =
      (app print ["Error: MLRisc bug: ",msg,"\n"];
       TextIO.flushOut TextIO.stdOut;
       raise Error)
  fun error(module,msg) = impossible(module^"."^msg)
end



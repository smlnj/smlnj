(* COPYRIGHT (c) 1996 Bell Laboratories*)
(* elabdebug.sml *)

signature ELABDEBUG =
sig
  val debugMsg : bool ref -> string -> unit
  val debugPrint : bool ref -> (string * NewPP.format) -> unit
  val envBoundSymbols : StaticEnv.staticEnv -> Symbol.symbol list
  val checkBound : StaticEnv.staticEnv * Symbol.symbol -> string
  val withInternals : ('a -> NewPP.format) -> unit  (* ??? will this work? *)
end (* signature ELABDEBUG *)

structure ElabDebug : ELABDEBUG =
struct

local
  structure S  = Symbol
  structure SE = StaticEnv
  structure PP = NewPP
  structure PPU = NewPPUtil
  structure EM = ErrorMsg

in

fun debugMsg (debugging: bool ref) (msg: string) =
    if (!debugging)
    then PP.printFormatNL (PP.text msg)
    else ()

fun debugPrint (debugging: bool ref) (msg: string, format: PP.format) =
    if (!debugging)
    then PP.printFormatNL (PP.vcat (PP.text msg, format))
    else ()

(* envBoundSymbols : SE.staticEnv -> S.symbol list *)
fun envBoundSymbols (env: SE.staticEnv) =
      SE.fold (fn ((s,_),sl) => s::sl) nil env

(* checkBound : SE.staticEnv * S.symbol -> bool *)
fun checkBound (env: SE.staticEnv, sym: S.symbol) =
      (SE.look(env,sym); "YES") handle SE.Unbound => "NO"

(* withInternals : (unit -> 'a) -> 'a
 *  execute a thunk with internals flags on, restoring the flags afterward *)
fun withInternals (f: unit -> 'a) =
    let val savedInternals = ElabDataControl.setInternals ()
     in (f() before ElabDataControl.resetInternals savedInternals)
        handle exn => (ElabDataControl.resetInternals savedInternals; raise exn)
    end

end (* local *)
end (* structure ElabDebug *)

(* COPYRIGHT (c) 1996 Bell Laboratories*)
(* elabdebug.sml *)

signature ELABDEBUG =
sig
  val debugMsg : bool ref -> string -> unit
  val debugPrint : bool ref -> (string * ('a -> NewPP.format) * 'a) -> unit
  val fmtSymList : Symbol.symbol list -> NewPP.format
  val envSymbols : StaticEnv.staticEnv -> Symbol.symbol list
  val checkEnv : StaticEnv.staticEnv * Symbol.symbol -> string
  val withInternals : (unit -> 'a) -> 'a
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

fun debugPrint (debugging: bool ref)
               (msg: string, formatter: 'a -> PP.format, arg: 'a) =
    if (!debugging)
    then PP.printFormatNL
	    (PP.vcat
	       (PP.text msg,
		formatter arg)
	     closeBox ppstrm;
    else ()

fun fmtSymList (syms: S.symbol list) = PP.formatList PPU.fmtSym syms

(* more debugging *)
fun envSymbols (env: SE.staticEnv) =
      SE.fold (fn ((s,_),sl) => s::sl) nil env

fun checkEnv (env: SE.staticEnv, sym: S.symbol) =
      (SE.look(env,sym); "YES") handle SE.Unbound => "NO"

fun withInternals (f: unit -> 'a) =
    let val savedInternals = ElabDataControl.setInternals ()
     in (f() before ElabDataControl.resetInternals savedInternals)
        handle exn => (ElabDataControl.resetInternals savedInternals; raise exn)
    end

end (* local *)
end (* structure ElabDebug *)

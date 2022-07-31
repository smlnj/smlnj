(* COPYRIGHT (c) 1996 Bell Laboratories*)
(* elabdebug.sml *)

signature ELABDEBUG =
sig
  val debugMsg : bool ref -> string -> unit
  val debugPrint : bool ref
                   -> (string *
		       (PrettyPrint.stream -> 'a -> unit) *
		       'a)
                   -> unit
  val ppSymList : PrettyPrint.stream -> Symbol.symbol list -> unit
  val envSymbols : StaticEnv.staticEnv -> Symbol.symbol list
  val checkEnv : StaticEnv.staticEnv * Symbol.symbol -> string
  val withInternals : (unit -> 'a) -> 'a

end (* signature ELABDEBUG *)


structure ElabDebug : ELABDEBUG =
struct

local
  structure S  = Symbol
  structure SE = StaticEnv
  structure PP = PrettyPrint
  structure PU = PPUtil
  structure EM = ErrorMsg

  open PP

in

fun debugMsg (debugging: bool ref) (msg: string) =
    if (!debugging)
    then with_default_pp
	  (fn ppstrm =>
	    (openHVBox ppstrm (PP.Rel 0);
	     PP.string ppstrm msg;
	     closeBox ppstrm;
	     newline ppstrm;
	     PP.flushStream ppstrm))
    else ()

fun debugPrint (debugging: bool ref)
               (msg: string, printfn: PP.stream -> 'a -> unit, arg: 'a) =
    if (!debugging)
    then with_default_pp
	  (fn ppstrm =>
	    (openHVBox ppstrm (PP.Abs 2);
	     PP.string ppstrm msg;
	     PP.nbSpace ppstrm 1;
	     printfn ppstrm arg;
	     closeBox ppstrm;
	     newline ppstrm;
	     PP.flushStream ppstrm))
    else ()

fun ppSymList ppstrm (syms: S.symbol list) =
     PU.ppClosedSequence ppstrm
     {front=(fn ppstrm => PP.string ppstrm "["),
      sep=(fn ppstrm => (PP.string ppstrm ",")),
      back=(fn ppstrm => PP.string ppstrm "]"),
      style=PU.INCONSISTENT,
      pr=PU.ppSym}
     syms


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

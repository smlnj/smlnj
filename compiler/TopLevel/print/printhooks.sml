(* printhooks.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

(* NOT USED *)

structure PrintHooks :
  sig
    (* all output goes to Control.Print.out *)
    val fmtAbsyn : StaticEnv.staticEnv -> Absyn.dec -> Formatting.format
  end = 

struct

   fun fmtAbsyn env dec  = PPAbsyn.fmtDec (env,NONE) (dec,200)

end (* structure PrintHooks *)

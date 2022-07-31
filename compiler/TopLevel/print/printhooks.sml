(* printhooks.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure PrintHooks :
  sig
    (* all output goes to Control.Print.out *)
    val prAbsyn : StaticEnv.staticEnv -> Absyn.dec -> unit
  end = 

struct

   fun prAbsyn env d  = 
	PrettyPrint.with_default_pp
          (fn ppstrm => PPAbsyn.ppDec(env,NONE) ppstrm (d,200))

end (* structure PrintHooks *)

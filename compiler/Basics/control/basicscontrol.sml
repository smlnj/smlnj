(* basiccontrol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

signature BASICS_CONTROL =
sig
    (* if false, suppress all warning messages *)
    val printWarnings : bool ref
end (* signature BASICS_CONTROL *)

structure BasicsControl : BASICS_CONTROL =
struct

  val {newBool, ...} = MakeControls.make {name = "Basics", priority = [1]}

  val printWarnings = newBool ("print-warnings", 
			       "whether warnings get generated",
			       true)

end (* structure BasicsControl *)
    

(* tkind.sml *)

(* front-end version of PLambdaType tkinds for use in module elaboration.
 * These tkind values can be easily translated to PLambdaType tkind when 
 * needed in Translate. *)

structure TKind =
struct

datatype tkind
  = TKCint of int
  | TKCfun of tkind list * tkind
  | TKCseq of tkind list

end (* structure TKind *)

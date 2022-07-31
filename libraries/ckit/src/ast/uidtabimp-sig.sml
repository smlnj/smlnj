(* Copyright (c) 1998 by Lucent Technologies *)

(* imperative uid tables *)

signature UIDTABIMP =
sig

  type uid
  type 'a uidtab

  val insert : 'info uidtab * uid * 'info -> unit
  val find : 'info uidtab * uid -> 'info option
  val listItems : 'info uidtab -> 'info list
  val listItemsi : 'info uidtab -> (uid * 'info) list

  val uidtab : unit -> 'info uidtab

end




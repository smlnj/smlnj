(* Copyright (c) 1998 by Lucent Technologies *)

(* UID = "Unique IDentifiers" *)

signature UID = sig
  type uid = int

  val initial : uid
  val new : unit -> uid
  val reset : int -> unit
  val equal: uid * uid -> bool
  val compare: uid * uid -> order
  val toWord: uid -> Word.word
  val toString: uid -> string
end

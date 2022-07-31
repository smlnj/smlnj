(* codeString.sig
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 *)

signature CODE_STRING = sig
  type code_string
  val init          : int -> unit
  val update        : int * Word8.word -> unit
  val getCodeString : int -> code_string
end


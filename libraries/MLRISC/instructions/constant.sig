(* constant.sml --- constants used to specialize MLRISC and the code generators.
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

signature CONSTANT = sig
   type const

   val toString : const -> string
   val valueOf : const -> int
   val hash    : const -> word
   val ==      : const * const -> bool
end

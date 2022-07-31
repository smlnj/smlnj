(* lsplit-arg.sml
 *   Convert string representation of a lambda-plitting specification
 *   into something matching Control.LambdaSplitting.localsetting.
 *   (That type is simply int option option, so we do all this without
 *   actually referring to that structure in order to avoid additional
 *   static dependencies.)
 *
 * (C) 2002 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@research.bell-lab.com)
 *)
structure LSplitArg : sig
    val arg : string -> int option option option
end = struct
    val UseDefault = NONE
    val Suggest = SOME
    fun arg "default" = SOME UseDefault
      | arg "infinity" = SOME (Suggest (SOME 100000000))
      | arg "on" = SOME (Suggest (SOME 0))
      | arg "off" = SOME (Suggest NONE)
      | arg n =
	(case Int.fromString n of
	     SOME i => SOME (Suggest (SOME i))
	   | NONE => NONE)
end

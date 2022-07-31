(*
 * Environments used during dependency analysis.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure DAEnv = struct

    datatype env =
	EMPTY
      | FCTENV of Symbol.symbol -> value option
      | BINDING of Symbol.symbol * value
      | LAYER of env * env
      | FILTER of SymbolSet.set * env
      | SUSPEND of unit -> env

    withtype value = env
end

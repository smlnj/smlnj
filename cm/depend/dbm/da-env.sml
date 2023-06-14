(* cm/depend/dbm/da-env.sml
 *
 * Environments used during dependency analysis.
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 *)

structure DAEnv =
struct

    datatype env
      = EMPTY
      | FCTENV of Symbol.symbol -> env option
      | BINDING of Symbol.symbol * env
      | LAYER of env * env
      | FILTER of SymbolSet.set * env
      | SUSPEND of unit -> env

(*    type value = env  (* why do we need this synonym? *) *)

end (* structure DAEnv *)

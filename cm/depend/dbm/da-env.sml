(* cm/depend/dbm/da-env.sml
 *
 * Environments used during dependency analysis. ("da" stands for dependency analysis)
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 * Edited: DBM, 2023.6
 *)

structure DAEnv =
struct

  datatype env
    = EMPTY
    | FUNENV of Symbol.symbol -> env option (* env as partial function *)
    | BINDING of Symbol.symbol * env
    | LAYER of env * env    (* or LAYERED of env list ? *)
    | FILTER of SymbolSet.set * env

end (* structure DAEnv *)

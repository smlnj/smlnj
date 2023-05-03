(* pidenv.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * Environments that bind persistent IDs.
 * (Instantiated to dynamic and symbolic environments by the compiler.)
 *)
local
    type pid = PersStamps.persstamp
in
signature PIDENV = sig

    type binding
    type env

    val empty : env
    val look: env -> pid -> binding option
    val bind: pid * binding * env -> env
    val atop: env * env -> env
    val remove: pid list * env -> env
    val consolidate: env -> env
    val singleton: pid * binding -> env
    val listItemsi: env -> (pid * binding) list
    val fromListi: (pid * binding) list -> env

    val mk : pid option * binding option -> env
end (* signature SYMENV *)
end (* local *)

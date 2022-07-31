(*
 * Handling compile-servers.
 *
 *  This is still rather crude and not very robust.  A "real" implementation
 *  exists only for Unix.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature SERVERS = sig

    type server_handle

    (* add a compile server *)
    val start : { name: string, cmd: string * string list,
		  pathtrans: (string -> string) option,
		  pref: int } -> server_handle option

    val stop : server_handle -> unit

    val kill : server_handle -> unit

    val name : server_handle -> string

    (* Reset scheduler and wait until all servers are idle.
     * The "bool" argument makes reset suitable as an argument to
     * SafeIO.perform. *)
    val reset : bool -> unit

    (* check whether all servers are currently idle *)
    val allIdle : unit -> bool

    (* signal all servers that we are starting with a new .cm file *)
    val cm : { archos: string, project: string } -> unit

    (* signal all servers that we are doing another CMB.make *)
    val cmb : { dirbase: string, archos: string, root: string } -> unit

    (* make the slave's CMB engine perform a reset *)
    val cmb_reset : { archos: string } -> unit

    (* schedule a compilation *)
    val compile : string -> bool

    (* run some thunk with compile parallelism enabled *)
    val withServers : (unit -> 'a) -> 'a

    (* check whether there are any servers attached *)
    val noServers : unit -> bool
end

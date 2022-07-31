(* old-cml-sig.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This is essentially the 0.9.8 version of the core CML interface.  The only
 * thing missing is poll and the low-level I/O synchronization.  Also, there
 * is an additional substructure (NewCML), to allow access to the new features.
 *)

signature OLD_CML =
  sig

    structure NewCML : CML

    val version : {major : int, minor : int, rev : int, date : string}
    val versionName : string

  (** events **)
    type 'a event

    val sync   : 'a event -> 'a
    val select : 'a event list -> 'a

    val choose : 'a event list -> 'a event

    val guard : (unit -> 'a event) -> 'a event

    val wrap        : ('a event * ('a -> 'b)) -> 'b event
    val wrapHandler : ('a event * (exn -> 'a)) -> 'a event
    val wrapAbort   : ('a event * (unit -> unit)) -> 'a event

    val always : 'a -> 'a event
    val ALWAYS : unit event (** for backward compatibility **)

  (** threads **)
    type thread_id

    val spawn : (unit -> unit) -> thread_id

    val yield : unit -> unit
    val exit : unit -> 'a

    val getTid : unit -> thread_id
    val sameThread : (thread_id * thread_id) -> bool
    val tidLessThan : (thread_id * thread_id) -> bool
    val tidToString : thread_id -> string

    val threadWait : thread_id -> unit event

  (** condition variables **)
    type 'a cond_var

    val condVar : unit -> '1a cond_var

    val writeVar : ('a cond_var * 'a) -> unit
    exception WriteTwice

    val readVar : 'a cond_var -> 'a
    val readVarEvt : 'a cond_var -> 'a event

  (** channels **)
    type 'a chan

(* +DEBUG **
val dumpCh : 'a chan -> string
** -DEBUG *)
    val channel : unit -> '1a chan

    val send   : ('a chan * 'a) -> unit
    val sendc  : 'a chan -> 'a -> unit
    val accept : 'a chan -> 'a

    val sameChannel : ('a chan * 'a chan) -> bool

    val transmit  : ('a chan * 'a) -> unit event
    val transmitc : 'a chan -> 'a -> unit event
    val receive   : 'a chan -> 'a event

  (** real-time synchronization **)
    val waitUntil : Time.time -> unit event
    val timeout   : Time.time -> unit event

  end (* signature CONCUR_ML *)

(* Copyright 1996 by AT&T Bell Laboratories *)
(* Re-written by Matthias Blume (3/2000) *)
(* stamps.sig *)

signature STAMPS =
sig
    type stamp
    type ord_key = stamp		(* to match signature ORD_KEY *)
    type pid = PersStamps.persstamp	(* for global stamps *)

    val eq : stamp * stamp -> bool
    val compare : stamp * stamp -> order (* instead of "cmp" (ORD_KEY) *)

    type generator
    val newGenerator : unit -> generator
    val fresh : generator -> stamp

    (* Make a new "special" stamp (for things in primEnv). *)
    val special : string -> stamp

    (* Make a "global" stamp (i.e., one that comes from a different
     * compilation unit). Used only by the unpickler. *)
    val global  : { pid: pid, cnt: int } -> stamp

    (* Case analysis on the abstract type with built-in alpha-conversion
     * for fresh stamps. Used by the pickler. *)
    type converter
    val newConverter : unit -> converter
    val Case : converter  -> stamp ->
	       { fresh   : int -> 'a,	(* already alpha-converted *)
		 global  : { pid: pid, cnt: int } -> 'a,
		 special : string -> 'a } -> 'a

    (* testing for freshness quickly... *)
    val isFresh : stamp -> bool

    (* for debugging: *)
    val toString : stamp -> string
    val toShortString : stamp -> string
end

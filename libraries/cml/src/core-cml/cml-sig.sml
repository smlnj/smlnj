(* cml-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The interface to the core CML features.
 *)

signature CML =
  sig
    val version : {system : string, version_id : int list, date : string}
    val banner : string

    include THREAD
    include CHANNEL
    include EVENT
    include TIME_OUT
  end


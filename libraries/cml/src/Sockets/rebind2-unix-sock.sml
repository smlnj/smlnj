(* rebind2-unix-sock.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies
 *
 * Rebind the UnixSock structures to the exported names.
 *)


structure UnixSock = (* CML_UnixSock *) struct open CML_UnixSock end

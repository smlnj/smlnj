(* rebind2.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Rebind the Socket structures to the exported names.
 *)

structure GenericSock = (* CML_GenericSock *) struct open CML_GenericSock end
structure INetSock = (* CML_INetSock *) struct open CML_INetSock end


(* rebind-socket.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Rebind the Socket structures to the exported names.
 *)

structure Socket = CML_Socket
structure GenericSock : GENERIC_SOCK = CML_GenericSock
structure INetSock : INET_SOCK = CML_INetSock
structure UnixSock : UNIX_SOC = CML_UnixSock


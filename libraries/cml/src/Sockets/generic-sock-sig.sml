(* generic-sock-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * $Log$
 * Revision 1.4  2003/09/24 20:09:40  mblume
 * sync of socket implementation with Basis spec;
 * minor updates to Real64 code
 *
 * Revision 1.3.2.1  2003/09/23 20:37:17  mblume
 * switched to spec-conforming implementation of Sockets;
 * updated CML accordingly
 *
 * Revision 1.3  2001/06/20 20:39:14  blume
 * CML compiles and works again
 *
 * Revision 1.2.4.1  2001/06/20 17:54:59  blume
 * CML now compiles under old and new CM
 *
 * Revision 1.2  1996/06/03  21:11:42  jhr
 * Sockets API cleanup.
 *
 * Revision 1.1.1.1  1996/01/31  16:02:36  george
 * Version 109
 * 
 *)

signature GENERIC_SOCK =
  sig
(*
    val addressFamilies : unit -> Socket.AF.addr_family list
	(* returns a list of the supported address families; this should include
	 * at least:  Socket.AF.inet.
	 *)

    val socketTypes : unit -> Socket.SOCK.sock_type
	(* returns a list of the supported socket types; this should include at
	 * least:  Socket.SOCK.stream and Socket.SOCK.dgram.
	 *)
*)

  (* create sockets using default protocol *)
    val socket : (Socket.AF.addr_family * Socket.SOCK.sock_type)
	  -> ('a, 'b) Socket.sock
    val socketPair : (Socket.AF.addr_family * Socket.SOCK.sock_type)
	  -> (('a, 'b) Socket.sock * ('a, 'b) Socket.sock)

  (* create sockets using the specified protocol *)
    val socket' : (Socket.AF.addr_family * Socket.SOCK.sock_type * int)
	  -> ('a, 'b) Socket.sock
    val socketPair' : (Socket.AF.addr_family * Socket.SOCK.sock_type * int)
	  -> (('a, 'b) Socket.sock * ('a, 'b) Socket.sock)

  end

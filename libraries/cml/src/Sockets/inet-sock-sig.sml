(* inet-sock-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * $Log$
 * Revision 1.3  2001/06/20 20:39:14  blume
 * CML compiles and works again
 *
 * Revision 1.2.4.1  2001/06/20 17:54:59  blume
 * CML now compiles under old and new CM
 *
# Revision 1.2  1996/09/06  00:35:42  george
# *** empty log message ***
#
# Revision 1.1  1996/06/03  21:11:43  jhr
# Sockets API cleanup.
#
 * Revision 1.1.1.1  1996/01/31  16:02:37  george
 * Version 109
 * 
 *)

signature INET_SOCK =
  sig

    type inet

    type 'a sock = (inet, 'a) Socket.sock
    type 'a stream_sock = 'a Socket.stream sock
    type dgram_sock = Socket.dgram sock

    type sock_addr = inet Socket.sock_addr

    val inetAF : Socket.AF.addr_family   (* DARPA internet protocols *)

    val toAddr   : (NetHostDB.in_addr * int) -> sock_addr
    val fromAddr : sock_addr -> (NetHostDB.in_addr * int)
    val any  : int -> sock_addr

    structure UDP : sig
	val socket  : unit -> dgram_sock
	val socket' : int -> dgram_sock
      end

    structure TCP : sig
	val socket  : unit -> 'a stream_sock
	val socket' : int -> 'a stream_sock
      (* tcp control options *)
	val getNODELAY : 'a stream_sock -> bool
	val setNODELAY : ('a stream_sock * bool) -> unit
      end
  end


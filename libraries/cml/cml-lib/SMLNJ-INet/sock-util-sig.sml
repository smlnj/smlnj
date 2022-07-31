(* sock-util-sig.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies.
 *
 * Various utility functions for programming with sockets.
 *)

signature SOCK_UTIL =
  sig

    datatype port = PortNumber of int | ServName of string
	(* a port can be identified by number, or by the name of a service *)

    datatype hostname = HostName of string | HostAddr of NetHostDB.in_addr

    val scanAddr : (char, 'a) StringCvt.reader
	  -> ({host : hostname, port : port option}, 'a) StringCvt.reader
	(* scan an address, which has the form
	 *   addr [ ":" port ]
	 * where the addr may either be numeric or symbolic host name and the
	 * port is either a service name or a decimal number.  Legal host names
	 * must begin with a letter, and may contain any alphanumeric character,
	 * the minus sign (-) and period (.), where the period is used as a
	 * domain separator.  
	 *)

    exception BadAddr of string

    val resolveAddr : {host : hostname, port : port option}
	  -> {host : string, addr : NetHostDB.in_addr, port : int option}
	(* Given a hostname and optional port, resolve them in the host
	 * and service database.  If either the host or service name is not
	 * found, then BadAddr is raised.
	 *)

    type 'a stream_sock = ('a, Socket.active Socket.stream) Socket.sock

    val connectINetStrm : {addr : NetHostDB.in_addr, port : int}
	  -> INetSock.inet stream_sock
	(* establish a client-side connection to a INET domain stream socket *)

    val recvVec : ('a stream_sock * int) -> Word8Vector.vector
    val recvStr : ('a stream_sock * int) -> string
    val sendVec : ('a stream_sock * Word8Vector.vector) -> unit
    val sendStr : ('a stream_sock * string) -> unit
    val sendArr : ('a stream_sock * Word8Array.array) -> unit

  end;

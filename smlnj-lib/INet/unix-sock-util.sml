(* unix-sock-util.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Bind SockUtil structure on Unix systems
 *)

signature UNIX_SOCK_UTIL =
  sig

    include SOCK_UTIL

    val connectUnixStrm : string -> UnixSock.unix stream_sock
	(* establish a client-side connection to a Unix-domain stream socket *)

  end


structure UnixSockUtil : UNIX_SOCK_UTIL =
  struct

    open SockUtil

  (* establish a client-side connection to a Unix-domain stream socket *)
    fun connectUnixStrm path = let
	  val sock = UnixSock.Strm.socket ()
	  in
	    Socket.connect (sock, UnixSock.toAddr path);
	    sock
	  end

  end

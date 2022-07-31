(* cml-unix-sock.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure CML_UnixSock : UNIX_SOCK =
  struct
    structure SOCK = Socket.SOCK

    type unix = UnixSock.unix

    type 'a sock = (unix, 'a) Socket.sock
    type 'a stream_sock = 'a Socket.stream sock
    type dgram_sock = Socket.dgram sock

    type sock_addr = unix Socket.sock_addr

    val unixAF = UnixSock.unixAF
    val toAddr = UnixSock.toAddr
    val fromAddr = UnixSock.fromAddr

    structure Strm =
      struct
	fun socket () = CML_GenericSock.socket (unixAF, SOCK.stream)
	fun socket' proto = CML_GenericSock.socket' (unixAF, SOCK.stream, proto)
	fun socketPair () = CML_GenericSock.socketPair (unixAF, SOCK.stream)
	fun socketPair' proto = CML_GenericSock.socketPair' (unixAF, SOCK.stream, proto)
      end

    structure DGrm =
      struct
	fun socket () = CML_GenericSock.socket (unixAF, SOCK.dgram)
	fun socket' proto = CML_GenericSock.socket' (unixAF, SOCK.dgram, proto)
	fun socketPair () = CML_GenericSock.socketPair (unixAF, SOCK.dgram)
	fun socketPair' proto = CML_GenericSock.socketPair' (unixAF, SOCK.dgram, proto)
      end

  end

(* unix-sock.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure UnixSock : UNIX_SOCK =
  struct
    structure SOCK = SocketImp.SOCK

    fun sockFn x = CInterface.c_function "SMLNJ-Sockets" x

    datatype unix = UNIX

    type 'a sock = (unix, 'a) Socket.sock
    type 'a stream_sock = 'a SocketImp.stream sock
    type dgram_sock = SocketImp.dgram sock

    type sock_addr = unix Socket.sock_addr

    val unixAF = Option.valOf(SocketImp.AF.fromString "UNIX")

(** We should probably do some error checking on the length of the string *)
    local
      val toUnixAddr : string -> Socket.addr = sockFn "toUnixAddr"
      val fromUnixAddr : Socket.addr -> string = sockFn "fromUnixAddr"
    in
    fun toAddr s = Socket.ADDR(toUnixAddr s)
    fun fromAddr (Socket.ADDR addr) = fromUnixAddr addr
    end

    structure Strm =
      struct
	fun socket () = GenericSock.socket (unixAF, SOCK.stream)
	fun socket' proto = GenericSock.socket' (unixAF, SOCK.stream, proto)
	fun socketPair () = GenericSock.socketPair (unixAF, SOCK.stream)
	fun socketPair' proto = GenericSock.socketPair' (unixAF, SOCK.stream, proto)
      end
    structure DGrm =
      struct
	fun socket () = GenericSock.socket (unixAF, SOCK.dgram)
	fun socket' proto = GenericSock.socket' (unixAF, SOCK.dgram, proto)
	fun socketPair () = GenericSock.socketPair (unixAF, SOCK.dgram)
	fun socketPair' proto = GenericSock.socketPair' (unixAF, SOCK.dgram, proto)
      end
  end

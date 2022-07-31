(* inet-sock.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)
structure INetSock : INET_SOCK =
  struct
    structure SOCK = SocketImp.SOCK

    fun sockFn x = CInterface.c_function "SMLNJ-Sockets" x

    datatype inet = INET

    type 'a sock = (inet, 'a) Socket.sock
    type 'a stream_sock = 'a SocketImp.stream sock
    type dgram_sock = SocketImp.dgram sock

    type sock_addr = inet Socket.sock_addr

    val inetAF = Option.valOf(SocketImp.AF.fromString "INET")

    local
      val toInetAddr : (Socket.addr * int) -> Socket.addr = sockFn "toInetAddr"
      val fromInetAddr : Socket.addr -> (Socket.addr * int) = sockFn "fromInetAddr"
      val inetAny  : int -> Socket.addr = sockFn "inetany"
    in
    fun toAddr (ina, port) =
	Socket.ADDR(toInetAddr(NetHostDBInternal.unINADDR ina, port))
    fun fromAddr (Socket.ADDR addr) = let
	  val (a, port) = fromInetAddr addr
	  in
	    (NetHostDBInternal.INADDR a, port)
	  end
    fun any port = Socket.ADDR(inetAny port)
    end

    structure UDP =
      struct
	fun socket () = GenericSock.socket (inetAF, SOCK.dgram)
	fun socket' proto = GenericSock.socket' (inetAF, SOCK.dgram, proto)
      end

    structure TCP =
      struct
	fun socket () = GenericSock.socket (inetAF, SOCK.stream)
	fun socket' proto = GenericSock.socket' (inetAF, SOCK.stream, proto)
      (* tcp control options *)
	local
	  val ctlDELAY : (int * bool option) -> bool = sockFn "ctlNODELAY"
	in
	fun getNODELAY (Socket.SOCK { fd, ... }) =
	    ctlDELAY(fd, NONE)
	fun setNODELAY (Socket.SOCK { fd, ... }, flg) =
	    ignore(ctlDELAY(fd, SOME flg))
	end (* local *)
      end
  end

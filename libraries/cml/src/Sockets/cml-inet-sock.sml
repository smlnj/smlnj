(* cml-inet-sock.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure CML_INetSock : INET_SOCK =
  struct
    type inet = INetSock.inet

    type 'a sock = (inet, 'a) Socket.sock
    type 'a stream_sock = 'a Socket.stream sock
    type dgram_sock = Socket.dgram sock

    type sock_addr = inet Socket.sock_addr

    val inetAF = INetSock.inetAF

    val toAddr = INetSock.toAddr
    val fromAddr = INetSock.fromAddr
    val any = INetSock.any

    structure UDP =
      struct
	fun socket () =
	      CML_GenericSock.socket (inetAF, Socket.SOCK.dgram)
	fun socket' proto =
	      CML_GenericSock.socket' (inetAF, Socket.SOCK.dgram, proto)
      end

    structure TCP =
      struct
	fun socket () =
	      CML_GenericSock.socket (inetAF, Socket.SOCK.stream)
	fun socket' proto =
	      CML_GenericSock.socket' (inetAF, Socket.SOCK.stream, proto)
      (* tcp control options *)
	fun getNODELAY (PreSock.CMLSock{sock, ...}) =
	      INetSock.TCP.getNODELAY sock
	fun setNODELAY (PreSock.CMLSock{sock, ...}, flg) =
	      INetSock.TCP.setNODELAY(sock, flg)
      end

  end

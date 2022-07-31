(* pre-sock.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Provide some utility operations for CML sockets.
 *)

structure PreSock : sig

    datatype socket_state
      = Unconnected	(* initial state *)
      | Connecting	(* when waiting for a connect to complete *)
      | Connected	(* when connected *)
      | Accepting	(* when waiting for an accept to complete *)
      | WaitingOnIO	(* when waiting on an input and/or output operation *)
      | Closed

    datatype ('a, 'b) sock = CMLSock of {
	state : socket_state SyncVar.mvar,
	sock : ('a, 'b) Socket.sock
      }

    val mkSock : ('a, 'b) Socket.sock -> ('a, 'b) sock

    val inEvt : ('a, 'b) sock -> unit CML.event
    val outEvt : ('a, 'b) sock -> unit CML.event

  end = struct

    datatype socket_state
      = Unconnected	(* initial state *)
      | Connecting	(* when waiting for a connect to complete *)
      | Connected	(* when connected *)
      | Accepting	(* when waiting for an accept to complete *)
      | WaitingOnIO	(* when waiting on an input and/or output operation *)
      | Closed

    datatype ('a, 'b) sock = CMLSock of {
	state : socket_state SyncVar.mvar,
	sock : ('a, 'b) Socket.sock
      }

    (* given an SML socket, return a CML socket *)
    fun mkSock s =
	CMLSock { state = SyncVar.mVarInit Unconnected,
		  sock = s }

    local
	fun pollD sock =
	    case OS.IO.pollDesc (Socket.ioDesc sock) of
		SOME pd => pd
	      | NONE => raise Fail "no polling on socket I/O descriptor"
    in
    fun inEvt (CMLSock{sock, ...}) =
	  CML.wrap(IOManager.ioEvt (OS.IO.pollIn (pollD sock)), ignore)
    fun outEvt (CMLSock{sock, ...}) =
	  CML.wrap(IOManager.ioEvt(OS.IO.pollOut (pollD sock)), ignore)

    end
  end;

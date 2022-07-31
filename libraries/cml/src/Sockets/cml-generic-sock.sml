(* cml-generic-sock.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure CML_GenericSock : GENERIC_SOCK =
  struct

(*
  (* returns a list of the supported address families; this should include
   * at least:  Socket.AF.inet.
   *)
    val addressFamilies = GenericSock.addressFamilies

  (* returns a list of the supported socket types; this should include at
   * least:  Socket.SOCK.stream and Socket.SOCK.dgram.
   *)
    val socketTypes = GenericSock.socketTypes
*)

  (* create sockets using default protocol *)
    fun socket arg = PreSock.mkSock(GenericSock.socket arg)
    fun socketPair arg = let
	  val (s1, s2) = GenericSock.socketPair arg
	  in
	    (PreSock.mkSock s1, PreSock.mkSock s2)
	  end

  (* create sockets using the specified protocol *)
    fun socket' arg = PreSock.mkSock(GenericSock.socket' arg)
    fun socketPair' arg = let
	  val (s1, s2) = GenericSock.socketPair' arg
	  in
	    (PreSock.mkSock s1, PreSock.mkSock s2)
	  end

  end

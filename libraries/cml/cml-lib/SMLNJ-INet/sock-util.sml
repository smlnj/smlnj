(* sock-util.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies.
 *
 * Various utility functions for programming with sockets.
 *)

structure SockUtil : SOCK_UTIL =
  struct

    structure C = Char
    structure PC = ParserComb

    datatype port = PortNumber of int | ServName of string
	(* a port can be identified by number, or by the name of a service *)

    datatype hostname = HostName of string | HostAddr of NetHostDB.in_addr

(** This belongs in an Option structure **)
    fun filterPartial pred NONE = NONE
      | filterPartial pred (SOME x) = if (pred x) then SOME x else NONE

    fun scanName getc strm = let
	  fun isNameChr (#".", _) = true
	    | isNameChr (#"-", _) = true
	    | isNameChr (c, _) = C.isAlphaNum c
	  fun getName (strm, cl) = (case filterPartial isNameChr (getc strm)
		 of SOME(c, strm') => getName(strm', c::cl)
		  | NONE => SOME(implode(rev cl), strm)
		(* end case *))
	  in
	    case (filterPartial (C.isAlpha o #1) (getc strm))
	     of SOME(c, strm) => getName(strm, [c])
	      | NONE => NONE
	    (* end case *)
	  end

  (* scan an address, which has the form
   *   addr [ ":" port ]
   * where the addr may either be numeric or symbolic host name and the
   * port is either a service name or a decimal number.  Legal host names
   * must begin with a letter, and may contain any alphanumeric character,
   * the minus sign (-) and period (.), where the period is used as a
   * domain separator.  
   *)
    fun scanAddr getc strm =
	  PC.seqWith (fn (host, port) => {host=host, port=port}) (
	    PC.or (
	      PC.wrap (scanName, HostName),
	      PC.wrap (NetHostDB.scan, HostAddr)),
	    PC.option (
	      PC.seqWith #2 (
		PC.eatChar (fn c => (c = #":")),
		PC.or (
		  PC.wrap (scanName, ServName),
		  PC.wrap (Int.scan StringCvt.DEC, PortNumber))))) getc strm

    exception BadAddr of string

    fun resolveAddr {host, port} = let
	  fun err (a, b) = raise BadAddr(concat[a, " \"", b, "\" not found"])
	  val (name, addr) = (case host
		 of HostName s => (case NetHostDB.getByName s
		       of NONE => err ("hostname", s)
			| (SOME entry) => (s, NetHostDB.addr entry)
		      (* end case *))
		  | HostAddr addr => (case NetHostDB.getByAddr addr
		       of NONE => err ("host address", NetHostDB.toString addr)
			| (SOME entry) => (NetHostDB.name entry, addr)
		      (* end case *))
		(* end case *))
	  val port = (case port
		 of (SOME(PortNumber n)) => SOME n
		  | (SOME(ServName s)) => (case NetServDB.getByName(s, NONE)
		       of (SOME entry) => SOME(NetServDB.port entry)
			| NONE => err("service", s)
		      (* end case *))
		  | NONE => NONE
		(* end case *))
	  in
	    {host = name, addr = addr, port = port}
	  end

    type 'a stream_sock = ('a, Socket.active Socket.stream) Socket.sock

  (* establish a client-side connection to a INET domain stream socket *)
    fun connectINetStrm {addr, port} = let
	  val sock = INetSock.TCP.socket ()
	  in
	    Socket.connect (sock, INetSock.toAddr(addr, port));
	    sock
	  end

(** If the server closes the connection, do we get 0 bytes or an error??? **)
  (* read exactly n bytes from a stream socket *)
    fun recvVec (sock, n) = let
	  fun get (0, data) = Word8Vector.concat(rev data)
	    | get (n, data) = let
		val v = Socket.recvVec (sock, n)
		in
		  if (Word8Vector.length v = 0)
		    then raise OS.SysErr("closed socket", NONE)
		    else get (n - Word8Vector.length v, v::data)
		end
	  in
	    if (n < 0) then raise Size else get (n, [])
	  end

    fun recvStr arg = Byte.bytesToString (recvVec arg)

  (* send the complete contents of a vector *)
    fun sendVec (sock, vec) = let
	  val len = Word8Vector.length vec
	  fun send i = Socket.sendVec (sock, Word8VectorSlice.slice (vec, i, NONE))
	  fun put i = if (i < len)
		then put(i + send i)
		else ()
	  in
	    put 0
	  end

    fun sendStr (sock, str) = sendVec (sock, Byte.stringToBytes str)

  (* send the complete contents of an array *)
    fun sendArr (sock, arr) = let
	  val len = Word8Array.length arr
	  fun send i = Socket.sendArr (sock, Word8ArraySlice.slice (arr, i, NONE))
	  fun put i = if (i < len)
		then put(i + send i)
		else ()
	  in
	    put 0
	  end

  end;

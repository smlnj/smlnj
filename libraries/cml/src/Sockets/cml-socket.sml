(* cml-socket.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure CML_Socket : CML_SOCKET =
  struct
    structure PS = PreSock

    type 'a event = 'a CML.event

  (* sockets are polymorphic; the instantiation of the type variables
   * provides a way to distinguish between different kinds of sockets.
   *)
    type ('af, 'sock) sock = ('af, 'sock) PS.sock
    type 'af sock_addr = 'af Socket.sock_addr

  (* witness types for the socket parameter *)
    type dgram = Socket.dgram
    type 'a stream = 'a Socket.stream
    type passive = Socket.passive
    type active = Socket.active

  (* address families *)
    structure AF = Socket.AF

  (* socket types *)
    structure SOCK = Socket.SOCK

  (* socket control operations *)
    structure Ctl =
      struct
	fun wrapSet f (PS.CMLSock{sock, ...}, v) = f(sock, v)
	fun wrapGet f (PS.CMLSock{sock, ...}) = f sock

      (* get/set socket options *)
        fun getDEBUG arg = wrapGet Socket.Ctl.getDEBUG arg
        fun setDEBUG arg = wrapSet Socket.Ctl.setDEBUG arg
        fun getREUSEADDR arg = wrapGet Socket.Ctl.getREUSEADDR arg
        fun setREUSEADDR arg = wrapSet Socket.Ctl.setREUSEADDR arg
        fun getKEEPALIVE arg = wrapGet Socket.Ctl.getKEEPALIVE arg
        fun setKEEPALIVE arg = wrapSet Socket.Ctl.setKEEPALIVE arg
        fun getDONTROUTE arg = wrapGet Socket.Ctl.getDONTROUTE arg
        fun setDONTROUTE arg = wrapSet Socket.Ctl.setDONTROUTE arg
        fun getLINGER arg = wrapGet Socket.Ctl.getLINGER arg
        fun setLINGER arg = wrapSet Socket.Ctl.setLINGER arg
        fun getBROADCAST arg = wrapGet Socket.Ctl.getBROADCAST arg
        fun setBROADCAST arg = wrapSet Socket.Ctl.setBROADCAST arg
        fun getOOBINLINE arg = wrapGet Socket.Ctl.getOOBINLINE arg
        fun setOOBINLINE arg = wrapSet Socket.Ctl.setOOBINLINE arg
        fun getSNDBUF arg = wrapGet Socket.Ctl.getSNDBUF arg
        fun setSNDBUF arg = wrapSet Socket.Ctl.setSNDBUF arg
        fun getRCVBUF arg = wrapGet Socket.Ctl.getRCVBUF arg
        fun setRCVBUF arg = wrapSet Socket.Ctl.setRCVBUF arg
        fun getTYPE arg = wrapGet Socket.Ctl.getTYPE arg
        fun getERROR arg = wrapGet Socket.Ctl.getERROR arg
	fun getPeerName arg = wrapGet Socket.Ctl.getPeerName arg
	fun getSockName arg = wrapGet Socket.Ctl.getSockName arg
	fun getNREAD arg = wrapGet Socket.Ctl.getNREAD arg
	fun getATMARK arg = wrapGet Socket.Ctl.getATMARK arg
      end (* Ctl *)

  (* socket address operations *)
    val sameAddr     = Socket.sameAddr
    val familyOfAddr = Socket.familyOfAddr

  (* socket management *)
    local
      fun acceptNB' sock =
	  case Socket.acceptNB sock of
	      SOME (sock', addr) =>
	        SOME (PreSock.mkSock sock', addr)
	    | NONE => NONE
      fun accept' sock = let
	  val (sock', addr) = Socket.accept sock
      in
	  (PreSock.mkSock sock', addr)
      end
    in
    fun acceptEvt (s as PS.CMLSock{sock, ...}) = CML.guard (fn () =>
	  case acceptNB' sock
	   of (SOME res) => CML.alwaysEvt res
	    | NONE => CML.wrap((PreSock.inEvt s), fn _ => accept' sock)
	  (* end case *))

    fun accept (s as PS.CMLSock{sock, ...}) = (
	  case acceptNB' sock
	   of (SOME res) => res
	    | NONE => (CML.sync(PreSock.inEvt s); accept' sock)
	  (* end case *))
    end (* local *)

    fun bind (PS.CMLSock{sock, ...}, addr) = Socket.bind(sock, addr)

    fun connectEvt (s as PS.CMLSock{sock, ...}, addr) = CML.guard (fn () =>
	  if Socket.connectNB (sock, addr) then CML.alwaysEvt ()
	  else PreSock.outEvt s)

    fun connect (s as PS.CMLSock{sock, ...}, addr) =
	  if Socket.connectNB (sock, addr) then ()
	  else CML.sync (PreSock.outEvt s)

    fun listen (PS.CMLSock{sock, ...}, n) = Socket.listen(sock, n)

    fun close (PS.CMLSock{sock, state}) = (
	  case (SyncVar.mTake state)
	   of PS.Closed => ()
	    | _ => Socket.close sock
	  (* end case *);
	  SyncVar.mPut(state, PS.Closed))

    structure S' : sig
	datatype shutdown_mode = NO_RECVS | NO_SENDS | NO_RECVS_OR_SENDS
      end = Socket
    open S'
    fun shutdown (PS.CMLSock{sock, ...}, how) = Socket.shutdown(sock, how)

    type sock_desc = Socket.sock_desc
    fun ioDesc (PS.CMLSock{sock,...}) = Socket.ioDesc sock
    fun sockDesc (PS.CMLSock{sock,...}) = Socket.sockDesc sock
    val sameDesc = Socket.sameDesc
    val select = Socket.select

  (* Sock I/O option types *)
    type out_flags = {don't_route : bool, oob : bool}
    type in_flags = {peek : bool, oob : bool}

    type 'a buf = {buf : 'a, i : int, sz : int option}

  (* Sock output operations *)
    fun sendVec (s as PS.CMLSock{sock, ...}, buf) =
	case Socket.sendVecNB (sock, buf)
	 of (SOME res) => res
	  | NONE => (CML.sync(PS.outEvt s); Socket.sendVec (sock, buf))
    fun sendArr (s as PS.CMLSock{sock, ...}, buf) =
	case Socket.sendArrNB (sock, buf)
	 of (SOME res) => res
	  | NONE => (CML.sync(PS.outEvt s); Socket.sendArr (sock, buf))
    fun sendVec' (s as PS.CMLSock{sock, ...}, buf, flgs) =
	case Socket.sendVecNB' (sock, buf, flgs)
	 of (SOME res) => res
	  | NONE => (CML.sync(PS.outEvt s); Socket.sendVec' (sock, buf, flgs))
    fun sendArr' (s as PS.CMLSock{sock, ...}, buf, flgs) =
	case Socket.sendArrNB' (sock, buf, flgs)
	 of (SOME res) => res
	  | NONE => (CML.sync(PS.outEvt s); Socket.sendArr' (sock, buf, flgs))
    fun sendVecTo (s as PS.CMLSock{sock, ...}, addr, buf) =
	  if Socket.sendVecToNB (sock, addr, buf) then ()
	  else (CML.sync(PS.outEvt s); Socket.sendVecTo (sock, addr, buf))
    fun sendArrTo (s as PS.CMLSock{sock, ...}, addr, buf) =
	if Socket.sendArrToNB (sock, addr, buf) then ()
	else (CML.sync(PS.outEvt s); Socket.sendArrTo (sock, addr, buf))
    fun sendVecTo' (s as PS.CMLSock{sock, ...}, addr, buf, flgs) =
	if Socket.sendVecToNB' (sock, addr, buf, flgs) then ()
	else (CML.sync(PS.outEvt s); Socket.sendVecTo' (sock, addr, buf, flgs))
    fun sendArrTo' (s as PS.CMLSock{sock, ...}, addr, buf, flgs) =
	if Socket.sendArrToNB' (sock, addr, buf, flgs) then ()
	else (CML.sync(PS.outEvt s); Socket.sendArrTo' (sock, addr, buf, flgs))

  (* Sock input operations *)
    fun recvVec (s as PS.CMLSock{sock, ...}, n) =
	case Socket.recvVecNB (sock, n)
	 of (SOME res) => res
	  | NONE => (CML.sync(PS.inEvt s); Socket.recvVec (sock, n))
    fun recvArr (s as PS.CMLSock{sock, ...}, buf) =
	case Socket.recvArrNB (sock, buf)
	 of (SOME res) => res
	  | NONE => (CML.sync(PS.inEvt s); Socket.recvArr (sock, buf))
    fun recvVec' (s as PS.CMLSock{sock, ...}, n, flgs) =
	case Socket.recvVecNB' (sock, n, flgs)
	 of (SOME res) => res
	  | NONE => (CML.sync(PS.inEvt s); Socket.recvVec' (sock, n, flgs))
    fun recvArr' (s as PS.CMLSock{sock, ...}, buf, flgs) =
	case Socket.recvArrNB' (sock, buf, flgs)
	 of (SOME res) => res
	  | NONE => (CML.sync(PS.inEvt s); Socket.recvArr' (sock, buf, flgs))
    fun recvVecFrom (s as PS.CMLSock{sock, ...}, n) =
	case Socket.recvVecFromNB (sock, n)
	 of (SOME res) => res
	  | NONE => (CML.sync(PS.inEvt s); Socket.recvVecFrom (sock, n))
    fun recvArrFrom (s as PS.CMLSock{sock, ...}, buf) =
	case Socket.recvArrFromNB (sock, buf)
	 of (SOME res) => res
	  | NONE => (CML.sync(PS.inEvt s); Socket.recvArrFrom (sock, buf))
    fun recvVecFrom' (s as PS.CMLSock{sock, ...}, n, flgs) =
	case Socket.recvVecFromNB' (sock, n, flgs)
	 of (SOME res) => res
	  | NONE => (CML.sync(PS.inEvt s); Socket.recvVecFrom' (sock, n, flgs))
    fun recvArrFrom' (s as PS.CMLSock{sock, ...}, buf, flgs) =
	case Socket.recvArrFromNB' (sock, buf, flgs)
	 of (SOME res) => res
	  | NONE => (CML.sync(PS.inEvt s);
		     Socket.recvArrFrom' (sock, buf, flgs))

  (* Sock input event constructors *)
    fun recvVecEvt (s as PS.CMLSock{sock, ...}, n) = CML.guard (fn () =>
	  case Socket.recvVecNB (sock, n)
	   of (SOME res) => CML.alwaysEvt res
	    | NONE => CML.wrap(PS.inEvt s, fn _ => Socket.recvVec (sock, n))
	  (* end case *))
    fun recvArrEvt (s as PS.CMLSock{sock, ...}, buf) = CML.guard (fn () =>
	  case Socket.recvArrNB (sock, buf)
	   of (SOME res) => CML.alwaysEvt res
	    | NONE => CML.wrap(PS.inEvt s, fn _ => Socket.recvArr (sock, buf))
	  (* end case *))
    fun recvVecEvt' (s as PS.CMLSock{sock, ...}, n, flgs) = CML.guard (fn () =>
	  case Socket.recvVecNB' (sock, n, flgs)
	   of (SOME res) => CML.alwaysEvt res
	    | NONE => CML.wrap(PS.inEvt s, fn _ => Socket.recvVec' (sock, n, flgs))
	  (* end case *))
    fun recvArrEvt' (s as PS.CMLSock{sock, ...}, buf, flgs) = CML.guard (fn () =>
	  case Socket.recvArrNB' (sock, buf, flgs)
	   of (SOME res) => CML.alwaysEvt res
	    | NONE => CML.wrap(PS.inEvt s, fn _ => Socket.recvArr' (sock, buf, flgs))
	  (* end case *))
    fun recvVecFromEvt (s as PS.CMLSock{sock, ...}, n) = CML.guard (fn () =>
	  case Socket.recvVecFromNB (sock, n)
	   of (SOME res) => CML.alwaysEvt res
	    | NONE => CML.wrap(PS.inEvt s, fn _ => Socket.recvVecFrom (sock, n))
	  (* end case *))
    fun recvArrFromEvt (s as PS.CMLSock{sock, ...}, buf) = CML.guard (fn () =>
	  case Socket.recvArrFromNB (sock, buf)
	   of (SOME res) => CML.alwaysEvt res
	    | NONE => CML.wrap(PS.inEvt s, fn _ => Socket.recvArrFrom (sock, buf))
	  (* end case *))
    fun recvVecFromEvt' (s as PS.CMLSock{sock, ...}, n, flgs) = CML.guard (fn () =>
	  case Socket.recvVecFromNB' (sock, n, flgs)
	   of (SOME res) => CML.alwaysEvt res
	    | NONE =>
		CML.wrap(PS.inEvt s, fn _ => Socket.recvVecFrom' (sock, n, flgs))
	  (* end case *))
    fun recvArrFromEvt' (s as PS.CMLSock{sock, ...}, buf, flgs) = CML.guard (fn () =>
	  case Socket.recvArrFromNB' (sock, buf, flgs)
	   of (SOME res) => CML.alwaysEvt res
	    | NONE =>
		CML.wrap(PS.inEvt s, fn _ => Socket.recvArrFrom' (sock, buf, flgs))
	  (* end case *))

  end

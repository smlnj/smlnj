(* socket.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

local
    structure Int = IntImp
    structure OS = OSImp
in
structure SocketImp : SOCKET =
  struct

    structure CI = CInterface
    structure W8A = Word8Array
    structure W8V = Word8Vector

    fun sockFn x = CI.c_function "SMLNJ-Sockets" x

    type w8vector = W8V.vector
    type w8array = W8A.array

  (* the system's representation of a socket *)
    type sockFD = Socket.sockFD

  (* to inherit the various socket related types *)
    open Socket

  (* bind socket C functions *)
    fun netdbFun x = CI.c_function "SMLNJ-Sockets" x

(*    val dummyAddr = ADDR(W8V.fromList[]) *)

  (* address families *)
    structure AF =
      struct
        open AF
	val listAddrFamilies : unit -> CI.system_const list
	      = sockFn "listAddrFamilies"
	fun list () =
	      List.map (fn arg => (#2 arg, AF arg)) (listAddrFamilies ())
        fun toString (AF(_, name)) = name
	fun fromString name = (
	      case CI.findSysConst(name, listAddrFamilies ())
	       of NONE => NONE
		| (SOME af) => SOME(AF af)
	      (* end case *))
      end

  (* socket types *)
    structure SOCK =
      struct
        open SOCK
	val listSockTypes : unit -> CI.system_const list
	      = sockFn "listSockTypes"
	val stream = SOCKTY(CI.bindSysConst ("STREAM", listSockTypes ()))
	val dgram = SOCKTY(CI.bindSysConst ("DGRAM", listSockTypes ()))
	fun list () =
	      List.map (fn arg => (#2 arg, SOCKTY arg)) (listSockTypes ())
	fun toString (SOCKTY(_, name)) = name
	fun fromString name = (case CI.findSysConst(name, listSockTypes ())
	       of NONE => NONE
		| (SOME ty) => SOME(SOCKTY ty)
	      (* end case *))
      end

  (* socket control operations *)
    structure Ctl =
      struct
	local
	  fun getOpt ctlFn (Socket.SOCK { fd, ... }) = ctlFn(fd, NONE)
	  fun setOpt ctlFn (Socket.SOCK { fd, ... }, value) =
	        ignore(ctlFn(fd, SOME value))
	  val ctlDEBUG     : (sockFD * bool option) -> bool =
		sockFn "ctlDEBUG"
	  val ctlREUSEADDR : (sockFD * bool option) -> bool =
		sockFn "ctlREUSEADDR"
	  val ctlKEEPALIVE : (sockFD * bool option) -> bool =
		sockFn "ctlKEEPALIVE"
	  val ctlDONTROUTE : (sockFD * bool option) -> bool =
		sockFn "ctlDONTROUTE"
	  val ctlLINGER    : (sockFD * int option option) -> int option =
		sockFn "ctlLINGER"
	  val ctlBROADCAST : (sockFD * bool option) -> bool =
		sockFn "ctlBROADCAST"
	  val ctlOOBINLINE : (sockFD * bool option) -> bool =
		sockFn "ctlOOBINLINE"
	  val ctlSNDBUF    : (sockFD * int option) -> int =
		sockFn "ctlSNDBUF"
	  val ctlRCVBUF    : (sockFD * int option) -> int =
		sockFn "ctlSNDBUF"
	in
      (* get/set socket options *)
	fun getDEBUG x = getOpt ctlDEBUG x
	fun setDEBUG x = setOpt ctlDEBUG x
	fun getREUSEADDR x = getOpt ctlREUSEADDR x
	fun setREUSEADDR x = setOpt ctlREUSEADDR x
	fun getKEEPALIVE x = getOpt ctlKEEPALIVE x
	fun setKEEPALIVE x = setOpt ctlKEEPALIVE x
	fun getDONTROUTE x = getOpt ctlDONTROUTE x
	fun setDONTROUTE x = setOpt ctlDONTROUTE x
	fun getLINGER sock = (case (getOpt ctlLINGER sock)
	       of NONE => NONE
		| (SOME t) => SOME (TimeImp.fromSeconds (Int.toLarge t))
	      (* end case *))
(* NOTE: probably shoud do some range checking on the argument *)
	fun setLINGER (sock, NONE) = setOpt ctlLINGER (sock, NONE)
	  | setLINGER (sock, SOME t) =
	      setOpt ctlLINGER (sock,SOME(Int.fromLarge(TimeImp.toSeconds t)))
	fun getBROADCAST x = getOpt ctlBROADCAST x
	fun setBROADCAST x = setOpt ctlBROADCAST x
	fun getOOBINLINE x = getOpt ctlOOBINLINE x
	fun setOOBINLINE x = setOpt ctlOOBINLINE x
	fun getSNDBUF x = getOpt ctlSNDBUF x
(* NOTE: probably shoud do some range checking on the argument *)
	fun setSNDBUF x = setOpt ctlSNDBUF x
	fun getRCVBUF x = getOpt ctlRCVBUF x
(* NOTE: probably shoud do some range checking on the argument *)
	fun setRCVBUF x = setOpt ctlRCVBUF x
	local
	  val getTYPE'  : sockFD -> CI.system_const = sockFn "getTYPE"
	  val getERROR' : sockFD -> bool = sockFn "getERROR"
	in
        fun getTYPE (SOCK { fd, ... }) = SOCK.SOCKTY(getTYPE' fd)
        fun getERROR (SOCK { fd, ... }) = getERROR' fd
	end (* local *)

	local
	  val getPeerName' : sockFD -> addr = sockFn "getPeerName"
	  val getSockName' : sockFD -> addr = sockFn "getSockName"
	  fun getName f (SOCK { fd, ... }) = ADDR (f fd)
	in
	fun getPeerName	sock = getName getPeerName' sock
	fun getSockName	sock = getName getSockName' sock
	end

	local
	  val getNREAD'  : sockFD -> int = sockFn "getNREAD"
	  val getATMARK' : sockFD -> bool = sockFn "getATMARK"
	in
	fun getNREAD (SOCK { fd, ... }) = getNREAD' fd
	fun getATMARK (SOCK { fd, ... }) = getATMARK' fd
	end

	end (* local *)
      end (* Ctl *)


    val setNBIO'   : (sockFD * bool) -> unit = sockFn "setNBIO"
(*
    fun setNBIO (SOCK fd, flg) = setNBIO'(fd, flg)
*)

    (* extract a blocking file descriptor; implicitly set socket to
     * blocking mode if necessary: *)
    fun fdB (SOCK { fd, nb = nbr as ref nb }) =
	if nb then (setNBIO' (fd, false); nbr := false; fd) else fd

    (* same for non-blocking *)
    fun fdNB (SOCK{fd, nb = nbr as ref nb }) =
	if nb then fd else (setNBIO' (fd, true); nbr := true; fd)

    val wrapNB_o = OpsysDetails.wrapNB_o
    val wrapNB_b = OpsysDetails.wrapNB_b

    fun sockB fd = SOCK{ fd = fd, nb = ref false }

    (* socket address operations *)
    fun sameAddr (ADDR a1, ADDR a2) = (a1 = a2)
    local
	val getAddrFamily : addr -> af = sockFn "getAddrFamily"
    in
    fun familyOfAddr (ADDR a) = AF.AF(getAddrFamily a)
    end

    (* socket management *)
    local
	val accept'	: int -> (int * addr)	= sockFn "accept"
	val bind'	: (int * addr) -> unit	= sockFn "bind"
	val connect'	: (int * addr) -> unit	= sockFn "connect"
	val listen'	: (int * int) -> unit	= sockFn "listen"
	val close'	: int -> unit		= sockFn "close"
    in

    fun bind (SOCK{ fd, ... }, ADDR addr) = bind' (fd, addr)
(** Should do some range checking on backLog *)
    fun listen (SOCK{ fd, ... }, backLog) = listen' (fd, backLog)

    fun accept0 (sock, getfd) s = let
	  val (newFD, addr) = accept' (getfd s)
	  in
	    (sock newFD, ADDR addr)
	  end
    fun accept s = accept0 (sockB, fdB) s
    local
    (*
     * On Windows, sockets returned from accept inherit their parents' non-blocking
     * status, unlike normal UNIX sockets, which are always set to blocking.
     *)
      val sockIsNB = (SysInfo.WIN32 = SysInfo.getOSKind())
    in
    fun acceptNB s = let
          val result = wrapNB_o (accept0 (sockB, fdNB)) s
          in
            case result
             of SOME(SOCK{nb, ...},_) => (nb := sockIsNB)
              | NONE => ()
            (* end case *);
            result
          end
    end (* local *)

    fun connect0 getfd (s, ADDR addr) = connect' (getfd s, addr)
    fun connect arg = connect0 fdB arg
    fun connectNB arg = wrapNB_b (connect0 fdNB) arg

    fun close (SOCK { fd, ... }) = close' fd
    end

    local
      val shutdown' : (int * int) -> unit = sockFn "shutdown"
      fun how NO_RECVS = 0
	| how NO_SENDS = 1
	| how NO_RECVS_OR_SENDS = 2
    in
    fun shutdown (SOCK{ fd, ... }, mode) = shutdown' (fd, how mode)
    end

    fun ioDesc (SOCK{ fd, ... }) = OpsysDetails.mkIODesc fd

    fun pollDesc sock = Option.valOf (OS.IO.pollDesc (ioDesc sock)) (** delete **)
    (* for now we implement select in terms of polling... *)
    val sockDesc = ioDesc
    fun sameDesc (d1, d2) = OS.IO.compare (d1, d2) = EQUAL
    fun select { rds, wrs, exs, timeout } = let
	fun rd d = Option.map OS.IO.pollIn (OS.IO.pollDesc d)
		   handle OS.IO.Poll => NONE
	fun wr d = Option.map OS.IO.pollOut (OS.IO.pollDesc d)
		   handle OS.IO.Poll => NONE
	fun ex d = Option.map OS.IO.pollPri (OS.IO.pollDesc d)
		   handle OS.IO.Poll => NONE
	val dl =
	    List.mapPartial rd rds @
	    List.mapPartial wr wrs @
	    List.mapPartial ex exs
	val il = OS.IO.poll (dl, timeout)
	fun split3 ([], rds, wrs, exs) = { rds = rds, wrs = wrs, exs = exs }
	  | split3 (i :: is, rds, wrs, exs) = let
		val d = OS.IO.pollToIODesc (OS.IO.infoToPollDesc i)
	    in
		if OS.IO.isIn i then split3 (is, d :: rds, wrs, exs)
		else if OS.IO.isOut i then split3 (is, rds, d :: wrs, exs)
		else split3 (is, rds, wrs, d :: exs)
	    end
    in
	split3 (rev il, [], [], [])
    end

    val vbuf = Word8VectorSlice.base
    val abuf = Word8ArraySlice.base

  (* default flags *)
    val dfltDon'tRoute = false
    val dfltOOB = false
    val dfltPeek = false

  (* Sock output operations *)
    local
      val sendV : (int * w8vector * int * int * bool * bool) -> int
	    = sockFn "sendBuf"
      val sendA : (int * w8array * int * int * bool * bool) -> int
	    = sockFn "sendBuf"
    in

    fun sendVec0 getfd (s, buffer) = let
	val fd = getfd s
	val (vec, i, len) = vbuf buffer
    in
	if (len > 0) then sendV (fd, vec, i, len, dfltDon'tRoute, dfltOOB) else 0
    end
    fun sendVec arg = sendVec0 fdB arg
    fun sendVecNB arg = wrapNB_o (sendVec0 fdNB) arg

    fun sendVec'0 getfd (sock, buffer, {don't_route, oob}) = let
	val fd = getfd sock
	val (vec, i, len) = vbuf buffer
    in
	if (len > 0) then sendV (fd, vec, i, len, don't_route, oob) else 0
    end
    fun sendVec' arg = sendVec'0 fdB arg
    fun sendVecNB' arg = wrapNB_o (sendVec'0 fdNB) arg

    fun sendArr0 getfd (sock, buffer) = let
	val fd = getfd sock
	val (arr, i, len) = abuf buffer
    in
	if (len > 0) then sendA (fd, arr, i, len, dfltDon'tRoute, dfltOOB)
	else 0
    end
    fun sendArr arg = sendArr0 fdB arg
    fun sendArrNB arg = wrapNB_o (sendArr0 fdNB) arg

    fun sendArr'0 getfd (sock, buffer, {don't_route, oob}) = let
	val fd = getfd sock
	val (arr, i, len) = abuf buffer
    in
	if (len > 0) then sendA (fd, arr, i, len, don't_route, oob) else 0
    end
    fun sendArr' arg = sendArr'0 fdB arg
    fun sendArrNB' arg = wrapNB_o (sendArr'0 fdNB) arg
    end (* local *)

    local
      val sendToV : (int * w8vector * int * int * bool * bool * addr) -> int
	    = sockFn "sendBufTo"
      val sendToA : (int * w8array * int * int * bool * bool * addr) -> int
	    = sockFn "sendBufTo"
    in
    fun sendVecTo0 getfd (sock, ADDR addr, buffer) = let
	val fd = getfd sock
	val (vec, i, len) = vbuf buffer
    in
	if (len > 0) then
	    sendToV(fd, vec, i, len, dfltDon'tRoute, dfltOOB, addr)
	else 0;
	()
    end
    fun sendVecTo arg = sendVecTo0 fdB arg
    fun sendVecToNB arg = wrapNB_b (sendVecTo0 fdNB) arg

    fun sendVecTo'0 getfd (sock, ADDR addr, buffer, {don't_route, oob}) = let
	val fd = getfd sock
	val (vec, i, len) = vbuf buffer
    in
	if (len > 0) then
	    sendToV(fd, vec, i, len, don't_route, oob, addr)
	else 0;
	()
    end
    fun sendVecTo' arg = sendVecTo'0 fdB arg
    fun sendVecToNB' arg = wrapNB_b (sendVecTo'0 fdNB) arg

    fun sendArrTo0 getfd (sock, ADDR addr, buffer) = let
	val fd = getfd sock
	val (arr, i, len) = abuf buffer
    in
	if (len > 0) then
	    sendToA(fd, arr, i, len, dfltDon'tRoute, dfltOOB, addr)
	else 0;
	()
    end
    fun sendArrTo arg = sendArrTo0 fdB arg
    fun sendArrToNB arg = wrapNB_b (sendArrTo0 fdNB) arg

    fun sendArrTo'0 getfd (sock, ADDR addr, buffer, {don't_route, oob}) = let
	val fd = getfd sock
	val (arr, i, len) = abuf buffer
    in
	if (len > 0) then
	    sendToA(fd, arr, i, len, don't_route, oob, addr)
	else 0;
	()
    end
    fun sendArrTo' arg = sendArrTo'0 fdB arg
    fun sendArrToNB' arg = wrapNB_b (sendArrTo'0 fdNB) arg
    end (* local *)

  (* Sock input operations *)
    local
      val recvV' : (int * int * bool * bool) -> w8vector
	    = sockFn "recv"
      fun recvV _ (_, 0, _, _) = W8V.fromList[]
	| recvV getfd (sock, nbytes, peek, oob) =
	    if nbytes < 0 then raise Size
	    else recvV' (getfd sock, nbytes, peek, oob)
      val recvA : (int * w8array * int * int * bool * bool) -> int
	    = sockFn "recvBuf"
    in
    fun recvVec0 getfd (sock, sz) = recvV getfd (sock, sz, dfltPeek, dfltOOB)
    fun recvVec arg = recvVec0 fdB arg
    fun recvVecNB arg = wrapNB_o (recvVec0 fdNB) arg

    fun recvVec'0 getfd (sock, sz, {peek, oob}) =
	recvV getfd (sock, sz, peek, oob)
    fun recvVec' arg = recvVec'0 fdB arg
    fun recvVecNB' arg = wrapNB_o (recvVec'0 fdNB) arg

    fun recvArr0 getfd (sock, buffer) = let
	val fd = getfd sock
	val (buf, i, sz) = abuf buffer
    in
	if sz > 0 then recvA (fd, buf, i, sz, dfltPeek, dfltOOB) else 0
    end
    fun recvArr arg = recvArr0 fdB arg
    fun recvArrNB arg = wrapNB_o (recvArr0 fdNB) arg

    fun recvArr'0 getfd (sock, buffer, {peek, oob}) = let
	val fd = getfd sock
	val (buf, i, sz) = abuf buffer
    in
	if sz > 0 then recvA (fd, buf, i, sz, peek, oob) else 0
    end
    fun recvArr' arg = recvArr'0 fdB arg
    fun recvArrNB' arg = wrapNB_o (recvArr'0 fdNB) arg
    end (* local *)

    local
      val recvFromV' : (int * int * bool * bool) -> (w8vector * addr)
	    = sockFn "recvFrom"
      fun recvFromV _ (_, 0, _, _) = (W8V.fromList[], (ADDR(W8V.fromList[])))
	| recvFromV getfd (sock, sz, peek, oob) =
	  if sz < 0 then raise Size
	  else let val fd = getfd sock
		   val (data, addr) = recvFromV' (fd, sz, peek, oob)
	       in
		   (data, ADDR addr)
	       end
      val recvFromA : (int * w8array * int * int * bool * bool) -> (int * addr)
	  = sockFn "recvBufFrom"
    in
    fun recvVecFrom0 getfd (sock, sz) =
	recvFromV getfd (sock, sz, dfltPeek, dfltOOB)
    fun recvVecFrom arg = recvVecFrom0 fdB arg
    fun recvVecFromNB arg = wrapNB_o (recvVecFrom0 fdNB) arg

    fun recvVecFrom'0 getfd (sock, sz, {peek, oob}) =
	recvFromV getfd (sock, sz, peek, oob)
    fun recvVecFrom' arg = recvVecFrom'0 fdB arg
    fun recvVecFromNB' arg = wrapNB_o (recvVecFrom'0 fdNB) arg

    fun recvArrFrom0 getfd (sock, asl) = let
	val fd = getfd sock
	val (buf, i, sz) = abuf asl
    in
	if sz > 0 then let
		val (n, addr) = recvFromA(fd, buf, i, sz, dfltPeek, dfltOOB)
	    in
		(n, ADDR addr)
	    end
	else (0, ADDR(W8V.fromList[]))
    end
    fun recvArrFrom arg = recvArrFrom0 fdB arg
    fun recvArrFromNB arg = wrapNB_o (recvArrFrom0 fdNB) arg

    fun recvArrFrom'0 getfd (sock, asl, {peek, oob}) = let
	val fd = getfd sock
	val (buf, i, sz) = abuf asl
    in
	if sz > 0 then let
		val (n, addr) = recvFromA(fd, buf, i, sz, peek, oob)
	    in
		(n, ADDR addr)
	    end
	else (0, (ADDR(W8V.fromList[])))
    end
    fun recvArrFrom' arg = recvArrFrom'0 fdB arg
    fun recvArrFromNB' arg = wrapNB_o (recvArrFrom'0 fdNB) arg
    end (* local *)

  end (* Socket *)
end

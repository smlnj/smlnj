(* posix-tty.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Structure for POSIX 1003.1 operations on terminal devices
 *)

local
  structure SysWord = SysWordImp
in
structure POSIX_TTY =
  struct

    structure FS = POSIX_FileSys
    structure P = POSIX_Process

    type pid = POSIX_Process.pid
    type file_desc = POSIX_FileSys.file_desc

    type s_word = SysWord.word
    type s_int = SysInt.int

    val ++ = SysWord.orb
    val & = SysWord.andb
    infix ++ &

    fun cfun x = CInterface.c_function "POSIX-TTY" x
    val osval : string -> s_int = cfun "osval"
    val w_osval = SysWord.fromInt o osval

   (* for now, we will protect against the function not being defined in the runtime
    * system, but this code can be simplified.
    *)
    val getWindowSz : file_desc -> { nLines : int, nCols : int } option = (
          (* try *)
            let
            val getwinsz : int -> (int * int) option =
                  CInterface.c_function "POSIX-TTY" "getwinsz"
            in
              fn (POSIX_FileSys.FD{fd}) => (case getwinsz fd
                 of SOME(nr, nc) => SOME{nLines=nr, nCols=nc}
                  | NONE => NONE
                (* end case *))
            end)
              handle CInterface.CFunNotFound _ => (fn _ => NONE)

    structure V =
      struct
        structure WV = Word8Vector
        structure WA = Word8Array
        structure B = Byte

        val nccs = osval "NCCS"

        val eof   = (osval "EOF")
        val eol   = (osval "EOL")
        val erase = (osval "ERASE")
        val intr  = (osval "INTR")
        val kill  = (osval "KILL")
        val min   = (osval "MIN")
        val quit  = (osval "QUIT")
        val susp  = (osval "SUSP")
        val time  = (osval "TIME")
        val start = (osval "START")
        val stop  = (osval "STOP")

        datatype cc = CC of WV.vector

        fun mkCC (arr, l) = let
              fun update (i, c) = WA.update(arr, i, B.charToByte c)
              in
                List.app update l;
                CC (WA.vector arr)
              end

        fun cc vals = mkCC (WA.array(nccs, 0w0), vals)
        fun update (CC v, vals) =
              mkCC (WA.tabulate (nccs, fn i => WV.sub(v,i)), vals)
        fun sub (CC v, i) = B.byteToChar (WV.sub(v,i))
      end

    structure I =
      struct
        local structure BF = BitFlagsFn ()
	in
	    open BF
	end

        val brkint = fromWord (w_osval "BRKINT")
        val icrnl  = fromWord (w_osval "ICRNL")
        val ignbrk = fromWord (w_osval "IGNBRK")
        val igncr  = fromWord (w_osval "IGNCR")
        val ignpar = fromWord (w_osval "IGNPAR")
        val inlcr  = fromWord (w_osval "INLCR")
        val inpck  = fromWord (w_osval "INPCK")
        val istrip = fromWord (w_osval "ISTRIP")
        val ixoff  = fromWord (w_osval "IXOFF")
        val ixon   = fromWord (w_osval "IXON")
        val parmrk = fromWord (w_osval "PARMRK")
      end

    structure O =
      struct
        local structure BF = BitFlagsFn ()
	in
	    open BF
	end

        val opost = fromWord (w_osval "OPOST")
      end

    structure C =
      struct
        local structure BF = BitFlagsFn ()
	in
	    open BF
	end

        val clocal = fromWord (w_osval "CLOCAL")
        val cread  = fromWord (w_osval "CREAD")
        val csize  = fromWord (w_osval "CSIZE")
        val cs5    = fromWord (w_osval "CS5")
        val cs6    = fromWord (w_osval "CS6")
        val cs7    = fromWord (w_osval "CS7")
        val cs8    = fromWord (w_osval "CS8")
        val cstopb = fromWord (w_osval "CSTOPB")
        val hupcl  = fromWord (w_osval "HUPCL")
        val parenb = fromWord (w_osval "PARENB")
        val parodd = fromWord (w_osval "PARODD")
      end

    structure L =
      struct
        local structure BF = BitFlagsFn ()
	in
	    open BF
	end

        val echo   = fromWord (w_osval "ECHO")
        val echoe  = fromWord (w_osval "ECHOE")
        val echok  = fromWord (w_osval "ECHOK")
        val echonl = fromWord (w_osval "ECHONL")
        val icanon = fromWord (w_osval "ICANON")
        val iexten = fromWord (w_osval "IEXTEN")
        val isig   = fromWord (w_osval "ISIG")
        val noflsh = fromWord (w_osval "NOFLSH")
        val tostop = fromWord (w_osval "TOSTOP")
      end

    datatype speed = B of s_word
    fun compareSpeed (B w, B w') =
          if SysWord.<(w, w') then LESS
          else if w = w' then EQUAL
          else GREATER
    fun speedToWord (B w) = w
    fun wordToSpeed w = B w
    val b0 = B (w_osval "B0")
    val b50 = B (w_osval "B50")
    val b75 = B (w_osval "B75")
    val b110 = B (w_osval "B110")
    val b134 = B (w_osval "B134")
    val b150 = B (w_osval "B150")
    val b200 = B (w_osval "B200")
    val b300 = B (w_osval "B300")
    val b600 = B (w_osval "B600")
    val b1200 = B (w_osval "B1200")
    val b1800 = B (w_osval "B1800")
    val b2400 = B (w_osval "B2400")
    val b4800 = B (w_osval "B4800")
    val b9600 = B (w_osval "B9600")
    val b19200 = B (w_osval "B19200")
    val b38400 = B (w_osval "B38400")

    datatype termios = TIOS of {
        iflag : I.flags,
        oflag : O.flags,
        cflag : C.flags,
        lflag : L.flags,
        cc : V.cc,
        ispeed : speed,
        ospeed : speed
      }

    fun termios arg = TIOS arg
    fun fieldsOf (TIOS arg) = arg
    fun getiflag (TIOS{iflag, ...}) = iflag
    fun getoflag (TIOS{oflag, ...}) = oflag
    fun getcflag (TIOS{cflag, ...}) = cflag
    fun getlflag (TIOS{lflag, ...}) = lflag
    fun getcc (TIOS{cc,...}) = cc

    structure CF =
      struct
	fun getospeed (TIOS{ospeed,...}) = ospeed
	fun getispeed (TIOS{ispeed,...}) = ispeed

	fun setospeed (TIOS r, ospeed) = TIOS {
		iflag = #iflag r,
		oflag = #oflag r,
		cflag = #cflag r,
		lflag = #lflag r,
		cc = #cc r,
		ispeed = #ispeed r,
		ospeed = ospeed
	      }
	fun setispeed (TIOS r, ispeed) = TIOS {
		iflag = #iflag r,
		oflag = #oflag r,
		cflag = #cflag r,
		lflag = #lflag r,
		cc = #cc r,
		ispeed = ispeed,
		ospeed = #ospeed r
	      }
      end

    structure TC =
      struct
        datatype set_action = SA of s_int

        val sanow = SA (osval "TCSANOW")
        val sadrain = SA (osval "TCSADRAIN")
        val saflush = SA (osval "TCSAFLUSH")

        datatype flow_action = FA of s_int

        val ooff = FA (osval "TCOOFF")
        val oon = FA (osval "TCOON")
        val ioff = FA (osval "TCIOFF")
        val ion = FA (osval "TCION")

        datatype queue_sel = QS of s_int

        val iflush = QS (osval "TCIFLUSH")
        val oflush = QS (osval "TCOFLUSH")
        val ioflush = QS (osval "TCIOFLUSH")

	type termio_rep = (
	       s_word *       	(* iflags *)
	       s_word *       	(* oflags *)
	       s_word *       	(* cflags *)
	       s_word *       	(* lflags *)
	       V.WV.vector *	(* cc *)
	       s_word *		(* inspeed *)
	       s_word		(* outspeed *)
	     )

	val tcgetattr : int -> termio_rep = cfun "tcgetattr"
	fun getattr fd = let
	      val (ifs,ofs,cfs,lfs,cc,isp,osp) = tcgetattr (FS.intOf fd)
	      in
		TIOS {
		  iflag = I.fromWord ifs,
		  oflag = O.fromWord ofs,
		  cflag = C.fromWord cfs,
		  lflag = L.fromWord lfs,
		  cc = V.CC cc,
		  ispeed = B isp,
		  ospeed = B osp
		}
	      end

	val tcsetattr : int * s_int * termio_rep -> unit = cfun "tcsetattr"
	fun setattr (fd, SA sa, TIOS tios) = let
	      val iflag = I.toWord (#iflag tios)
	      val oflag = O.toWord (#oflag tios)
	      val cflag = C.toWord (#cflag tios)
	      val lflag = L.toWord (#lflag tios)
	      val (V.CC cc) = #cc tios
	      val (B ispeed) = #ispeed tios
	      val (B ospeed) = #ospeed tios
	      val trep = (iflag,oflag,cflag,lflag,cc,ispeed,ospeed)
	      in
		tcsetattr (FS.intOf fd, sa, trep)
	      end

	val tcsendbreak : int * int -> unit = cfun "tcsendbreak"
	fun sendbreak (fd, duration) = tcsendbreak (FS.intOf fd, duration)

	val tcdrain : int -> unit = cfun "tcdrain"
	fun drain fd = tcdrain (FS.intOf fd)

	val tcflush : int * s_int -> unit = cfun "tcflush"
	fun flush (fd, QS qs) = tcflush (FS.intOf fd, qs)

	val tcflow : int * s_int -> unit = cfun "tcflow"
	fun flow (fd, FA action) = tcflow (FS.intOf fd, action)

	val tcgetpgrp : int -> s_int = cfun "tcgetpgrp"
	fun getpgrp fd = P.PID(tcgetpgrp(FS.intOf fd))

	val tcsetpgrp : int * s_int -> unit = cfun "tcsetpgrp"
	fun setpgrp (fd, P.PID pid) = tcsetpgrp(FS.intOf fd, pid)
      end (* structure TC *)

  end (* structure POSIX_TTY *)

end (* local *)

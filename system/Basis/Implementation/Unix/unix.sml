(* unix.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Unix : UNIX =
  struct

    structure P = Posix.Process
    structure PS = POSIX_Signal
    structure PE = Posix.ProcEnv
    structure PF = Posix.FileSys
    structure PIO = Posix.IO
    structure SS = Substring

    type signal = PS.signal
    datatype exit_status = datatype P.exit_status

    datatype 'stream stream =
	UNOPENED of PIO.file_desc
      | OPENED of { stream: 'stream, close: unit -> unit }

    datatype proc_status =
	DEAD of OS.Process.status
      | ALIVE of P.pid

    datatype ('instream, 'outstream) proc =
	PROC of { base: string,
		  instream: 'instream stream ref,
		  outstream: 'outstream stream ref,
		  status: proc_status ref }

    val fromStatus = P.fromStatus

    fun protect f x = let
          val _ = Signals.maskSignals Signals.MASKALL
          val y = (f x) handle ex => 
                    (Signals.unmaskSignals Signals.MASKALL; raise ex)
          in
            Signals.unmaskSignals Signals.MASKALL; y
          end

    fun fdTextReader (name : string, fd : PIO.file_desc) =
	  PosixTextPrimIO.mkReader {
              initBlkMode = true,
              name = name,
              fd = fd
            }

    fun fdBinReader (name : string, fd : PIO.file_desc) =
	  PosixBinPrimIO.mkReader {
              initBlkMode = true,
              name = name,
              fd = fd
            }

    fun fdTextWriter (name, fd) =
          PosixTextPrimIO.mkWriter {
	      appendMode = false,
              initBlkMode = true,
              name = name,
              chunkSize=4096,
              fd = fd
            }

    fun fdBinWriter (name, fd) =
          PosixBinPrimIO.mkWriter {
	      appendMode = false,
              initBlkMode = true,
              name = name,
              chunkSize=4096,
              fd = fd
            }

    fun openTextOutFD (name, fd) =
	  TextIO.mkOutstream (
	    TextIO.StreamIO.mkOutstream (
	      fdTextWriter (name, fd), IO.BLOCK_BUF))

    fun openBinOutFD (name, fd) =
	  BinIO.mkOutstream (
	    BinIO.StreamIO.mkOutstream (
	      fdBinWriter (name, fd), IO.BLOCK_BUF))

    fun openTextInFD (name, fd) =
	  TextIO.mkInstream (
	    TextIO.StreamIO.mkInstream (
	      fdTextReader (name, fd), ""))

    fun openBinInFD (name, fd) =
	  BinIO.mkInstream (
	    BinIO.StreamIO.mkInstream (
	      fdBinReader (name, fd), Byte.stringToBytes ""))

    fun streamOf (sel, sfx, opener, closer) (PROC p) =
	case sel p of
	    ref (OPENED s) => #stream s
	  | r as ref (UNOPENED fd) =>
	      let val s = opener (#base p ^ "_ext_" ^ sfx, fd)
	      in
		  r := OPENED { stream = s, close = fn () => closer s };
		  s
	      end

    fun textInstreamOf p =
	streamOf (#instream, "txt_in", openTextInFD, TextIO.closeIn) p
    fun binInstreamOf p =
	streamOf (#instream, "bin_in", openBinInFD, BinIO.closeIn) p
    fun textOutstreamOf p =
	streamOf (#outstream, "txt_out", openTextOutFD, TextIO.closeOut) p
    fun binOutstreamOf p =
	streamOf (#outstream, "bin_out", openBinOutFD, BinIO.closeOut) p

    fun streamsOf p = (textInstreamOf p, textOutstreamOf p)

    fun executeInEnv (cmd, argv, env) =
	let val p1 = PIO.pipe ()
            val p2 = PIO.pipe ()
            fun closep () =
		(PIO.close (#outfd p1); 
                 PIO.close (#infd p1);
                 PIO.close (#outfd p2); 
                 PIO.close (#infd p2))
            val base = SS.string(SS.taker (fn c => c <> #"/") (SS.full cmd))
            fun startChild () =
		case protect P.fork () of
		    SOME pid =>  pid           (* parent *)
		  | NONE =>
		    let val oldin = #infd p1
			val newin = Posix.FileSys.wordToFD 0w0
			val oldout = #outfd p2
			val newout = Posix.FileSys.wordToFD 0w1
                    in
			PIO.close (#outfd p1);
			PIO.close (#infd p2);
			if (oldin = newin) then ()
			else (PIO.dup2{old = oldin, new = newin};
                              PIO.close oldin);
			if (oldout = newout) then ()
			else (PIO.dup2{old = oldout, new = newout};
                              PIO.close oldout);
			P.exece (cmd, base::argv, env)
		    end
            val _ = TextIO.flushOut TextIO.stdOut
            val pid = (startChild ()) handle ex => (closep(); raise ex)
	    val infd = #infd p2
	    val outfd = #outfd p1
	in
            (* close the child-side fds *)
            PIO.close (#outfd p2);
            PIO.close (#infd p1);
            (* set the fds close on exec *)
            PIO.setfd (#infd p2, PIO.FD.flags [PIO.FD.cloexec]);
            PIO.setfd (#outfd p1, PIO.FD.flags [PIO.FD.cloexec]);
	    PROC { base = base,
		   instream = ref (UNOPENED infd),
		   outstream = ref (UNOPENED outfd),
		   status = ref (ALIVE pid) }
	end

    fun execute (cmd, argv) = executeInEnv (cmd, argv, PE.environ())

    fun kill (PROC { status = ref (ALIVE pid), ... }, signal) =
	  P.kill (P.K_PROC pid, signal)
      | kill _ = ()			(* raise an exception here? *)

    fun reap (PROC { status = ref (DEAD s), ... }) = s
      | reap (PROC { status = status as ref (ALIVE pid), instream, outstream, ... }) =
	let
            (* protect is probably too much; typically, one
             * would only mask SIGINT, SIGQUIT and SIGHUP
             *)
            fun waitProc () =
		case #2(protect P.waitpid (P.W_CHILD pid,[])) of
		    W_EXITED => 0
		  | W_EXITSTATUS s => Word8Imp.toInt s
		  | W_SIGNALED (PS.SIG s) => 256 + s
		  | W_STOPPED (PS.SIG s) => (* should not happen! *) 512 + s
	    fun close (UNOPENED fd) = PIO.close fd
	      | close (OPENED s) = #close s ()
	    val _ = close (!instream)
	    val _ = close (!outstream) handle _ => ()
	    val s = waitProc ()
        in
	    status := DEAD s;
	    s
        end

    val exit = P.exit

  end (* structure Unix *)

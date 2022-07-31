(* windows.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Structure for the interface to Windows.
 *)

structure Windows : WINDOWS =
  struct
    structure Key = Windows_KEY
    structure Reg = Windows_REG
    structure Config = Windows_CONFIG
    structure DDE = Windows_DDE
    structure Status = Windows_STATUS
    structure W32G = Win32.General

    fun cfun x = CInterface.c_function "WIN32" x
    fun cfunProc x = CInterface.c_function "WIN32-PROCESS" x

    fun getVolumeInformation driveWithTrailingSlash = let
	  val getVolumeInformation : string -> string*string*SysWord.word*int = cfun "config_get_volume_information"
	  val (vol,sys,serial,cl) = getVolumeInformation driveWithTrailingSlash
	  in
	    {volumeName=vol, systemName=sys, serialNumber=serial, maximumComponentLength=cl}

	  end
    val findExecutable : string -> string option = cfunProc "find_executable"
    val launchApplication : string * string -> unit = cfunProc "launch_application"
    val openDocument : string -> unit = cfunProc "open_document"

    val waitForSingleObject : Handle.t -> W32G.word option = cfunProc "wait_for_single_object"
    fun loopingSleepingWait procHandle = (case waitForSingleObject procHandle
	   of NONE => (
		OS_Process.sleep (TimeImp.fromMilliseconds 100);
		loopingSleepingWait procHandle)
	    | SOME x => x
	  (* end case *))

    local
      val createProcess : string -> Handle.t = cfunProc "create_process"
    in
    fun simpleExecute (cmd, arg) = let
	  val createProcess = cfunProc "create_process"
	  val procHandle = createProcess (StringImp.concat[cmd, " ", arg])
	  in
	    loopingSleepingWait procHandle
	  end
    end (* local *)

    (*val fromStatus : OS.Process.status -> Status.status *)
    fun fromStatus(status) = if (OS_Process.failure = status) then Status.timeout else status

    (* val exit : Status.status -> 'a = cfunProc "exit_process" *)
    val exit = OS_Process.exit


    (* Redirected I/O process support *)
    datatype proc_status
      = DEAD of OS.Process.status
      | ALIVE of Handle.t

    datatype 'stream stream
      = UNOPENED of Handle.t
      | OPENED of { stream: 'stream, close: unit -> unit }

    datatype ('a, 'b) proc = PROC of {
	instream: 'a stream ref,
	outstream: 'b stream ref,
	status: proc_status ref
      }

    (* val execute : string * string -> ('a, 'b) proc *)
    local
      val cpRedirect : string -> Handle.t * Handle.t * Handle.t =
	    cfunProc "create_process_redirect_handles"
    in
    fun execute (cmd, arg) = let
	  val (hProcess, hIn, hOut) = cpRedirect (StringImp.concat[cmd, " ", arg])
	  in
	    PROC{
		instream = ref (UNOPENED(hIn)),
		outstream = ref (UNOPENED(hOut)),
		status = ref (ALIVE hProcess)
	      }
	  end
    end (* local *)

    fun hndlTextReader (name : string, hndl : W32G.hndl) =
	  Win32TextPrimIO.mkReader {
              initBlkMode = true,
              name = name,
              fd = hndl
            }

    fun hndlBinReader (name : string, hndl : W32G.hndl) =
	  Win32BinPrimIO.mkReader {
              initBlkMode = true,
              name = name,
              fd = hndl
            }

    fun hndlTextWriter (name, hndl) =
          Win32TextPrimIO.mkWriter {
	      appendMode = false,
              initBlkMode = true,
              name = name,
              chunkSize=4096,
              fd = hndl
            }

    fun hndlBinWriter (name, hndl) =
          Win32BinPrimIO.mkWriter {
	      appendMode = false,
              initBlkMode = true,
              name = name,
              chunkSize=4096,
              fd = hndl
            }

    fun openTextOutHNDL (name, hndl) =
	  TextIO.mkOutstream (
	    TextIO.StreamIO.mkOutstream (
	      hndlTextWriter (name, hndl), IO.BLOCK_BUF))

    fun openBinOutHNDL (name, hndl) =
	  BinIO.mkOutstream (
	    BinIO.StreamIO.mkOutstream (
	      hndlBinWriter (name, hndl), IO.BLOCK_BUF))

    fun openTextInHNDL (name, hndl) =
	  TextIO.mkInstream (
	    TextIO.StreamIO.mkInstream (
	      hndlTextReader (name, hndl), ""))

    fun openBinInHNDL (name, hndl) =
	  BinIO.mkInstream (
	    BinIO.StreamIO.mkInstream (
	      hndlBinReader (name, hndl), Byte.stringToBytes ""))

    fun streamOf (sel, sfx, opener, closer) (PROC p) = (case sel p
	   of ref(OPENED s) => #stream s
	    | r as ref (UNOPENED hndl) => let
		val s = opener (sfx, hndl)
		in
		  r := OPENED { stream = s, close = fn () => closer s };
		  s
		end
	  (* end case *))

    fun textInstreamOf p =
	  streamOf (#instream, "txt_in", openTextInHNDL, TextIO.closeIn) p
    fun binInstreamOf p =
	  streamOf (#instream, "bin_in", openBinInHNDL, BinIO.closeIn) p
    fun textOutstreamOf p =
	  streamOf (#outstream, "txt_out", openTextOutHNDL, TextIO.closeOut) p
    fun binOutstreamOf p =
	  streamOf (#outstream, "bin_out", openBinOutHNDL, BinIO.closeOut) p

  (* val reap : ('a, 'b) proc -> OS.Process.status  *)
    fun reap (PROC{ status = ref (DEAD s), ... }) = s
      | reap (PROC{ status = status as ref (ALIVE hProcess), instream, outstream, ... }) = let
	  fun close (UNOPENED hndl) = Win32_IO.close hndl
	    | close (OPENED s) = #close s ()
	  val _ = close (!instream)
	  val _ = close (!outstream) handle _ => ()
	  val s = loopingSleepingWait hProcess
	  in
	    status := DEAD s;
	    s
	  end

  end

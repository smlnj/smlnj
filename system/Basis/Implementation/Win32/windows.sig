(* windows.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Signature for the interface to windows.
 *)

signature WINDOWS =
  sig
    structure Key : WINDOWS_KEY
    structure Reg : WINDOWS_REG
    structure Config : WINDOWS_CONFIG
    structure DDE : WINDOWS_DDE

    val getVolumeInformation : string -> {
	    volumeName : string,
	    systemName : string,
	    serialNumber : SysWord.word,
	    maximumComponentLength : int
	  }

    val findExecutable : string -> string option
    val launchApplication : string * string -> unit
    val openDocument : string -> unit
    val simpleExecute : string * string -> OS.Process.status

    type ('a,'b) proc

    val execute : string * string -> ('a, 'b) proc
    val textInstreamOf : (TextIO.instream, 'a) proc -> TextIO.instream
    val binInstreamOf  : (BinIO.instream, 'a) proc -> BinIO.instream
    val textOutstreamOf : ('a, TextIO.outstream) proc -> TextIO.outstream
    val binOutstreamOf  : ('a, BinIO.outstream) proc -> BinIO.outstream
    val reap : ('a, 'b) proc -> OS.Process.status

    structure Status : WINDOWS_STATUS

    val fromStatus : OS.Process.status -> Status.status
    val exit : Status.status -> 'a

  end

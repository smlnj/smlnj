(* windows-config.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Signature with Windows platform information
 *)

signature WINDOWS_CONFIG = sig
    val platformWin32s       : SysWord.word
    val platformWin32Windows : SysWord.word
    val platformWin32NT      : SysWord.word
    val platformWin32CE      : SysWord.word
    val getVersionEx : unit -> {
	    majorVersion : SysWord.word,
	    minorVersion : SysWord.word,
	    buildNumber : SysWord.word,
	    platformId : SysWord.word,
	    csdVersion : string
	  }
    val getWindowsDirectory : unit -> string
    val getSystemDirectory : unit -> string
    val getComputerName : unit -> string
    val getUserName : unit -> string
  end

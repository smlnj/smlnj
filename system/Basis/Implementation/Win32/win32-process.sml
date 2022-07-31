(* win32-process.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Hooks to Win32 Process functions.
 *)

structure Win32_Process : WIN32_PROCESS =
  struct
    structure W32G = Win32_General
    structure Word64 = Word64Imp

    fun cf name = W32G.cfun "WIN32-PROCESS" name

    val system' : string -> W32G.word = cf "system"

    fun exitProcess (w: W32G.word) : 'a = cf "exit_process" w

    val getEnvironmentVariable' : string -> string option =
	  cf "get_environment_variable"

    val sleep' : Word64.word -> unit = cf "sleep"

    val sleep = sleep' o Word64.fromLargeInt o TimeImp.toMilliseconds

  end

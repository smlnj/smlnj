(* win32-process.sig
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Signature for hooks to Win32 Process functions.
 *
 *)

signature WIN32_PROCESS = 
    sig
	val system' : string -> Win32_General.word
	val exitProcess : Win32_General.word -> 'a
	val getEnvironmentVariable' : string -> string option
	val sleep : Time.time -> unit
    end

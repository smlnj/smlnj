(* os-process.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Win32 implementation of the OS.FileSys structure
 *
 *)

structure OS_Process : OS_PROCESS = 
    struct
	structure CU = CleanUp
	structure W32G = Win32_General
	structure W32P = Win32_Process

	type status = W32G.word

	val success = W32G.Word.fromInt 0
	val failure = W32G.Word.fromInt 1

	fun isSuccess x = W32G.Word.toInt x = 0

	val system = W32P.system'

	val atExit = AtExit.atExit

	fun exit code = (CU.clean CU.AtExit;
			 W32P.exitProcess code)

	fun terminate code = W32P.exitProcess code

	val getEnv = W32P.getEnvironmentVariable'

	val sleep = W32P.sleep
    end

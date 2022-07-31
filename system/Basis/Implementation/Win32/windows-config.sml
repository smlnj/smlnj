(* windows-config.sml
 *
 * COPYRIGHT (c) 2008 Fellowship of SML/NJ
 *
 * Structure with Windows platform information
 *
 *)

structure Windows_CONFIG : WINDOWS_CONFIG = struct
    fun cfun x = CInterface.c_function "WIN32" x

    val platformWin32s       = 0wx0  : Word32.word
    val platformWin32Windows = 0wx1  : Word32.word
    val platformWin32NT      = 0wx10 : Word32.word
    val platformWin32CE      = 0wx11 : Word32.word
    val internalGetVersion : unit -> Word32.word * Word32.word * Word32.word * Word32.word * string 
      = cfun "config_get_version_ex"
    fun getVersionEx() =
	let
	    val (major, minor, build, plat, version) = internalGetVersion()
	in
	    {majorVersion=major, minorVersion=minor, buildNumber=build,
	     platformId=plat, csdVersion=version}
	end
    val getWindowsDirectory : unit -> string = cfun "config_get_windows_directory"
    val getSystemDirectory : unit -> string = cfun "config_get_system_directory"
    val getComputerName : unit -> string = cfun "config_get_computer_name"
    val getUserName : unit -> string = cfun "config_get_user_name"
  end 



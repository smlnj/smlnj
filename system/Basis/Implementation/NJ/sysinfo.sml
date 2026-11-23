(* sysinfo.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Get information about the underlying hardware and OS.
 *)

structure SysInfo : SYS_INFO =
  struct

    exception UNKNOWN

    fun getInfoStr NONE = raise UNKNOWN
      | getInfoStr (SOME s) = s

    datatype os_kind
      = UNIX	(* one of the many flavours of UNIX (incl macOS and Linux) *)
      | WINDOWS	(* Microsoft Windows *)

    fun sysInfo (s: string): string option =
	  CInterface.c_function "SMLNJ-RunT" "sysInfo" s
    fun getFlag flag = (case (getInfoStr(sysInfo flag))
	   of "NO" => false
	    | _ => true
	  (* end case *))

    fun getOSName () = getInfoStr(sysInfo "OS_NAME")
    fun getOSKind () = (case getOSName()
	   of ("Linux"|"BSD"|"Darwin"|"Cygwin") => UNIX
	    | "Win32" => WINDOWS
	    | _ => raise Fail "unknown OS"
	  (* end case *))
    fun getOSVersion () = getInfoStr(sysInfo "OS_VERSION")

    fun getArchName () = getInfoStr(sysInfo "ARCH_NAME")
    fun getArchSize () = Unsafe.wordSize()

    fun hasSoftwarePolling () = getFlag "HAS_SOFT_POLL"
    fun hasMultiprocessing () = getFlag "HAS_MP"

    fun getHeapSuffix () = getInfoStr (sysInfo "HEAP_SUFFIX")

  end

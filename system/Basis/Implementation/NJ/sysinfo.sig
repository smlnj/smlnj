(* sysinfo.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Get information about the underlying hardware and OS.
 *)

signature SYS_INFO =
  sig

    exception UNKNOWN
	(* this exception is raised when the runtime cannot provide the
	 * requested information.
	 *)

    datatype os_kind
      = UNIX	(* one of the many flavours of UNIX (incl macOS and Linux) *)
      | WIN32	(* Win32 API *)

    val getOSKind    : unit -> os_kind
    val getOSName    : unit -> string
    val getOSVersion : unit -> string

    val getArchName  : unit -> string
	(* returns the name of the architecture we are running on. *)
    val getArchSize : unit -> int
	(* returns word size of the architecture (either 32 or 64) *)

    val getHostSize : unit -> int	(* DEPRECATED: use getArchSize *)
    val getHostArch   : unit -> string	(* DEPRECATED: use getArchName *)
    val getTargetArch : unit -> string	(* DEPRECATED: use getArchName *)

    val hasSoftwarePolling : unit -> bool
	(* returns true, if the run-time system was compiled to support software
	 * polling.
	 *)

    val hasMultiprocessing : unit -> bool
	(* returns true, if the run-time system was compiled to support the
	 * multiprocessing hooks.  This does not mean that the underlying
	 * hardware is a multiprocessor.
	 *)

    val getHeapSuffix : unit -> string

  end

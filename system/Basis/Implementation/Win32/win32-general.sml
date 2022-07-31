(* win32-general.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * General Win32 stuff.
 *)

structure Win32_General : WIN32_GENERAL =
  struct

    structure Word = Word32Imp
    type word = Word.word	(* DWORD *)

    type hndl = Handle.t

    val isValidHandle = Handle.isValid

    val arcSepChar = #"\\"

    val cfun = CInterface.c_function

    val sayDebug : string -> unit = cfun "SMLNJ-RunT" "debug"

    val getConst' : (string * string) -> word = cfun "WIN32" "get_const"
    fun getConst kind name = getConst'(kind,name)

    val getLastError : unit -> word = cfun "WIN32" "get_last_error"

  end

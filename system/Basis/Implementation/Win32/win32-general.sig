(* win32-general.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Signature for general Win32 stuff.
 *)

signature WIN32_GENERAL =
  sig
    structure Word : WORD
    type word

    type hndl = Handle.t

    val isValidHandle : hndl -> bool

    val arcSepChar : char

    val cfun : string -> string -> 'a -> 'b
    val getConst : string -> string -> word

    val sayDebug : string -> unit

    val getLastError : unit -> word

  end

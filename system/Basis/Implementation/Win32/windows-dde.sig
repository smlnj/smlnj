(* windows-dde.sig
 *
 * COPYRIGHT (c) 2008 Fellowship of SML/NJ
 *
 * Signature with the windows DDE APIs
 *
 *)

signature WINDOWS_DDE = sig
    type info
    val startDialog : string * string -> info
    val executeString : info * string * int * Time.time -> unit
    val stopDialog : info -> unit
  end


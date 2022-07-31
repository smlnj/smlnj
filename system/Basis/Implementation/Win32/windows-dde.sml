(* windows-dde.sml
 *
 * COPYRIGHT (c) 2008 Fellowship of SML/NJ
 *
 * Structure with the windows DDE APIs
 *)

local
    structure Time = TimeImp
    structure IntInf = IntInfImp
in
structure Windows_DDE : WINDOWS_DDE =
  struct
    type info = Handle.t	(* HCONV, which is an alias for HANDLE *)

    fun cfun x = CInterface.c_function "WIN32" x

    val startDialog : string * string -> info = cfun "dde_start_dialog"

    fun executeString (conversation, command, retry, delay) = let
	  val realDelay = IntInf.toInt(Time.toMilliseconds delay)
	  val executeStringInternal : info * string * int * int -> unit = cfun "dde_execute_string"
	  in
	    executeStringInternal (conversation, command, retry, realDelay)
	  end

    val stopDialog : info -> unit = cfun "dde_stop_dialog"

  end
end (* local *)

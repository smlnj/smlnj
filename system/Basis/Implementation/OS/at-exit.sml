(* at-exit.sml
 *
 * COPYRIGHT (c) 2007 Fellowship of SML/NJ
 *
 * The (generic) support for the OS.Process.atExit function.
 *)

structure AtExit : sig

    val atExit : (unit -> unit) -> unit

  end = struct

    structure CU = CleanUp

    val hooks = ref ([] : (unit -> unit) list)

  (* Note that the semantics of atExit require that calls to exit
   * in an atExit action cause the remaining atExit actions to be
   * performed.  
   *)
    fun doAtExit () = (case !hooks
	   of [] => ()
	    | (f::r) => (hooks := r;  f() handle _ => (); doAtExit())
	  (* end case *))

    fun cleaner CU.AtExit = doAtExit()
      | cleaner CU.AtExportFn = hooks := []
      | cleaner _ = ()

    val _ = CU.addCleaner ("OS.Process", [CU.AtExit, CU.AtExportFn], cleaner)

    fun atExit hook = hooks := hook :: !hooks

  end;


(* version.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure Version : sig

    val version : {system : string, version_id : int list, date : string}
    val banner : string

  end = struct


    val version = {
	    system = "Concurrent ML",
	    version_id = [1, 0, 10],
	    date = "September 15, 1997"
	  }

    fun f ([], l) = l
      | f ([x : int], l) = (Int.toString x)::l
      | f (x::r, l) = (Int.toString x) :: "." :: f(r, l)

    val banner = concat (
	  #system version :: ", Version " ::
	    f (#version_id version, [", ", #date version]))

  end;


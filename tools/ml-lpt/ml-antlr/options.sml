(* options.sml
 *
 * COPYRIGHT (c) 2007-2016 Fellowship of SML/NJ
 *
 * Processing of command line arguments
 *)

structure Options =
  struct

    val unitActions 		= ref false
    val debug			= ref false
    val dotOutput		= ref false
    val texOutput		= ref false
    val fname			= ref ""

  (* process the command line arguments; return true if there is an error *)
    fun processArgs args = let
	  fun procArg "--dot" = (dotOutput := true; false)
	    | procArg "--latex" = (texOutput := true; false)
	    | procArg "--unit-actions" = (unitActions := true; false)
	    | procArg "--debug" = (debug := true; false)
	    | procArg _ = true
	  in
	    case List.filter procArg args
	     of [file] => (fname := file; false)
	      | _ => true (* error: exactly one file should be specified *)
	    (* end case *)
	  end

  (* usage message *)
    val usage = "usage: ml-antlr [--dot] [--latex] [--unit-actions | --debug] <file>"

  end
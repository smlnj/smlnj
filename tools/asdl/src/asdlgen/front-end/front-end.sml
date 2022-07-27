(* front-end.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure FrontEnd : sig

    val doFile : string -> {
	    modules : AST.module list
	  } option

  end = struct

  (* check for and report any errors *)
    fun anyErrors {includes, file} = let
	  val errFlg = ref false
	  fun anyErrors' (file : Parser.file) = if Error.anyErrors (#errStrm file)
		then (
		  errFlg := true;
		  Error.report (TextIO.stdErr, #errStrm file))
		else ()
	  in
	    List.app anyErrors' includes;
	    anyErrors' file;
	    !errFlg
	  end

    fun doFile file = let
	  val parseTree = Parser.parse file
	  in
	    if anyErrors parseTree
	      then NONE
	      else let
		val modules = Typecheck.check parseTree
		in
		  if (anyErrors parseTree)
		    then NONE
		    else SOME{
			modules = modules
		      }
		end
	  end

  end



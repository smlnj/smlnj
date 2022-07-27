(* driver.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Test driver for ASDL front end
 *)

structure Driver : sig

    val doit : string -> bool

  end = struct

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

    fun doit file = let
	  val parseTree = Parser.parse file
	  in
	    if anyErrors parseTree
	      then true
	      else let
		val modules = Typecheck.check parseTree
		in
		  if (anyErrors parseTree)
		    then true
		    else (PPAST.output (TextIO.stdOut, modules); false)
		end
	  end

  end

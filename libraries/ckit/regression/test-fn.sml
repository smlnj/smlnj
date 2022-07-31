(* Functorized regression testing for valid C code.
 *
 * trans   - function which takes the output of buildAst (parsed/typechecked c code)
 *           and performs a transformation on it returning an Ast.
 *
 * testDir - the directory where the valid C code lives.
 *
 * outDir  - directory where to place the prettyprinted transformed code.
 *
 * For each C file in the testDir, the following actions are performed
 *
 * - the file is parsed and typechecked
 *
 * - the transformation, trans, is applied to the resulting ast representation
 *
 * - the transformed ast is pretty-printed to outDir using the same name as
 *   the file from which it was read.
 *
 * - the original C file and the generated C file are compiled and run with the
 *   results saved in their respective directories.  These output files are then
 *   compared using diff.
 *)

functor TestFn (val testDir : string;
                val outDir : string;
                val trans : BuildAst.astBundle -> Ast.ast
	       ) = struct

  fun isCFile s = 
      case rev (explode s) 
	of (#"c")::(#".")::_ => true
	 | (#"i")::(#".")::_ => true
         | _ => false

  fun dirList dir = 
      let val ds = OS.FileSys.openDir dir
	  fun loop () =
	      case OS.FileSys.readDir ds 
		of "" => []
	         | s => if isCFile s then s::(loop ()) else loop ()
      in loop () before OS.FileSys.closeDir ds end

  fun spaces n = 
      let fun loop 0 a = String.concat a
	    | loop n a = loop (n-1) (" "::a)
      in loop n [] end


  fun normalize file = 
      case 15 - (String.size file)
	of 0 => file
	 | n => if n < 0 then String.substring (file,0,15)
	        else file^(spaces n)

  fun ppTrans os file =
      let val pinfo as {ast, tidtab, errorCount, warningCount, ...} =
	        ParseToAst.fileToAst (testDir^"/"^file)
	  val ast = trans pinfo
	  val fileOs = TextIO.openOut (outDir^"/"^file)
      in (PPLib.ppToStrm (PPAst.ppAst () tidtab) fileOs ast;
	  (case (errorCount, warningCount) of
	     (0, 0) => TextIO.output (os,"\t[success]")
	   | (i, 0) => 
	       TextIO.output (os,"\t[" ^ (Int.toString i) ^ " errors]")
	   | (0, j) => 
	       TextIO.output (os,"\t[" ^ (Int.toString j) ^ " warnings]")
	   | (i, j) => 
	       TextIO.output (os,"\t[" ^ (Int.toString i) ^
			         " errors (" ^ (Int.toString j) ^"w)]"));
	  TextIO.closeOut fileOs;
	  true)
	  handle _ =>
	    (TextIO.output (os,"\t[failed]");
	     TextIO.closeOut fileOs;
	     false)
      end  

  fun compileCommand dir file = "cc "^dir^"/"^file^" -o "^dir^"/"^file^".exe"

  fun compile os file =
      case OS.Process.system (compileCommand testDir file)
	of 0 => (case OS.Process.system (compileCommand outDir file)
		   of 0 => (TextIO.output (os,"\t[cc succeeded]"); true)
		    | _ => (TextIO.output (os,"\t[trans cc failed]"); false))

         | _ => (TextIO.output (os,"\t[orig cc failed]"); false)

  fun executeCommand dir file = dir^"/"^file^".exe > "^dir^"/"^file^".out"
  fun execute os file =
      case OS.Process.system (executeCommand testDir file)
	of 0 => (case OS.Process.system (executeCommand outDir file)
		   of 0 => (TextIO.output (os,"\t[execution succeeded]"); true)
		    | _ => (TextIO.output (os,"\t[trans execution failed]"); false))

         | _ => (TextIO.output (os,"\t[orig execution failed]"); false)

  fun compare os file =
      let val diffCommaind = "diff "^testDir^"/"^file^".out "^outDir^"/"^file^".out"
      in case OS.Process.system (executeCommand testDir file)
	   of 0 => (TextIO.output (os,"\t[output the same]"); true)
            | _ => (TextIO.output (os,"\t[output different]"); false)
      end

  fun test os file = 
      ( TextIO.output (os,normalize file)
      ; print ( (normalize file) ^ "\n" )
      ; TextIO.flushOut os
      ; if not (ppTrans os file) then ()
	else if not (compile os file) then ()
	     else if not (execute os file) then ()
		  else if not (compare os file) then ()
		       else ()
      ; TextIO.output (os,"\n")
      )

  fun testOne file = test TextIO.stdOut file

  fun testAll () = 
      let val os = TextIO.openOut (outDir^"/summary");
	  fun loop [] = TextIO.closeOut os
	    | loop (file::files) =
	      ( test os file 
	        handle _ => TextIO.output (os,"FAILED WITH EXTREME PREJUDICE\n")
	      ; loop files
	      )
      in loop (dirList (testDir^"/")) end
end


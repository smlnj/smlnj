(*
 * This module starts a graph viewer.
 *
 * -- Allen 
 *)

functor GraphViewer(D : GRAPH_DISPLAY) : GRAPH_VIEWER =
struct

   structure L = GraphLayout
   structure G = Graph
   structure FileSys = OS.FileSys
   
   val tmpName = MLRiscControl.getString "tmpName"

   fun display exec (layout as G.GRAPH l) filename = 
      let val filename  = filename ^ D.suffix()
	  val _     = print("[ "^ #name l^": "^ 
                            D.program() ^ " " ^ filename ^ 
                            " "^Int.toString(#order l ())^" nodes"^
                            " "^Int.toString(#size l ())^" edges");
          val file  = TextIO.openOut filename
          val out   = fn s => TextIO.output(file,s)
          val _     = D.visualize out layout
          val _     = TextIO.closeOut file
          val _     = print(" ]\n")
          val _     = exec filename
      in  
          ()
      end handle e => 
        (print("[Uncaught exception in "^exnName e^" graph viewer]\n"); raise e)

   fun system filename = (OS.Process.system 
			   ((D.program()) ^ " " ^ filename);
                          FileSys.remove filename)

   fun fork filename = (OS.Process.system(
			  "(" ^ (D.program()) ^ " " ^ filename ^ 
			      "; /bin/rm " ^ filename ^ ") &"))

   fun getTmpName() =
       if !tmpName = "" then FileSys.tmpName() else !tmpName

   fun view layout = display system layout (getTmpName())
end


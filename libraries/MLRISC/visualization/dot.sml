(*
 * This communicates with the dot tool
 *
 * -- Allen
 *)

structure Dot : GRAPH_DISPLAY =
struct

   structure L = GraphLayout
   structure G = Graph

   fun suffix() = ".dot"
   fun program() = "dotty"

   fun visualize out (G.GRAPH G) =
   let val spaces = "                                           ";
       fun int n  = out (Int.toString n) 
       fun tab t  = out(String.substring(spaces,0,t)) handle _ => out spaces
       fun semi() = out ";"
       fun name n = if n < 0 then (out "XX"; int(~n))
                    else (out "X"; int n)
       fun attribs t a = (out "[ shape=box"; doAttribs t "," a; out "]")

       and doAttrib t comma (L.LABEL "")   = false
         | doAttrib t comma (L.LABEL l)    = (out comma; tab t; label l; true)
	 | doAttrib t comma (L.COLOR c)    = 
	     (out comma; tab t; out "color=\"";  out c;  out "\"";  true)
         | doAttrib t comma _              = false

       and doAttribs t comma [] = ()
         | doAttribs t comma (l::ls) =
            doAttribs t (if doAttrib t comma l then ",\n" else comma) ls

       and label l = (out "label=\""; out(String.toString l); out "\"\n")

       fun doNode t (n,a) = (tab t; name n; attribs t a; semi())

       fun doEdge t (i,j,a) =
           (tab t; name i; out "-> "; name j; attribs t a; semi())

   in  out("digraph " ^ #name G ^ " {\n");
       #forall_nodes G (doNode 2);
       #forall_edges G (doEdge 2);
       out "}\n" 
   end

end

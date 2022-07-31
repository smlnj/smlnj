(*
 * This module communicates with the vcg tool.
 * 
 * -- Allen
 *)

structure VCG : GRAPH_DISPLAY =
struct

   structure L = GraphLayout
   structure G = Graph

   fun suffix() = ".vcg"
   fun program() = "xvcg"
  
   fun visualize out (G.GRAPH G) =
   let val spaces = "                                           ";
       fun int n  = out (Int.toString n) 
       fun nl()   = out "\n" 
       fun tab t  = out(String.substring(spaces,0,t)) handle _ => out spaces
       fun color k c = (out k; out c; nl()) 
       fun openBrace t k = (tab t; out k; out ": {\n")
       fun closeBrace t = (tab t; out "}\n")

       fun doStyle t (L.ALGORITHM a) = 
               (tab t; out "layoutalgorithm: "; out a; nl()) 
         | doStyle t (L.NODE_COLOR c) = (tab t; color "node.color: " c)
         | doStyle t (L.EDGE_COLOR c) = (tab t; color "edge.color: " c)
         | doStyle t (L.TEXT_COLOR c) = (tab t; color "textcolor: " c)
         | doStyle t (L.ARROW_COLOR c) = (tab t; color "arrowcolor: " c)
         | doStyle t (L.BACKARROW_COLOR c) = (tab t; color "backarrowcolor: " c)
         | doStyle t (L.BORDER_COLOR c) = (tab t; color "bordercolor: " c)
         | doStyle t _ = ()

       fun label l = (out "label: \""; out(String.toString l); out "\"")
       
       fun doAttrib t (L.LABEL "")   = ()
         | doAttrib t (L.LABEL l)    = (tab t; label l; nl())
         | doAttrib t (L.COLOR c)    = (tab t; color "color: " c)
         | doAttrib t (L.BORDERLESS) = (tab t; color "bordercolor: " "white")
         | doAttrib t _              = ()

       fun doNode t (n,a) =
           (openBrace t "node";
            tab (t+2); out "title: \""; int n; out "\"\n";
            app (doAttrib (t+2)) a; 
            closeBrace t)

       fun doEdge t kind (i,j,a) =
           (openBrace t kind;
            tab (t+2); out "sourcename: \""; int i; out "\"\n";
            tab (t+2); out "targetname: \""; int j; out "\"\n";
            app (doAttrib (t+2)) a;
            closeBrace t)

       fun defaultStyle t = 
          (tab t; out "display_edge_labels: yes\n";
           tab t; out "layoutalgorithm: minbackward\n"
          )
       
   in  out "graph: {\n";
       defaultStyle 2;
       app (doStyle 2) (#graph_info G);
       #forall_nodes G (doNode 2);
       #forall_edges G (doEdge 2 "edge");
       out "}\n" 
   end
   

end


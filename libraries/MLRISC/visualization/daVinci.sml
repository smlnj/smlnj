(*
 * This backend communicates with the daVinci tool.
 *
 * -- Allen
 *)

structure daVinci : GRAPH_DISPLAY =
struct

   structure L = GraphLayout
   structure G = Graph

   fun suffix() = ".daVinci"
   fun program() = "daVinci"

   fun visualize out (G.GRAPH G) =
   let val l = ref 0
       fun newLabel() = (l := !l + 1; "L" ^ Int.toString(!l))
       val spaces = "                                           ";
       fun int n   = out (Int.toString n)
       fun nl()    = out "\n"
       fun tab t   = out(String.substring(spaces,0,t)) handle _ => out spaces
       fun nice l  =  String.toString (String.map (fn #"\t" => #" "
                                                       | c => c) l)
       fun quote s      = (out "\""; out s; out "\"")
       fun comma()      = out ", "   
       fun atom(a,b)    = (out "a("; quote a; comma(); quote b; out ")") 
       fun OBJECT l     = atom("OBJECT",nice l)
       fun FONTFAMILY f = atom("FONTFAMILY",f)
       fun FONTSTYLE s  = atom("FONTSTYLE",s)
       fun COLOR c      = atom("COLOR",c)
       fun EDGECOLOR c  = atom("EDGECOLOR",c)
       fun Dir ()       = atom("_DIR","none")
       fun label l      = (OBJECT l;             comma(); 
                           FONTFAMILY "courier"; comma();
                           FONTSTYLE "normal"
                          ) 

       exception FOUND of string

       fun nodeAttrib (L.LABEL l) = label l
         | nodeAttrib (L.COLOR c) = COLOR c
         | nodeAttrib (L.BORDERLESS) = atom("_GO","text")
         | nodeAttrib (L.BORDER_COLOR c) = COLOR c
         | nodeAttrib _ = ()

       and isNodeAttrib (L.LABEL l) = true
         | isNodeAttrib (L.COLOR c) = true
         | isNodeAttrib (L.BORDERLESS) = true
         | isNodeAttrib (L.BORDER_COLOR c) = true
         | isNodeAttrib _ = false

       and edgeAttrib (L.COLOR c)       = EDGECOLOR c
         | edgeAttrib (L.ARROW_COLOR c) = EDGECOLOR c
         | edgeAttrib (L.EDGEPATTERN p) = atom("EDGEPATTERN",p)
         | edgeAttrib L.DIR = Dir()
         | edgeAttrib _ = () 

       and isEdgeAttrib (L.COLOR c)       = true
         | isEdgeAttrib (L.ARROW_COLOR c) = true
         | isEdgeAttrib (L.EDGEPATTERN p) = true
         | isEdgeAttrib (L.DIR) = true
         | isEdgeAttrib _ = false 

       and findEdgeLabel ((L.LABEL "")::l) = findEdgeLabel l
         | findEdgeLabel ((L.LABEL l)::_) = raise FOUND l
         | findEdgeLabel (_::l) = findEdgeLabel l
         | findEdgeLabel []     = ()

       and listify comma f []      = ()
         | listify comma f [x]     = f x
         | listify comma f (x::xs) = (f x; comma(); listify comma f xs)

       and attribs t (p,gen) a =
          (tab t; out "[\n";
           tab (t+2); listify comma gen (List.filter p a); nl();
           tab t; out "]\n"
          )

       fun doNode t (n,a) =
           ( tab t; 
             out "l(\""; int n; out "\",n(\"\",\n";
             attribs (t+2) (isNodeAttrib,nodeAttrib) a;
             comma();
             tab (t+2); out "[\n";
             listify comma (doEdge (t+2)) (#out_edges G n);
             tab (t+2); out "]))\n"
           )

       and doEdge t (i,j,a) =
          ((findEdgeLabel a;
            tab t; out "l(\""; 
            int i; out "->"; int j; 
            (* dummy label; daVinci chokes on duplicated edge names *)
            out "-"; out(newLabel()); 
            out "\",e(\"\",\n";
            attribs (t+2) (isEdgeAttrib,edgeAttrib) a;
            tab t; out ",r(\""; int j; out "\")))")
            handle FOUND l =>
            let val x = newLabel()
            in
            (tab t; out "l(\""; int i; out("->"^x^"\",e(\"\",");
             attribs (t+2) (isEdgeAttrib,edgeAttrib) (L.DIR::a);
             out ",l(\""; out(newLabel());
             out "\",n(\"\",[a(\"OBJECT\",\"";
             out l; out "\"),a(\"_GO\",\"text\")],";
             out("[l(\""^x^"->"); int j; out "\",e(\"\",";
             attribs (t+2) (isEdgeAttrib,edgeAttrib) a;
             tab t; out ",r(\""; int j; out "\")))]))))"
            )
            end
          )

   in  out "[\n";
       listify comma (doNode 2) (#nodes G ());
       out "]\n" 
   end
   

end


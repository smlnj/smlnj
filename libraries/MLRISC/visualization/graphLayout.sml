(*
 * Here are some graph layout annotations.
 *
 * -- Allen
 *)

structure GraphLayout =
struct

   datatype format = 
     LABEL of string
   | COLOR of string
   | NODE_COLOR of string
   | EDGE_COLOR of string
   | TEXT_COLOR of string
   | ARROW_COLOR of string
   | BACKARROW_COLOR of string
   | BORDER_COLOR of string
   | BORDERLESS 
   | SHAPE of string 
   | ALGORITHM of string
   | EDGEPATTERN of string
   | DIR (* for internal use only! *)

   val STYLE = Annotations.new(SOME(fn _ => "STYLE")) : 
         format list Annotations.property 

   type ('n,'e,'g) style = 
      { edge  : 'e Graph.edge -> format list,
        node  : 'n Graph.node -> format list,
        graph : 'g -> format list
      }

   type layout = (format list, format list, format list) Graph.graph

   fun makeLayout {node,edge,graph} G = 
       IsomorphicGraphView.map node edge graph G

   fun makeLayout' G =
   let val edgeColor = [COLOR "red"]
   in  makeLayout {node=fn (i,_) => [LABEL(Int.toString i)],
                   edge=fn _ => edgeColor,
                   graph=fn _ => []} G
   end
end


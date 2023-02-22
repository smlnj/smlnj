(* smlnj-lib/PrettyPrint/examples/wadler-trees1.sml *)

local
structure PP = PrettyPrint
in

datatype tree = Node of string * tree list

fun formatTree (Node (s, trees)) =
      PP.cBlock [PP.text s, formatTrees trees]

and formatTrees nil = PP.empty
  | formatTrees trees = PP.closedSequenceWithMap {
        left = PP.text "[",
        sep = PP.text ",",
        right = PP.text "]",
        fmt = formatTree
      } trees

val tree1 =
    Node ("aaa",
	  [Node ("bbbbb",
		 [Node ("ccc", nil),
		  Node ("dd", nil)]),
	   Node ("eee", nil),
	   Node ("ffff",
		 [Node ("gg", nil),
		  Node ("hhh", nil),
		  Node ("ii", nil)])]);

end

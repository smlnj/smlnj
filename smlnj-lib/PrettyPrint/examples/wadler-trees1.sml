(* smlnj-lib/PrettyPrint/examples/wadler-trees1.sml *)

local open PrettyPrint
in

datatype tree = Node of string * tree list

fun formatTree (Node (s, trees)) = 
    ccat (text s, formatTrees trees)

and formatTrees nil = empty
  | formatTrees trees = brackets (vsequence comma (map formatTree trees))

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

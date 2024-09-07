(* smlnj-lib/PrettyPrint/examples/wadler-trees1.sml *)

local
  structure F = Formatting
in

datatype tree = Node of string * tree list

fun formatTree (Node (s, trees)) =
      F.cBlock [F.text s, formatTrees trees]

and formatTrees nil = F.empty
  | formatTrees trees = F.closedSequenceWithMap {
        align = F.V,
        front = F.text "[",
        sep = F.text ",",
        back = F.text "]",
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

end (* local *)

(* test/symbol.sml *)

structure Symbol =
struct

  type symbol = string

  val compare = String.compare

end

(* occ.sig *)

  (*
   * 8/18/92: cleaned up occ "state machine" some and fixed bug #612.
   * Known behaviour of the attributes about the context that are kept:
   * lamd = # of Abstr's seen so far.  Starts at 0 with Root.
   * top = true iff haven't seen a LetDef yet.
   *)

signature OCCURRENCE =
sig

  type occ

  val Root : occ

  val LetDef : occ -> occ

  val Abstr : occ -> occ

  val lamdepth : occ -> int

  val toplevel : occ -> bool

end (* signature OCCURRENCE *)

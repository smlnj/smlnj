(* ElabData/types/occ.sml *)

(* Occurrence: OCCURRENCE
 * Tracking depth of lambda abstractions and whether
 *  one has entered a let (definiens?).
 *
 * 8/18/92: cleaned up occ "state machine" some and fixed bug #612.
 *  Known behaviour of the attributes about the context that are kept:
 *  lamd = # of Abstr's entered so far.  Starts at 0 with Root.
 *  top = true iff have not entered the declaration part of 
 *  a Let (let, local (1st decl), structure let, functor let) yet.
 *  Q: Is OCC{lamb = 0, top = false} possible?  (yes?)
 *     Is OCC{lamb > 0, top = true} possible?
 *)

structure Occurrence :> OCCURRENCE =
struct

  datatype occ = OCC of {lamd: int, top: bool}

  val Root = OCC{lamd=0, top=true}

  fun LetDef(OCC{lamd,...}) = OCC{lamd=lamd, top=false}

  fun Abstr(OCC{lamd,top})  = OCC{lamd=lamd+1, top=top}

  fun lamdepth (OCC{lamd,...}) = lamd

  fun toplevel (OCC{top,...})  = top

end (* structure Occurrence *)

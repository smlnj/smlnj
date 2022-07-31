(* interval-domain-sig.sml
 *
 * COPYRIGHT (c) 2005 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * The domain over which we define interval sets.
 *)

signature INTERVAL_DOMAIN =
  sig

  (* the abstract type of elements in the domain *)
    type point

  (* compare the order of two points *)
    val compare : (point * point) -> order

  (* successor and predecessor functions on the domain *)
    val succ : point -> point
    val pred : point -> point

  (* isSucc(a, b) ==> (succ a) = b and a = (pred b). *)
    val isSucc : (point * point) -> bool

  (* the minimum and maximum bounds of the domain; we require that
   * pred minPt = minPt and succ maxPt = maxPt.
   *)
    val minPt : point
    val maxPt : point

  end

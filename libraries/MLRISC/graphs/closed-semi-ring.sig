(*
 * Signature of a closed semi ring.  A closed semi ring has the
 * closure operation (star)
 * 
 * -- Allen
 *)

signature CLOSED_SEMI_RING =
sig

  type elem

  val zero : elem
  val one  : elem
  val +    : elem * elem -> elem
  val *    : elem * elem -> elem
  val star : elem -> elem

end


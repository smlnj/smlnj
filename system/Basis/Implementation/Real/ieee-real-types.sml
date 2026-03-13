(* ieee-real-types.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Types-only definition of the IEEEReal structure.  We need to factor
 * these out of the IEEEReal structure to avoid cyclic module dependencies.
 *)

structure IEEERealTypes =
  struct

    datatype real_order = LESS | EQUAL | GREATER | UNORDERED

    datatype float_class
      = NAN
      | INF
      | ZERO
      | NORMAL
      | SUBNORMAL

    datatype rounding_mode
      = TO_NEAREST
      | TO_NEGINF
      | TO_POSINF
      | TO_ZERO

    type decimal_approx = {
	class : float_class,
	sign : bool,
	digits : int list,
	exp : int
      }

  end

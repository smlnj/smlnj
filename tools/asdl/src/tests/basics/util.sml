(* util.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Testing utility functions for types defined in test-spec.asdl
 *)

structure Util =
  struct

    local
      open TestSpec
    in

  (* tree *)
    fun tree_same (EMPTY, EMPTY) = true
      | tree_same (NODE nd1, NODE nd2) = (#value nd1 = #value nd2)
	  andalso tree_same(#left nd1, #left nd2)
	  andalso tree_same(#right nd1, #right nd2)
      | tree_same _ = false

    fun tree_toString EMPTY = "EMPTY"
      | tree_toString (NODE{value, left, right}) = concat[
	    "ND(", value, ", ", tree_toString left, ", ", tree_toString right, ")"
	  ]

  (* coord *)
    fun coord_same (c1 : coord, c2 : coord) = (#x c1 = #x c2) andalso (#y c1 = #y c2)

    fun coord_toString {x, y} = concat["(", IntInf.toString x, ", ", IntInf.toString y, ")"]

  (* pos *)
    fun pos_same ((x1, y1) : pos, (x2, y2)) = (x1 = x2) andalso (y1 = y2)

    fun pos_toString (x, y) = concat["(", IntInf.toString x, ", ", IntInf.toString y, ")"]

  (* nat *)
    fun nat_same (ZERO, ZERO) = true
      | nat_same (SUCC n1, SUCC n2) = nat_same(n1, n2)
      | nat_same _ = false

    fun nat_toString ZERO = "Z"
      | nat_toString (SUCC n) = concat["S(", nat_toString n, ")"]

  (* value *)
    fun value_same (BOOL b1, BOOL b2) = (b1 = b2)
      | value_same (INT i1, INT i2) = (i1 = i2)
      | value_same (STRING s1, STRING s2) = (s1 = s2)
      | value_same _ = false

    fun value_toString (BOOL b) = Bool.toString b
      | value_toString (INT n) = IntInf.toString n
      | value_toString (STRING s) = concat["\"", String.toString s, "\""]

  (* color *)
    fun color_same (c1 : color, c2) = (c1 = c2)

    fun color_toString RED = "RED"
      | color_toString GREEN = "GREEN"
      | color_toString BLUE = "BLUE"

  (* wrap_bool *)
    fun wrap_bool_same (WRAP b1, WRAP b2) = (b1 = b2)

    fun wrap_bool_toString (WRAP b) = concat["WRAP(", Bool.toString b, ")"]

  (* unit *)
    fun unit_same (UNIT, UNIT) = true

    fun unit_toString UNIT = "UNIT"

    end (* local *)

  end

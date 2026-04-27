# `compiler/Basics/prim`

This directory contains common datatype definitions used to represent primitive operations
in the **Absyn**, **FLINT**, **CPS**, and **CFG** IRs.  The primitive operators are
organized into structures based on their use in the various IRs.

* `CTypes` (`c-types.sml`) -- This module defines a representation of **C** types
  that is used in the "Raw C Call" primitive operation.

* `NumKind` (`numkind.sml`) -- This module defines a datatype specifying the knind of
  numeric operations (*i.e.*, signed integer, unsigned word, or real).

* `CommonOps` (`common-ops.sml`) -- This module defines primitive operators that are
  common across both **Absyn** and **FLINT**.

* `PureOps` (`pure-ops.sml`) -- This module defines numeric operators that are
  "pure" (*i.e.*, side-effect free).  These operators are used in all of the IRs.

* `ArithOps` (`arith-ops.sml`) -- This module defines trapping numeric operators
  (*i.e.*, trapping arithmentic on signed integers).  These operators are used in
  all of the IRs.

* `CompareOps` (`compare-ops.sml`) -- This module defines comparison operations
  on numbers.

Additional primitive operators are defined by the different IRs (*e.g.*, the
`InlineOps` module in the **Absyn** IR).

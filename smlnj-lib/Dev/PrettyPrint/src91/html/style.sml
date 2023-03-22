(* smlnj-lib/Dev/PrettyPrint/src91/html/style.sml *)

(* The style type for HTML rendering *)

structure Style =
struct

   datatype style
      = NOEMPH
      | TT | I | B | U | STRIKE | EM
      | STRONG | DFN | CODE | SAMP | KBD
      | VAR | CITE
      | COLOR of string
      | A of {href : string option, name : string option}

(* the names of colors will be as spacified for HTML 3.2 *)

end (* structure Style *)

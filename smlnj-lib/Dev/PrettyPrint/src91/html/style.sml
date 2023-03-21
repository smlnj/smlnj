(* smlnj-lib/Dev/PrettyPrint/src91/html/html-style.sml *)

structure HTMLStyle =
struct

   datatype style
      = NOEMPH
      | TT | I | B | U | STRIKE | EM
      | STRONG | DFN | CODE | SAMP | KBD
      | VAR | CITE
      | COLOR of string
      | A of {href : string option, name : string option}
      | STYS of style list

end (* structure HTMLStyle *)

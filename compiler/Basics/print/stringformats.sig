(* Basics/print/stringformats.sig *)
(* Copyright 2003, the Fellowship of SML/NJ (https://www.smlnj.org) *)

signature STRING_FORMATS =
sig

  (* The following functions produce strings, and do not print. *)

  val formatString : string -> string
      (* quotes and trims string according to Control_Print.stringDepth. *)

  val formatIntInf : IntInf.int -> string
      (* Calls IntInf.toString but trims output according to Contro_Print.intinfDepth. *)

end (* signature STRING_FORMATS *)

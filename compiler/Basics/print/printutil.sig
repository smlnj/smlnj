(* Basics/print/printutil.sig *)
(* Copyright 2003, the Fellowship of SML/NJ (https://www.smlnj.org) *)

signature PRINTUTIL =
sig

(* The following functions produce strings, and do not print.  Therefore the
 * signature name (and corresponding structure name PrintUtil) are inappropriate. *)

  val formatString : string -> string
      (* quotes and trims string according to Control_Print.stringDepth. *)

  val formatIntInf : IntInf.int -> string
      (* Calls IntInf.toString but trims output according to Contro_Print.intinfDepth. *)

end (* signature PRINTUTIL *)

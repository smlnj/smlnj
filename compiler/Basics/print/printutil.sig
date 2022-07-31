(* Copyright 1996 by AT&T Bell Laboratories *)
(* printutil.sig *)

signature PRINTUTIL =
sig

(* following functions actually print (to stdout) *)

  val newline : unit -> unit
  val tab : int -> unit
  val nlindent : int -> unit

  val printSequence : string -> ('a -> unit) -> 'a list -> unit
  val printClosedSequence : (string*string*string) -> ('a -> unit) ->
					 'a list -> unit
  val printSym : Symbol.symbol -> unit

  val printvseq : int -> string -> ('a -> unit) -> 'a list -> unit

(* following functions translate to strings, and do not print *)

  (*  val quoteString : string -> string *)
      (* was "mlstr"? -- puts double quote characters around string *)

  val formatString : string -> string
      (* was "pr_mlstr"? -- quotes and trims string according to
       * Control_Print.stringDepth. *)

  val formatIntInf : IntInf.int -> string
      (* was "pr_intinf". Calls IntInf.toString but trims output according to
       * Contro_Print.intinfDepth. *)

  val listToString :  (string * string * string) -> ('a -> string)  -> 'a list -> string
      (* list (front,sep,back) prfn : formats a list as a string with froont and back
       * as beginning and ending brackets, and sep as the separator, using prfn to print
       * the members of the list *)

  val interpws : string list -> string
  (* interpolate white space (" ") between the elements of a list of strings, then concat *)

end (* signature PRINTUTIL *)

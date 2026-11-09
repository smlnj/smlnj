(* printutil.sig
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature PRINTUTIL =
  sig

    (* the following functions actually print (to stdout) *)
    val newline : unit -> unit
    val tab : int -> unit
    val nlindent : int -> unit

    (* getters for the printing controls with sanity checking *)
    val printDepth  : unit -> int         (* min 1 *)
    val printLength : unit -> int         (* min 1 *)
    val stringDepth : unit -> int         (* min 3 *)
    val intinfDepth : unit -> int         (* min 3 *)
    val lineWidth   : unit -> int         (* min 40 *)

    val printSequence : string -> ('a -> unit) -> 'a list -> unit
    val printClosedSequence : (string*string*string) -> ('a -> unit) ->
                                           'a list -> unit

    val printvseq : int -> string -> ('a -> unit) -> 'a list -> unit

    (* the following functions translate to strings and do not print *)

    (*  val quoteString : string -> string *)
        (* was "mlstr"? -- puts double quote characters around string *)

    val formatString : string -> string
        (* quotes and trims string according to `stringDepth()`. *)

    val formatIntInf : IntInf.int -> string
        (* Calls IntInf.toString but trims output according to `intinfDepth()`. *)

    val listToString :  (string * string * string) -> ('a -> string)  -> 'a list -> string
        (* list (front,sep,back) prfn : formats a list as a string with froont and back
         * as beginning and ending brackets, and sep as the separator, using prfn to print
         * the members of the list
         *)

    val interpws : string list -> string
    (* interpolate white space (" ") between the elements of a list of strings,
     * then concat
     *)

  end (* signature PRINTUTIL *)

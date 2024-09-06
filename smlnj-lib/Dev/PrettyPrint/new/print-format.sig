(* print-format.sig
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Basic printing of formats to TextIO.stdOut.
 *)

signature PRINT_FORMAT =
  sig

    structure Device : PP_DEVICE

    (* `renderStdOut {...} fmt` renders `fmt` to TextIO.stdOut using the specifies
     * style map, token map, and line width.
     *)
    val renderStdout : {
            styleMap : Formatting.style -> Device.style,
            tokenMap : Formatting.token -> Device.token option,
            width : int
          } -> Formatting.format -> unit

    (* `renderStdOut width fmt` renders `fmt` to TextIO.stdOut using the default
     * style map and a default line width of 80.
     *)
    val printFormat : Formatting.format -> unit

    (* like printFormat, but with a newline appended to the format *)
    val printFormatNL : Formatting.format -> unit

  end (* signature PRINT_FORMAT *)

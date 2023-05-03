(* smlnj-lib/PrettyPrint/src/printforamat.sig *)

(* Version 8.5: new file
 * Imports : Format
 * Exports :
 *   signature PRINT_FORMAT
 *)

signature PRINT_FORMAT =
sig
    
  (* functions used to define and access the line width
     [May include lineWidth with a "device" (Render) functor parameter ] *)

    val setLineWidth : int -> unit
	(* sets the current lineWidth value *)

    val getLineWidth : unit -> int
	(* returns the current line width *)

  (* Printing formats *)

    val render : Format.format * int -> unit

    val printFormatLW  : int -> format -> unit
        (* printing to stdOut, with line width (LW) as first argument *)

    val printFormat : Format.format -> unit
        (* print to stdOut with lineWidth = getLineWidth (), (typically = !Control.Print.lineWidth) *)

    val printFormatNL : Format.format -> unit
	(* like printFormat, but with newline appened *)

end (* signature PRINT_FORMAT *)

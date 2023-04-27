(* smlnj-lib/PrettyPrint/src/printformat.sml *)

(*** printing (i.e., rendering) formats ***)

(* Version 8.5: new file
 * Imports: Render
 * Exports: structure PrintFormat
 *)

structure PrintFormat =
struct

local

  structure PP = Formatting
  structure R = Render

in 		      

(*** functions for setting and accessing the line width ***)
(* NOTE: Should PrettyPrint have its own state variable for lineWidth, or should it use
 *   PrintControl.lineWidth?
 *)

local
  val lineWidthRef : int ref = ref 90 (* or use PrintControl.lineWidth to initialize ? *)
in
  fun setLineWidth (lw: int) : unit =  lineWidthRef := lw
  fun getLineWidth () : int = !lineWidthRef
end

(* render : format * int -> unit *)
fun render (fmt, lineWidth) =
    R.render (PP.formatRep fmt, lineWidth)

(* printFormatLW : int -> format -> unit *)
fun printFormatLW lw format = render (PP.appendNewLine format, lw)

(* printFormatLW' : format -> int -> unit *)
fun printFormatLW' format lw = render (PP.appendNewLine format, lw)

(* printFormat : format -> unit *)
fun printFormat format = render (format, getLineWidth ())

(* printFormatNL : format -> unit *)
fun printFormatNL format = render (PP.appendNewLine format, getLineWidth ())

end (* top local *)
end (* structure PrintFormat *)

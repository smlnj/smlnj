(* pp.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Compile the pretty printer and define a print function.
 *)

local
  structure Dev = ANSITermDev
  structure F = Formatting
  fun styleMap (F.STY "kw") = [ANSITerm.BF, ANSITerm.FG ANSITerm.Blue]
    | styleMap _ = []
in
fun printFormatLW n = PrintANSI.renderStdout {
        styleMap = styleMap,
        tokenMap = fn _ => NONE,
        width = n
      } o Formatting.appendNewLine
end

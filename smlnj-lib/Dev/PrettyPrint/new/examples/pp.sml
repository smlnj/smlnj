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
  (* we assume single-character-wide UTF-8 tokens *)
  fun tokenMap (F.TOK{name, ...}) = SOME name
in
fun printFormatLW n = PrintANSI.renderStdout {
        styleMap = styleMap,
        tokenMap = tokenMap,
        width = n
      } o Formatting.appendNewLine
end

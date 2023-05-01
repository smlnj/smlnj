(* pp.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Compile the pretty printer and define a print function.
 *)

 structure Dev = ANSITermDev
 structure Render = RenderFn(Dev)
 val styleMap : PrettyPrint.style -> ANSITermDev.style option = let
       val tbl = AtomTable.mkTable(8, Fail "styleMap")
       in
         List.app (fn (x, sty) => AtomTable.insert tbl (Atom.atom x, sty)) [
             ("kw", [ANSITerm.BF, ANSITerm.FG ANSITerm.Blue])
           ];
         AtomTable.find tbl
       end
 fun printFormatLW n = let
       val dev = Dev.openDev{dst = TextIO.stdOut, wid = n}
       in
         Render.render {dev = dev, styleMap = styleMap, tokenMap = Render.nullMap}
       end;

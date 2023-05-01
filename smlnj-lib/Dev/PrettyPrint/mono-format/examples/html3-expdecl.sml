(* html3-expdecl.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

CM.make "../src/pretty-print-lib.cm";
CM.make "../devices/sources.cm";

(* HTML Renderer *)
local
  structure Dev = HTML3Dev
  structure Render = RenderFn(Dev)
  val styleMap : PrettyPrint.style -> Dev.style option = let
        val tbl = AtomTable.mkTable(8, Fail "styleMap")
        in
          List.app (fn (x, sty) => AtomTable.insert tbl (Atom.atom x, sty)) [
              ("kw", Dev.styleB)
            ];
          AtomTable.find tbl
        end
in
  fun printFormatLW n fmt = let
        val dev = Dev.openDev{wid = n, textWid=NONE}
        in
          Render.render {dev = dev, styleMap = styleMap, tokenMap = Render.nullMap} fmt;
          Dev.done dev
        end;
end;

use "expdecl.sml"

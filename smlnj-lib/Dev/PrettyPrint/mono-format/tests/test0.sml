local
  structure PP = PrettyPrint
in
val f0 = PP.indent 3 (PP.hBlock [PP.text "aa", PP.text "bbb"]);

val f1 = PP.indent 3 (PP.text "aa");

val f2 = PP.hBlock [PP.text "xxxx", f1, PP.text "yyy"];

val f3 = PP.vBlock [PP.text "xxxx", f1, PP.text "yyy"];

val f4 = PP.indent 3 (PP.vBlock [PP.text "aa", PP.text "bbb"]);

val f3 = PP.vBlock [PP.text "xxxx", f4, PP.text "yyy"];
end
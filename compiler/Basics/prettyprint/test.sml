val f0 = breakIndent 3 (hblock [text "aa", text "bbb"]);

val f1 = breakIndent 3 (text "aa");

val f2 = hblock [text "xxxx", f1, text "yyy"];

val f3 = vblock [text "xxxx", f1, text "yyy"];

val f4 = breakIndent 3 (vblock [text "aa", text "bbb"]);

val f3 = vblock [text "xxxx", f4, text "yyy"];

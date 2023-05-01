val f0 = indent 3 (hblock [text "aa", text "bbb"]);

val f1 = indent 3 (text "aa");

val f2 = hblock [text "xxxx", f1, text "yyy"];

val f3 = vblock [text "xxxx", f1, text "yyy"];

val f4 = indent 3 (vblock [text "aa", text "bbb"]);

val f3 = vblock [text "xxxx", f4, text "yyy"];

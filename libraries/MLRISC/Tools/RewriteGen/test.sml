val _ = use "make.sml";

fun writeOut(file, text) =
let val s = TextIO.openOut file
in  TextIO.output(s, text);
    TextIO.closeOut s
end;

val _ = writeOut("wff.sml",gen "wff.gsml");

val _ = use "wff.sig";
val _ = use "wff.sml";

fun readAll s = let
      val f = OS.FileSys.tmpName ()
      val outS = TextIO.openOut f
      val () = TextIO.output (outS, s)
      val () = TextIO.closeOut outS
      val inS = TextIO.openIn f
      fun lp acc = (case (CSVReadList.getRow inS
                            handle CSVReadList.BadRow{msg, pos, ...} =>
                              SOME ["<BadRow: " ^ msg ^ " @" ^ Int.toString pos ^ ">"])
             of NONE => List.rev acc
              | SOME r => lp (r::acc))
      val rows = lp []
      in
        TextIO.closeIn inS; OS.FileSys.remove f; rows
      end;

fun sh rows = String.concatWith " | "
      (List.map (fn r => "[" ^ String.concatWith ";"
          (List.map (fn s => "\"" ^ String.toString s ^ "\"") r) ^ "]") rows);

val cases = [
      ("a,b,c\n",              "[\"a\";\"b\";\"c\"]",            "three plain fields"),
      ("a,b,\n",               "[\"a\";\"b\";\"\"]",             "BUG 2: trailing empty field"),
      (",a\n",                 "[\"\";\"a\"]",                   "leading empty field"),
      (",\n",                  "[\"\";\"\"]",                    "two empty fields"),
      ("\"a\",b\n",            "[\"a\";\"b\"]",                  "BUG 1: quoted first field"),
      ("\"a,b\",c\n",          "[\"a,b\";\"c\"]",                "BUG 1: comma inside a quoted first field"),
      ("\"a\"\"b\",c\n",       "[\"a\\\"b\";\"c\"]",             "BUG 1: escaped quote in first field"),
      ("\"line1\nline2\",b\n", "[\"line1\\nline2\";\"b\"]",      "BUG 1: multi-line first field"),
      ("a,\"b\",c\n",          "[\"a\";\"b\";\"c\"]",            "quoted middle field (worked before)"),
      ("a,b,\"c\"\n",          "[\"a\";\"b\";\"c\"]",            "quoted last field (worked before)"),
      ("a\n",                  "[\"a\"]",                        "single field"),
      ("\n",                   "[\"\"]",                         "blank line"),
      ("a,b",                  "[\"a\";\"b\"]",                  "no trailing newline"),
      ("a,b\nc,d\n",           "[\"a\";\"b\"] | [\"c\";\"d\"]",  "two rows")
    ];

val bad = ref 0;
val () = print "\n";
val () = List.app (fn (inp, expect, what) => let val got = sh (readAll inp)
      in
        if got <> expect then bad := !bad + 1 else ();
        print (concat[StringCvt.padRight #" " 24 ("\"" ^ String.toString inp ^ "\""),
                      StringCvt.padRight #" " 30 got,
                      if got = expect then "ok   " else "WRONG (want " ^ expect ^ ") ",
                      what, "\n"])
      end) cases;
val () = print (concat["\n", Int.toString (!bad), " wrong results\n"]);


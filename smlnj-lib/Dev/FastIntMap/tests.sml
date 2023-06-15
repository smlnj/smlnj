structure B = OkasakiMapBig
structure L = OkasakiMapLittle

fun fromList l = List.foldl (fn ((k, v), m) => IM.insert(m, k, v)) IM.empty l;

val m1 = fromList [(0w0, "a"), (0w1, "b"), (0w2, "c"), (0w3, "d")];
val m2 = fromList [(0w0, "a'"), (0w1, "b'"), (0w2, "c'"), (0w3, "d'")];

fun dump m = IM.dump Fn.id (TextIO.stdOut, m);

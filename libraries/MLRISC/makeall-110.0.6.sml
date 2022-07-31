(*
 * Recompile everything in this directory
 *)
val current = ref "";
fun make f = (print("[Compiling "^f^"]\n"); current := f; CM.make'("cm/"^f));
fun again _ = make(!current);
val _ = app make files;

(*
 * Recompile everything in this directory
 *)
CM.autoload "full-cm.cm";
val current = ref "";
fun make f = (print("[Compiling "^f^"]\n"); current := f; CM.recomp("cm/"^f));
fun again _ = make(!current);
val _ = app CM.Anchor.cancel files;

fun makeall [] = true
  | makeall(f::fs) = make f andalso makeall fs
;

val _ = makeall files;

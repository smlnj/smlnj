(*
 * Recompile everything in this directory
 *)
(* CM.autoload "$/full-cm.cm"; *)

(* Register the nowhere tool *)
CM.make "$smlnj/cm/tools.cm";
val _ = Tools.registerStdShellCmdTool
        { tool = "Nowhere",
          class = "nowhere",
          suffixes = ["peep"],
          cmdStdPath = "nowhere",
          template = NONE,
          extensionStyle =
              Tools.REPLACE (["nowhere"], [("sml", SOME "sml", fn too => too)]),
          dflopts = [] };

val current = ref "";
fun make f = (print("[Compiling "^f^"]\n"); current := f; CM.recomp("cm/"^f));
fun again _ = make(!current);
fun makeall [] = true
  | makeall(f::fs) = make f andalso makeall fs
;

fun set f = #set(CM.Anchor.anchor f) (SOME "cm");
val _ = app set files;
val _ = makeall files;

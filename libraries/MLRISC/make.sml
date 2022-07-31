(*
 * Regenerates all the machine description generated files.
 * This works for only 110.39+
 *)
(* val () = #set(CM.symval "UNSHARED_MLRISC") (SOME 1); *)

(* From 110.57 on, we need the following new magic *)

fun set f = #set(CM.Anchor.anchor f) (SOME "cm");
val _ = app set ["Control.cm", "Lib.cm", "Graphs.cm", "MLRISC.cm",
                 "MLTREE.cm"];

fun b() = CM.make "Tools/MDL/sources.cm"; 
val _ = b(); 
fun c f = MDLGen.gen(f^"/"^f^".mdl");
val _ = app c
[ "x86"
, "amd64"
, "sparc"
, "alpha"
, "hppa"
, "ppc"
(* , "mips" *)
];

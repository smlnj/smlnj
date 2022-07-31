(*
 * Recompile everything in this directory
 *)
use "autoload.sml" handle _ => ();

val files =
[
 "Control.cm",
 "Lib.cm",

 "MLRISC.cm",
 "SPARC.cm",
 "ALPHA.cm",
 "HPPA.cm",
 "IA32.cm",
 "PPC.cm",
(* "MIPS.cm",  *)

 "Peephole.cm",
 "ALPHA-Peephole.cm",
 "SPARC-Peephole.cm",
 "IA32-Peephole.cm",

 "Graphs.cm",
 "Visual.cm",
 "ir.cm",
 "MLTREE.cm",
 "RA.cm",
 "GC.cm",
 "IR.cm",
 "RTL.cm",
 "Region.cm",

 "ALPHA-RTL.cm",
 "SPARC-RTL.cm",
 "HPPA-RTL.cm",
 "IA32-RTL.cm",

 "SSA.cm",

 "Opt.cm",

 "ALPHA-SSA.cm",
 "SPARC-SSA.cm",
 "HPPA-SSA.cm",
 "IA32-SSA.cm"

(* "VLIW.cm", *)
(* "Sched.cm", *)

(*
 "ALPHA-Sched.cm",
 "SPARC-Sched.cm",
 "HPPA-Sched.cm",
 "PPC-Sched.cm",
 "IA32-Sched.cm"
*)
 (*"ALPHA-GC.cm",
 "SPARC-GC.cm",
 "HPPA-GC.cm",
 "IA32-GC.cm",
 "PPC-GC.cm",*)
];                        

val _ =  (* Try to guess the version *)
use (case #version_id(Compiler.version) of
      [110,0,_] => "makeall-110.0.6.sml"
    | 110::ver::_ => if Int.>=(ver,30) then "makeall-new.sml"
                     else "makeall-110.25.sml"
    )
;

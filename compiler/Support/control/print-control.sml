(* Admin/control/print-control.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure PrintControl : PRINT_CONTROL =
struct

  val printDepth = ref 10 (* "max print depth" *)
  val printLength = ref 16 (* "max print length" *)
  val stringDepth = ref 70 (* "max string print depth" *)
  val intinfDepth = ref 70 (* "max IntInf.int print depth" *)
  val lineWidth = ref 79 (* "line-width hint for pretty printer" *)

  val printLoop = ref true (* "print loop?" *)
  val signatures = ref 2 (* "max signature expansion depth" *)
  val printOpens = ref true (* "print `open'" *)
  val out = ref {say = fn s => TextIO.output(TextIO.stdOut,s),
		 flush = fn () => TextIO.flushOut TextIO.stdOut}
  fun say s = #say (!out) s
  fun flush () = #flush (!out) ()

end (* structure PrintControl *)

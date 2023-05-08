(* compiler/Basics/print/printcontrol.sml
 *
 * (C) 2023 The Fellowship of SML/NJ
 *)
signature PRINTCONTROL =
sig

    val printDepth  : int ref
    val printLength : int ref
    val stringDepth : int ref
    val intinfDepth : int ref
    val lineWidth   : int ref
    val printLoop   : bool ref
    val signatures  : int ref
    val printOpens  : bool ref
    val out : {say : string -> unit, flush : unit -> unit} ref
    val say : string -> unit
    val flush : unit -> unit

end (* signature PRINTCONTROL *)


structure PrintControl : PRINTCONTROL =
struct

  val {newBool, newInt, ...} = MakeControls.make {name = "Print", priority = [1]}

  val printDepth = newInt ("depth", "max print depth", 10)
  val printLength = newInt ("length", "max print length", 16)
  val stringDepth = newInt ("string-depth", "max string print depth", 70)
  val intinfDepth = newInt ("intinf-depth", "max IntInf.int print depth", 70)
  val lineWidth = newInt ("lineWidth", "line-width hint for pretty printer", 79)

  val printLoop = newBool ("loop", "print loop", true) (* ? *)
  val signatures = newInt ("signatures", "max signature expansion depth", 2) (* ? *)
  val printOpens = newBool ("opens", "print `open'", true)

  val out = ref {say = fn s => TextIO.output(TextIO.stdOut,s),
		 flush = fn () => TextIO.flushOut TextIO.stdOut}

  fun say s = #say (!out) s
  fun flush() = #flush (!out) ()

end (* structure PrintControl *)

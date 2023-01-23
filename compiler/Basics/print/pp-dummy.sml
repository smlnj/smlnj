 (* pp-dummy.sml
 *
 * COPYRIGHT (c) 2006 The SML/NJ Fellowship
 *
 * Dummy use of pp-lib (smlnj-lib/PP)
 *
 *)


signature PPDUMMY =
sig
  type device
  type stream
  val with_pp : device -> (stream -> unit) -> unit
end

structure PPDummy : PPDUMMY =
struct

structure Dev =
struct
  type device =
       {consumer : string -> unit,
        linewidth : unit -> int,
        flush : unit -> unit}
  type style = unit
  fun sameStyle _ = true
  fun pushStyle _ = ()
  fun popStyle _ = ()
  fun defaultStyle _ = ()
  fun maxDepth _ = NONE
  fun setMaxDepth _ = ()
  fun ellipses _ = ("", 0)
  fun setEllipses _ = ()
  fun setEllipsesWithSz _ = ()
  fun lineWidth ({consumer, linewidth, flush} : device) = SOME(linewidth())
  fun setLineWidth _ = ()
  fun maxIndent _ = NONE
  fun setMaxIndent _ = ()
  fun textWidth _ = NONE
  fun setTextWidth _ = ()
  fun space ({consumer, linewidth, flush}, n) =
	consumer (StringCvt.padLeft #" " n "")
  val indent = space
  fun newline ({consumer, linewidth, flush}: device) = consumer "\n"
  fun string ({consumer, linewidth, flush}: device, s) = consumer s
  fun char ({consumer, linewidth, flush}: device, c) = consumer(str c)
  fun flush ({consumer, linewidth, flush}: device) = flush()
end

(* create an instance of the pretty printer library *)
structure PP = PPStreamFn (structure Token = StringToken structure Device = Dev)

type device = Dev.device
type stream = PP.stream

fun with_pp device (f: PP.stream -> unit) =
    let val ppstrm = PP.openStream device
     in f ppstrm;
	PP.closeStream ppstrm
    end

end (* structure PP-Dummy *)

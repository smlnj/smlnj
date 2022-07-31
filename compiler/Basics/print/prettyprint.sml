 (* prettyprint.sml
 *
 * COPYRIGHT (c) 2006 The SML/NJ Fellowship
 *
 * PrettyPrinter initialization using the new Pretty Printing
 * API in SMLNJ-lib
 *
 * An implementation of SML/NJ's PP interface.
 *   - This is an (almost) literal copy of the original code in
 *     smlnj-lib/PP/examples/old-pp.sml
 *)


signature PRETTYPRINT =
sig
  include PP_STREAM
  val openVBoxI : stream -> int -> unit
  val defaultDevice : device
  val mkDevice : int -> device
  val with_pp : device -> (stream -> unit) -> unit
  val with_pp_sans : device -> (stream -> unit) -> unit
  val with_default_pp : (stream -> unit) -> unit
  val with_default_pp_sans: int -> (stream -> unit) -> unit  (* 1st arg is linewidth *)
  val pp_to_string : int -> (stream -> 'a -> unit) -> 'a -> string
  val pp_to_string_sans : int -> (stream -> 'a -> unit) -> 'a -> string
end

structure PrettyPrint : PRETTYPRINT =
  struct

    type ppconsumer = {
	consumer : string -> unit,
	linewidth : unit -> int,
	flush : unit -> unit
      }

    structure Dev =
      struct
	type device = ppconsumer
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
	fun newline {consumer, linewidth, flush} = consumer "\n"
	fun string ({consumer, linewidth, flush}, s) = consumer s
	fun char ({consumer, linewidth, flush}, c) = consumer(str c)
	fun flush {consumer, linewidth, flush} = flush()
      end

    (* create an instance of the pretty printer library *)
    structure PP = PPStreamFn
	(structure Token = StringToken
	 structure Device = Dev)

    open PP

    (* open a VBox with Abs indent and immediately cut *)
    fun openVBoxI ppstream (indent: int) =
	(openVBox ppstream (Abs indent);
	 cut ppstream)

    (* extend the pretty printer interface with the following functions *)

    val defaultDevice : device =
	{consumer = Control_Print.say,
	 linewidth = (fn () => !Control_Print.lineWidth),
	 flush = Control_Print.flush}

    fun mkDevice (lineWidth : int) : device =
	{consumer = Control_Print.say,
	 linewidth = (fn () => lineWidth),
	 flush = Control_Print.flush}

    fun with_pp device (f: PP.stream -> unit) =
	let val ppstrm = PP.openStream device
	 in f ppstrm;
	    PP.closeStream ppstrm
	end

    fun with_pp_sans device (f: PP.stream -> unit) =
	let val ppstrm = PP.openStream device
	 in f ppstrm;
	    PP.closeStream ppstrm
	end

    fun with_default_pp (f: PP.stream -> unit) =
	let val ppstrm = PP.openStream(defaultDevice)
	 in f ppstrm;
	    PP.closeStream ppstrm
	end

    fun with_default_pp_sans lineLength (f: PP.stream -> unit) =
	let val device = mkDevice (lineLength)
	    val ppstrm = PP.openStream(device)
	 in f ppstrm;
	    PP.closeStream ppstrm
	end

    fun pp_to_string wid ppFn obj =
	let val l = ref ([] : string list)
	    fun attach s = l := s :: !l
	    val device = {consumer = attach, linewidth = (fn _ => wid),
			  flush = fn()=>()}
	 in with_pp device
	      (fn ppStrm => ppFn ppStrm obj);
	    String.concat(List.rev(!l))
	end

    fun pp_to_string_sans wid ppFn obj =
	let val buffer = ref ([] : string list)
	    fun attach s = buffer := s :: !buffer
	    val device = {consumer = attach,
			  linewidth = (fn _ => wid),
			  flush = fn()=>()}
	 in with_pp_sans device (fn ppStrm => ppFn ppStrm obj);
	    String.concat(List.rev(!buffer))
	end

end (* structure PrettyPrint *)

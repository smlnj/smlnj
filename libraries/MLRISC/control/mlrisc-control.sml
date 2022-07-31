(* mlrisc-control.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *)

signature MLRISC_CONTROL =
sig
    val registry : ControlRegistry.registry
    val prefix : string
    val priority : Controls.priority

    type cpu_time = {gc:Time.time,usr:Time.time,sys:Time.time}

    val mlrisc        : bool ref               (* use the MLRISC optimizer? *)
    val mlrisc_phases : string list ref        (* the optimization phases *)
    val debug_stream  : TextIO.outstream ref   (* debugging output goes here *)

    type 'a set = ('a, 'a ref) ControlSet.control_set

        (* Flags and counters *)
    val counters    : int set
    val ints        : int set
    val flags       : bool set
    val reals       : real set
    val strings     : string set
    val stringLists : string list set
    val timings     : cpu_time set

    val mkCounter    : string * string -> int ref
    val mkInt        : string * string -> int ref
    val mkFlag       : string * string -> bool ref
    val mkReal       : string * string -> real ref
    val mkString     : string * string -> string ref
    val mkStringList : string * string -> string list ref
    val mkTiming     : string * string -> cpu_time ref

    val counter      : string -> int ref
    val int          : string -> int ref
    val flag         : string -> bool ref
    val real         : string -> real ref
    val string       : string -> string ref
    val stringList   : string -> string list ref
    val timing       : string -> cpu_time ref

    (* The following is the old interface.  Its use is deprecated
     * since it does not provide documentation strings. *)
    val getCounter    : string -> int ref
    val getInt	      : string -> int ref
    val getFlag       : string -> bool ref
    val getReal       : string -> real ref
    val getString     : string -> string ref
    val getStringList : string -> string list ref
    val getTiming     : string -> cpu_time ref

end

structure MLRiscControl : MLRISC_CONTROL = struct

    val priority = [10, 3]
    val obscurity = 3
    val prefix = "mlrisc"

    val registry = ControlRegistry.new { help = "MLRISC" }

    type cpu_time = {gc:Time.time,usr:Time.time,sys:Time.time}

    type 'a set = ('a, 'a ref) ControlSet.control_set

    val counters      = ControlSet.new () : int set
    val ints          = ControlSet.new () : int set
    val flags         = ControlSet.new () : bool set
    val reals         = ControlSet.new () : real set
    val strings       = ControlSet.new () : string set
    val stringLists   = ControlSet.new () : string list set
    val timings       = ControlSet.new () : cpu_time set

    local
	val timing =
	    { tyName = "timing",
	      fromString = fn _ => (NONE : cpu_time option),
	      toString = fn _ => "<timing>" }

	fun no x = NONE
	fun yes x =
	    SOME (ControlUtil.EnvName.toUpper "MLRISC_" (Controls.name x))

	val nextpri = ref 0

	fun mk (set, cvt, fallback, en) (stem, descr) =
	    case ControlSet.find (set, Atom.atom stem) of
		SOME { ctl, info = cell } => cell
	      | NONE => let
		    val cell = ref fallback
		    val p = !nextpri
		    val ctl = Controls.control { name = stem,
						 pri = [p],
						 obscurity = obscurity,
						 help = descr,
						 ctl = cell }
		in
		    nextpri := p + 1;
		    ControlRegistry.register registry
			{ ctl = Controls.stringControl cvt ctl,
			  envName = en ctl };
		    ControlSet.insert (set, ctl, cell);
		    cell
		end
    in
        fun mkCounter x = mk (counters, ControlUtil.Cvt.int, 0, no) x
	fun mkInt x = mk (ints, ControlUtil.Cvt.int, 0, yes) x
	fun mkFlag x = mk (flags, ControlUtil.Cvt.bool, false, yes) x
	fun mkReal x = mk (reals, ControlUtil.Cvt.real, 0.0, yes) x
	fun mkString x = mk (strings, ControlUtil.Cvt.string, "", yes) x
	fun mkStringList x =
	    mk (stringLists, ControlUtil.Cvt.stringList, [], yes) x
	fun mkTiming x = mk (timings, timing, {gc =Time.zeroTime,
					       usr=Time.zeroTime,
					       sys=Time.zeroTime}, no) x

	val mlrisc = mkFlag ("mlrisc", "?")
	val mlrisc_phases = mkStringList ("phases", "MLRISC phases")
	val debug_stream  = ref TextIO.stdOut
    end

    local
	fun find set stem =
	    case ControlSet.find (set, Atom.atom stem) of
		SOME { ctl, info = cell } => cell
	      | NONE => raise Fail ("Control.MLRISC: no such control: " ^ stem)
    in
        val counter = find counters
	val int = find ints
	val flag = find flags
	val real = find reals
	val string = find strings
	val stringList = find stringLists
	val timing = find timings
    end

    local
	fun old_for mkFoo s = mkFoo (s, s ^ " setting")
    in
        val getCounter = old_for mkCounter
        val getInt = old_for mkInt
        val getFlag = old_for mkFlag
        val getReal = old_for mkReal
        val getString = old_for mkString
        val getStringList = old_for mkStringList
        val getTiming = old_for mkTiming
    end
end

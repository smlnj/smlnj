(* printcontrol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
signature PRINTCONTROL = sig
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
end

structure Control_Print : PRINTCONTROL = struct

    val priority = [10, 10, 2]
    val obscurity = 2
    val prefix = "print"

    val registry = ControlRegistry.new { help = "compiler print settings" }

    val _ = BasicControl.nest (prefix, registry, priority)

    val bool_cvt = ControlUtil.Cvt.bool
    val int_cvt = ControlUtil.Cvt.int

    val nextpri = ref 0

    fun new (c, n, h, d) = let
	val r = ref d
	val p = !nextpri
	val ctl = Controls.control { name = n,
				     pri = [p],
				     obscurity = obscurity,
				     help = h,
				     ctl = r }
    in
	nextpri := p + 1;
	ControlRegistry.register
	    registry
	    { ctl = Controls.stringControl c ctl,
	      envName = SOME (ControlUtil.EnvName.toUpper "PRINT_" n) };
	r
    end

    val printDepth = new (int_cvt, "depth", "max print depth", 10)
    val printLength = new (int_cvt, "length", "max print length", 16)
    val stringDepth =
	new (int_cvt, "string-depth", "max string print depth", 70)
    val intinfDepth =
	new (int_cvt, "intinf-depth", "max IntInf.int print depth", 70)
    val lineWidth =
	new (int_cvt, "lineWidth", "line-width hint for pretty printer", 79)

    val printLoop = new (bool_cvt, "loop", "print loop", true) (* ? *)
    val signatures =
	new (int_cvt, "signatures", "max signature expansion depth", 2) (* ? *)
    val printOpens = new (bool_cvt, "opens", "print `open'", true)
    val out = ref{
		  say = fn s => TextIO.output(TextIO.stdOut,s),
		  flush = fn () => TextIO.flushOut TextIO.stdOut
		  }
    fun say s = #say (!out) s
    fun flush() = #flush (!out) ()
end

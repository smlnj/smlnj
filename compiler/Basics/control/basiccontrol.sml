(* basiccontrol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
signature BASIC_CONTROL = sig
    (* if false, suppress all warning messages *)
    val printWarnings : bool ref

    (* the top-level registry of the compiler: *)
    val topregistry : ControlRegistry.registry

    (* nest a tier-2 registry within the top-level registry *)
    val nest : string * ControlRegistry.registry * Controls.priority -> unit
end

structure BasicControl : BASIC_CONTROL = struct

    val topregistry = ControlRegistry.new { help = "SML/NJ controls" }

    val registry = ControlRegistry.new { help = "compiler settings" }

    fun nest (prefix, reg, pri) =
	ControlRegistry.nest topregistry { prefix = SOME prefix,
					   pri = pri,
					   obscurity = 0,
					   reg = reg }

    val _ = nest ("basic", registry, [10, 10, 1])

    val printWarnings = let
	val r = ref true
	val ctl = Controls.control { name = "print-warnings",
				     pri = [0],
				     obscurity = 1,
				     help = "whether warnings get generated",
				     ctl = r }
	val sctl = Controls.stringControl ControlUtil.Cvt.bool ctl
    in
	ControlRegistry.register registry
	    { ctl = sctl, envName = SOME "PRINT_CONTROL" };
	r
    end
end

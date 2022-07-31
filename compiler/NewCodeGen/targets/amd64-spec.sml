(* amd64spec.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * See dev-notes/amd64-stack-frame.numbers for stack-frame layout information.
 *)

structure AMD64Spec : MACH_SPEC =
  struct

    structure DMS = DefaultMachSpecFn (
      struct
	val wordByteWidth = 8
	val addressByteWidth = 8
      end)
    open DMS

    val architecture = "amd64"
    val llvmTargetName = "x86_64"
    val bigEndian = false

  (* spill-area info; this should match the layout in runtime/mach-dep/AMD.prim.asm,
   * which is also documented in dev-info/amd64-stack-frame.numbers.
   *)
    val spillAreaSz = 8 * 1024

    val numRegs = 10	(* length AMD64CpsRegs.miscregs + 3 *)
    val numArgRegs = 4  (* non-callee-save misc regs *)
    val numFloatRegs = 16
    val numFloatCalleeSaves = 0

  end

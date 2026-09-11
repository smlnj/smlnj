(* amd64spec.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
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

    (* the AMD64 has 7 misc regs and 16 float regs *)
    val numRegs = 10	        (* #misc regs + 3 *)
    val numArgRegs = 5          (* non-callee-save misc regs + stdArg *)
    val numFloatRegs = 16
    val numFloatArgRegs = 14
    val numFloatCalleeSaves = 0

  end

(* arm64-spec.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * See dev-notes/amd64-stack-frame.numbers for stack-frame layout information.
 *)

structure Arm64Spec : MACH_SPEC =
  struct

    structure DMS = DefaultMachSpecFn (
      struct
	val wordByteWidth = 8
	val addressByteWidth = 8
      end)
    open DMS

    val architecture = "arm64"
    val llvmTargetName = "aarch64"
    val bigEndian = false

    val numRegs = 20	                (* #misc regs + 3 *)
    val numArgRegs = 14                 (* non-callee-save misc regs *)
    val numFloatRegs = 32               (* number of floating-point registers *)

  end

(* arm64-spec.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
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

    (* the ARM64 has 18 misc regs and 32 float regs *)
(* FIXME: numRegs = 22? (stdLink + stdClos + stdCont + stdArg + 17 misc regs *)
    val numRegs = 21	                (* #misc regs + 3 *)
(* numArgsRegs = 15? *)
    val numArgRegs = 16                 (* non-callee-save misc regs + stdArg *)
    val numFloatRegs = 16               (* number of floating-point registers *)
    val numFloatArgRegs = 16            (* number of floating-point argument registers *)

  end

(* mach-spec.sig
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This signature contains various machine and code-generator specific
 * parameters.
 *
 * When should a code-generator parameter be put in this signature?
 * Only when changing it will yield incompatible code.
 * Parameters that change optimization algorithms but yield compatible
 * code should not go here.       -- A. Appel
 *
 *)

signature MACH_SPEC =
  sig

  (* the architecture name used by CM when generating bin files, etc. *)
    val architecture : string

  (* the LLVM name for the target architecture.  It must match one of the names
   * used in `codegen/src/target-info.cxx`.
   *)
    val llvmTargetName : string

  (* true to enable use of floating-point registers, for argument passing, etc.
   * It is true by default.
   *)
    val unboxedFloats : bool

  (* the number of registers available for SML values; this value is usually the
   * number of misc registers plus three (three of stdarg, stdlink, stdclos, and
   * stdcont, which leaves a register to hold a pointer to a record containing
   * the other values).
   *)
    val numRegs : int
  (* the number of registers for function-argument passing; this includes the
   * standard argument and the non-callee-save misc registers.
   *)
    val numArgRegs : int
  (* the number of floating-point registers available to SML code *)
    val numFloatRegs : int
  (* the number of FP registers used for args. *)
    val numFloatArgRegs : int
  (* number of general-purpose callee-save registers; this should be three
   * (changing it will break everything!!!).
   *)
    val numCalleeSaves : int
  (* the number of floating-point callee-save registers; this is zero for all
   * targets.
   *)
    val numFloatCalleeSaves : int

  (* machine representations *)
    type value_tag = {
	tagbits : int,		(* number of tag bits *)
	tagval : int		(* value of tag bits *)
      }

    val intTag : value_tag	(* tag for tagged integer values *)
    val ptrTag : value_tag	(* tag for pointers *)
    val descTag : value_tag	(* tag for object descriptors *)

  (* representations of object descriptors *)
    structure ObjDesc : OBJECT_DESC

    val valueSize : int		(* number of bytes for an ML value *)
    val charSize : int		(* number of bytes for a char *)
    val realSize : int		(* number of bytes of the default real type *)
    val realAlign : bool	(* if true, reals are realSize aligned *)

    val bigEndian : bool	(* true, if this is a big-endian machine *)

  (* the size (in bytes) of the area reserved for register spilling by
   * the native code generator. The CPS spill phase should guarantee that we
   * never have more spillable values that can be held in this area.
   *)
    val spillAreaSz : int

  (* number of bits and bytes per ML word *)
    val wordBitWidth	: int
    val wordByteWidth	: int

  (* number of bits and bytes per C pointer *)
    val addressByteWidth : int
    val addressBitWidth  : int

  end (* MACH_SPEC *)

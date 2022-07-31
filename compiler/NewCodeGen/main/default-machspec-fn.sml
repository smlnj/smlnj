(* default-machspec-fn.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor DefaultMachSpecFn (Sizes : sig

(* 64BIT: since we no longer support the DEC Alpha, which was the only 32/64-bit
 * target, we can assume that wordByteWidth == addressByteWidth for all targets.
 *)
    val wordByteWidth : int		(* size of ML value (aka word) in bytes *)
    val addressByteWidth : int		(* native size of an address/pointer *)

  end) : MACH_SPEC = struct

    val architecture = ""
    val llvmTargetName = ""

    val numRegs = ~1
    val numFloatRegs = ~1
    val bigEndian = false

  (* the area reserved for the register spilling is sized to hold 1K SML
   * values.
   *)
    val spillAreaSz = Sizes.wordByteWidth * 1024

    val unboxedFloats = true
    val numArgRegs = ~1
    val numFloatArgRegs = ~1
    val numCalleeSaves = 3	(* miscreg0, miscreg1, miscreg2 *)
    val numFloatCalleeSaves = 0

    type value_tag = {
	tagbits : int,
	tagval : int
      }

    val intTag = {tagbits=1, tagval=1}
    val ptrTag = {tagbits=2, tagval=0}
    val descTag= {tagbits=2, tagval=2}

  (* representations of object descriptors *)
    structure ObjDesc = ObjectDesc

    val valueSize = Sizes.wordByteWidth
    val charSize  = 1
    val realSize  = 8
    val realAlign = true

  (* number of bits and bytes per ML word *)
    val wordByteWidth = Sizes.wordByteWidth
    val wordBitWidth = 8*wordByteWidth

  (* number of bits and bytes per C pointer *)
    val addressByteWidth = Sizes.addressByteWidth
    val addressBitWidth = 8 * addressByteWidth

  end (* DefaultMachSpecFn *)

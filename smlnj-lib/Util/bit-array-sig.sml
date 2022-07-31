(* bit-array-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Signature for mutable bit array.  The model here treats bit array as an
 * array of bools.
 *)

signature BIT_ARRAY =
  sig

    include MONO_ARRAY

    val fromString : string -> array option
      (* The string argument gives a hexadecimal
       * representation of the bits set in the
       * array. Characters 0-9, a-f and A-F are
       * allowed. For example,
       *  fromString "1af8" = 0001101011111000
       *  (by convention, 0 corresponds to false and 1 corresponds
       *  to true, bit 0 appears on the right,
       *  and indices increase to the left)
       * The length of the array will be 4*(size string).
       * Returns NONE if a non-hexadecimal character
       * appears in the string.
       *)

    val bits : (int * int list) -> array
      (* Create array of the given length with the indices of its set bits
       * given by the list argument.
       * Raises Subscript if a list item is < 0 or >= length.
       *)

    val getBits : array -> int list
      (* Returns list of bits set in bit array, in increasing
       * order of indices.
       *)

    val toString : array -> string
      (* Inverse of stringToBits.
       * The bit array is zero-padded to the next
       * length that is a multiple of 4.
       *)

    val isZero  : array -> bool
      (* Returns true if and only if no bits are set. *)

    val extend0 : (array * int) -> array
    val extend1 : (array * int) -> array
      (* Extend bit array by 0's or 1's to given length.
       * If bit array is already >= argument length, return a copy
       * of the bit array.
       * Raises Size if length < 0.
       *)

    val eqBits : (array * array) -> bool
      (* true if set bits are identical *)
    val equal : (array * array) -> bool
      (* true if same length and same set bits *)

    val andb : (array * array * int) -> array
    val orb  : (array * array * int) -> array
    val xorb : (array * array * int) -> array
      (* Create new array of the given length
       * by logically combining bits of original
       * array using and, or and xor, respectively.
       * If necessary, the array are
       * implicitly extended by 0 to be the same length
       * as the new array.
       *)

    val notb  : array -> array
      (* Create new array with all bits of original
       * array inverted.
       *)

    val >> : (array * word) -> array
    val << : (array * word) -> array
	(* shift operations *)

    val lshift  : (array * int) -> array		(* DEPRECATED *)
      (* lshift(ba,n) creates a new array by
       * inserting n 0's on the right of ba.
       * The new array has length n + length ba.
       *)

    val rshift  : (array * int) -> array		(* DEPRECATED *)
      (* rshift(ba,n) creates a new array of
       * of length max(0,length ba - n) consisting
       * of bits n,n+1,...,length ba - 1 of ba.
       * If n >= length ba, the new array has length 0.
       *)

  (* mutable operations for array *)

    val setBit : (array * int) -> unit
    val clrBit : (array * int) -> unit
      (* Update value at given index to new value.
       * Raises Subscript if index < 0 or >= length.
       * setBit(ba,i) = update(ba,i,true)
       * clrBit(ba,i) = update(ba,i,false)
       *)

    val union : array -> array -> unit
    val intersection : array -> array -> unit
      (* Or (and) second bitarray into the first. Second is
       * implicitly truncated or extended by 0's to match
       * the length of the first.
       *)

    val complement : array -> unit
      (* Invert all bits. *)

  end (* BIT_ARRAY *)
    where type elem = bool

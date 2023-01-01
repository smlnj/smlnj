(* target32-native.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Aliases for the native-size word and integer structures on 32-bit
 * platforms.  These are defined so that we can refer to them in
 * signatures.
 *)

structure NativeInt = Int32
structure NativeWord = Word32

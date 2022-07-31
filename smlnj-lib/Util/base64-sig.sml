(* base64-sig.sml
 *
 * COPYRIGHT (c) 2012 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Support for Base64 encoding/decoding as specified by RFC 4648.
 *
 *	http://www.ietf.org/rfc/rfc4648.txt
 *)

signature BASE64 =
  sig

  (* return true if a character is in the base64 alphabet *)
    val isBase64 : char -> bool

    val encode : Word8Vector.vector -> string
    val encodeSlice : Word8VectorSlice.slice -> string

  (* raised if a Base64 string does not end in a complete encoding quantum (i.e., 4
   * characters including padding characters).
   *)
    exception Incomplete

  (* raised if an invalid Base64 character is encountered during decode.  The int
   * is the position of the character and the char is the invalid character.
   *)
    exception Invalid of (int * char)

  (* decode functions that ignore whitespace *)
    val decode : string -> Word8Vector.vector
    val decodeSlice : substring -> Word8Vector.vector

  (* strict decode functions that only accept the base64 characters *)
    val decodeStrict : string -> Word8Vector.vector
    val decodeSliceStrict : substring -> Word8Vector.vector

  end

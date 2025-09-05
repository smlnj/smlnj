(* test-leb128.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Tests for LEB128 encoding and decoding
 *)

structure TestLEB128 =
  struct

    local
      structure W8V = Word8Vector
      structure W8VS = Word8VectorSlice
      fun sameVec (v1, v2) = (W8V.collate Word8.compare (v1, v2) = EQUAL)
    in

    fun testInt64 n = let
          fun error msg = raise Fail(concat["testInt ", Int64.toString n, ": ", msg])
          val v = LEB128.encodeInt n
          in
            case LEB128.decodeInt W8VS.getItem (W8VS.full v)
             of SOME(n', v') => if (n <> n')
                    then error "encode/decode mismatch"
                  else if (W8VS.isEmpty v')
                    then ()
                    else error "non-empty residual"
              | NONE => error "incomplete decode"
            (* end case *)
          end

    fun testIntInf n = let
          fun error msg = raise Fail(concat["testInt ", IntInf.toString n, ": ", msg])
          val v = LEB128.encodeIntInf n
          in
            case LEB128.decodeIntInf W8VS.getItem (W8VS.full v)
             of SOME(n', v') => if (n <> n')
                    then error "encode/decode mismatch"
                  else if (W8VS.isEmpty v')
                    then ()
                    else error "non-empty residual"
              | NONE => error "incomplete decode"
            (* end case *)
          end

    fun testWord64 n = let
          fun error msg = raise Fail(concat["testInt 0wx", Word64.toString n, ": ", msg])
          val v = LEB128.encodeInt n
          in
            case LEB128.decodeInt W8VS.getItem (W8VS.full v)
             of SOME(n', v') => if (n <> n')
                    then error "encode/decode mismatch"
                  else if (W8VS.isEmpty v')
                    then ()
                    else error "non-empty residual"
              | NONE => error "incomplete decode"
            (* end case *)
          end

(* some example encodings:

    encodeInt 0 = Word8Vector.fromList [0w0]

    encodeInt 63 = Word8Vector.fromList [0wx3F]

    encodeInt 64 = Word8Vector.fromList [0wxC0, 0wx0]

    encodeInt ~1 = Word8Vector.fromList [0wx7F]]

    encodeInt ~123456 =  Word8Vector.fromList [0wxC0, 0wxBB, 0wx78]

    encodeWord 0w0 = Word8Vector.fromList [0w0]

    encodeWord 0w63 = Word8Vector.fromList [0wx3F]

    encodeWord 0w64 = Word8Vector.fromList [0wx40]

    encodeWord 0w127 = Word8Vector.fromList [0wx7F]

    encodeWord 0w128 = Word8Vector.fromList [0wx80,0wx1]

    encodeWord 0w624485 = Word8Vector.fromList [0wxE5 0wx8E 0wx26]
*)

    end (* local *)

  end

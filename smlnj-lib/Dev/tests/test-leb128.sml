(* test-leb128.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Tests for LEB128 encoding and decoding
 *)

CM.make "../../Util/smlnj-lib.cm";

structure TestLEB128 =
  struct

    local
      structure W8V = Word8Vector
      structure W8VS = Word8VectorSlice
      fun sameVec (v1, v2) = (W8V.collate Word8.compare (v1, v2) = EQUAL)

      fun wToS w = "0wx" ^ Word.toString w
      fun w64ToS w = "0wx" ^ Word64.toString w

      fun okay name = TextIO.output(TextIO.stdOut, concat[name, " okay\n"])
      fun failure (name, msg) = (
            TextIO.output(TextIO.stdOut, concat(name :: " failed: " :: msg));
            TextIO.output(TextIO.stdOut, "\n"))
    in

    fun testInt64 n = let
          fun error msg = raise Fail(concat["testInt64 ", Int.toString n, ": ", msg])
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
          fun error msg = raise Fail(concat["testIntInf ", IntInf.toString n, ": ", msg])
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
          fun error msg = raise Fail(concat["testWord ", w64ToS n, ": ", msg])
          val v = LEB128.encodeWord64 n
          in
            case LEB128.decodeWord64 W8VS.getItem (W8VS.full v)
             of SOME(n', v') => if (n <> n')
                    then error "encode/decode mismatch"
                  else if (W8VS.isEmpty v')
                    then ()
                    else error "non-empty residual"
              | NONE => error "incomplete decode"
            (* end case *)
          end

    (* decode tests *)
    local
      fun get [] = NONE
        | get (b::r) = SOME(b, r)
      datatype 'a decode_result = NO_RESULT | OVERFLOW | RESULT of 'a
      fun wrapDecode decodeFn arg =
            ((* try *)
              (case decodeFn get arg of SOME(v, _) => RESULT v | _ => NO_RESULT)
            handle Overflow => OVERFLOW)
      fun test toS decodeFn (name, src, expected) = (
            case (wrapDecode decodeFn src, expected)
             of (NO_RESULT, NO_RESULT) => okay name
              | (RESULT a, RESULT b) => if (a = b)
                  then okay name
                  else failure (name, ["expected ", toS b, ", but got ", toS a])
              | (RESULT _, _) => failure (name, ["unexpected result"])
              | (OVERFLOW, OVERFLOW) => okay name
              | (OVERFLOW, _) =>
                  failure (name, ["unexpected overflow"])
              | (NO_RESULT, RESULT a) =>
                  failure (name, [" expected ", toS a, ", but got no result"])
              | (NO_RESULT, OVERFLOW) =>
                  failure (name, [" expected Overflow, but got no result"])
            (* end case *))
      val testInt = test Int.toString LEB128.decodeInt
      val testInt64 = test Int64.toString LEB128.decodeInt64
      val testIntInf = test IntInf.toString LEB128.decodeIntInf
      val testWord = test wToS LEB128.decodeWord
      val testWord64 = test w64ToS LEB128.decodeWord64
    in
    fun decodeIntTests () = (
          testInt ("decodeInt-001", [0w0], RESULT 0);
          testInt ("decodeInt-002", [0wx3F], RESULT 63);
          testInt ("decodeInt-003", [0wxC0, 0wx0], RESULT 64);
          testInt ("decodeInt-004", [0wx7F], RESULT ~1);
          testInt ("decodeInt-005", [0wxC0, 0wxBB, 0wx78], RESULT ~123456);
          testInt ("decodeInt-006", [], NO_RESULT);
          testInt ("decodeInt-007", [0wxC0, 0wxBB], NO_RESULT);
          (* the maximum 63-bit integer *)
          testInt ("decodeInt-008",
            [0wxFF,0wxFF,0wxFF,0wxFF,0wxFF,0wxFF,0wxFF,0wxFF,0wx3F],
            RESULT 4611686018427387903);
          (* the maximum 63-bit integer + 1 *)
          testInt ("",
            [0wx80,0wx80,0wx80,0wx80,0wx80,0wx80,0wx80,0wx80,0wxC0,0wx0],
            OVERFLOW);
          ())
    end (* local *)
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

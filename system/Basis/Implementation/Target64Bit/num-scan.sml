(* num-scan.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * The string to number conversions for the default word and int types
 * on 64-bit machines.
 *)

structure NumScan : sig

    val scanWord : StringCvt.radix
	  -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
    val scanInt  : StringCvt.radix
	  -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader

  end = struct

    fun scanWord radix = let
          val scanLarge = NumScan64.scanWord radix
          fun scan getc cs = (case (scanLarge getc cs)
                 of NONE => NONE
                  | (SOME(w, cs')) => if InlineT.Word64.>(w, 0wx7FFFFFFFFFFFFFFF)
                      then raise Overflow
                      else SOME(InlineT.Word.fromWord64 w, cs')
                (* end case *))
          in
            scan
          end

    fun scanInt radix = let
          val scanInt64 = NumScan64.scanInt radix
          fun f getc cs = (case scanInt64 getc cs
                   of NONE => NONE
                    | SOME(i, cs') => SOME(InlineT.Int64.toInt i, cs')
                  (* end case *))
          in
            f
          end

  end

(* num-scan.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * The string to number conversions for the default word and int types
 * on 32-bit machines.
 *)

structure NumScan : sig

    val scanWord : StringCvt.radix
	  -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
    val scanInt  : StringCvt.radix
	  -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader

  end = struct

    fun scanWord radix = let
          val scanLarge = NumScan32.scanWord radix
          fun scan getc cs = (case (scanLarge getc cs)
                 of NONE => NONE
                  | (SOME(w, cs')) => if InlineT.Word32.>(w, 0wx7FFFFFFF)
                      then raise Overflow
                      else SOME(InlineT.Word.fromWord32 w, cs')
                (* end case *))
          in
            scan
          end

    fun scanInt radix = let
          val scanInt32 = NumScan32.scanInt radix
          fun f getc cs = (case scanInt32 getc cs
                   of NONE => NONE
                    | SOME(i, cs') => SOME(InlineT.Int32.toInt i, cs')
                  (* end case *))
          in
            f
          end

  end

(* this is a SML implementation of the Assembly.A.scalb function
 * that is currently implemented in machine-specific assembly code.
 *)

local
structure W64 = Word64
val signMask : W64.word = 0wx8000000000000000
val expMask : W64.word = 0wx7ff0000000000000
val notExpMask = W64.notb expMask
val posInfBits : W64.word = 0wx7ff0000000000000
val toBits = Unsafe.Real64.castToWord
val fromBits = Unsafe.Real64.castFromWord
in
fun scalb (x : real, n : int) = let
      val bits = toBits x
      val exp = W64.andb(bits, 0wx7ff0000000000000)
      in
        if (exp = 0w0)
          (* `x` is either zero or sub-normal; so do not adjust the exponent *)
          then x
          else let
            val exp' = W64.>>(exp, 0w52) + Word64.fromInt n
            in
              if (exp' < 0w2047)
                then let
                  (* insert adjusted exponent back into bits *)
                  val bits' = W64.orb(
                        W64.<<(exp', 0w52),
                        W64.andb(bits, 0wx800fffffffffffff))
                  in
                    fromBits bits'
                  end
                else let
                  val signBit = W64.andb(signMask, bits)
                  in
                    (* test sign of adjusted exponent *)
                    if (W64.andb(exp', 0wx8000000000000000) = 0w0)
                      (* overflow: return signed inf *)
                      then fromBits(Word64.orb(signBit, posInfBits))
                      (* underflow: return signed 0 *)
                      else fromBits signBit
                  end
            end
      end
end (* local *)

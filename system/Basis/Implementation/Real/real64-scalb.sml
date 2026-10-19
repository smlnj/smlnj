(* real64-scalb.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * This module is an SML implementation of the `Assembly.A.scalb` function
 * that is currently implemented in machine-specific assembly code.
 *)

structure Real64Scalb : sig

    (* `scalb (x, k)` returns `2^k * x`.  We assume that `x` is finite (*i.e.*,
     * neither a infinity or NaN.
     *)
    val scalb : real * int -> real

  end = struct

    local
      structure W64 = struct
          open Word64
          val rshiftl = >>
          val lshift = <<
        end
      structure R64 = struct
          open Real64
          val fromBits = Unsafe.Real64.castFromWord
          val toBits = Unsafe.Real64.castToWord
          val from_int64 = Real64.fromLargeInt o I64.toLarge
        end
      val signMask : W64.word = 0wx8000000000000000
      val expMask : W64.word = 0wx7ff0000000000000
      val notExpMask = W64.notb expMask
      val posInfBits : W64.word = 0wx7ff0000000000000
      val toBits = Unsafe.Real64.castToWord
      val fromBits = Unsafe.Real64.castFromWord
    in
    fun scalb (x : real, n : int) = let
        val bits = R64.toBits x
        val exp = W64.andb(bits, 0wx7ff0000000000000)
        in
          if (exp = 0w0)
            (* `x` is either zero or sub-normal; so do not adjust the exponent *)
            then x
            else let
              val exp' = W64.>>(exp, 0w52) + W64.fromInt n
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

(* from Math64

  (* get the biased exponent from a 64-bit real as an int in the range (0 .. 2047) *)
    fun getBiasedExp x = Word.toIntX(Word.andb(
	  Word.fromLarge(Word64.rshiftl(InlineT.Real64.toBits x, 0w52)),
	  0wx7ff))

  (* This function is IEEE double-precision specific;
     it works correctly on subnormal inputs and outputs;
     we do not apply it to 0.0, inf's, or nan's *)
    fun scalb (x, k) = let
	  val biasedExp = getBiasedExp x
	  in
	    if (biasedExp = 0)
	      then scalb(x * two_to_the_54, I.-(k, 54))			(*2*)
	    else if I.ltu(I.+(k, biasedExp), 2047)
	      then Assembly.A.scalb(x,k)				(*1*)
	      else let
	      (* unbias exponent and add to k *)
		val k' = I.+(k, I.-(biasedExp, 1023))
		in
		  if I.<(k',0)
		    then if I.<(k', I.-(~1022,54))
		      then 0.0						(*3*)
		      else scalb(x,I.+(k,54)) * two_to_the_minus_54	(*4*)
		    else x * plusInfinity				(*5*)
		end
	  end
 (* Proof of correctness of scalb:      (Appel)
     1. if x is normal and x*2^k is normal
           then case (*1*) applies, computes right answer
     2. if x is subnormal and x*2^k is normal
           then case (*2*) reduces problem to case 1.
     3. if x*2^k is sub-subnormal (i.e. 0)
           then case (*3*) applies, returns 0.0
     4. if x*2^k is subnormal
           then ~1076 < k' <= ~1023, case (*4*) applies,
                 computes right answer
     5. if x*2^k is supernormal (i.e. infinity)
           then case (*5*) computes right answer
*)
*)
  end (* Real64Scalb *)

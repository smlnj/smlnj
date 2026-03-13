(* rand48.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * An SML implementation of the rand48 functions from the C standard
 * library.
 *
 * As described in the Open Group Specification, these routines work
 * by generating a sequence of 48-bit integer values, X{i}, according
 * to the linear congruential formula:
 *
 *      X{i+1} = (a * X{i} + c) mod m  for i >= 0
 *
 * where
 *
 *      m = 2^48
 *      a = 0x5DEECE66D = 25214903917
 *      c = 0xB = 11
 *)

structure Rand48 : RAND48 =
  struct

    structure W64 = Word64

    type t = {
        x : W64.word ref,
        a : W64.word,
        c : W64.word
      }

    (* mask low 48 bits *)
    val mask48 = W64.<<(0w1, 0w48) - 0w1
    (* mask low 32 bits *)
    val mask32 = W64.<<(0w1, 0w32) - 0w1
    (* mask low 31 bits *)
    val mask31 = W64.<<(0w1, 0w31) - 0w1

    (* IEEE exponent bias *)
    val ieeeExpBias = Word64.<<(0wx3ff, 0w52)

    val buffer : t = { x = ref 0wx1234abcd330e, a = 0wx5deece66d, c = 0wxb }

    fun seed w = let
          val old = !(#x buffer)
          in
            #x buffer := W64.andb(w, mask48);
            old
          end

    fun srand w = let
          val w = W64.fromLarge(Word.toLarge w)
          in
            ignore (seed (W64.orb (W64.<< (w, 0w16), 0wx330e)))
          end

    fun randStep ({x, a, c} : t) = let
          val next = W64.andb(!x * a + c, mask48)
          in
            x := next; next
          end

    (* convert a 48-bit value to a IEEE double in the range [0..1) *)
    fun mkReal w = let
          val r = Unsafe.Real64.castFromWord (W64.orb(ieeeExpBias, W64.<<(w, 0w4)))
          in
            r - 1.0
          end

    fun drand () = mkReal (randStep buffer)

    fun lrand () = Word.fromLarge (W64.toLarge(W64.andb(randStep buffer, mask31)))

    (* returns a random signed 32-bit number (i.e., in the range -2^31..2^31-1) *)
    fun mrand () = let
          val w = W64.andb(randStep buffer, mask32)
          in
            W64.toIntX(W64.~>>(W64.<<(w, 0w32), 0w32))
          end

  end

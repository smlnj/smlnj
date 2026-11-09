(* rand48-sig.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature RAND48 =
  sig

    (* `seed s` initializes the internal buffer with the 48 low-order
     * bits from `s`.  It returns the previous contents of the buffer.
     *)
    val seed : Word64.word -> Word64.word

    (* `srand w` sets the internal buffer to `(w << 16) | 0x330E` *)
    val srand : word -> unit

    (* returns a random real number in the range 0..1 *)
    val drand : unit -> real

    (* returns a random unsigned 31-bit number (i.e., in the range 0..2^31-1) *)
    val lrand : unit -> word

    (* returns a random signed 32-bit number (i.e., in the range -2^31..2^31-1) *)
    val mrand : unit -> int

  end

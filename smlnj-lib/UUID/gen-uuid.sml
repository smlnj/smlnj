(* gen-uuid.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure GenUUID : sig

  (* generate a new Variant 1, Type 4 UUID. *)
    val new : unit -> UUID.t

  end = struct

    structure W8V = Word8Vector

  (* maximum `int` value plus 1 *)
    val maxInt = IntInf.fromInt (valOf Int.maxInt) + 1

  (* get the current time as (seconds, useconds) *)
    fun getTime () = IntInf.divMod (Time.toMicroseconds(Time.now()), 1000000)

    fun seedRand () = let
	  val (secs, usecs) = getTime ()
	(* initial random seed *)
	  val r = Random.rand (Int.fromLarge(secs mod maxInt), Int.fromLarge usecs)
	(* run the random number generator a few steps *)
	  val r = let
		val n = let val (s, us) = getTime()
		      in
			IntInf.andb(IntInf.xorb(secs, usecs), 0x1F)
		      end
		fun lp 0 = r
		  | lp i = (ignore (Random.randInt r); lp (i-1))
		in
		  lp n
		end
	  in
	    r
	  end

    fun randByte r () = let
	  val w = LargeWord.fromInt(Random.randInt r)
	  in
	    Word8.fromLargeWord(LargeWord.>>(w, 0w7))
	  end

  (* generate a Variant 1, Type 4 UUID. *)
    fun new () = let
	  val randByte = randByte (seedRand())
	  fun gen 6 = let
	      (* byte 6 has the version (0b0100) in the upper 4 bits *)
		val b = randByte()
		in
		  Word8.orb(0wx40, Word8.andb(b, 0wxF))
		end
	    | gen 8 = let
	      (* byte 8 has the variant (0b10) in the upper 2 bits *)
		val b = randByte()
		in
		  Word8.orb(0wx80, Word8.andb(b, 0wx3F))
		end
	    | gen _ = randByte()
	  in
	    UUID.fromBytes (W8V.tabulate(16, gen))
	  end

  end

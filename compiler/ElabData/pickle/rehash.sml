(* rehash.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Compute hash for a library that is the product of filtering
 * a larger environment.  Since every environment (after unpickling)
 * contains references to its own hash id, re-hashing requires
 * the original hash id (to be able to recognize it).  The result
 * of re-hashing will then be the same value that would have been
 * produced had the smaller environment been pickled (and hashed) in
 * the first place. *)

structure Rehash :
  sig
    val addGUID : { hash: PersStamps.persstamp, guid: string }
		    -> PersStamps.persstamp
    val rehash : { env: StaticEnv.staticEnv,
		   orig_pid: PersStamps.persstamp,
		   guid: string }
		 -> PersStamps.persstamp
  end =

struct

  fun addGUID { hash, guid } = let
      val crc = CRC.fromString (Byte.bytesToString (PersStamps.toBytes hash))
      fun append (c, x) = CRC.append (x, c)
      val crc' = CharVector.foldl append crc guid
  in
      PersStamps.fromBytes (Byte.stringToBytes (CRC.toString crc'))
  end

  fun rehash { env, orig_pid, guid } =
      addGUID { hash = #hash (PickMod.pickleEnv
                                  (PickMod.REHASH orig_pid) env),
                guid = guid }

end (* structure Rehash *)

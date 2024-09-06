(* persstamps.sml  --> pid.sml *)
 *
 *   pid abstraction (persistent identifier or stamp)
 *
 * Copyright (c) 2024 by The Fellowship of SML/NJ
 *)

(* PersStamps should be renamed Pid, for "persistent identifier", and the type perstamp should
 * be renamed "pid".  Then the definition pid = PersStamps.persstamp in stamps.sml could
 * be dropped.
 *)

structure PersStamps :> PERSSTAMPS =  (* Pid :> PID *)
struct

  type persstamp = Word8Vector.vector
(*  type pid = Word8Vector.vector *)

  (* all pids have a fixed size of 16 bytes *)
  val persStampSize = 16  (* --> size *)
(*  val size = 16 *)

  fun compare (v1: perstamp, v2: perstamp) =
      String.compare(Byte.bytesToString v1, Byte.bytesToString v2)

  fun toBytes x = x

  fun fromBytes v = 
      if Word8Vector.length v = persStampSize then v
      else ErrorMsg.impossible "PersStamps.fromBytes"

  (* convert the persstamp to a printable representation (hex digits) *)
  fun toHex pid =
      let fun cvtByte b = StringCvt.padLeft #"0" 2 (Word8.toString b)
          fun f (b, l) = cvtByte b :: l
       in String.concat (Word8Vector.foldr f [] pid)
      end

  fun fromHex s =
      let fun onebyte i =
	      let val i2 = 2 * i
		  val c1 = String.sub (s, i2)
		  val c2 = String.sub (s, i2 + 1)
               in valOf (Word8.fromString (implode [c1, c2]))
              end
       in SOME (Word8Vector.tabulate (persStampSize, onebyte))
      end handle _ => NONE

end (* structure PersStamps (Pid) *)

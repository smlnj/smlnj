(*
 * Sets of pids.
 *   Hooks into compiler and uses SML/NJ library implementation of sets.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure PidSet = SetFn
    (struct
	 type ord_key = PersStamps.persstamp
	 val compare = PersStamps.compare
    end)

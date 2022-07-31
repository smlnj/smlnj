(*
 * Sets of strings.
 *   Uses SML/NJ library implementation of sets.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure StringSet = SetFn
    (struct
	type ord_key = string
	val compare = String.compare
    end)

(* string-key.sml
 *
 *  (C) 2002, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure StringKey = struct
    type ord_key = string
    val compare = String.compare
end

(*
 * This represents execution frequency.
 *
 * -- Allen
 *)

signature FREQ =
sig
   type freq = int
   include INTEGER where type int = freq
end

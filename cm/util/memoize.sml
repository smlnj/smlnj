(*
 * Simple memoization.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure Memoize :> sig
    (* If g is the result of (memoize f), then calling g the first time
     * will result in f being called with the same argument.
     * Any subsequent call to g simply returns the result that was
     * computed during the first call.  Thus, f will be called at most
     * once on g's behalf. *)
    val memoize : ('a -> 'b) -> ('a -> 'b)
end = struct
    fun memoize f = let
	val cache = ref (fn _ => raise Fail "Memoize.memoize: uninitialized")
	fun first_time x = let
	    val v = f x
	    fun later_on _ = v
	in
	    cache := later_on;
	    v
	end
    in
	cache := first_time;
	(fn x => !cache x)
    end
end

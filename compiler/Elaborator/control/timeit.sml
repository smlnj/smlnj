(* timeit.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * QUESTION: why is this file here and not in Basics/stats?
 *)

structure TimeIt =
struct

local

    fun say msg = (Control_Print.say msg; Control_Print.flush ())
    open Time Timer
in

fun timeIt (doit: bool) (name: string, loc: string, f : 'a -> 'b, arg: 'a) : 'b =
    if doit
    then let val timer = startCPUTimer ()
	     val result = f arg
	     val {usr, sys} = checkCPUTimer timer
	  in say (concat ["TIME[", name, ", ", loc, "]\n   "]);
	     say "usr: "; say (LargeInt.toString (toMicroseconds usr));
	     say ", sys: "; say (LargeInt.toString (toMicroseconds sys));
	     say "\n\n";
	     result
	 end
    else f arg

end (* local *)
end (* structure TimeIt *)

(* susp.sig
 *
 *   Lazy thunks.
 *
 * Copyright (c) 2005 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
signature SUSP = sig

    type 'a susp

    val delay : (unit -> 'a) -> 'a susp
    val force : 'a susp -> 'a
end

(* susp.sml
 *
 *   Lazy thunks (lifted from Core).
 *
 * Copyright (c) 2005 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure Susp : SUSP = struct

    type 'a susp = 'a Core.susp

    val delay = Core.delay
    val force = Core.force
end

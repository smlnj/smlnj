(* memory.sig
 *
 *   Primitives for "raw" memory access and allocation.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
signature CMEMORY = sig
    include CMEMACCESS
    include CMEMALLOC where type addr' = addr
end

(* install-back-trace.sml
 *
 *   A module that causes (at link time) to have the back-trace
 *   plugin installed into its core hook.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure InstallBackTrace = struct
    val _ = BackTrace.install ()
end

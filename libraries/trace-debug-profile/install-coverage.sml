(* install-coverage.sml
 *
 *   A module that causes (at link time) to have the test coverage
 *   plugin installed into its core hook.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure InstallCoverage = struct
    val _ = Coverage.install ()
end

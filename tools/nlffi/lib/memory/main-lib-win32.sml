(* main-lib-win32.sml
 *
 *   Name to be used for access to the "main" library.
 *   (For win32 we apparently need an explicit name.)
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure MainLib = struct
    val name = SOME "kernel32.dll"
end

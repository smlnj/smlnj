(* main-lib-unix.sml
 *
 *   Name to be used for access to the "main" library.
 *   (For Unix we don't need a name; dlopen gets an argument of NULL.)
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure MainLib = struct
    val name = NONE : string option
end

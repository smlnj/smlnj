(*
 * Instantiating the tools library for CM.
 *
 *   Copyright (c) 2000 by Lucent Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
local
    structure SrcPath = SrcPath
    structure String = String
in
    structure Tools :> TOOLS = CM0.Tools
end

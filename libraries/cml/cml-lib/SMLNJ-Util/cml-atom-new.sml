(* cml-atom-new.sml
 *
 *   Thread-safe version of Atom (protecting the global hashtable
 *   with a lock).
 *
 * Copyright (c) 2005 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure Atom : ATOM = struct

    open Atom			(* from $/smlnj-lib.cm *)

    local val l = SyncVar.mVarInit ()
    in
    fun atomically f a = (SyncVar.mTake l; f a before SyncVar.mPut (l, ()))
    end

    val atom = atomically atom
    val atom' = atomically atom'
end

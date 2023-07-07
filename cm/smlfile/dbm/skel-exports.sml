(* cm/smlfile/dbm/skel-exports.sml
 *
 * Get the toplevel exports from a skeleton.
 *
 * (C) 2023, The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 *)

signature SKELEXPORTS =
sig
    val exports : Skeleton.decl -> SymbolSet.set
end (* signature SKELEXPORTS *)

structure SkelExports :> SKELEXPORTS =
struct

local
  structure SK = Skeleton
  structure SS = SymbolSet
in

  (* exports : SK.decl -> SS.set *)
  fun exports d =
      let fun e (SK.Bind (s, _), a) = SS.add (a, s)
	    | e (SK.Local (l, b), a) = e (b, a)
	    | e (SK.Par l, a) = foldl e a l
	    | e (SK.Seq l, a) = foldl e a l
	    | e (SK.Open _, a) = a	(* cannot happen *)
	    | e (SK.Ref _, a) = a
       in e (d, SS.empty)
      end

end (* top local *)
end (* structure SkelExports *)

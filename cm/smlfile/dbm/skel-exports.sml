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
  fun exports (decl: SK.decl) : SS.set =
      let (* export0 : SK.decl * SS.set -> SS.set *)
	  fun export0 (SK.Bind (sym, _), symset) = SS.add (symset, sym)
	    | export0 (SK.Local (local, body), symset) = export0 (body, symset)
	    | export0 (SK.Par decls, symset) = foldl export0 symset decls
	    | export0 (SK.Seq decls, symset) = foldl export0 symset decls
	    | export0 (SK.Open _, symset) = symset  (* cannot happen !? (why?) *)
	    | export0 (SK.Ref _, symset) = symset
       in export0 (decl, SS.empty)
      end

end (* top local *)
end (* structure SkelExports *)

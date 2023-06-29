(*
 * Get the toplevel exports from a skeleton.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * contact: Matthias Blume (blume@cs.princeton.edu)
 *)
signature SKELEXPORTS = sig
    val exports : Skeleton.decl -> SymbolSet.set
end

structure SkelExports :> SKELEXPORTS = struct

    structure SK = Skeleton
    structure SS = SymbolSet

    fun exports d = let
	fun e (SK.Bind (s, _), a) = SS.add (a, s)
	  | e (SK.Local (l, b), a) = e (b, a)
	  | e (SK.Par l, a) = foldl e a l
	  | e (SK.Seq l, a) = foldl e a l
	  | e (SK.Open _, a) = a	(* cannot happen *)
	  | e (SK.Ref _, a) = a
    in
	e (d, SS.empty)
    end
end

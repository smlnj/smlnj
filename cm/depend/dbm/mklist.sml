(* cm/depend/dbm/mklist.sml
 *
 * Produce a linear listing of information pertaining to nodes in
 *  a given dependency graph.
 *
 * Used only in bootstrap/mkbootlist.sml.
 * 
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 *)

structure MkList :>
sig
    val group : { bininfo: BinInfo.info -> 'element,
		  smlinfo: SmlInfo.info -> 'element,
		  Cons: 'element * 'elements -> 'elements,
		  Nil: 'elements }
		-> GroupGraph.group
		-> 'elements
end =

struct

local 

  structure DG = DependencyGraph
  structure GG = GroupGraph

in

  (* stab_isreg: (StableSet.elem * SmlInfoSet.elem) * BinInfo.info -> bool *)
  fun stab_isreg ((bs, ss), i) = StableSet.member (bs, i)

  (* sml_isreg: (StableSet.elem * SmlInfoSet.elem) * SmlInfo.info -> bool *)
  fun sml_isreg ((bs, ss), i) = SmlInfoSet.member (ss, i)

  (* stab_reg: (StableSet.elem * SmlInfoSet.elem) * BinInfo.info
       -> StableSet.elem * SmlInfoSet.elem *)
  fun stab_reg ((bs, ss), i) = (StableSet.add (bs, i), ss)

  (* stab_reg: (StableSet.elem * SmlInfoSet.elem) * SmlInfo.info
       -> StableSet.elem * SmlInfoSet.elem *)
  fun sml_reg ((bs, ss), i) = (bs, SmlInfoSet.add (ss, i))

  fun do_list do_elem [] k m = k m
    | do_list do_elem (h :: t) k m = do_elem h (do_list do_elem t k) m

  fun group { Nil, ... } GG.ERRORGROUP = Nil
    | group x (g as GG.GROUP { exports, ... }) =
      let val { bininfo, smlinfo, Cons, Nil } = x
	  fun bnode (DG.BNODE n) k m =
	      let val { bininfo = i, localimports = l, globalimports = g } = n
		  fun k' m = Cons (bininfo i, k (stab_reg (m, i)))
	       in if stab_isreg (m, i) then k m
		  else do_list bnode l (do_list lazy_farbnode g k') m
	      end

	  and farbnode (_, n, _) = bnode n

	  and lazy_farbnode th = farbnode (th ())

	  fun snode (DG.SNODE n) k m =
	      let val { smlinfo = i, localimports = l, globalimports = g } = n
		  fun k' m = Cons (smlinfo i, k (sml_reg (m, i)))
	       in if sml_isreg (m, i) then k m
		  else do_list snode l (do_list farsbnode g k') m
	      end

	  and farsbnode (_, DG.SB_BNODE (n, _, _)) = bnode n
	    | farsbnode (_, DG.SB_SNODE n) = snode n

	  fun impexp (nth, _, _) = farsbnode (nth ())

      in do_list impexp (SymbolMap.listItems exports)
		 (fn _ => Nil)
		 (StableSet.empty, SmlInfoSet.empty)
      end (* fun group *)

end (* top local *)
end (* structure MkList *)

(* cm/depend/checksharing.sml
 * Check for consistency of "private" and "shared" annotations.
 *
 * (C) 2022 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 *)
local
    structure DG = DependencyGraph
    structure EM = ErrorMsg
    structure PP = Formatting
in
  signature CHECKSHARING = sig
    val check : DG.impexp SymbolMap.map * GeneralParams.info -> unit
  end

  structure CheckSharing :> CHECKSHARING = struct

    fun check (exports, gp) = let

	fun check (Sharing.DONTCARE, _, s, _) =
	    (s, if StringSet.isEmpty s then Sharing.SHARE false
		else Sharing.DONTSHARE)
	  | check (Sharing.PRIVATE, x, _, _) =
	    (StringSet.singleton x, Sharing.DONTSHARE)
	  | check (Sharing.SHARED, x, s, err) =
	    let val errorBody =
		    PP.vblock
		      (map PP.text
			   ("because of dependence on non-shareable state in:" ::
			    (StringSet.listItems s)))
	     in if StringSet.isEmpty s
	        then (s, Sharing.SHARE true)
	        else (err EM.COMPLAIN ("cannot share state of " ^ x) errorBody;
		      (s, Sharing.DONTSHARE))
	    end

	val smlmap = ref SmlInfoMap.empty

	fun bn (DG.BNODE { bininfo = i, ... }) =
	    case BinInfo.sh_mode i of
		Sharing.DONTSHARE => StringSet.singleton (BinInfo.describe i)
	      | _ => StringSet.empty

	fun sn (DG.SNODE n) = let
	    val { smlinfo = i, localimports = li, globalimports = gi, ... } = n
	    fun acc f (arg, s) = StringSet.union (f arg, s)
	in
	    case SmlInfoMap.find (!smlmap, i) of
		SOME s => s
	      | NONE => let
		    val gs = foldl (acc fsbn) StringSet.empty gi
		    val ls = foldl (acc sn) gs li
		    val (s, m) = check (SmlInfo.sh_spec i, SmlInfo.descr i, ls,
					SmlInfo.error gp i)
		in
		    smlmap := SmlInfoMap.insert (!smlmap, i, s);
		    SmlInfo.set_sh_mode (i, m);
		    s
		end
	end

	and sbn (DG.SB_BNODE (n, _, _)) = bn n
	  | sbn (DG.SB_SNODE n) = sn n

	and fsbn (_, n) = sbn n

	fun impexp (nth, _, _) = ignore (fsbn (nth ()))
    in
	SymbolMap.app impexp exports
    end
  end
end

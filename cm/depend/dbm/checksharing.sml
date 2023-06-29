(* cm/depend/dbm/checksharing.sml
 *
 * Check for consistency of "private" and "shared" annotations.
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 * Edited: DBM, 2023.6
 *)

signature CHECKSHARING =
sig

  val check : DependencyGraph.impexp SymbolMap.map * GeneralParams.info -> unit

end

structure CheckSharing :> CHECKSHARING =
struct

local

  structure DG = DependencyGraph
  structure EM = ErrorMsg
  structure PP = Formatting

in

  (* check : DG.impexp SymbolMap.map * GeneralParams.info -> unit *)
  fun check (exports: DG impexp SymbolMap.map, gp_info: GeneralParams.info) =
      let val error = SmlInfo.error gp_info
	  
	  (* check0 : SmlInfo.info, StringSet.set -> StringSet.set * Sharing.mode *)
	  (* This is the function where the actual "sharing checking" is executed.
	   * The second. string set argument is a set of filepaths for files that are
	   * related how to the smlinfo file? *)
	  fun check0 (smlinfo: SmlInfo.info, strings: StringSet.set) =
	      case SmlInfo.sh_spec smlinfo
	       of Sharing.DONTCARE =>
		    (strings,
		     if StringSet.isEmpty strings
		     then Sharing.SHARE false
		     else Sharing.DONTSHARE)
		| Sharing.PRIVATE =>
	            (StringSet.singleton (SmlInfo.descr smlinfo), Sharing.DONTSHARE)
		| Sharing.SHARED =>
		    let val name = SmlInfo.filepath smlinfo
			val errorBody =
			    PP.vblock
				(map PP.text
				     ("because of dependence on non-shareable state in:" ::
				      (StringSet.listItems strings)))
		    in if StringSet.isEmpty strings
		       then (strings, Sharing.SHARE true)
		       else (error smlinfo EM.COMPLAIN ("cannot share state of " ^ name) errorBody;
			     (strings, Sharing.DONTSHARE))  (* "dummy" return following error *)
		    end

	  (* smlmap : StringSet.set SmlInfoMap.map ref *)
	  val smlmap : StringSet.set SmlInfoMap.map ref = ref SmlInfoMap.empty

	  (* f_bnode : DG.bnode -> StringSet.set *)
	  fun f_bnode (DG.BNODE { bininfo, ... }) =
	      case BinInfo.sh_mode bininfo
	        of Sharing.DONTSHARE => StringSet.singleton (BinInfo.describe bininfo)
		 | _ => StringSet.empty

	  (* f_snode : DG.snode -> StringSet.set *)
	  fun f_snode (DG.SNODE {smlinfo, localimports, globalimports, ...}) =
	      let fun acc f (arg, strings) = StringSet.union (f arg, strings)
	       in case SmlInfoMap.find (!smlmap, smlinfo)
		    of SOME s => s
		     | NONE => let val global = foldl (acc f_sbnode) StringSet.empty globalimports
				   val local_global = foldl (acc f_snode) global localimports
				   val (all_files, sh_mode) = check0 (smlinfo, local_global)
			        in smlmap := SmlInfoMap.insert (!smlmap, smlinfo, all_files);
				   SmlInfo.set_sh_mode (smlinfo, sh_mode);
				   all_files
			       end
	      end

	  (* f_sbnode : DG.farsbnode -> StringSet.set *)
	  and f_sbnode (_, DG.SB_BNODE (n, _, _)) = f_bnode n
	    | f_sbnode (_, DG.SB_SNODE n) = f_snode n

	  (* f_impexp : DG.impexp -> unit *)
	  fun impexp (farsbnode, _, _) = ignore (f_sbnode farsbnode)

       in SymbolMap.app impexp exports
      end (* fun check *)

end (* top local *)
end (* structure CheckSharing *)


(* NOTES:

1. Dependence of the check function on the GeneralParams.info parameter for the
"group registration" map used by SmlInfo.error could possibly be eliminated or
streamlined.  What is actually needed or useful for the error message (and depdends
on the group registration)?



*)

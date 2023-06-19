(* cm/bootstrap/dbm/mkbootlist.sml 
 * 
 * Building the bootlist from a dependency graph...
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 *)

(* DBM: the code of MkList (depend/mklist.sml) has been merged with the code for
 * MkBootList, currently in depend/dbm/mklist.sml. *)

structure MkBootList =
struct

local

  structure PS = PersStamps

in

  datatype info =
      BININFO of BinInfo.info
    | SMLINFO of SmlInfo.info

  (* same_info : info -> info -> bool *)
  fun same_info (BININFO i) (BININFO i') =
	BinInfo.compare (i, i') = EQUAL
    | same_info (SMLINFO i) (SMLINFO i') =
	SmlInfo.compare (i, i') = EQUAL
    | same_info _ _ = false

  (* group: (string -> string) -> GG.group -> X list *)
  fun group (listName: string -> string) (group: GG.group) =
      let

	  (* Nil : { l : 'a list, ss = StableSet.set } *)
	  val Nil = { l = [], ss = StableSet.empty }

	  (* Cons : {x : 'a , s = BinInfo.info option} -> {l : 'a list, ss = StableSet.set}
	            -> {l : 'a list, ss : StableSet.set} *)
	  fun Cons ({ x, s = NONE }, { l, ss }) = { l = x :: l, ss = ss }
	    | Cons ({ x, s = SOME i }, { l , ss }) =
		{ l = x :: l, ss = StableSet.add (ss, i) }

	  (* f_bininfo : BinInfo.info -> { x : info * string, s : BinInfo.info option } *)
	  fun f_bininfo (i: BinInfo.info) =
	      case BinInfo.rts_pid i
		of SOME p => { x = (BININFO i, "#" ^ PS.toHex p), s = NONE }
		 | NONE =>
		     let val x = concat [listName (BinInfo.stablename i),
					 "@", Int.toString (BinInfo.offset i),
					 ":", BinInfo.describe i]
			 val s = case BinInfo.sh_mode i
				   of Sharing.DONTSHARE => NONE
				    | _ => SOME i
		      in { x = (BININFO i, x), s = s }
		     end

	  (* f_smlinfo : SmlInfo.info -> { x: info * string, s = BinInfo.info option } *)
	  fun f_smlinfo (i: SmlInfo.info) =
	        { x = (SMLINFO i, listName (SmlInfo.binname i)),
		  s = NONE }

	  val arg = { f_bininfo = f_bininfo,
		      f_smlinfo = f_smlinfo }

       in MkList.group arg g
      end

end (* top local *)
end (* structure MkBootList *)

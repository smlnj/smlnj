(*
 * Building the bootlist from a dependency graph...
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure PS = PersStamps
    type pid = PS.persstamp
in
    structure MkBootList = struct
        datatype info =
	    BININFO of BinInfo.info
	  | SMLINFO of SmlInfo.info
	fun same_info (BININFO i) (BININFO i') =
	      BinInfo.compare (i, i') = EQUAL
	  | same_info (SMLINFO i) (SMLINFO i') =
	      SmlInfo.compare (i, i') = EQUAL
	  | same_info _ _ = false
	fun group listName g = let
	    val Nil = { l = [], ss = StableSet.empty }
	    fun Cons ({ x, s = NONE }, { l, ss }) = { l = x :: l, ss = ss }
	      | Cons ({ x, s = SOME i }, { l , ss }) =
		{ l = x :: l, ss = StableSet.add (ss, i) }
	    fun bininfo i =
		case BinInfo.rts_pid i of
		    SOME p => { x = (BININFO i, "#" ^ PS.toHex p), s = NONE }
		  | NONE => let
			val x = concat [listName (BinInfo.stablename i),
					"@", Int.toString (BinInfo.offset i),
					":", BinInfo.describe i]
			val s = case BinInfo.sh_mode i of
			    Sharing.DONTSHARE => NONE
			  | _ => SOME i
		    in { x = (BININFO i, x), s = s }
		    end
	    fun smlinfo i = { x = (SMLINFO i, listName (SmlInfo.binname i)),
			      s = NONE }
	    val arg = { bininfo = bininfo,
		        smlinfo = smlinfo,
			Cons = Cons, Nil = Nil }
	in
	    MkList.group arg g
	end
    end
end

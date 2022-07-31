(*
 * Internal data structure representing a CM dependency graph.
 * (fine-grain: compilation units)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure DependencyGraph = struct

    type filter = SymbolSet.set option

    datatype bnode =
	BNODE of { bininfo: BinInfo.info,
		   localimports: bnode list,
		   globalimports: (unit -> farbnode) list }

    withtype farbnode = filter * bnode * int option

    datatype snode =
	SNODE of { smlinfo: SmlInfo.info,
		   localimports: snode list,
		   globalimports: farsbnode list }

    and sbnode =
	SB_BNODE of bnode * IInfo.info * int option
      | SB_SNODE of snode

    withtype farsbnode = filter * sbnode

    type impexp = (unit -> farsbnode) * DAEnv.env * SymbolSet.set

    fun describeSBN (SB_BNODE (BNODE { bininfo = i, ... }, _, _)) =
	BinInfo.describe i
      | describeSBN (SB_SNODE (SNODE { smlinfo = i, ... })) =
	SmlInfo.descr i

    fun describeFarSBN (_, sbn) = describeSBN sbn

    (* comparing various nodes for equality *)
    fun beq (BNODE { bininfo = i, ... }, BNODE { bininfo = i', ... }) =
	BinInfo.compare (i, i') = EQUAL
    fun seq (SNODE { smlinfo = i, ... }, SNODE { smlinfo = i', ... }) =
	SmlInfo.eq (i, i')

    fun sbeq (SB_SNODE n, SB_SNODE n') = seq (n, n')
      | sbeq (SB_BNODE (n, _, _), SB_BNODE (n', _, _)) = beq (n, n')
      | sbeq _ = false
end


(* cm/depend/dbm/graph.sml
 *
 * Internal data structure representing a CM dependency graph.
 * (fine-grain: compilation units)
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 *)

structure DependencyGraph =
struct

  type filter = SymbolSet.set option

  (* binary nodes *)
  datatype bnode =
      BNODE of { bininfo: BinInfo.info,
		 localimports: bnode list,
		 globalimports: farbnode list }  (* unthunkified globalimports *)
(*		 globalimports: (unit -> farbnode) list }  -- thunkified globalimports *)

  withtype farbnode = filter * bnode * int option

  (* source nodes *)
  datatype snode =
      SNODE of { smlinfo: SmlInfo.info,
		 localimports: snode list,
		 globalimports: farsbnode list }

  (* union of source nodes and binary nodes *)
  and sbnode =
      SB_BNODE of bnode * IInfo.info * int option
    | SB_SNODE of snode

  withtype farsbnode = filter * sbnode

(*  type impexp = (unit -> farsbnode) * DAEnv.env * SymbolSet.set *)
  type impexp = farsbnode * DAEnv.env * SymbolSet.set  (* unthunkified farsbnode *)

  (* describeSBN : sbnode -> string *)
  fun describeSBN (SB_BNODE (BNODE { bininfo = i, ... }, _, _)) =
	BinInfo.describe i
    | describeSBN (SB_SNODE (SNODE { smlinfo = i, ... })) =
	SmlInfo.describe i

  (* describeFarSBNode : farsbnode -> string *)
  fun describeFarSBNode ((_, sbnode): farsbnode) = describeSBN sbnode

  (* beq: bnode -> bool *)
  (* comparing various nodes for equality *)
  fun beq (BNODE { bininfo = i, ... }, BNODE { bininfo = i', ... }) =
      BinInfo.compare (i, i') = EQUAL

  (* seq : snode -> bool *)
  fun seq (SNODE { smlinfo = i, ... }, SNODE { smlinfo = i', ... }) =
      SmlInfo.eq (i, i')

  (* sbeq : sbnode -> bool *)
  fun sbeq (SB_SNODE n, SB_SNODE n') = seq (n, n')
    | sbeq (SB_BNODE (n, _, _), SB_BNODE (n', _, _)) = beq (n, n')
    | sbeq _ = false

end (* structure DependencyGraph *)

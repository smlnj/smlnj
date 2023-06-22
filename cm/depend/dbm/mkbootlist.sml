(* cm/depend/dbm/mklist.sml  ->  mkbootlist.sml
 *
 * Produce a linear listing of information pertaining to nodes in
 *  a given dependency graph, together with a StableSet.set
 *
 * Used only in bootstrap/mkbootlist.sml, where the BOOTLIST is constructed.
 * 
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 *)

(* This version combines the code from depend/mklist.sml and bootstrap/mkbootstrap.sml. *)

structure MkBootList :>
sig

    datatype info
      = BININFO of BinInfo.info
      | SMLINFO of SmlInfo.info

    val same_info : info * info -> bool

    val group : (string -> string) -> GroupGraph.group -> {l : info * string, ss = StableSet.set}

end =

struct

local 

  structure DG = DependencyGraph
  structure GG = GroupGraph

  structure PS = PersStamps

in

  (* infosets: pair of a set of BinInfo.info and a set of SmlInfo.info *)
  (* The StableSet.set "represents" a set of binfiles, while the
   * SmlInfoSet "represents" a set of sml source files. *)

  type infosets = StableSet.set * SmlInfoSet.set

  val empty_infosets = (StableSet.empty, SmlInfoSet.empty)

  (* stab_isreg: infosets * BinInfo.info -> bool *)
  (* has a binfile (via its BinInfo.info) been "registered" in the StableSet.set? *)
  fun stab_isreg ((bs, ss): infosets, i: BinInfo.info) =
      StableSet.member (bs, i)

  (* sml_isreg: infosets * SmlInfo.info -> bool *)
  (* has a binfile (via its BinInfo.info) been "registered" in the StableSet.set? *)
  fun sml_isreg ((bs, ss): infosets, i: SmlInfo.info) =
      SmlInfoSet.member (ss, i)

  (* stab_reg: infosets * BinInfo.info -> infosets *)
  (* "registering a binfile by adding its BinInfo.info to the StableSet.set *)
  fun stab_reg ((bs, ss): infosets, i: BinInfo.info) : infosets =
      (StableSet.add (bs, i), ss)

  (* stab_reg: infosets * SmlInfo.info -> infosets *)
  (* "registering" an sml file by adding its SmlInfo.info to the SmlInfoSet.set *)
  fun sml_reg ((bs, ss): infosets, i: SmlInfo.info) =
      (bs, SmlInfoSet.add (ss, i))


(* Merging the contents of MkBootList into mklist (with the merged structure 
 * named MkBootList). *)

  (* info: the descriminated union of BinInfo.info and SmlInfo.info *)
  datatype info
    = BININFO of BinInfo.info
    | SMLINFO of SmlInfo.info

  (* same_info : info * info -> bool *)
  (* equality relation for the info type *)
  fun same_info (BININFO i, BININFO i') =
	(case BinInfo.compare (i, i')
	   of EQUAL => true
	    | _ => false)
    | same_info (SMLINFO i, SMLINFO i') =
	(case SmlInfo.compare (i, i')
	   of EQUAL => true
	    | _ => false)
    | same_info _ = false

  (* types associated with the "group function *)
  type X = {x: info * string, s: BinInfo.info option}
  type Y = infosets -> X list
  type Z = {l: (info * string) list, ss : StableSet.set}

  val emptyZ : Z = {l = nil, ss = StableSet.empty}
 
  (* group: [listName:](string -> string) -> GG.group -> Z *)
  fun group _ GG.ERRORGROUP = emptyZ
    | group (listName: string -> string)
	    (g as GG.GROUP { exports: DG.impexp SymbolMap.map, ... }) =
      let
	  (* f_bininfo : BinInfo.info -> X *)
	  fun f_bininfo (bininfo: BinInfo.info): X =
	      case BinInfo.rts_pid bininfo
		of SOME p => { x = (BININFO bininfo, "#" ^ PS.toHex p), s = NONE }
		 | NONE =>
		     let val desc = concat
				      [listName (BinInfo.stablename bininfo),
				       "@", Int.toString (BinInfo.offset bininfo),
				       ":", BinInfo.describe bininfo]
			 val bininfoOp =
			       case BinInfo.sh_mode bininfo
				 of Sharing.DONTSHARE => NONE
				  | _ => SOME bininfo
		      in { x = (BININFO bininfo, desc), s = bininfoOp }
		     end

	  (* f_smlinfo : SmlInfo.info -> X *)
	  fun f_smlinfo (smlinfo: SmlInfo.info) =
	        { x = (SMLINFO smlinfo, listName (SmlInfo.binname smlinfo)),
		  s = NONE }

          (* f_bnode : DB.bnode * Y -> Y *)
	  fun f_bnode (DG.BNODE {bininfo: BinInfo.info,
				 localimports: DG.bnode list,
				 globalimports DG.farbnode list },
		       k: Y) : Y =
	      let
		  (* k' : Y *)
		  fun k' (iss: infosets) = f_bininfo bininfo :: k (stab_reg (iss, bininfo))

		  (* globalimports : Y *)
		  val globalimports =
			List.foldl
			  (f_farbnode: DG.farbnode * Y -> Y)
			  (k': Y)
			  (globalimports : DG.farbnode list)

		  (* combinedimports : Y *)
		  val combinedimports =
		        List.foldl (* do_list *)
			  (f_bnode : DB.bnode * Y -> Y)
			  (globalimports : Y)
			  (localimports : DB.bnode list)

		  (* return : Y *)
	          fun return (m: infosets) =
		      if stab_isreg (m, bininfo)
		      then k m
		      else combinedimports m

	       in return
	      end

	  (* f_farbnode : DG.farbnode * Y -> Y *)
	  and f_farbnode ((_, n: bnode, _): DG.farbnode, y) = f_bnode (n, y)


	  (* f_snode : DG.snode * Y -> Y *)
	  fun f_snode (DG.SNODE {smlinfo : SmlInfo.info,
				 localimports: DG.snode list,
				 globalimports: DG.farsbnode list },
		       k: Y) : Y =
	      let 
		  (* k' : Y *)
		  fun k' (iss: infosets) = f_smlinfo smlinfo :: k (sml_reg (iss, smlinfo))

		  (* globalimports : Y *)
		  val globalimports =
		        List.foldl
			  (f_farsbnode: DG.farsbnode * Y -> Y)
			  (k': Y)
			  (globalimports: DG.farsbnode list)

		  (* combinedimports : Y *)
		  val combinedimports =
		      List.foldl 
			(f_snode: DG.snode * Y -> Y)
			(globalimports : Y)
			(localimports: DG.snode list)

		  (* return : Y *)
		  fun return (m: infosets) : X list = 
	              if sml_isreg (m, smlinfo)
		      then k m
		      else combinedimports m

	       in return
	      end

	  (* f_farsbnode : DG.farsbnode * Y -> Y *)
	  and f_farsbnode ((_, DG.SB_BNODE (n, _, _)) : DG.farsbnode, y: Y) = f_bnode (n, y)
	    | f_farsbnode ((_, DG.SB_SNODE n) : DG.farsbnode, y: Y) = f_snode (n, y)

	  (* f_impexp : (DG.impexp * Y -> Y *)
	  fun f_impexp (impexp : DG.impexp, y: Y) = f_farsbnode (#1 impexp, y)

	  val f_group : Y =
	      List.foldl
		(f_impexp : DG.impexp * Y -> Y)
		((fn _ => nil) : Y)
		(SymbolMap.listItems exports : DG.impexp list)
							  
	  val xs : X list = f_group (empty_infosets : infosets)

	  (* collect : X * Z -> Z *)
	  (* the final collection function, which converts a list of
	   * X values into a single Z value *)
	  fun collect ({x, s = SOME i}: X, {l, ss}: Z) =
	      {l = x::l, ss = StableSet.add (ss, i)}
	    | collect ({x, s = NONE} : X, {l, ss}: Z) = {l = x::l, ss = ss}


       in List.foldl collect emptyZ xs

      end (* fun group *)

end (* top local *)
end (* structure MkBootList *)

(* NOTES:

The original version of MkList/MkBootList uses an odd, disguised variant of foldl:

   fun do_list do_elem [] k m = k m
     | do_list do_elem (h :: t) k m = do_elem h (do_list do_elem t k) m

   do_list : [f]('x -> 'y -> 'm -> 'y)
               -> [nil, h::t]'x list  =>  h: 'x, t: 'x list
	       -> [k]('m -> 'y)
	       -> [m]'m
	       -> 'y

  The following "derivation" shows that do_list was essentially doing foldl.

  (* g:  (f: 'x -> ('m -> 'y) -> ('m -> 'y), k: ('m -> 'y)) -> ('x list -> ('m -> 'y) *)
  fun g (f: 'x -> ('m -> 'y) -> ('m -> 'y), k: ('m -> 'y)) =
      let fun do_list nil = k   (* do_list : 'x list -> ('m -> 'y) *)
            | do_list (h :: t) = f h (do_list t)
       in do_list
      end

  (* g:  (f: 'x -> 'z -> 'z, k: 'z) -> ('x list -> 'z *)
  fun g (f: 'x -> 'z -> 'z, k: 'z) =
      let fun do_list nil = k   (* do_list : 'x list -> 'z *)
            | do_list (h :: t) = f h (do_list t)
       in do_list
      end

  (* g:  ([f:] 'x -> 'z -> 'z)  -> ([k:] 'z) -> ('x list -> 'z *)
  (* f = do_elem, k = k *)
  fun g (f: 'x * 'z  -> 'z) (k: 'z) (l : 'x list) =
      let fun fold nil = k   (* fold : 'x list -> 'z *)
            | fold (h :: t) = f (h, fold t)
       in fold l
      end

  fun g f k xs = List.foldl f k xs

  val g = List.fold

END NOTES
*)

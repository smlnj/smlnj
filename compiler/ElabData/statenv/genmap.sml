(* genmap.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * Rapid modmap generation based on modtrees.
 *    (Modtrees are embedded into static environments during unpickling.
 *     This module cannot deal with environments that did not come out
 *     of the unpickler.)
 *
 * (March 2000, Matthias Blume)
 *)
structure GenModIdMap : sig
    val mkMap : StaticEnv.staticEnv -> ModuleId.tmap
    val mkMap' : StaticEnv.staticEnv * ModuleId.tmap -> ModuleId.tmap
end = struct

    structure M = Modules
    structure MI = ModuleId

    fun mkMap' (se: StaticEnv.staticEnv, initial) = let
	fun tree (t, m) = let
	    fun rc (r, stubOf, treeOf, part, id, insert, look) = let
		val i = id r
	    in
		case look (m, i) of
		    SOME _ => m
		  | NONE => let
			val m' = insert (m, i, part)
		    in
			case stubOf part of
			    NONE => ErrorMsg.impossible "ModIdSet:no stubinfo"
			  | SOME stb => tree (treeOf stb, m')
		    end
	    end
	in
	    case t of
		M.TYCNODE r => MI.insertTyc (m, MI.tycId r, r)
	      | M.SIGNODE r =>
		rc (r, #stub, #tree, r, MI.sigId, MI.insertSig, MI.lookSig)
	      | M.STRNODE r =>
		rc (r, #stub, #tree, #rlzn r,
		    MI.strId, MI.insertStr, MI.lookStr)
	      | M.FCTNODE r =>
		rc (r, #stub, #tree, #rlzn r,
		    MI.fctId, MI.insertFct, MI.lookFct)
	      | M.ENVNODE r =>
		rc (r, #stub, #tree, r, MI.envId, MI.insertEnv, MI.lookEnv)
	      | M.BRANCH l => foldl tree m l
	end
	fun bnd ((_, (_, SOME t)), m) = tree (t, m)
	  | bnd (_, m) = m
    in
	StaticEnv.realfold bnd initial se
    end

    val mkMap' = Stats.doPhase (Stats.makePhase "Compiler 923 genmap") mkMap'

    fun mkMap se = mkMap' (se, MI.emptyTmap)
end

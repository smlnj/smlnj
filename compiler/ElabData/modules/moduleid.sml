(* moduleid.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Re-written by M.Blume (3/2000)
 *)

signature MODULE_ID = sig

    type tycId	(* type-constructor IDs *)
    type sigId	(* signature IDs *)
    type strId	(* structure IDs *)
    type fctId	(* functor IDs *)
    type envId	(* environment IDs *)

    (* create the various kinds of IDs *)
    val tycId : Types.gtrec -> tycId
    val sigId : Modules.sigrec -> sigId
    val strId : Modules.strrec -> strId
    val fctId : Modules.fctrec -> fctId
    val envId : Modules.envrec -> envId

    val strId2 : Modules.sigrec * Modules.strEntity -> strId
    val fctId2 : Modules.fctSig * Modules.fctEntity -> fctId

    (* equality tests for IDs *)
    val sameTyc : tycId * tycId -> bool
    val sameSig : sigId * sigId -> bool
    val sameStr : strId * strId -> bool
    val sameFct : fctId * fctId -> bool
    val sameEnv : envId * envId -> bool

    (* tests if IDs are "fresh", which means that they are local to the current
     * compilation unit.
     *)
    val freshTyc : tycId -> bool
    val freshSig : sigId -> bool
    val freshStr : strId -> bool
    val freshFct : fctId -> bool
    val freshEnv : envId -> bool

    type tmap

    val emptyTmap : tmap

    val lookTyc : tmap * tycId -> Types.gtrec option
    val lookSig : tmap * sigId -> Modules.sigrec option
    val lookStr : tmap * strId -> Modules.strEntity option
    val lookFct : tmap * fctId -> Modules.fctEntity option
    val lookEnv : tmap * envId -> Modules.envrec option

    val insertTyc : tmap * tycId * Types.gtrec -> tmap
    val insertSig : tmap * sigId * Modules.sigrec -> tmap
    val insertStr : tmap * strId * Modules.strEntity -> tmap
    val insertFct : tmap * fctId * Modules.fctEntity -> tmap
    val insertEnv : tmap * envId * Modules.envrec -> tmap

    val tycId' : Types.tycon -> tycId

    type 'a umap

    val emptyUmap : 'a umap

    val uLookTyc : 'a umap * tycId -> 'a option
    val uLookSig : 'a umap * sigId -> 'a option
    val uLookStr : 'a umap * strId -> 'a option
    val uLookFct : 'a umap * fctId -> 'a option
    val uLookEnv : 'a umap * envId -> 'a option

    val uInsertTyc : 'a umap * tycId * 'a -> 'a umap
    val uInsertSig : 'a umap * sigId * 'a -> 'a umap
    val uInsertStr : 'a umap * strId * 'a -> 'a umap
    val uInsertFct : 'a umap * fctId * 'a -> 'a umap
    val uInsertEnv : 'a umap * envId * 'a -> 'a umap

end (* signature MODULE_ID *)

structure ModuleId : MODULE_ID = struct

    structure M = Modules
    structure T = Types
    structure A = Access
    structure ST = Stamps

    fun bug m = ErrorMsg.impossible ("ModuleId: " ^ m)

    type stamp = ST.stamp

    type tycId = stamp
    type sigId = stamp
    type strId = {
	sign: stamp,	(* the stamp of the structure's signature *)
	rlzn: stamp	(* the stamp of the structure's realization *)
      }
    type fctId = {
	paramsig: stamp,	(* the stamp of the functor's parameter signature *)
	bodysig: stamp,		(* the stamp of the functor's signature *)
	rlzn: stamp		(* the stamp of the functor's realization *)
      }
    type envId = stamp

    val freshTyc = ST.isFresh
    val freshSig = ST.isFresh
    fun freshStr { sign, rlzn } = ST.isFresh sign orelse ST.isFresh rlzn
    fun freshFct { paramsig, bodysig, rlzn } =
	ST.isFresh paramsig orelse ST.isFresh bodysig orelse ST.isFresh rlzn
    val freshEnv = ST.isFresh

    fun tycId (r: Types.gtrec) = #stamp r
    fun sigId (s: Modules.sigrec) = #stamp s
    fun strId2 (sign: M.sigrec, rlzn: M.strEntity) =
	{ sign = #stamp sign, rlzn = #stamp rlzn }
    fun strId ({ sign = Modules.SIG s, rlzn, ... }: Modules.strrec) =
	{ sign = #stamp s, rlzn = #stamp rlzn }
      | strId _ = bug "strId: bad signature"
    fun fctId2 (M.FSIG { paramsig = M.SIG psg, bodysig = M.SIG bsg, ... },
		rlzn: M.fctEntity) =
	{ paramsig = #stamp psg, bodysig = #stamp bsg, rlzn = #stamp rlzn }
      | fctId2 _ = bug "fctId2/fctId2: bad funsig"
    fun fctId ({ sign, rlzn, ... }: Modules.fctrec) = fctId2 (sign, rlzn)
    fun envId (e: Modules.envrec) = #stamp e

    structure StrKey = struct
        type ord_key = strId
	fun compare (i1: strId, i2: strId) =
	    case ST.compare (#sign i1, #sign i2) of
		EQUAL => ST.compare (#rlzn i1, #rlzn i2)
	      | unequal => unequal
    end
    structure FctKey = struct
        type ord_key = fctId
	fun compare (i1: fctId, i2: fctId) =
	    case ST.compare (#paramsig i1, #paramsig i2) of
		EQUAL => (case ST.compare (#bodysig i1, #bodysig i2) of
			      EQUAL => ST.compare (#rlzn i1, #rlzn i2)
			    | unequal => unequal)
	      | unequal => unequal
    end

    structure StampM = RedBlackMapFn (ST)
    structure StrM = RedBlackMapFn (StrKey)
    structure FctM = RedBlackMapFn (FctKey)

    val sameTyc = ST.eq
    val sameSig = ST.eq
    fun sameStr (x, y) = StrKey.compare (x, y) = EQUAL
    fun sameFct (x, y) = FctKey.compare (x, y) = EQUAL
    val sameEnv = ST.eq

    type tmap = { m_tyc: T.gtrec StampM.map,
		  m_sig: M.sigrec StampM.map,
		  m_str: M.strEntity StrM.map,
		  m_fct: M.fctEntity FctM.map,
		  m_env: M.envrec StampM.map }

    val emptyTmap = { m_tyc = StampM.empty,
		      m_sig = StampM.empty,
		      m_str = StrM.empty,
		      m_fct = FctM.empty,
		      m_env = StampM.empty }

    local
	fun look (sel, find) (m as { m_tyc, m_sig, m_str, m_fct, m_env }, k) =
	    find (sel m, k)
    in
        fun lookTyc x = look (#m_tyc, StampM.find) x
	fun lookSig x = look (#m_sig, StampM.find) x
	fun lookStr x = look (#m_str, StrM.find) x
	fun lookFct x = look (#m_fct, FctM.find) x
	fun lookEnv x = look (#m_env, StampM.find) x
    end

    fun insertTyc ({ m_tyc, m_sig, m_str, m_fct, m_env }, k, t) =
	{ m_tyc = StampM.insert (m_tyc, k, t),
	  m_sig = m_sig, m_str = m_str, m_fct = m_fct, m_env = m_env }

    fun insertSig ({ m_tyc, m_sig, m_str, m_fct, m_env }, k, t) =
	{ m_sig = StampM.insert (m_sig, k, t),
	  m_tyc = m_tyc, m_str = m_str, m_fct = m_fct, m_env = m_env }

    fun insertStr ({ m_tyc, m_sig, m_str, m_fct, m_env }, k, t) =
	{ m_str = StrM.insert (m_str, k, t),
	  m_tyc = m_tyc, m_sig = m_sig, m_fct = m_fct, m_env = m_env }

    fun insertFct ({ m_tyc, m_sig, m_str, m_fct, m_env }, k, t) =
	{ m_fct = FctM.insert (m_fct, k, t),
	  m_tyc = m_tyc, m_sig = m_sig, m_str = m_str, m_env = m_env }

    fun insertEnv ({ m_tyc, m_sig, m_str, m_fct, m_env }, k, t) =
	{ m_env = StampM.insert (m_env, k, t),
	  m_tyc = m_tyc, m_sig = m_sig, m_str = m_str, m_fct = m_fct }

    fun tycId' (T.GENtyc r) = tycId r
      | tycId' (T.DEFtyc { stamp, ... }) = stamp
      | tycId' _ = bug "tycId': neither GENtyc nor DEFtyc"

    (* and now for uniformely typed maps (implementations are shared)... *)

    type 'a umap = { m_tyc: 'a StampM.map,
		     m_sig: 'a StampM.map,
		     m_str: 'a StrM.map,
		     m_fct: 'a FctM.map,
		     m_env: 'a StampM.map }

    val emptyUmap = emptyTmap

    val uLookTyc = lookTyc
    val uLookSig = lookSig
    val uLookStr = lookStr
    val uLookFct = lookFct
    val uLookEnv = lookEnv

    val uInsertTyc = insertTyc
    val uInsertSig = insertSig
    val uInsertStr = insertStr
    val uInsertFct = insertFct
    val uInsertEnv = insertEnv

end (* structure ModuleId *)

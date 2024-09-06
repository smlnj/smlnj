(* moduleid.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Re-written by M.Blume (3/2000)
 * Revised by DBM 2024.09.06
 *)

(* [DBM, 20240906] What is the basic function of Module Ids?
   using stamps, or combinations of stamps (for str, fct) to "identify" modules
   (mainly) for the purpose of ... (stubification/patching?)
 *)

signature MODULE_ID =
sig

    (* the Id types, which all involves stamps of the corresponding static constructs *)
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

    (* tmap -- collection (record) of finite maps from Ids to appropriate module rep values *)
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

    (* 'a umap -- collection (record) of finite maps from Ids to some type 'a *)
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

structure ModuleId : MODULE_ID =
struct

(* imported modules start*)

    structure M = Modules
    structure T = Types
    structure A = Access
    structure ST = Stamps

(* imported modules end *)

    fun bug m = ErrorMsg.impossible ("ModuleId: " ^ m)

(* the Id types *)
    type tycId = ST.stamp

    type sigId = ST.stamp

    type strId =  (* two stamps, from sig and rlzn, to identity a structure *)
	 {sign: ST.stamp,	(* the stamp of the structure's signature *)
	  rlzn: ST.stamp}	(* the stamp of the structure's realization *)

    type fctId =  (* three stamps, from parameter sig, body sig, and rlzn, to identify a functor *)
	 {paramsig: ST.stamp,	(* the stamp of the functor's parameter signature *)
	  bodysig: ST.stamp,	(* the stamp of the functor's signature *)
	  rlzn: ST.stamp}	(* the stamp of the functor's realization *)

    type envId = ST.stamp

(* testing for "freshness", ie new, locally generated stamps that haven't been "exported" *)

    (* freshTyc : tycId -> bool *)
    val freshTyc : tycId -> bool = ST.isFresh

    (* freshSig : sigId -> bool *)
    val freshSig : sigId -> bool = ST.isFresh

    (* freshStr : strId -> bool *)
    fun freshStr ({ sign, rlzn }: strId) = ST.isFresh sign orelse ST.isFresh rlzn

    (* freshFct : fctId -> bool *)
    fun freshFct ({ paramsig, bodysig, rlzn }: fctId) =
	ST.isFresh paramsig orelse ST.isFresh bodysig orelse ST.isFresh rlzn

    (* freshEnv : envId -> bool *)
    val freshEnv : envId -> bool = ST.isFresh


(* functions mapping type and module constructs to their Ids *)
				       
    (* tycId : T.gtrec -> tycId *)
    fun tycId ({stamp,...}: T.gtrec) : tycId = stamp

    (* tycId' : T.tycon -> tycId, extending domain to general tycons, but only GENtye and DEFtyc *)
    fun tycId' (T.GENtyc r) = tycId r
      | tycId' (T.DEFtyc { stamp, ... }) = stamp
      | tycId' _ = bug "tycId': neither GENtyc nor DEFtyc"

    (* sigId : M.sigrec -> tycId *)
    fun sigId ({stamp,...}: M.sigrec) : sigId = stamp

    (* strId : M.Structure -> strId *)
    fun strId ({ sign = M.SIG {stamp=ssig, ...}, rlzn = {stamp=srlzn, ... },...}: M.strrec) : strId =
	  { sign = ssig, rlzn = srlzn }
      | strId _ = bug "strId: bad structure"

    (* strId2 : M.sigrec * M.strEntity -> strId *)
    fun strId2 ({stamp,...}: M.sigrec, {stamp,...}: M.strEntity) : strId =
	  { sign = #stamp sign, rlzn = #stamp rlzn }

    (* fctId2 : M.fsig * M.fctEntity -> fctId *)
    fun fctId2 (M.FSIG { paramsig = M.SIG {stamp = spsig, ...}, bodysig = M.SIG {stamp = sbsig, ... }, ...}
		{stamp = srlzn, ...}: M.fctEntity) : fctId =
	  { paramsig = spsig, bodysig = sbsig, rlzn = srlzn }
      | fctId2 _ = bug "fctId2/fctId2: bad funsig"

    (* fctId2 : M.fctrec -> fctId *)
    fun fctId ({ sign, rlzn, ... }: M.fctrec) : fctId = fctId2 (sign, rlzn)

    (* envId : M.envrec -> envId *)
    fun envId ({stamp,...}: M.envrec) : envId = stamp


(* KEY structures for maps over structure and functor Ids, and corresponding
 * finite map structures *)

    (* Key structure for strIds *)
    structure StrKey =
    struct
      type ord_key = strId
      fun compare ({sign=sign1, rlzn=rlzn1}: strId,
		   {sign=sign2, rlzn=rlzn2}: strId) =
	    case ST.compare (sign1, sign2)
	      of EQUAL => ST.compare (rlzn1, rlzn2)
	       | unequal => unequal
    end (* structure StrKey *)

    (* Key structure for fctIds *)
    structure FctKey =
    struct
      type ord_key = fctId
      fun compare ({paramsig=psig1, bodysig=bsig1, rlzn=rlzn1}: fctId,
		   {paramsig=psig2, bodysig=bsig2, rlzn=rlzn2}: fctId) =
	    case ST.compare (psig1, psig2)
	      of EQUAL => (case ST.compare (bsig1, bsig2)
			     of EQUAL => ST.compare (rlzn1, rlzn2)
			      | unequal => unequal)
	       | unequal => unequal
    end (* structure FctKey *)


(* equality functions for Ids; sameStr and sameFct use StrKey and FctKey, respectively *)

    val sameTyc : tycId * tycId -> bool = ST.eq

    val sameSig : sigId * sigId -> bool = ST.eq

    fun sameStr (x: strId, y: strId): bool =
	case StrKey.compare (x, y)
	  of EQUAL => true
	   | _ => false

    fun sameFct (x: fctId, y: fctId) : bool =
	case FctKey.compare (x, y)
	  of EQUAL => true
	   | _ => false

    val sameEnv : envId * invId -> bool = ST.eq


(* finite map structures for domain types: stamp, strId, fctId *)

    structure StampM = RedBlackMapFn (ST)   (* finite maps over stamps *)
    structure StrM = RedBlackMapFn (StrKey) (* finite maps over strIds *)
    structure FctM = RedBlackMapFn (FctKey) (* finite maps over fctIds *)


(* key map records, two kinds: tmap and umap *)

   (* tmap 
    *   collection (record) of five maps from Ids to appropriate "module representations";
    *   each component map has a different range type *)

    type tmap =
         { m_tyc: T.gtrec StampM.map,
	   m_sig: M.sigrec StampM.map,
	   m_str: M.strEntity StrM.map,
	   m_fct: M.fctEntity FctM.map,
	   m_env: M.envrec StampM.map }

    val emptyTmap : tmap =
        { m_tyc = StampM.empty,
	  m_sig = StampM.empty,
	  m_str = StrM.empty,
	  m_fct = FctM.empty,
	  m_env = StampM.empty }


    (* lookup functions for tmaps (monomorphic) *)

    (* lookTyc : tmap * tycId -> T.gtrec option *)
    fun lookTyc ({m_tyc,...}: tmap, k: tycId) : T.gtrec option =
	StampM.find (m_tyc, k)
         
    (* lookSig : tmap * sigId -> M.sigrec option *)
    fun lookSig ({m_sig,...}: tmap, k: sigId) : M.sigrec option =
	StampM.find (m_sig, k)
	
    (* lookStr : tmap * strId -> M.strEntity option *)
    fun lookStr ({m_str,...}: tmap, k: strId) : M.strEntity option =
	StrM.find (m_str, k)

    (* lookFct : tmap * fctId -> M.fctEntity option *)
    fun lookFct ({m_fct,...}: tmap, k: fctId) : M.fctEntity option =
	StrM.find (m_fct, k)

    (* lookEnv : tmap * envId -> M.envrec option *)
    fun lookEnv ({m_env,...}: tmap, k: envId) : M.envrec option =
	StampM.find (m_env, k)


    (* insertion (binding) functions for tmaps
     * DBM: could really use functional record update here! *)

    (* insertTyc : tmap * tycId * T.gtrec -> tmap *)
    fun insertTyc ({ m_tyc, m_sig, m_str, m_fct, m_env }: tmap, k: tycId, t: T.gtrec) : tmap =
	{ m_tyc = StampM.insert (m_tyc, k, t),
	  m_sig = m_sig, m_str = m_str, m_fct = m_fct, m_env = m_env }

    (* insertSig : tmap * sigId * M.sigrec -> tmap *)
    fun insertSig ({ m_tyc, m_sig, m_str, m_fct, m_env }: tmap, k: sigId, M.sigrec) : tmap =
	{ m_sig = StampM.insert (m_sig, k, t),
	  m_tyc = m_tyc, m_str = m_str, m_fct = m_fct, m_env = m_env }

    (* insertStr : tmap * strId * M.strEntity -> tmap *)
    fun insertStr ({ m_tyc, m_sig, m_str, m_fct, m_env }, k, t) =
	{ m_str = StrM.insert (m_str, k, t),
	  m_tyc = m_tyc, m_sig = m_sig, m_fct = m_fct, m_env = m_env }

    (* insertTyc : tmap * tycId * T.gtrec -> tmap *)
    fun insertFct ({ m_tyc, m_sig, m_str, m_fct, m_env }, k, t) =
	{ m_fct = FctM.insert (m_fct, k, t),
	  m_tyc = m_tyc, m_sig = m_sig, m_str = m_str, m_env = m_env }

    (* insertTyc : tmap * tycId * T.gtrec -> tmap *)
    fun insertEnv ({ m_tyc, m_sig, m_str, m_fct, m_env }, k, t) =
	{ m_env = StampM.insert (m_env, k, t),
	  m_tyc = m_tyc, m_sig = m_sig, m_str = m_str, m_fct = m_fct }


    (* and now for uniformely typed maps ("u" suggesting "uniform"?) *)

    type 'a umap = { m_tyc: 'a StampM.map,
		     m_sig: 'a StampM.map,
		     m_str: 'a StrM.map,
		     m_fct: 'a FctM.map,
		     m_env: 'a StampM.map }

    val 'a emptyTmap : 'a umap =
        { m_tyc = StampM.empty,
	  m_sig = StampM.empty,
	  m_str = StrM.empty,
	  m_fct = FctM.empty,
	  m_env = StampM.empty }

(*
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

We will be less clever and define the new uLook and uInsert functions separately rather than in terms
of the corresponding functions for tmap (which we have defined above as monomorphic functions).
*)

    (* lookup functions for umaps, these are polymorphic *)

    (* uLookTyc : 'a umap * tycId -> 'a *)
    fun 'a uLookTyc ({m_tyc,...}: 'a umap, k: tycId) : 'a = 
	StampM.find (m_tyc, k)
         
    (* uLookSig : 'a umap * sigId -> 'a *)
    fun 'a uLookSig ({m_sig,...}: 'a umap, k: sigId) : 'a =
	StampM.find (m_sig, k)
	
    (* uLookStr : tmap * strId -> 'a *)
    fun 'a uLookStr ({m_str,...}: 'a umap, k: strId) : 'a =
	StrM.find (m_str, k)

    (* uLookFct : 'a umap * fctId -> 'a *)
    fun 'a uLookFct ({m_fct,...}: 'a umap, k: fctId) : 'a =
	StrM.find (m_fct, k)

    (* uLookEnv : 'a umap * envId -> 'a *)
    fun 'a uLookEnv ({m_env,...}: 'a umap, k: envId) : 'a =
	StampM.find (m_env, k)


    (* insertion (binding) functions for umaps, also polymorphic
     * DBM: could really use functional record update here! *)

    (* uInsertTyc : 'a umap * tycId * 'a -> 'a umap *)
    fun 'a uInsertTyc ({ m_tyc, m_sig, m_str, m_fct, m_env }: 'a umap, k: tycId, t: 'a) : 'a umap =
	{ m_tyc = StampM.insert (m_tyc, k, t),
	  m_sig = m_sig, m_str = m_str, m_fct = m_fct, m_env = m_env }

    (* uInsertSig : 'a umap * sigId * 'a -> 'a umap *)
    fun 'a uInsertSig ({ m_tyc, m_sig, m_str, m_fct, m_env }: 'a umap, k: sigId, t: 'a) : 'a umap =
	{ m_sig = StampM.insert (m_sig, k, t),
	  m_tyc = m_tyc, m_str = m_str, m_fct = m_fct, m_env = m_env }

    (* uInsertStr : 'a umap * strId * 'a -> 'a umap *)
    fun 'a uInsertStr ({ m_tyc, m_sig, m_str, m_fct, m_env }: 'a umap, k: strId, t: 'a) : 'a umap =
	{ m_str = StrM.insert (m_str, k, t),
	  m_tyc = m_tyc, m_sig = m_sig, m_fct = m_fct, m_env = m_env }

    (* uInsertTyc : 'a umap * tycId * 'a -> 'a umap *)
    fun 'a uInsertFct ({ m_tyc, m_sig, m_str, m_fct, m_env }: 'a umap, k: fctId, t: 'a) : 'a umap =
	{ m_fct = FctM.insert (m_fct, k, t),
	  m_tyc = m_tyc, m_sig = m_sig, m_str = m_str, m_env = m_env }

    (* uIinsertTyc : 'a umap * tycId * 'a -> 'a umap *)
    fun 'a uInsertEnv ({ m_tyc, m_sig, m_str, m_fct, m_env }: 'a umap, k: envId, t: 'a) : 'a umap =
	{ m_env = StampM.insert (m_env, k, t),
	  m_tyc = m_tyc, m_sig = m_sig, m_str = m_str, m_fct = m_fct }

end (* structure ModuleId *)

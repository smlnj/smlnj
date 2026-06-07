(* pickmod.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * The revised pickler using the new "generic" pickling facility.
 *
 * March 2000, Matthias Blume
 *)

signature PICKMOD = sig

    (* There are three possible reasons to run the pickler.  Each form
     * of context (see datatype context below) corresponds to one of them:
     *
     *  1. The initial pickle.  This is done right after a new static
     *     environment has been constructed by the elaborator.  The context
     *     is used to identify those identifiers (ModuleId.<xxx>Id) that
     *     correspond to stubs.  Only the domain of the given map is relevant
     *     here, but since we (usually) need the full map right afterwards
     *     for unpickling, there is no gain in using a set.
     *
     *  2. Pickling a previously pickled-and-unpickled environment from
     *     which some parts may have been pruned.  This is used to calculate
     *     a new hash value that is equal to the hash obtained from an initial
     *     pickle (1.) of the environment if it had been pruned earlier.
     *     (This is used by the compilation manager's cutoff recompilation
     *     system.  Pickles obtained here are never unpickled.)
     *     No actual context is necessary because stubification info is
     *     fully embedded in the environment to be pickled.  However, we
     *     must provide the original pid obtained from the first pickling
     *     because occurences of that pid have to be treated the same way
     *     their "not-yet-occurrences" had been treated in step 1.
     *
     *  3. A set of environments that have already gone through an initial
     *     pickling-and-unpickling is pickled as part of a stable library.
     *     The context is a sequence of maps together with information of
     *     how to get hold of the same map later during unpickling.
     *     (The full context of a stable library is a set of other stable
     *     libraries, but during unpickling we want to avoid unpickling
     *     all of these other libraries in full.)  *)
    datatype context =
	INITIAL of ModuleId.tmap
      | REHASH of PersStamps.persstamp
      | LIBRARY of ((int * Symbol.symbol) option * ModuleId.tmap) list

    type map
    val emptyMap : map

    val envPickler : (LambdaVar.lvar -> unit) ->
		     context ->
		     (map, StaticEnv.staticEnv) PickleUtil.pickler

    val pickleEnv : context ->
		    StaticEnv.staticEnv ->
		    { hash: PersStamps.persstamp,
		      pickle: Word8Vector.vector,
		      exportLvars: LambdaVar.lvar list,
		      hasExports: bool }

    val pickle2hash: Word8Vector.vector -> PersStamps.persstamp

    val dontPickle :
	{ env: StaticEnv.staticEnv, count: int } ->
        { newenv: StaticEnv.staticEnv, hash: PersStamps.persstamp,
	  exportLvars: LambdaVar.lvar list, hasExports: bool }
end

structure PickMod :> PICKMOD =
  struct

    datatype context
      = INITIAL of ModuleId.tmap
      | REHASH of PersStamps.persstamp
      | LIBRARY of ((int * Symbol.symbol) option * ModuleId.tmap) list

    (* to gather some statistics... *)
    val addPickles = Stats.addStat (Stats.makeStat "Pickle Bytes")

    fun bug msg = ErrorMsg.impossible ("PickMod: " ^ msg)

    structure A = Access
    structure T = Types
    structure SP = SymPath
    structure IP = InvPath
    structure MI = ModuleId
    structure POI = PrimopId
    structure V = Variable
    structure ED = EntPath.EvDict
    structure PS = PersStamps
    structure P = PrimOps
    structure InlP = InlineOps
    structure ArithP = ArithOps
    structure PureP = PureOps
    structure CmpP = CompareOps
    structure CP = CommonOps
    structure M = Modules
    structure B = Bindings

    (** NOTE: the CRC functions really ought to work on Word8Vector.vectors **)
    fun pickle2hash pickle =
	PS.fromBytes
	  (Byte.stringToBytes
	     (CRC.toString
	        (CRC.fromString
		  (Byte.bytesToString pickle))))

    fun symCmp (a, b) =
	if Symbol.symbolGt (a, b) then GREATER
	else if Symbol.eq (a, b) then EQUAL else LESS

    structure DTMap = StampMap
    structure MBMap = StampMap

    structure PU = PickleUtil
    structure PSymPid = PickleSymPid

    type map =
	{ dt: PU.id DTMap.map,
	  mb: PU.id MBMap.map,
	  mi: PU.id MI.umap }

    val emptyMap = { dt = DTMap.empty, mb = MBMap.empty, mi = MI.emptyUmap }

    (* type info *)
    val NK = 1          (* datatype NumKind.t *)
    val AO = 2          (* datatype ArithOps.t *)
    val PAO = 3         (* datatype PureOps.t *)
    val CO = 4          (* datatype CompareOps.t *)
    val PO = 5          (* datatype PrimOps.t *)
    val CS = 6          (* datatype Access.consig *)
    val A = 7           (* datatype Access.access *)
    val CR = 8          (* datatype Access.conrep *)
    val LT = 9          (* unused *)
    val TC = 10         (* unused *)
    val TK = 11         (* unused *)
    val V = 12          (* unused *)
    val C = 13          (* unused *)
    val E = 14          (* unused *)
    val FK = 15         (* unused *)
    val RK = 16         (* unused *)
    val ST = 17         (* datatype Stamps.stamp *)
    val MI = 18         (* unused *)
    val EQP = 19        (* datatype Types.eqprop *)
    val TYCKIND = 20    (* datatype Types.tyckind *)
    val DTI = 21        (* {stamps, freetycs, family} *)
    val DTF = 22        (* type Types.dtypeFamily *)
    val TYCON = 23      (* datatype Types.tycon *)
    val T = 24          (* datatype Types.ty *)
    val PI = 25         (* datatype PrimopId.prim_id *)
    val VAR = 26        (* datatype Variable.var *)
    val SD = 27         (* datatype Modules.strDef *)
    val SG = 28         (* datatype Modules.Signature *)
    val FSG = 29        (* datatype Modules.fctSig *)
    val SP = 30         (* datatype Modules.spec *)
    val STR = 31        (* datatype Modules.Structure *)
    val F = 32          (* datatype Modules.Functor *)
    val STE = 33        (* datatype Modules.stampExp *)
    val TCE = 34        (* datatype Modules.tycExp *)
    val STRE = 35       (* datatype Modules.strExp *)
    val FE = 36         (* datatype Modules.fctExp *)
    val EE = 37         (* datatype Modules.entityExp *)
    val ED = 38         (* datatype Modules.entityDec *)
    val EEV = 39        (* datatype Modules.entityEnv *)
    val FX = 40         (* datatype Fixity.fixity *)
    val EN = 41         (* datatype Modules.entity *)
    val B = 42          (* datatype Bindings.binding *)
    val DCON = 43       (* unused *)
    val DICT = 44       (* unused *)
    val FPRIM = 45      (* unused *)
    val FUNDEC = 46     (* unused *)
    val TFUNDEC = 47    (* unused *)
    val DATACON = 48    (* datatype Types.datacon *)
    val DTMEM = 49      (* type Types.dtmember *)
    val NRD = 50        (* type Types.dconDesc *)
    val OVERLD = 51     (* unused *)
    val FCTC = 52       (* datatype Modules.fctClosure *)
    val SEN = 53        (* type Modules.strEntity *)
    val FEN = 54        (* type Modules.fctEntity *)
    val SPATH = 55      (* datatype SymPath.path *)
    val IPATH = 56      (* datatype InvPath.path *)
    val STRID = 57      (* type ModuleId.strId *)
    val FCTID = 58      (* type ModuleId.fctId *)
    val CCI = 59        (* C call info *)
    val CTYPE = 60      (* datatype CTypes.c_type *)
    val CCALL_TYPE = 61 (* datatype CommonOps.ccall_type *)
    val SPE = 62        (* datatype PrimId.str_prim_elem *)
    val TSI = 63        (* datatype Modules.tycSpecInfo *)

    (* this is a bit awful...
     * (we really ought to have syntax for "functional update") *)
    fun DTs x = { find = fn (m: map, _) => DTMap.find (#dt m, x),
		  insert = fn ({ dt, mb, mi }, _, v) =>
		           { dt = DTMap.insert (dt, x, v),
			     mb = mb,
			     mi = mi } }
    fun MBs x = { find = fn (m: map, _) => MBMap.find (#mb m, x),
		  insert = fn ({ dt, mb, mi }, _, v) =>
		           { dt = dt,
			     mb = MBMap.insert (mb, x, v),
			     mi = mi } }
    fun TYCs id = { find = fn (m: map, _) => MI.uLookTyc (#mi m, id),
		    insert = fn ({ dt, mb, mi }, _, v) =>
				{ dt = dt,
				  mb = mb,
				  mi = MI.uInsertTyc (mi, id, v) } }
    val SIGs = { find = fn (m: map, r) => MI.uLookSig (#mi m, MI.sigId r),
		 insert = fn ({ dt, mb, mi }, r, v) =>
			     { dt = dt,
			       mb = mb,
			       mi = MI.uInsertSig (mi, MI.sigId r, v) } }
    fun STRs i = { find = fn (m: map, _) => MI.uLookStr (#mi m, i),
		   insert = fn ({ dt, mb, mi }, _, v) =>
			       { dt = dt,
				 mb = mb,
				 mi = MI.uInsertStr (mi, i, v) } }
    fun FCTs i = { find = fn (m: map, _) => MI.uLookFct (#mi m, i),
		   insert = fn ({ dt, mb, mi }, _, v) =>
			       { dt = dt,
				 mb = mb,
				 mi = MI.uInsertFct (mi, i, v) } }
    val ENVs = { find = fn (m: map, r) => MI.uLookEnv (#mi m, MI.envId r),
		 insert = fn ({ dt, mb, mi }, r, v) =>
			     { dt = dt,
			       mb = mb,
			       mi = MI.uInsertEnv (mi, MI.envId r, v) } }

    infix 3 $

    val int = PU.w_int
    val intinf = PU.w_intinf
    val string = PU.w_string
    val share = PU.ah_share
    val list = PU.w_list
    val pair = PU.w_pair
    val bool = PU.w_bool
    val option = PU.w_option
    val symbol = PSymPid.w_symbol
    val pid = PSymPid.w_pid

  (* create a function that maps lvars to integers *)
    fun mkAlphaConvert () = let
	  val m = ref LambdaVar.Map.empty
	  val cnt = ref 0
	  fun cvt lv = (case  LambdaVar.Map.find (!m, lv)
		 of SOME i' => i'
		  | NONE => let
		      val i' = !cnt
		      in
			cnt := i' + 1;
			m :=  LambdaVar.Map.insert (!m, lv, i');
			i'
		      end
		(* end case *))
	  in
	    cvt
	  end

    fun ctype t = let
	val op $ = PU.$ CTYPE
	fun ?n = String.str (Char.chr n)
	fun %?n = ?n $ []
        in
          case t
           of CTypes.C_void => %?0
            | CTypes.C_float => %?1
            | CTypes.C_double => %?2
            | CTypes.C_long_double => %?3
            | CTypes.C_unsigned CTypes.I_char => %?4
            | CTypes.C_unsigned CTypes.I_short => %?5
            | CTypes.C_unsigned CTypes.I_int => %?6
            | CTypes.C_unsigned CTypes.I_long => %?7
            | CTypes.C_unsigned CTypes.I_long_long => %?8
            | CTypes.C_signed CTypes.I_char => %?9
            | CTypes.C_signed CTypes.I_short => %?10
            | CTypes.C_signed CTypes.I_int => %?11
            | CTypes.C_signed CTypes.I_long => %?12
            | CTypes.C_signed CTypes.I_long_long => %?13
            | CTypes.C_PTR => %?14
            | CTypes.C_ARRAY (t, i) => ?20 $ [ctype t, int i]
            | CTypes.C_STRUCT l => ?21 $ [list ctype l]
            | CTypes.C_UNION l => ?22 $ [list ctype l]
        end

    fun ccall_type t = let
        val op $ = PU.$ CCALL_TYPE
        in
          case t
           of CP.CCI32 => "\000" $ []
            | CP.CCI64 => "\001" $ []
            | CP.CCR64 => "\002" $ []
            | CP.CCML  => "\003" $ []
        end

    fun ccall_info { c_proto = { conv, retTy, paramTys },
		     ml_args, ml_res_opt, reentrant } = let
	val op $ = PU.$ CCI
        in
          "C" $ [
              string conv, ctype retTy, list ctype paramTys,
              list ccall_type ml_args, option ccall_type ml_res_opt,
              bool reentrant
            ]
        end

    fun numkind arg = let
	val op $ = PU.$ NK
	fun nk (P.INT i) = "A" $ [int i]
	  | nk (P.UINT i) = "B" $ [int i]
	  | nk (P.FLOAT i) = "C" $ [int i]
	in
	  nk arg
	end

    fun arithop oper = let
	val op $ = PU.$ AO
	fun arithopc ArithP.IADD = "\000"
	  | arithopc ArithP.ISUB = "\001"
	  | arithopc ArithP.IMUL = "\002"
	  | arithopc ArithP.IDIV = "\003"
	  | arithopc ArithP.IMOD = "\004"
          | arithopc ArithP.IQUOT = "\005"
	  | arithopc ArithP.IREM = "\006"
	  | arithopc ArithP.INEG = "\007"
	in
	  arithopc oper $ []
	end

    fun pureop oper = let
	val op $ = PU.$ PAO
	fun arithopc PureP.ADD = "\000"
	  | arithopc PureP.SUB = "\001"
	  | arithopc PureP.MUL = "\002"
          | arithopc PureP.QUOT = "\003"
	  | arithopc PureP.REM = "\004"
	  | arithopc PureP.NEG = "\005"
	  | arithopc PureP.LSHIFT = "\006"
	  | arithopc PureP.RSHIFT = "\007"
	  | arithopc PureP.RSHIFTL = "\008"
	  | arithopc PureP.ORB = "\009"
	  | arithopc PureP.XORB = "\010"
	  | arithopc PureP.ANDB = "\011"
	  | arithopc PureP.NOTB = "\012"
          | arithopc PureP.CNTPOP = "\013"
          | arithopc PureP.CNTLZ = "\014"
          | arithopc PureP.CNTTZ = "\015"
          | arithopc PureP.ROTL = "\016"
          | arithopc PureP.ROTR = "\017"
	  | arithopc PureP.FDIV = "\018"
	  | arithopc PureP.FABS = "\019"
          | arithopc PureP.FSQRT = "\020"
	in
	  arithopc oper $ []
	end

    fun cmpop oper = let
	val op $ = PU.$ CO
	fun cmpopc CmpP.GT = "\000"
	  | cmpopc CmpP.GTE = "\001"
	  | cmpopc CmpP.LT = "\002"
	  | cmpopc CmpP.LTE = "\003"
	  | cmpopc CmpP.EQL = "\004"
	  | cmpopc CmpP.NEQ = "\005"
	in
	  cmpopc oper $ []
	end

    fun primop p = let
	val op $ = PU.$ PO
	fun ?n = String.str (Char.chr n)
	fun fromto tag (from, to) = ?tag $ [int from, int to]
	fun %?n = ?n $ []
	in
          case p
           of P.INLINE(InlP.DIV kind) => ?89 $ [numkind kind]
            | P.INLINE(InlP.MOD kind) => ?90 $ [numkind kind]
            | P.INLINE(InlP.QUOT kind) => ?91 $ [numkind kind]
            | P.INLINE(InlP.REM kind) => ?92 $ [numkind kind]
            | P.INLINE(InlP.LSHIFT kind) => ?93 $ [numkind kind]
            | P.INLINE(InlP.RSHIFT kind) => ?94 $ [numkind kind]
            | P.INLINE(InlP.RSHIFTL kind) => ?95 $ [numkind kind]
            | P.INLINE(InlP.NUMSUBSCRIPT kind) => ?101 $ [numkind kind]
            | P.INLINE(InlP.NUMSUBSCRIPTV kind) => ?102 $ [numkind kind]
            | P.INLINE(InlP.NUMUPDATE kind) => ?103 $ [numkind kind]
            | P.INLINE(InlP.MIN kind) => ?110 $ [numkind kind]
            | P.INLINE(InlP.MAX kind) => ?111 $ [numkind kind]
            | P.INLINE(InlP.ABS kind) => ?112 $ [numkind kind]
            | P.INLINE InlP.SUBSCRIPT => %?5
            | P.INLINE InlP.SUBSCRIPTV => %?6
            | P.INLINE InlP.MKARRAY => %?7
            | P.INLINE InlP.UPDATE => %?29
            | P.INLINE InlP.NOT => %?35
            | P.INLINE InlP.COMPOSE => %?36
            | P.INLINE InlP.BEFORE => %?37
            | P.INLINE InlP.IGNORE => %?48
            | P.INLINE InlP.IDENTITY => %?49
            | P.INLINE InlP.CHR => %?50
            | P.ARITH{ oper, sz } => ?80 $ [arithop oper, int sz]
            | P.PURE{oper, kind } => ?81 $ [pureop oper, numkind kind]
            | P.CMP{ oper, kind } => ?82 $ [cmpop oper, numkind kind]
            | P.PRIM(CP.FSGN sz) => ?83 $ [ int sz ]
            | P.PRIM(CP.TEST x) => fromto 84 x
            | P.PRIM(CP.TESTU x) => fromto 85 x
            | P.PRIM(CP.TRUNC x) => fromto 86 x
            | P.PRIM(CP.EXTEND x) => fromto 87 x
            | P.PRIM(CP.COPY x) => fromto 88 x
            | P.PRIM(CP.REAL_TO_INT{ floor, from, to }) => ?96 $ [bool floor, int from, int to]
            | P.PRIM(CP.INT_TO_REAL{ from, to }) => ?97 $ [int from, int to]
            | P.PRIM(CP.NUMSUBSCRIPT kind) => ?98 $ [numkind kind]
            | P.PRIM(CP.NUMSUBSCRIPTV kind) => ?99 $ [numkind kind]
            | P.PRIM(CP.NUMUPDATE kind) => ?100 $ [numkind kind]
            | P.PRIM(CP.RAW_LOAD kind) => ?106 $ [numkind kind]
            | P.PRIM(CP.RAW_STORE kind) => ?107 $ [numkind kind]
            | P.PRIM(CP.RAW_CCALL(SOME i)) => ?108 $ [ccall_info i]
            | P.PRIM(CP.RAW_RECORD{ align=64 }) => ?109 $ [bool true]
            | P.PRIM(CP.RAW_RECORD{ ... }) => ?109 $ [bool false]
            | P.PRIM(CP.TEST_INF i) => ?113 $ [int i]
            | P.PRIM(CP.TRUNC_INF i) => ?114 $ [int i]
            | P.PRIM(CP.EXTEND_INF i) => ?115 $ [int i]
            | P.PRIM(CP.COPY_INF i) => ?116 $ [int i]
            | P.PRIM(CP.REAL_TO_BITS i) => ?117 $ [int i]
            | P.PRIM(CP.BITS_TO_REAL i) => ?118 $ [int i]
            | P.PRIM CP.SUBSCRIPT => %?3
            | P.PRIM CP.SUBSCRIPTV => %?4
            | P.PRIM CP.PTREQL => %?8
            | P.PRIM CP.PTRNEQ => %?9
            | P.PRIM CP.POLYEQL => %?10
            | P.PRIM CP.POLYNEQ => %?11
            | P.PRIM CP.BOXED => %?12
            | P.PRIM CP.UNBOXED => %?13
            | P.PRIM CP.LENGTH => %?14
            | P.PRIM CP.OBJLENGTH => %?15
            | P.PRIM CP.CAST => %?16
            | P.PRIM CP.GETHDLR => %?18
            | P.PRIM CP.SETHDLR => %?19
            | P.PRIM CP.GETVAR => %?20
            | P.PRIM CP.SETVAR => %?21
            | P.PRIM CP.MAKEREF => %?22
            | P.PRIM CP.CALLCC => %?23
            | P.PRIM CP.CAPTURE => %?24
            | P.PRIM CP.THROW => %?25
            | P.PRIM CP.DEREF => %?26
            | P.PRIM CP.ASSIGN => %?27
            | P.PRIM CP.UPDATE => %?28
            | P.PRIM CP.UNBOXEDUPDATE => %?30
            | P.PRIM CP.GETTAG => %?31
            | P.PRIM CP.MKSPECIAL => %?32
            | P.PRIM CP.SETSPECIAL => %?33
            | P.PRIM CP.GETSPECIAL => %?34
            | P.PRIM CP.ISOLATE => %?40
            | P.PRIM CP.NEW_ARRAY0 => %?42
            | P.PRIM CP.GET_SEQ_DATA => %?43
            | P.PRIM CP.SUBSCRIPT_REC => %?44
            | P.PRIM CP.SUBSCRIPT_RAW64 => %?45
            | P.PRIM CP.UNBOXEDASSIGN => %?46
            | P.PRIM(CP.RAW_CCALL NONE) => %?47
            | P.PRIM CP.CPTR_TO_WORD => %?53
            | P.PRIM CP.WORD_TO_CPTR => %?54
            | P.INLINE InlP.HOST_WORD_SIZE => %?55
            | P.INLINE InlP.HOST_BIG_ENDIAN => %?56
            (* new bitops *)
            | P.INLINE(InlP.CNTZ kind) => ?57 $ [numkind kind]
            | P.INLINE(InlP.CNTO kind) => ?58 $ [numkind kind]
            | P.INLINE(InlP.CNTLZ kind) => ?59 $ [numkind kind]
            | P.INLINE(InlP.CNTLO kind) => ?60 $ [numkind kind]
            | P.INLINE(InlP.CNTTZ kind) => ?61 $ [numkind kind]
            | P.INLINE(InlP.CNTTO kind) => ?62 $ [numkind kind]
            | P.INLINE(InlP.IS_POW2 kind) => ?63 $ [numkind kind]
            | P.INLINE(InlP.CEIL_LOG2 kind) => ?64 $ [numkind kind]
            | _ => raise Fail(concat["unexpected primop '", P.toString p, "'"])
          (* end case *)
        end

    fun consig arg = let
	val op $ = PU.$ CS
	fun cs (A.CSIG (i, j)) = "S" $ [int i, int j]
	  | cs A.CNIL = "N" $ []
        in
	  cs arg
        end

    fun mkAccess { lvar, isLocalPid } = let
	val op $ = PU.$ A
	fun access (A.LVAR i) = "A" $ [lvar i]
	  | access (A.EXTERN p) = "B" $ [pid p]
	  | access (A.PATH (a as A.EXTERN p, i)) =
	    (* isLocalPid always returns false for in the "normal pickler"
	     * case.  It returns true in the "repickle" case for the
	     * pid that was the hash of the original whole pickle.
	     * Since alpha-conversion has already taken place if we find
	     * an EXTERN pid, we don't call "lvar" but "int". *)
	    if isLocalPid p then "A" $ [int i]
	    else "C" $ [access a, int i]
	  | access (A.PATH (a, i)) = "C" $ [access a, int i]
	  | access A.NO_ACCESS = "D" $ []

	val op $ = PU.$ CR
	fun conrep A.UNTAGGED = "A" $ []
	  | conrep (A.TAGGED i) = "B" $ [int i]
	  | conrep A.TRANSPARENT = "C" $ []
	  | conrep (A.CONSTANT i) = "D" $ [int i]
	  | conrep A.REF = "E" $ []
	  | conrep (A.EXN a) = "F" $ [access a]
	  | conrep A.LISTCONS = "G" $ []
	  | conrep A.LISTNIL = "H" $ []
	  | conrep (A.SUSP NONE) = "I" $ []
	  | conrep (A.SUSP (SOME (a, b))) = "J" $ [access a, access b]
        in
	  { access = access, conrep = conrep }
        end

    (* the environment pickler *)
    fun envPickler registerLvar context = let
	val { tycStub, sigStub, strStub, fctStub, envStub,
	      isLocalPid, isLib } =
	    case context of
		INITIAL tmap => let
		    fun stub (xId, freshX, lookX) r = let
			val id = xId r
		    in
			if freshX id then NONE
			else if isSome (lookX (tmap, id)) then SOME (NONE, id)
			else NONE
		    end
		in
		    { tycStub = stub (MI.tycId, MI.freshTyc, MI.lookTyc),
		      sigStub = stub (MI.sigId, MI.freshSig, MI.lookSig),
		      strStub = stub (MI.strId, MI.freshStr, MI.lookStr),
		      fctStub = stub (MI.fctId, MI.freshFct, MI.lookFct),
		      envStub = stub (MI.envId, MI.freshEnv, MI.lookEnv),
		      isLocalPid = fn _ => false,
		      isLib = false }
		end
	      | REHASH myPid => let
		    fun isLocalPid p = PersStamps.compare (p, myPid) = EQUAL
		    fun stub (idX, stubX, owner) r =
			case stubX r of
			    NONE => bug "REHASH:no stubinfo"
			  | SOME stb =>
			    if isLocalPid (owner stb) then SOME (NONE, idX r)
			    else NONE
		in
		    { tycStub = stub (MI.tycId, #stub, #owner),
		      sigStub = stub (MI.sigId, #stub, #owner),
		      strStub = stub (MI.strId, #stub o #rlzn, #owner),
		      fctStub = stub (MI.fctId, #stub o #rlzn, #owner),
		      envStub = stub (MI.envId, #stub, #owner),
		      isLocalPid = isLocalPid,
		      isLib = false }
		end
	      | LIBRARY l => let
		    fun stub (idX, stubX, lookX, lib) r = let
			fun get id = let
			    fun loop [] =
				bug "LIBRARY:import info missing"
			      | loop ((lms, m) :: t) =
				if isSome (lookX (m, id)) then lms else loop t
			in
			    loop l
			end
		    in
			case stubX r of
			    NONE => bug "LIBRARY:no stubinfo"
			  | SOME stb => let
				val id = idX r
			    in
				if lib stb then SOME (get id, id) else NONE
			    end
		    end
		in
		    { tycStub = stub (MI.tycId, #stub, MI.lookTyc, #lib),
		      sigStub = stub (MI.sigId, #stub, MI.lookSig, #lib),
		      strStub = stub (MI.strId, #stub o #rlzn,
				      MI.lookStr, #lib),
		      fctStub = stub (MI.fctId, #stub o #rlzn,
				      MI.lookFct, #lib),
		      envStub = stub (MI.envId, #stub, MI.lookEnv, #lib),
		      isLocalPid = fn _ => false,
		      isLib = true }
		end

	(* Owner pids of stubs are pickled only in the case of libraries,
	 * otherwise they are ignored completely. *)
	fun libPid x =
	    if isLib then
		case x of
		    (NONE, _) => []
		  | (SOME stb, ownerOf) => [pid (ownerOf stb)]
	    else []

	fun libModSpec lms = option (pair (int, symbol)) lms

	val stampConverter = Stamps.newConverter ()

	fun stamp s = let
	    val op $ = PU.$ ST
	in
	    Stamps.Case	stampConverter s
		{ fresh = fn i => "A" $ [int i],
		  global = fn { pid = p, cnt } => "B" $ [pid p, int cnt],
		  special = fn s => "C" $ [string s] }
	end

	val tycId = stamp
	val sigId = stamp
	fun strId { sign, rlzn } = let
	    val op $ = PU.$ STRID
	in
	    "D" $ [stamp sign, stamp rlzn]
	end
	fun fctId { paramsig, bodysig, rlzn } = let
	    val op $ = PU.$ FCTID
	in
	    "E" $ [stamp paramsig, stamp bodysig, stamp rlzn]
	end
	val envId = stamp

	val entVar = stamp
	val entPath = list entVar

	val anotherLvar =
	    let val lvcount = ref 0
	    in (fn v => let val j = !lvcount
			in registerLvar v; lvcount := j + 1; j end)
	    end

	val { access, conrep } = mkAccess { lvar = int o anotherLvar,
					    isLocalPid = isLocalPid }

	val op $ = PU.$ SPATH
	fun spath (SP.SPATH p) = "s" $ [list symbol p]
	val op $ = PU.$ IPATH
	fun ipath (IP.IPATH p) = "i" $ [list symbol p]

	  (* for debugging *)
	fun showipath (IP.IPATH p) =
	    concat (map (fn s => Symbol.symbolToString s ^ ".") (rev p))

	val label = symbol

	fun eqprop eqp = let
	    val op $ = PU.$ EQP
	    fun eqc T.YES = "\000"
	      | eqc T.NO = "\001"
	      | eqc T.IND = "\002"
	      | eqc T.OBJ = "\003"
	      | eqc T.DATA = "\004"
	      | eqc T.ABS = "\005"
	      | eqc T.UNDEF = "\006"
	in
	    eqc eqp $ []
	end

	fun datacon (T.DATACON { name, const, typ, rep, sign, lazyp }) = let
	    val op $ = PU.$ DATACON
	in
	    "c" $ [symbol name, bool const, ty typ, conrep rep,
		   consig sign, bool lazyp]
	end

	and tyckind arg = let
	    val op $ = PU.$ TYCKIND
	    fun tk (T.PRIMITIVE) = "a" $ []
	      | tk (T.DATATYPE { index, family, stamps, root, freetycs, stripped }) =
		"b" $ [int index, option entVar root, bool stripped,
		       dtypeInfo (stamps, family, freetycs)]
	      | tk (T.ABSTRACT tyc) = "c" $ [tycon tyc]
	      | tk (T.FLEXTYC tps) = "d" $ [] (* "f" $ tycpath tps *)
	      (*** I (Matthias) carried through this message from Zhong:
	       tycpath should never be pickled; the only way it can be
	       pickled is when pickling the domains of a mutually
	       recursive datatypes; right now the mutually recursive
	       datatypes are not assigned accurate domains ... (ZHONG)
	       the preceding code is just a temporary gross hack.
	       ***)
	      | tk T.FORMAL = "d" $ []
	      | tk T.TEMP = "e" $ []
	in
	    tk arg
	end

	and dtypeInfo x = let
	    val op $ = PU.$ DTI
	    fun dti_raw (ss, family, freetycs) =
		"a" $ [list stamp (Vector.toList ss),
		       dtFamily family, list tycon freetycs]
	in
	    share (DTs (Vector.sub (#1 x, 0))) dti_raw x
	end

	and dtFamily x = let
	    val op $ = PU.$ DTF
	    fun dtf_raw { mkey, members, properties } =
		"b" $ [stamp mkey, list dtmember (Vector.toList members)]
	in
	    share (MBs (#mkey x)) dtf_raw x
	end

	and dtmember { tycname, dcons, arity, eq = ref e, lazyp, sign } = let
	    val op $ = PU.$ DTMEM
	in
	    "c" $ [symbol tycname, list nameRepDomain dcons, int arity,
		   eqprop e, bool lazyp, consig sign]
	end

	and nameRepDomain { name, rep, domain } = let
	    val op $ = PU.$ NRD
	in
	    "d" $ [symbol name, conrep rep, option ty domain]
	end

	and tycon arg = let
	    val op $ = PU.$ TYCON
	    fun tc (tyc as T.GENtyc g) =
		let fun gt_raw (g as { stamp = s, arity, eq = ref eq, kind,
				       path, stub }) =
			case tycStub g of
			    SOME (l, i) => "A" $ [libModSpec l, tycId i]
			  | NONE => "B" $ ([stamp s, int arity, eqprop eq,
					    tyckind kind, ipath path]
					   @ libPid (stub, #owner))
		in
		    share (TYCs (MI.tycId g)) gt_raw g
		end
	      | tc (tyc as T.DEFtyc dt) = let
		    fun dt_raw { stamp = s, tyfun, strict, path } = let
			val T.TYFUN { arity, body } = tyfun
		    in
			"C" $ [stamp s, int arity, ty body,
			       list bool strict, ipath path]
		    end
		in
		    share (TYCs (MI.tycId' tyc)) dt_raw dt
		end
	      | tc (T.PATHtyc { arity, entPath = ep, path }) =
		"D" $ [int arity, entPath ep, ipath path]
	      | tc (T.RECORDtyc l) = "E" $ [list label l]
	      | tc (T.RECtyc i) = "F" $ [int i]
	      | tc (T.FREEtyc i) = "G" $ [int i]
	      | tc T.ERRORtyc = "H" $ []
	in
	    tc arg
	end

	and ty arg = let
	    val op $ = PU.$ T
	    fun ty (T.VARty (ref (T.INSTANTIATED t))) = ty t
 	      | ty (T.VARty (ref (T.OPEN _))) =
		bug "uninstantiated VARty in pickmod"
	      | ty (T.CONty (c, l)) = "a" $ [tycon c, list ty l]
	      | ty (T.IBOUND i) = "b" $ [int i]
(* we probably do not need to support pickling WILDCARDty *)
	      | ty T.WILDCARDty = "c" $ []
	      | ty (T.POLYty { sign, tyfun = T.TYFUN { arity, body } }) =
		"d" $ [list bool sign, int arity, ty body]
(* we probably do not need to support pickling UNDEFty *)
	      | ty T.UNDEFty = "e" $ []
	      | ty (T.MARKty(ty1, region1)) = ty ty1
	      | ty _ = bug "unexpected type in pickmod-ty"
	in
	    ty arg
	end

        val op $ = PU.$ PI
        fun primId (POI.Prim s) = "A" $ [
		string (PrimopBind.nameOf s),
		ty (PrimopBind.typeOf s),
		primop (PrimopBind.defnOf s)
	      ]
          | primId (POI.NonPrim) = "B" $ []

        val op $ = PU.$ SPE
        fun strPrimElem(POI.PrimE p) = "a" $ [primId p]
          | strPrimElem(POI.StrE s) = "b" $ [list strPrimElem s]

	val op $ = PU.$ VAR
	fun var (V.VALvar { access = a, prim, path, typ = ref t, ...}) =
	    "1" $ [access a, primId prim, spath path, ty t]
	  | var (V.OVLDvar { name, variants }) =
	    "2" $ [symbol name, list var variants]
	  | var V.ERRORvar =
	    "3" $ []

	fun strDef arg = let
	    val op $ = PU.$ SD
	    fun sd (M.CONSTstrDef s) = "C" $ [Structure s]
	      | sd (M.VARstrDef (s, p)) = "V" $ [Signature s, entPath p]
	in
	    sd arg
	end

	(*
	 * boundeps is not pickled right now, but it really should
	 * be pickled in the future.
	 *)
	and Signature arg = let
	    val op $ = PU.$ SG
	    fun sg  M.ERRORsig = "A" $ []
	      | sg (M.SIG s) =
		(case sigStub s of
		     SOME (l, i) => "B" $ [libModSpec l, sigId i]
		   | NONE => let
			 fun sig_raw (s: M.sigrec) = let
			     val { stamp = sta, name, closed,
				   fctflag, elements,
				   properties,
				   stub, typsharing, strsharing } = s
(*			     val b = NONE (* = SigPropList.sigBoundeps s (currently turned off) *) *)
			 in
			     "C" $ ([stamp sta,
				     option symbol name,
				     bool closed,
				     bool fctflag,
				     list (pair (symbol, spec)) elements,
(*				     option (list (pair (entPath, tkind))) b, *)
				     list (list spath) typsharing,
				     list (list spath) strsharing]
				    @ libPid (stub, #owner))
			 end
		     in
			 share SIGs sig_raw s
		     end)
	in
	    sg arg
	end

	and fctSig arg = let
	    val op $ = PU.$ FSG
	    fun fsg M.ERRORfsig = "a" $ []
	      | fsg (M.FSIG { kind, paramsig, paramvar, paramsym, bodysig }) =
		"c" $ [option symbol kind, Signature paramsig,
		       entVar paramvar,
		       option symbol paramsym,
		       Signature bodysig]
	in
	    fsg arg
	end

	and spec arg = let
	    val op $ = PU.$ SP
	    fun sp (M.TYCspec { info, entVar = v }) =
		"1" $ [tycSpecInfo info, entVar v]
	      | sp (M.STRspec { sign, slot, def, entVar = v }) =
		"2" $ [Signature sign, int slot,
		       option (pair (strDef, int)) def, entVar v]
	      | sp (M.FCTspec { sign, slot, entVar = v }) =
		"3" $ [fctSig sign, int slot, entVar v]
	      | sp (M.VALspec { spec = t, slot }) =
                "4" $ [ty t, int slot]
	      | sp (M.CONspec { spec = c, slot }) =
		"5" $ [datacon c, option int slot]
	in
	    sp arg
	end

        and tycSpecInfo arg =
            let val op $ = PU.$ TSI
                fun tsi(M.RegTycSpec{spec = t, repl, scope}) =
                    "a" $ [tycon t, bool repl, int scope]
                  | tsi(M.InfTycSpec{name,arity}) =
                    "b" $ [symbol name, int arity]
             in tsi arg
            end

	and entity arg = let
	    val op $ = PU.$ EN
	    fun en (M.TYCent t) = "A" $ [tycEntity t]
	      | en (M.STRent t) = "B" $ [strEntity t]
	      | en (M.FCTent t) = "C" $ [fctEntity t]
	      | en M.ERRORent = "D" $ []
	in
	    en arg
	end

	and fctClosure (M.CLOSURE { param, body, env }) = let
	    val op $ = PU.$ FCTC
	in
	    "f" $ [entVar param, strExp body, entityEnv env]
	end

	and Structure arg = let
	    val op $ = PU.$ STR
	    fun str (M.STRSIG { sign, entPath = p }) =
		"A" $ [Signature sign, entPath p]
	      | str M.ERRORstr = "B" $ []
	      | str (M.STR (s as { sign, rlzn, access = a, prim })) =
		(case strStub s of
		     (* stub represents just the strerec suspension! *)
		     SOME (l, i) => "C" $ [Signature sign,
					   libModSpec l,
					   strId i,
					   access a,
					   list strPrimElem prim]
		   | NONE => "D" $ [Signature sign,
				    shStrEntity (MI.strId s) rlzn,
				    access a, list strPrimElem prim])
	in
	    str arg
	end

	and Functor arg = let
	    val op $ = PU.$ F
	    fun fct M.ERRORfct = "E" $ []
	      | fct (M.FCT (f as { sign, rlzn, access = a, prim })) =
		(case fctStub f of
		     SOME (l, i) => "F" $ [fctSig sign,
					   libModSpec l,
					   fctId i,
					   access a,
					   list strPrimElem prim]
		   | NONE => "G" $ [fctSig sign,
				    shFctEntity (MI.fctId f) rlzn,
				    access a, list strPrimElem prim])
	in
	    fct arg
	end

	and (* stampExp (M.CONST s) = PU.$ STE ("a", [stamp s])
	  | *) stampExp (M.GETSTAMP s) = PU.$ STE ("b", [strExp s])
	  | stampExp M.NEW = "c" $ []

        and tycExp (M.CONSTtyc t) = PU.$ TCE ("d", [tycon t])
	  | tycExp (M.FORMtyc t) = PU.$ TCE ("e", [tycon t])
	  | tycExp (M.VARtyc s) = PU.$ TCE ("f", [entPath s])

        and strExp arg = let
	    val op $ = PU.$ STRE
	    fun stre (M.VARstr s) = "g" $ [entPath s]
	      | stre (M.CONSTstr s) = "h" $ [strEntity s]
	      | stre (M.STRUCTURE { stamp = s, entDec }) =
		"i" $ [stampExp s, entityDec entDec]
	      | stre (M.APPLY (f, s)) = "j" $ [fctExp f, strExp s]
	      | stre (M.LETstr (e, s)) = "k" $ [entityDec e, strExp s]
	      | stre (M.ABSstr (s, e)) = "l" $ [Signature s, strExp e]
	      | stre (M.CONSTRAINstr { boundvar, raw, coercion }) =
		"m" $ [entVar boundvar, strExp raw, strExp coercion]
	      | stre (M.FORMstr fs) = "n" $ [fctSig fs]
	in
	    stre arg
	end

        and fctExp arg = let
	    val op $ = PU.$ FE
	    fun fe (M.VARfct s) = "o" $ [entPath s]
	      | fe (M.CONSTfct e) = "p" $ [fctEntity e]
	      | fe (M.LAMBDA { param, body }) =
		"q" $ [entVar param, strExp body]
	      | fe (M.LAMBDA_TP { param, body, sign }) =
		"r" $ [entVar param, strExp body, fctSig sign]
	      | fe (M.LETfct (e, f)) = "s" $ [entityDec e, fctExp f]
	in
	    fe arg
	end

        and entityExp arg = let
	    val op $ = PU.$ EE
	    fun ee (M.TYCexp t) = "t" $ [tycExp t]
	      | ee (M.STRexp s) = "u" $ [strExp s]
	      | ee (M.FCTexp f) = "v" $ [fctExp f]
	      | ee M.ERRORexp = "w" $ []
	      | ee M.DUMMYexp = "x" $ []
	in
	    ee arg
	end

        and entityDec arg = let
	    val op $ = PU.$ ED
	    fun ed (M.TYCdec (s, x)) = "A" $ [entVar s, tycExp x]
	      | ed (M.STRdec (s, x, n)) = "B" $ [entVar s, strExp x, symbol n]
	      | ed (M.FCTdec (s, x)) = "C" $ [entVar s, fctExp x]
	      | ed (M.SEQdec e) = "D" $ [list entityDec e]
	      | ed (M.LOCALdec (a, b)) = "E" $ [entityDec a, entityDec b]
	      | ed M.ERRORdec = "F" $ []
	      | ed M.EMPTYdec = "G" $ []
	in
	    ed arg
	end

        and entityEnv (M.MARKeenv m) =
	    (case envStub m of
		 SOME (l, i) => "D" $ [libModSpec l, envId i]
	       | NONE => let
		     fun mee_raw { stamp = s, env, stub } =
			 "E" $ ([stamp s, entityEnv env]
				@ libPid (stub: M.stubinfo option, #owner))
		 in
		     share ENVs mee_raw m
		 end)
	  | entityEnv (M.BINDeenv (d, r)) =
	    PU.$ EEV ("A", [list (pair (entVar, entity)) (ED.listItemsi d),
		           entityEnv r])
	  | entityEnv M.NILeenv = "B" $ []
	  | entityEnv M.ERReenv = "C" $ []

        and strEntity { stamp = s, entities, properties, rpath, stub } =
	    let val op $ = PU.$ SEN
	    in
		"s" $ ([stamp s, entityEnv entities, ipath rpath]
		       @ libPid (stub: M.stubinfo option, #owner))
	    end

	and shStrEntity id = share (STRs id) strEntity

        and fctEntity { stamp = s,
			closure, properties, tycpath, rpath, stub } =
	    let val op $ = PU.$ FEN
	    in
		"f" $ ([stamp s, fctClosure closure, ipath rpath]
		       @ libPid (stub: M.stubinfo option, #owner))
	    end

	and shFctEntity id = share (FCTs id) fctEntity

        and tycEntity x = tycon x

        fun fixity Fixity.NONfix = "N" $ []
	  | fixity (Fixity.INfix (i, j)) = PU.$ FX ("I", [int i, int j])

	val op $ = PU.$ B
	fun binding (B.VALbind x) = "1" $ [var x]
	  | binding (B.CONbind x) = "2" $ [datacon x]
	  | binding (B.TYCbind x) = "3" $ [tycon x]
	  | binding (B.SIGbind x) = "4" $ [Signature x]
	  | binding (B.STRbind x) = "5" $ [Structure x]
	  | binding (B.FSGbind x) = "6" $ [fctSig x]
	  | binding (B.FCTbind x) = "7" $ [Functor x]
	  | binding (B.FIXbind x) = "8" $ [fixity x]

	fun env e = let
	    val syms = ListMergeSort.uniqueSort symCmp (StaticEnv.symbols e)
	    val pairs = map (fn s => (s, StaticEnv.look (e, s))) syms
	in
	    list (pair (symbol, binding)) pairs
	end
    in
	env
    end (* envPickler *)

    fun pickleEnv context e = let
	val lvlist = ref []
	fun registerLvar v = lvlist := v :: !lvlist
	val pickler = envPickler registerLvar context
	val pickle = Byte.stringToBytes (PU.pickle emptyMap (pickler e))
	val exportLvars = rev (!lvlist)
	val hash = pickle2hash pickle
	val hasExports = not (List.null exportLvars)
    in
	addPickles (Word8Vector.length pickle);
	{ hash = hash, pickle = pickle, exportLvars = exportLvars,
	  hasExports = hasExports }
    end

    (* the dummy environment pickler *)
    fun dontPickle { env = senv, count } = let
	val hash = let
	    val toByte = Word8.fromLargeWord o Word32.toLargeWord
	    val >> = Word32.>>
	    infix >>
	    val w = Word32.fromInt count
	    in
	      PS.fromBytes
		(Word8Vector.fromList
		 [0w0,0w0,0w0,toByte(w >> 0w24),0w0,0w0,0w0,toByte(w >> 0w16),
		  0w0,0w0,0w0,toByte(w >> 0w8),0w0,0w0,0w0,toByte(w)])
	    end
        (* next line is an alternative to using Env.consolidate *)
	val syms = ListMergeSort.uniqueSort symCmp (StaticEnv.symbols senv)
	fun newAccess i = A.PATH (A.EXTERN hash, i)
	fun mapbinding (sym, (i, env, lvars)) = (case StaticEnv.look (senv, sym)
	     of B.VALbind(V.VALvar{access, prim=z, path=p, typ= ref t, btvs}) =>
	          (case access of
		     A.LVAR k =>
		     (i+1,
		      StaticEnv.bind (sym,
				      B.VALbind (V.VALvar
						     { access = newAccess i,
						       prim = z, path = p,
						       typ = ref t,
						       btvs = btvs}),
				      env),
		      k :: lvars)
		   | _ => bug (concat[
			"dontPickle 1: sym = '", Symbol.symbolToString sym,
			"', access = ", A.prAcc access
		      ])
		  (* end case *))
	      | B.STRbind (M.STR { sign = s, rlzn = r, access = a, prim =z }) =>
		(case a of
		     A.LVAR k =>
		     (i+1,
		      StaticEnv.bind (sym,
				      B.STRbind (M.STR
						     { access = newAccess i,
						       sign = s, rlzn = r,
						       prim = z }),
				env),
		      k :: lvars)
		   | _ => bug ("dontPickle 2" ^ A.prAcc a))
	      | B.FCTbind (M.FCT { sign = s, rlzn = r, access = a, prim = z }) =>
		(case a of
		     A.LVAR k =>
		     (i+1,
		      StaticEnv.bind (sym,
				      B.FCTbind (M.FCT
						     { access = newAccess i,
						       sign = s, rlzn = r,
						       prim = z }),
				      env),
		      k :: lvars)
		   | _ => bug ("dontPickle 3" ^ A.prAcc a))
	      | B.CONbind (T.DATACON { name = n, const = c, typ = t, sign = s,
				       lazyp= false, rep as (A.EXN a) }) => let
		    val newrep = A.EXN (newAccess i)
		in
		    case a of
			A.LVAR k =>
			(i+1,
			 StaticEnv.bind (sym,
					 B.CONbind (T.DATACON
							{ rep = newrep,
							  name = n,
							  lazyp = false,
							  const = c, typ = t,
							  sign = s }),
				   env),
			 k :: lvars)
		      | _ => bug ("dontPickle 4" ^ A.prAcc a)
		end
	      | binding => (i, StaticEnv.bind (sym, binding, env), lvars)
	    (* end case *))
	val (_,newenv,lvars) = foldl mapbinding (0, StaticEnv.empty, nil) syms
	val hasExports = not (List.null lvars)
    in
	{ newenv = newenv, hash = hash,
	  exportLvars = rev lvars, hasExports = hasExports }
    end
  end

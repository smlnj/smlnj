(* unpickmod.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * The new unpickler (based on the new generic unpickling facility).
 *
 * The unpickler embeds a "modtree" into the unpickled environment.
 * The modtree allows for very rapid construction of modmaps so that
 * modmaps do not have to be stored permanently but can be built on-demand.
 * (Permanently stored modmaps incur space problems: one has to be careful
 * that they don't hang on to bindings that no longer exist, and because
 * of sharing there can be significant overlap--and space overhead--in what
 * each such map points to.  Modtrees do not have these problems.)
 *
 * The embedding of modtrees into static environments follows the example
 * of the control-flow in the original "cmstatenv.sml" module.  This means
 * that not all possible branches of the environment data structure are
 * explored when building modmaps.  I dearly hope that the original code
 * was correct in its assumptions...
 *
 * March 2000, Matthias Blume
 *)

signature UNPICKMOD = sig

    type context = (int * Symbol.symbol) option ->
		   ModuleId.tmap *
		   (unit -> string) (* destription of module for debugging *)

    val unpickleEnv : context ->
		      PersStamps.persstamp * Word8Vector.vector ->
		      StaticEnv.staticEnv

    (* The env unpickler resulting from "mkUnpicklers" cannot be used for
     * "original" environments that come out of the elaborator.  For those,
     * continue to use "unpickleEnv".  "mkUnpicklers" is intended to be
     * used by CM's stable library mechanism. *)
    val mkUnpicklers :
	{ session: UnpickleUtil.session,
	  stringlist: string list UnpickleUtil.reader } ->
	context ->
	{ statenv: StaticEnv.staticEnv UnpickleUtil.reader,
	  symbol: Symbol.symbol UnpickleUtil.reader,
	  symbollist: Symbol.symbol list UnpickleUtil.reader }
end

structure UnpickMod : UNPICKMOD = struct

    type context = (int * Symbol.symbol) option ->
		   ModuleId.tmap * (unit -> string)

    structure A = Access
    structure LT = Lty
    structure T = Types
    structure SP = SymPath
    structure IP = InvPath
    structure MI = ModuleId
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
    structure POI = PrimopId

    structure UU = UnpickleUtil
    exception Format = UU.Format

    val arithop_table = #[
            ArithP.IADD, ArithP.ISUB, ArithP.IMUL,
            ArithP.IDIV, ArithP.IMOD,
            ArithP.IQUOT, ArithP.IREM,
            ArithP.INEG
          ]

    val pureop_table = #[
            PureP.ADD, PureP.SUB, PureP.MUL, PureP.QUOT, PureP.REM, PureP.NEG,
            PureP.LSHIFT, PureP.RSHIFT, PureP.RSHIFTL,
            PureP.ORB, PureP.XORB, PureP.ANDB, PureP.NOTB,
            PureP.CNTPOP, PureP.CNTLZ, PureP.CNTTZ,
            PureP.ROTL, PureP.ROTR,
            PureP.FDIV, PureP.FABS, PureP.FSQRT
          ]

    val cmpop_table =
	#[CmpP.GT, CmpP.GTE, CmpP.LT, CmpP.LTE, CmpP.EQL, CmpP.NEQ]

    val eqprop_table =
	#[T.YES, T.NO, T.IND, T.OBJ, T.DATA, T.ABS, T.UNDEF]

    val ctype_table =
	#[CTypes.C_void,
	  CTypes.C_float,
	  CTypes.C_double,
	  CTypes.C_long_double,
	  CTypes.C_unsigned CTypes.I_char,
	  CTypes.C_unsigned CTypes.I_short,
	  CTypes.C_unsigned CTypes.I_int,
	  CTypes.C_unsigned CTypes.I_long,
	  CTypes.C_unsigned CTypes.I_long_long,
	  CTypes.C_signed CTypes.I_char,
	  CTypes.C_signed CTypes.I_short,
	  CTypes.C_signed CTypes.I_int,
	  CTypes.C_signed CTypes.I_long,
	  CTypes.C_signed CTypes.I_long_long,
	  CTypes.C_PTR]

    fun & c (x, t) = (c x, t)

    fun branch l = let
	fun loop ([], [x]) = x
	  | loop ([], l) = M.BRANCH l
	  | loop (M.BRANCH [] :: t, l) = loop (t, l)
	  | loop (M.BRANCH [x] :: t, l) = loop (t, x :: l) (* never occurs! *)
	  | loop (x :: t, l) = loop (t, x :: l)
    in
	loop (l, [])
    end

    val notree = M.BRANCH []

    fun mkSharedStuff (session, lvar) = let

	fun share m f = UU.share session m f
	fun nonshare f = UU.nonshare session f

	val int = UU.r_int session
	val bool = UU.r_bool session
	fun list m r = UU.r_list session m r
	fun option m r = UU.r_option session m r
	val string = UU.r_string session
	val symbol = UnpickleSymPid.r_symbol (session, string)

	(* These maps will all acquire different types by being used in
	 * different contexts... *)
	val accM = UU.mkMap ()
	val crM = UU.mkMap ()
	val csM = UU.mkMap ()
	val nkM = UU.mkMap ()
	val poM = UU.mkMap ()
	val boolListM = UU.mkMap ()
	val boolOptionM = UU.mkMap ()
	val tkindM = UU.mkMap ()
	val tkindListM = UU.mkMap ()
	val ctypeM = UU.mkMap ()
	val ctypeListM = UU.mkMap ()
	val ccalltypeListM = UU.mkMap ()
	val ccalltypeOptionM = UU.mkMap ()
	val cciM = UU.mkMap ()
	val ioM = UU.mkMap ()

	val boollist = list boolListM bool
	val booloption = option boolOptionM bool
	val intoption = option ioM int

	val pid = UnpickleSymPid.r_pid (session, string)

	fun access () = let
	    fun a #"A" = lvar (int ())
	      | a #"B" = A.EXTERN (pid ())
	      | a #"C" = A.PATH (access (), int ())
	      | a #"D" = A.NO_ACCESS
	      | a _ = raise Format
            in
              share accM a
            end

	fun conrep () = let
	    fun cr #"A" = A.UNTAGGED
	      | cr #"B" = A.TAGGED (int ())
	      | cr #"C" = A.TRANSPARENT
	      | cr #"D" = A.CONSTANT (int ())
	      | cr #"E" = A.REF
	      | cr #"F" = A.EXN (access ())
	      | cr #"G" = A.LISTCONS
	      | cr #"H" = A.LISTNIL
	      | cr #"I" = A.SUSP NONE
	      | cr #"J" = A.SUSP (SOME (access (), access ()))
	      | cr _ = raise Format
            in
              share crM cr
            end

	fun consig () = let
	    fun cs #"S" = A.CSIG (int (), int ())
	      | cs #"N" = A.CNIL
	      | cs _ = raise Format
            in
              share csM cs
            end

	fun ctype () = let
	    fun ct #"\020" = CTypes.C_ARRAY (ctype (), int ())
	      | ct #"\021" = CTypes.C_STRUCT (ctypelist ())
	      | ct #"\022" = CTypes.C_UNION (ctypelist ())
	      | ct c =
		Vector.sub (ctype_table, Char.ord c)
		handle General.Subscript => raise Format
            in
              share ctypeM ct
            end

	and ctypelist () = list ctypeListM ctype ()

        fun ccalltype() = let
            fun ct #"\000" = CP.CCI32
              | ct #"\001" = CP.CCI64
              | ct #"\002" = CP.CCR64
	      | ct #"\003" = CP.CCML
              | ct _       = raise Format
            in
              nonshare ct
            end

        and ccalltypelist () = list ccalltypeListM ccalltype ()
        and ccalltypeoption () = option ccalltypeOptionM ccalltype ()

	fun ccall_info () = let
	    fun cp #"C" =
		{ c_proto = { conv = string (),
			      retTy = ctype (),
			      paramTys = ctypelist () },
		  ml_args = ccalltypelist (),
		  ml_res_opt = ccalltypeoption (),
		  reentrant = bool () }
	      | cp _ = raise Format
            in
              share cciM cp
            end

	fun numkind () = let
	    fun nk #"A" = P.INT (int ())
	      | nk #"B" = P.UINT (int ())
	      | nk #"C" = P.FLOAT (int ())
	      | nk _ = raise Format
            in
              share nkM nk
            end

        fun inlineop () = let
            fun inl #"\000" = InlP.DIV(numkind())
              | inl #"\001" = InlP.MOD(numkind())
              | inl #"\002" = InlP.QUOT(numkind())
              | inl #"\003" = InlP.REM(numkind())
              | inl #"\004" = InlP.LSHIFT(numkind())
              | inl #"\005" = InlP.RSHIFT(numkind())
              | inl #"\006" = InlP.RSHIFTL(numkind())
              | inl #"\007" = InlP.CNTZ(numkind())
              | inl #"\008" = InlP.CNTO(numkind())
              | inl #"\009" = InlP.CNTLZ(numkind())
              | inl #"\010" = InlP.CNTLO(numkind())
              | inl #"\011" = InlP.CNTTZ(numkind())
              | inl #"\012" = InlP.CNTTO(numkind())
              | inl #"\013" = InlP.IS_POW2(numkind())
              | inl #"\014" = InlP.CEIL_LOG2(numkind())
              | inl #"\015" = InlP.MIN(numkind())
              | inl #"\016" = InlP.MAX(numkind())
              | inl #"\017" = InlP.ABS(numkind())
              | inl #"\018" = InlP.CHR
              | inl #"\019" = InlP.MKARRAY
              | inl #"\020" = InlP.SUBSCRIPT
              | inl #"\021" = InlP.SUBSCRIPTV
              | inl #"\022" = InlP.UPDATE
              | inl #"\023" = InlP.UNBOXEDUPDATE
              | inl #"\024" = InlP.NUMSUBSCRIPT(numkind())
              | inl #"\025" = InlP.NUMSUBSCRIPTV(numkind())
              | inl #"\026" = InlP.NUMUPDATE(numkind())
              | inl #"\027" = InlP.NOT
              | inl #"\028" = InlP.COMPOSE
              | inl #"\029" = InlP.BEFORE
              | inl #"\030" = InlP.IGNORE
              | inl #"\031" = InlP.IDENTITY
              | inl #"\032" = InlP.HOST_WORD_SIZE
              | inl #"\033" = InlP.HOST_BIG_ENDIAN
              | inl _ = raise Format
            in
              nonshare inl
            end

	local
	  fun operator tbl = let
		fun rator c =
		      Vector.sub (tbl, Char.ord c)
			handle General.Subscript => raise Format
		in
		  fn () => nonshare rator
		end
	in
	val arithop = operator arithop_table
	val pureop = operator pureop_table
	val cmpop = operator cmpop_table
	end (* local *)

        fun commonop () = let
            fun fromto oper = oper(int(), int())
            fun cop #"\000" = CP.FSGN(int())
              | cop #"\001" = fromto CP.TESTU
              | cop #"\002" = fromto CP.TEST
              | cop #"\003" = fromto CP.TRUNC
              | cop #"\004" = fromto CP.EXTEND
              | cop #"\005" = fromto CP.COPY
              | cop #"\006" = CP.TEST_INF(int())
              | cop #"\007" = CP.TRUNC_INF(int())
              | cop #"\008" = CP.EXTEND_INF(int())
              | cop #"\009" = CP.COPY_INF(int())
              | cop #"\010" = CP.REAL_TO_INT{floor=bool(), from=int(), to=int()}
              | cop #"\011" = CP.INT_TO_REAL{from=int(), to=int()}
              | cop #"\012" = CP.NUMSUBSCRIPT(numkind())
              | cop #"\013" = CP.NUMSUBSCRIPTV(numkind())
              | cop #"\014" = CP.NUMUPDATE(numkind())
              | cop #"\015" = CP.SUBSCRIPT
              | cop #"\016" = CP.SUBSCRIPTV
              | cop #"\017" = CP.UPDATE
              | cop #"\018" = CP.UNBOXEDUPDATE
              | cop #"\019" = CP.LENGTH
              | cop #"\020" = CP.PTREQL
              | cop #"\021" = CP.PTRNEQ
              | cop #"\022" = CP.POLYEQL
              | cop #"\023" = CP.POLYNEQ
              | cop #"\024" = CP.BOXED
              | cop #"\025" = CP.UNBOXED
              | cop #"\026" = CP.CAST
              | cop #"\027" = CP.REAL_TO_BITS(int())
              | cop #"\028" = CP.BITS_TO_REAL(int())
              | cop #"\029" = CP.GETHDLR
              | cop #"\030" = CP.SETHDLR
              | cop #"\031" = CP.GETVAR
              | cop #"\032" = CP.SETVAR
              | cop #"\033" = CP.CALLCC
              | cop #"\034" = CP.CAPTURE
              | cop #"\035" = CP.THROW
              | cop #"\036" = CP.ISOLATE
              | cop #"\037" = CP.MAKEREF
              | cop #"\038" = CP.DEREF
              | cop #"\039" = CP.ASSIGN
              | cop #"\040" = CP.UNBOXEDASSIGN
              | cop #"\041" = CP.OBJLENGTH
              | cop #"\042" = CP.GETTAG
              | cop #"\043" = CP.MKSPECIAL
              | cop #"\044" = CP.SETSPECIAL
              | cop #"\045" = CP.GETSPECIAL
              | cop #"\046" = CP.NEW_ARRAY0
              | cop #"\047" = CP.GET_SEQ_DATA
              | cop #"\048" = CP.SUBSCRIPT_REC
              | cop #"\049" = CP.SUBSCRIPT_RAW64
              | cop #"\050" = CP.CPTR_TO_WORD
              | cop #"\051" = CP.WORD_TO_CPTR
              | cop #"\052" = CP.RAW_LOAD(numkind())
              | cop #"\053" = CP.RAW_STORE(numkind())
              | cop #"\054" = CP.RAW_CCALL NONE
              | cop #"\055" = CP.RAW_CCALL(SOME(ccall_info()))
              | cop #"\056" = CP.RAW_RECORD{align=int()}
              | cop _ = raise Format
            in
              nonshare cop
            end

	fun primop () = let
            fun po #"\000" = P.INLINE(inlineop())
              | po #"\001" = P.ARITH{ oper = arithop (), sz = int () }
              | po #"\002" = P.PURE{ oper = pureop (), kind = numkind () }
              | po #"\003" = P.CMP{ oper = cmpop (), kind = numkind () }
              | po #"\004" = P.PRIM(commonop())
              | po _ = raise Format
	    in
	      share poM po
	    end

    in
	{ pid = pid, string = string, symbol = symbol,
	  access = access, conrep = conrep, consig = consig,
	  primop = primop, boollist = boollist, intoption = intoption }
    end

    fun mkEnvUnpickler extraInfo sessionInfo context = let
	val { globalPid, symbollist, sharedStuff, lib } = extraInfo
	val { session, stringlist } = sessionInfo

	local
	    fun look what lk (m, i) =
		let val (mapping, descr) = context m
		in case lk (mapping, i) of
		       SOME x => x
		     | NONE =>
		         (ErrorMsg.impossible
			      (concat ["UnpickMod: stub lookup failed for ",
				       what, " in ", descr (),
				       ": m = ",
				       case m of
					   NONE => "NONE"
					 | SOME (x, sy) =>
					   "(" ^ Int.toString x ^
					   ", " ^
					   Symbol.describe sy ^ ")",
				       ", i = <moduleId>\n"]);
			  raise Format)
		end
	in
	    val lookTyc = look "type constructor" MI.lookTyc
	    val lookSig = look "signature" MI.lookSig
	    val lookStr = look "structure" MI.lookStr
	    val lookFct = look "functor" MI.lookFct
	    val lookEnv = look "environment" MI.lookEnv
	end

	fun list m r = UU.r_list session m r
	fun option m r = UU.r_option session m r
	val bool = UU.r_bool session
	fun pair m fp p = UU.r_pair session m fp p
	val int = UU.r_int session

	fun share m f = UU.share session m f
	fun nonshare f = UU.nonshare session f

	(* The following maps all acquire different types by being used
	 * in different contexts: *)
	val stampM = UU.mkMap ()
	val strIdM = UU.mkMap ()
	val fctIdM = UU.mkMap ()
	val stampOptionM = UU.mkMap ()
	val stampListM = UU.mkMap ()
	val symbolOptionM = UU.mkMap ()
	val symbolListM = UU.mkMap ()
	val spathListM = UU.mkMap ()
	val spathListListM = UU.mkMap ()
	val varListM = UU.mkMap ()
	val dataconM = UU.mkMap ()
	val tkM = UU.mkMap ()
	val dtiM = UU.mkMap ()
	val dtfM = UU.mkMap ()
	val dtmemberM = UU.mkMap ()
	val dtmListM = UU.mkMap ()
	val nrdM = UU.mkMap ()
	val nrdListM = UU.mkMap ()
	val tyconM = UU.mkMap ()
	val tyconListM = UU.mkMap ()
	val tyM = UU.mkMap ()
	val tyOptionM = UU.mkMap ()
	val tyListM = UU.mkMap ()
	val primIdM = UU.mkMap ()
        val strPrimElemM = UU.mkMap ()
	val speListM = UU.mkMap()
        val vM = UU.mkMap ()
	val sdM = UU.mkMap ()
	val sigM = UU.mkMap ()
	val fsigM = UU.mkMap ()
	val spM = UU.mkMap ()
	val tsiM = UU.mkMap ()
	val enM = UU.mkMap ()
	val fctcM = UU.mkMap ()
	val strM = UU.mkMap ()
	val fctM = UU.mkMap ()
	val steM = UU.mkMap ()
	val tceM = UU.mkMap ()
	val streM = UU.mkMap ()
	val feM = UU.mkMap ()
	val eeM = UU.mkMap ()
	val edM = UU.mkMap ()
	val eenvM = UU.mkMap ()
	val senM = UU.mkMap ()
	val fenM = UU.mkMap ()
	val fxM = UU.mkMap ()
	val bM = UU.mkMap ()
	val elementsM = UU.mkMap ()
	val bepsLM = UU.mkMap ()
	val bepsOM = UU.mkMap ()
	val spDefM = UU.mkMap ()
	val iiListM = UU.mkMap ()
	val overldM = UU.mkMap ()
	val olListM = UU.mkMap ()
	val edListM = UU.mkMap ()
	val eenvBindM = UU.mkMap ()
	val envM = UU.mkMap ()
	val spathM = UU.mkMap ()
	val ipathM = UU.mkMap ()
	val symSpecPM = UU.mkMap ()
	val epTkPM = UU.mkMap ()
	val sdIntPM = UU.mkMap ()
	val evEntPM = UU.mkMap ()
	val symBindPM = UU.mkMap ()
	val pidOptionM = UU.mkMap ()
	val lmsOptM = UU.mkMap ()
	val lmsPairM = UU.mkMap ()

	val { pid, string, symbol, access, conrep, consig, intoption,
	      primop, boollist } = sharedStuff

	fun libModSpec () = option lmsOptM (pair lmsPairM (int, symbol)) ()

	fun stamp () = let
	    fun st #"A" = Stamps.global { pid = globalPid (),
					  cnt = int () }
	      | st #"B" = Stamps.global { pid = pid (),
					  cnt = int () }
	      | st #"C" = Stamps.special (string ())
	      | st _ = raise Format
	in
	    share stampM st
	end

	val tycId = stamp
	val sigId = stamp
	fun strId () = let
	    fun si #"D" = { sign = stamp (), rlzn = stamp () }
	      | si _ = raise Format
	in
	    share strIdM si
	end
	fun fctId () = let
	    fun fi #"E" = { paramsig = stamp (), bodysig = stamp (),
			    rlzn = stamp () }
	      | fi _ = raise Format
	in
	    share fctIdM fi
	end
	val envId = stamp

	val stamplist = list stampListM stamp
	val stampoption = option stampOptionM stamp
	val pidoption = option pidOptionM pid

	val entVar = stamp
	val entVarOption = stampoption
	val entPath = stamplist

	val symbollist = list symbolListM symbol
	val symboloption = option symbolOptionM symbol

	fun spath () = let
	    fun sp #"s" = SP.SPATH (symbollist ())
	      | sp _ = raise Format
	in
	    share spathM sp
	end

	fun ipath () = let
	    fun ip #"i" = IP.IPATH (symbollist ())
	      | ip _ = raise Format
	in
	    share ipathM ip
	end

	val spathlist = list spathListM spath
	val spathlistlist = list spathListListM spathlist

	val label = symbol
	val labellist = symbollist

	fun eqprop () = let
	    fun eqp c =
		Vector.sub (eqprop_table, Char.ord c)
		handle General.Subscript => raise Format
	in
	    nonshare eqp
	end

	fun datacon' () = let
	    fun d #"c" =
		let val n = symbol ()
		    val c = bool ()
		    val (t, ttr) = ty' ()
		    val r = conrep ()
		    val s = consig ()
		    val l = bool ()
		in
		    (T.DATACON { name = n, const = c, typ = t,
				 rep = r, sign = s, lazyp = l },
		     ttr)
		end
	      | d _ = raise Format
	in
	    share dataconM d
	end

	and tyckind () = let
	    fun tk #"a" = T.PRIMITIVE
	      | tk #"b" = let
		    val index = int ()
		    val root = entVarOption ()
		    val stripped = bool ()
		    val (stamps, family, freetycs) = dtypeInfo ()
		in
		    T.DATATYPE { index = index, root = root,
				 stamps = stamps, family = family,
				 freetycs = freetycs, stripped = stripped }
		end
	      | tk #"c" = T.ABSTRACT (tycon ())
	      | tk #"d" = T.FORMAL
	      | tk #"e" = T.TEMP
	      | tk _ = raise Format
	in
	    share tkM tk
	end

	and dtypeInfo () = let
	    fun dti #"a" =
		(Vector.fromList (stamplist ()), dtFamily (), tyconlist ())
	      | dti _ = raise Format
	in
	    share dtiM dti
	end

	and dtFamily () = let
	    fun dtf #"b" =
		{ mkey = stamp (),
		  members = Vector.fromList (dtmemberlist ()),
		  properties = PropList.newHolder () }
	      | dtf _ = raise Format
	in
	    share dtfM dtf
	end

	and dtmember () = let
	    fun d #"c" = { tycname = symbol (), dcons = nrdlist (),
			   arity = int (), eq = ref (eqprop ()),
			   lazyp = bool (), sign = consig () }
	      | d _ = raise Format
	in
	    share dtmemberM d
	end

	and dtmemberlist () = list dtmListM dtmember ()

	and nameRepDomain () = let
	    fun n #"d" =
		{ name = symbol (), rep = conrep (), domain = tyoption () }
	      | n _ = raise Format
	in
	    share nrdM n
	end

	and nrdlist () = list nrdListM nameRepDomain ()

	and tycon () = let
	    fun tyc #"A" = T.GENtyc (lookTyc (libModSpec (), tycId ()))
	      | tyc #"B" = T.GENtyc { stamp = stamp (),
				      arity = int (),
				      eq = ref (eqprop ()),
				      kind = tyckind (),
				      path = ipath (),
				      stub = SOME { owner = if lib then pid ()
							    else globalPid (),
						    lib = lib } }
	      | tyc #"C" = T.DEFtyc { stamp = stamp (),
				      tyfun = T.TYFUN { arity = int (),
						        body = ty () },
				      strict = boollist (),
				      path = ipath () }
	      | tyc #"D" =  T.PATHtyc { arity = int (), entPath = entPath (),
				        path = ipath () }
	      | tyc #"E" = T.RECORDtyc (labellist ())
	      | tyc #"F" = T.RECtyc (int ())
	      | tyc #"G" = T.FREEtyc (int ())
	      | tyc #"H" = T.ERRORtyc
	      | tyc _ = raise Format
	in
	    share tyconM tyc
	end

	and tycon' () = let
	    val tyc = tycon ()
	    val tree =
		case tyc of
		    T.GENtyc r => M.TYCNODE r
		  | _ => notree
	in
	    (tyc, tree)
	end

	and tyconlist () = list tyconListM tycon ()

	and ty' () = let
	    fun t #"a" =
		let val (tyc, tyctr) = tycon' ()
		    val (tyl, tyltr) = tylist' ()
		in (T.CONty (tyc, tyl), branch [tyctr, tyltr])
		end
	      | t #"b" = (T.IBOUND (int ()), notree)
	      | t #"c" = (T.WILDCARDty, notree)
	      | t #"d" =
		let val s = boollist ()
		    val ar = int ()
		    val (b, btr) = ty' ()
		in
		    (T.POLYty { sign = s, tyfun = T.TYFUN { arity = ar,
							    body = b } },
		     btr)
		end
	      | t #"e" = (T.UNDEFty, notree)
	      | t _ = raise Format
	in
	    share tyM t
	end

	and ty () = #1 (ty' ())

	and tyoption () = option tyOptionM ty ()

	and tylist' () = let
	    val (l, trl) = ListPair.unzip (list tyListM ty' ())
	in
	    (l, branch trl)
	end

        and primId () = let
	      fun p #"A" = let
		      val n = string ()
		      val ty = ty ()
		      val p = primop ()
		    in
		      POI.Prim (PrimopBind.mk (n, ty, p))
		    end
		| p #"B" = POI.NonPrim
		| p _ = raise Format
	      in
		share primIdM p
	      end

        and strPrimElem () =
            let
                fun sp #"a" = POI.PrimE (primId ())
                  | sp #"b" = POI.StrE (spelist ())
		  | sp _ = raise Format
            in
                share strPrimElemM sp
            end

        and spelist () = list speListM strPrimElem ()

	and var' () = let
	    fun v #"1" =
		let val a = access ()
		    val i = primId ()
		    val p = spath ()
		    val (t, tr) = ty' ()
		in
		    (V.VALvar { access = a, prim = i, path = p, typ = ref t, btvs = ref [] },
		     tr)
		end
	      | v #"2" =
		let val n = symbol ()
		    val (vl,vltr) = varlist' ()
		in
		    (V.OVLDvar { name = n, variants = vl},
		     vltr)
		end
	      | v #"3" = (V.ERRORvar, notree)
	      | v _ = raise Format
	in
	    share vM v
	end

	and varlist' () = let
	    val (l, trl) = ListPair.unzip (list varListM var' ())
	in
	    (l, branch trl)
	end

	fun strDef () = let
	    fun sd #"C" = M.CONSTstrDef (Structure ())
	      | sd #"V" = M.VARstrDef (Signature (), entPath ())
	      | sd _ = raise Format
	in
	    share sdM sd
	end

	and Signature' () = let
	    fun sg #"A" = (M.ERRORsig, notree)
	      | sg #"B" =
		let val sr = lookSig (libModSpec (), sigId ())
		in
		    (M.SIG sr, M.SIGNODE sr)
		end
	      | sg #"C" =
		let val s = stamp ()
		    val n = symboloption ()
		    val c = bool ()
		    val ff = bool ()
		    val (el, eltrl) =
			ListPair.unzip
			    (map (fn (sy, (sp, tr)) => ((sy, sp), tr))
			         (list elementsM
				  (pair symSpecPM (symbol, spec')) ()))
		    val ts = spathlistlist ()
		    val ss = spathlistlist ()
		    val r = { stamp = s,
			      name = n,
			      closed = c,
			      fctflag = ff,
			      elements = el,
			      properties = PropList.newHolder (),
			      typsharing = ts,
			      strsharing = ss,
			      stub = SOME { owner = if lib then pid ()
						    else globalPid (),
					    tree = branch eltrl,
					    lib = lib } }
		in
		    (M.SIG r, M.SIGNODE r)
		end
	      | sg _ = raise Format
	in
	    share sigM sg
	end

	and Signature () = #1 (Signature' ())

	and fctSig' () = let
	    fun fsg #"a" = (M.ERRORfsig, notree)
	      | fsg #"c" =
		let val k = symboloption ()
		    val (ps, pstr) = Signature' ()
		    val pv = entVar ()
		    val psy = symboloption ()
		    val (bs, bstr) = Signature' ()
		in
		    (M.FSIG { kind = k, paramsig = ps,
			      paramvar = pv, paramsym = psy,
			      bodysig = bs },
		     branch [pstr, bstr])
		end
	      | fsg _ = raise Format
	in
	    share fsigM fsg
	end

	and spec' () = let
	    fun sp #"1" =
		let val (i, itr) = tycSpecInfo' ()
		in
		    (M.TYCspec { entVar = entVar (), info = i },
		     itr)
		end
	      | sp #"2" =
		let val (s, str) = Signature' ()
		in
		    (M.STRspec { sign = s, slot = int (),
				 def = option spDefM
				              (pair sdIntPM (strDef, int)) (),
				 entVar = entVar () },
		     str)
		end
	      | sp #"3" =
		let val (f, ftr) = fctSig' ()
		in
		    (M.FCTspec { sign = f, slot = int (), entVar = entVar () },
		     ftr)
		end
	      | sp #"4" =
		let val (t, ttr) = ty' ()
		in
		    (M.VALspec { spec = t, slot = int () }, ttr)
		end
	      | sp #"5" =
		let val (d, dtr) = datacon' ()
		in
		    (M.CONspec { spec = d, slot = intoption () }, dtr)
		end
	      | sp _ = raise Format
	in
	    share spM sp
	end

        and tycSpecInfo' () =
            let fun tsi #"a" =
                    let val (t,ttr) = tycon' ()
                    in (M.RegTycSpec{spec = t, repl = bool (), scope = int ()},
                        ttr)
                    end
                  | tsi #"b" =
                    (M.InfTycSpec{name = symbol (), arity = int ()},
                     notree)
                  | tsi _ = raise Format
             in share tsiM tsi
            end

	and entity' () = let
	    fun en #"A" = & M.TYCent (tycEntity' ())
	      | en #"B" = & M.STRent (strEntity' ())
	      | en #"C" = & M.FCTent (fctEntity' ())
	      | en #"D" = (M.ERRORent, notree)
	      | en _ = raise Format
	in
	    share enM en
	end

	and fctClosure' () = let
	    fun f #"f" =
		let val p = entVar ()
		    val (b, btr) = strExp' ()
		    val (e, etr) = entityEnv' ()
		in
		    (M.CLOSURE { param = p, body = b, env = e },
		     branch [btr, etr])
		end
	      | f _ = raise Format
	in
	    share fctcM f
	end

	(* The construction of the STRNODE in the modtree deserves some
	 * comment:  Even though it contains the whole strrec, it does
	 * _not_ take care of the Signature contained therein.  The reason
	 * why STRNODE has the whole strrec and not just the strEntity that
	 * it really guards is that the identity of the strEntity is not
	 * fully recoverable without also having access to the Signature.
	 * The same situation occurs in the case of FCTNODE. *)
	and Structure' () = let
	    fun str #"A" =
		let val (s, str) = Signature' ()
		in
		    (M.STRSIG { sign = s, entPath = entPath () }, str)
		end
	      | str #"B" = (M.ERRORstr, notree)
	      | str #"C" =
		let val (s, str) = Signature' ()
		    val r = { sign = s,
			      rlzn = lookStr (libModSpec (), strId ()),
			      access = access (),
			      prim = spelist () }
		in
		    (M.STR r, branch [str, M.STRNODE r])
		end
	      | str #"D" =
		let val (s, str) = Signature' ()
		    val r = { sign = s,
			      rlzn = strEntity (),
			      access = access (),
			      prim = spelist () }
		in
		    (M.STR r, branch [str, M.STRNODE r])
		end
	      | str _ = raise Format
	in
	    share strM str
	end

	and Structure () = #1 (Structure' ())

	(* See the comment about STRNODE, strrec, Signature, and strEntity
	 * in front of Structure'.  The situation for FCTNODE, fctrec,
	 * fctSig, and fctEntity is analogous. *)
	and Functor' () = let
	    fun fct #"E" = (M.ERRORfct, notree)
	      | fct #"F" =
		let val (s, str) = fctSig' ()
		    val r = { sign = s,
			      rlzn = lookFct (libModSpec (), fctId ()),
			      access = access (),
			      prim = spelist () }
		in
		    (M.FCT r, branch [str, M.FCTNODE r])
		end
	      | fct #"G" =
		let val (s, str) = fctSig' ()
		    val r = { sign = s,
			      rlzn = fctEntity (),
			      access = access (),
			      prim = spelist () }
		in
		    (M.FCT r, branch [str, M.FCTNODE r])
		end
	      | fct _ = raise Format
	in
	    share fctM fct
	end

	and stampExp () = let
	    fun ste #"b" = M.GETSTAMP (strExp ())
	      | ste #"c" = M.NEW
	      | ste _ = raise Format
	in
	    share steM ste
	end

	and tycExp' () = let
	    fun tce #"d" = & M.CONSTtyc (tycon' ())
	      | tce #"e" = (M.FORMtyc (tycon ()), notree) (* ? *)
	      | tce #"f" = (M.VARtyc (entPath ()), notree)
	      | tce _ = raise Format
	in
	    share tceM tce
	end

	and tycExp () = #1 (tycExp' ())

	and strExp' () = let
	    fun stre #"g" = (M.VARstr (entPath ()), notree)
	      | stre #"h" = & M.CONSTstr (strEntity' ())
	      | stre #"i" =
		let val s = stampExp ()
		    val (d, dtr) = entityDec' ()
		in
		    (M.STRUCTURE { stamp = s, entDec = d }, dtr)
		end
	      | stre #"j" =
		let val (f, ftr) = fctExp' ()
		    val (s, str) = strExp' ()
		in
		    (M.APPLY (f, s), branch [ftr, str])
		end
	      | stre #"k" =
		let val (d, dtr) = entityDec' ()
		    val (s, str) = strExp' ()
		in
		    (M.LETstr (d, s), branch [dtr, str])
		end
	      | stre #"l" =
		let val (s, str) = Signature' ()
		    val (e, etr) = strExp' ()
		in
		    (M.ABSstr (s, e), branch [str, etr])
		end
	      | stre #"m" =
		let val bv = entVar ()
		    val (r, rtr) = strExp' ()
		    val (c, ctr) = strExp' ()
		in
		    (M.CONSTRAINstr { boundvar = bv, raw = r, coercion = c },
		     branch [rtr, ctr])
		end
	      | stre #"n" = & M.FORMstr (fctSig' ())
	      | stre _ = raise Format
	in
	    share streM stre
	end

	and strExp () = #1 (strExp' ())

	and fctExp' () = let
	    fun fe #"o" = (M.VARfct (entPath ()), notree)
	      | fe #"p" = & M.CONSTfct (fctEntity' ())
	      | fe #"q" =
		let val p = entVar ()
		    val (b, btr) = strExp' ()
		in
		    (M.LAMBDA { param = p, body = b }, btr)
		end
	      | fe #"r" =
		let val p = entVar ()
		    val (b, btr) = strExp' ()
		    val (s, str) = fctSig' ()
		in
		    (M.LAMBDA_TP { param = p, body = b, sign = s },
		     branch [btr, str])
		end
	      | fe #"s" =
		let val (d, dtr) = entityDec' ()
		    val (f, ftr) = fctExp' ()
		in
		    (M.LETfct (d, f), branch [dtr, ftr])
		end
	      | fe _ = raise Format
	in
	    share feM fe
	end

	and fctExp () = #1 (fctExp' ())

	and entityExp () = let
	    fun ee #"t" = M.TYCexp (tycExp ())
	      | ee #"u" = M.STRexp (strExp ())
	      | ee #"v" = M.FCTexp (fctExp ())
	      | ee #"w" = M.ERRORexp
	      | ee #"x" = M.DUMMYexp
	      | ee _ = raise Format
	in
	    share eeM ee
	end

	and entityDec' () = let
	    fun ed #"A" =
		let val v = entVar ()
		    val (e, etr) = tycExp' ()
		in
		    (M.TYCdec (v, e), etr)
		end
	      | ed #"B" =
		let val v = entVar ()
		    val (e, etr) = strExp' ()
		    val s = symbol ()
		in
		    (M.STRdec (v, e, s), etr)
		end
	      | ed #"C" =
		let val v = entVar ()
		    val (e, etr) = fctExp' ()
		in
		    (M.FCTdec (v, e), etr)
		end
	      | ed #"D" = & M.SEQdec (entityDecList' ())
	      | ed #"E" =
		let val (d1, d1tr) = entityDec' ()
		    val (d2, d2tr) = entityDec' ()
		in
		    (M.LOCALdec (d1, d2), branch [d1tr, d2tr])
		end
	      | ed #"F" = (M.ERRORdec, notree)
	      | ed #"G" = (M.EMPTYdec, notree)
	      | ed _ = raise Format
	in
	    share edM ed
	end

	and entityDecList' () = let
	    val (l, trl) = ListPair.unzip (list edListM entityDec' ())
	in
	    (l, branch trl)
	end

	and entityEnv' () = let
	    fun eenv #"A" =
		let val l = list eenvBindM (pair evEntPM (entVar, entity')) ()
		    val l' = map (fn (v, (e, tr)) => ((v, e), tr)) l
		    val (l'', trl) = ListPair.unzip l'
		    fun add ((v, e), z) = ED.insert (z, v, e)
		    val ed = foldr add ED.empty l''
		    val (e, etr) = entityEnv' ()
		in
		    (M.BINDeenv (ed, e), branch (etr :: trl))
		end
	      | eenv #"B" = (M.NILeenv, notree)
	      | eenv #"C" = (M.ERReenv, notree)
	      | eenv #"D" =
		let val r = lookEnv (libModSpec (), envId ())
		in
		    (M.MARKeenv r, M.ENVNODE r)
		end
	      | eenv #"E" =
		let val s = stamp ()
		    val (e, etr) = entityEnv' ()
		    val r = { stamp = s,
			      env = e,
			      stub = SOME { owner = if lib then pid ()
						    else globalPid (),
					    tree = etr,
					    lib = lib } }
		in
		    (M.MARKeenv r, M.ENVNODE r)
		end
	      | eenv _ = raise Format
	in
	    share eenvM eenv
	end

	and strEntity' () = let
	    fun s #"s" =
		let val s = stamp ()
		    val (e, etr) = entityEnv' ()
		in
		    ({ stamp = s,
		       entities = e,
		       rpath = ipath (),
		       properties = PropList.newHolder (),
		       (* lambdaty = ref NONE, *)
		       stub = SOME { owner = if lib then pid ()
					     else globalPid (),
				     tree = etr,
				     lib = lib } },
		     etr)
		end
	      | s _ = raise Format
	in
	    share senM s
	end

	and strEntity () = #1 (strEntity' ())

	and fctEntity' () = let
	    fun f #"f" =
		let val s = stamp ()
		    val (c, ctr) = fctClosure' ()
		in
		    ({ stamp = s,
		       closure = c,
		       rpath = ipath (),
		       properties = PropList.newHolder (),
		       (* lambdaty = ref NONE, *)
		       tycpath = NONE,
		       stub = SOME { owner = if lib then pid ()
					     else globalPid (),
				     tree = ctr,
				     lib = lib } },
		     ctr)
		end
	      | f _ = raise Format
	in
	    share fenM f
	end

	and fctEntity () = #1 (fctEntity' ())

	and tycEntity' () = tycon' ()

	fun fixity () = let
	    fun fx #"N" = Fixity.NONfix
	      | fx #"I" = Fixity.INfix (int (), int ())
	      | fx _ = raise Format
	in
	    share fxM fx
	end

	fun binding' () = let
	    fun b #"1" = & B.VALbind (var' ())
	      | b #"2" = & B.CONbind (datacon' ())
	      | b #"3" = & B.TYCbind (tycon' ())
	      | b #"4" = & B.SIGbind (Signature' ())
	      | b #"5" = & B.STRbind (Structure' ())
	      | b #"6" = & B.FSGbind (fctSig' ())
	      | b #"7" = & B.FCTbind (Functor' ())
	      | b #"8" = (B.FIXbind (fixity ()), notree)
	      | b _ = raise Format
	in
	    share bM b
	end

	fun env () = let
	    val bindlist = list envM (pair symBindPM (symbol, binding')) ()
	    fun bind ((s, (b, t)), e) = StaticEnv.bindRB (s, (b, SOME t), e)
	in
	    StaticEnv.consolidate (foldl bind StaticEnv.empty bindlist)
	end
    in
	env
    end

    fun unpickleEnv context (hash, pickle) = let
	val session =
	    UU.mkSession (UU.stringGetter (Byte.bytesToString pickle))
	fun import i = A.PATH (A.EXTERN hash, i)
	val slM = UU.mkMap ()
	val sloM = UU.mkMap ()
	val sylM = UU.mkMap ()
	val sharedStuff = mkSharedStuff (session, import)
	val stringlist = UU.r_list session slM (#string sharedStuff)
	val symbollist = UU.r_list session sylM (#symbol sharedStuff)
	val extraInfo = { globalPid = fn () => hash,
			  symbollist = symbollist,
			  sharedStuff = sharedStuff,
			  lib = false }
	val sessionInfo = { session = session, stringlist = stringlist }
	val unpickle = mkEnvUnpickler extraInfo sessionInfo context
    in
	unpickle ()
    end

    fun mkUnpicklers sessionInfo context = let
	val { session, stringlist } = sessionInfo
	val sharedStuff = mkSharedStuff (session, A.LVAR o LambdaVar.fromId)
	val { symbol, pid, ... } = sharedStuff
	val sylM = UU.mkMap ()
	val symbollist = UU.r_list session sylM symbol
	val extraInfo = { globalPid = fn () => raise Format,
			  symbollist = symbollist,
			  sharedStuff = sharedStuff,
			  lib = true }
	val statenv = mkEnvUnpickler extraInfo sessionInfo context
    in
	{ statenv = statenv, symbol = symbol, symbollist = symbollist }
    end

    val unpickleEnv =
	fn c => Stats.doPhase (Stats.makePhase "Compiler 087 unpickleEnv")
			      (unpickleEnv c)
end

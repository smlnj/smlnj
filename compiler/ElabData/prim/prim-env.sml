(* prim-env.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PRIM_ENV =
  sig
    val primEnv : StaticEnv.staticEnv
  end (* signature PRIM_ENV *)

structure PrimEnv : PRIM_ENV =
  struct

    structure S = Symbol
    structure M = Modules
    structure B = Bindings
    structure SP = SymPath
    structure IP = InvPath
    structure SE = StaticEnv
    structure EE = EntityEnv

    structure BT = BasicTypes
    structure T = Types
    structure TU = TypesUtil
    structure MU = ModuleUtil

    structure ST = Stamps
    structure V = Variable

    structure A = Access

    fun mkTycElement (name: string, tyc) = let
	  val sym = S.tycSymbol name
	  val tyc = M.TYCspec{
		  entVar=ST.special name,
		  info=M.RegTycSpec{spec=tyc, repl=false, scope=0}
		}
	  in
	    (sym, tyc)
	  end

  (*
   * Note: this function only applies to constructors but not exceptions;
   * exceptions will have a non-trivial slot number
   *)
    fun mkConElement (name, d) = (S.varSymbol name, M.CONspec{spec=d, slot=NONE})

  (* primitive type constructors *)
    val tycElements = map mkTycElement [
	    ("bool", BT.boolTycon),
	    ("list", BT.listTycon),
	    ("ref", BT.refTycon),
	    ("unit", BT.unitTycon),
	    ("int", BT.intTycon),	(* architecture-dependent tagged int rep *)
	    ("int32", BT.int32Tycon),
	    ("int64", BT.int64Tycon),
	    ("intinf", BT.intinfTycon),
	    ("real", BT.realTycon),
(* REAL32: real32 / real64 *)
	    ("word", BT.wordTycon),	(* architecture-dependent tagged word rep *)
	    ("word8", BT.word8Tycon),
	    ("word32", BT.word32Tycon),
	    ("word64", BT.word64Tycon),
	    ("cont", BT.contTycon),
	    ("control_cont", BT.ccontTycon),
	    ("array", BT.arrayTycon),
	    ("vector", BT.vectorTycon),
	    ("object", BT.objectTycon),
	    ("c_function", BT.c_functionTycon),
	    ("c_pointer", BT.pointerTycon),	(* abstraction of machine address *)
	    ("word8vector", BT.word8vectorTycon),
	    ("word8array", BT.word8arrayTycon),
(* REAL32: real32array *)
(* TODO: real64vector *)
	    ("real64array", BT.real64arrayTycon),
	    ("spin_lock", BT.spin_lockTycon),
	    ("string", BT.stringTycon),
            ("chararray", BT.chararrayTycon),
	    ("char", BT.charTycon),
	    ("exn", BT.exnTycon),
	    ("frag", BT.fragTycon),
	    ("susp", BT.suspTycon)
	  ]

  (* primitive constructors *)
    val conElements = map mkConElement [
	    ("true", BT.trueDcon),
	    ("false", BT.falseDcon),
	    ("::", BT.consDcon),
	    ("nil", BT.nilDcon),
	    ("ref", BT.refDcon),
	    ("QUOTE", BT.QUOTEDcon),
	    ("ANTIQUOTE", BT.ANTIQUOTEDcon),
	    ("$", BT.dollarDcon)
	  ]

  (* primTypes structure *)
    val primTypes = let
          val allElements = tycElements@conElements
	  val entities = let
	      fun f ((_,M.TYCspec{entVar,info=M.RegTycSpec{spec,repl,scope}}),r) =
		    EE.bind(entVar,M.TYCent spec,r)
		| f _ = ErrorMsg.impossible "primTypes:entities"
	      in
		foldr f EE.empty tycElements
	      end
	  val entities = EntityEnv.mark(fn _ => ST.special"primEntEnv", entities)
	  val sigrec = {
		  stamp=ST.special "PrimTypesSig",
		  name=SOME(S.sigSymbol "PRIMTYPES"), closed=true,
		  fctflag=false,
		  elements=allElements,
		  typsharing=nil,strsharing=nil,
		  properties = PropList.newHolder (),
		  (* boundeps=ref (SOME []), *)
		  (* lambdaty=ref(NONE), *)
		  stub = NONE
		}
	  val strrec = {
		  sign=M.SIG sigrec,
		  rlzn={
		       stamp=ST.special "PrimTypesStr",
		       stub=NONE,
		       entities=entities,
		       properties = PropList.newHolder (),
		       (* lambdaty=ref NONE,  *)
		       rpath=IP.IPATH[S.strSymbol "primTypes"]
		     },
		  access=A.nullAcc, prim= []
		}
	  in
	    M.STR strrec
	  end (* primTypes *)


  (**************************************************************************
   *                 BUILDING A COMPLETE LIST OF PRIMOPS                    *
   **************************************************************************)

  (* We generate unique numbers for each primop, and bind them as components
  of a structure Inline, with the generic type all = (All 'a).'a. The primop
  intrinsic types will be specified in a separate table used in the translate
  phase (and FLINT?).
  *)

  (* uList structure *)
    val uList = let
	  val ev = ST.special "uListVar"
	  val allElements = [
		  (S.tycSymbol "list",
		   M.TYCspec{entVar=ev,
			     info=M.RegTycSpec{spec=BT.ulistTycon, repl=false,scope=0}}),
		   mkConElement("nil", BT.unilDcon),
		   mkConElement("::", BT.uconsDcon)
		]
	  val sigrec = {
		  stamp=ST.special "uListSig",
		  name=NONE, closed=true,
		  fctflag=false,
		  elements=allElements,
		  typsharing=nil, strsharing=nil,
		  properties = PropList.newHolder (),
		  stub = NONE
		}
	  in
	    M.STR{
		sign=M.SIG sigrec,
		rlzn={
		    stamp=ST.special "uListStr",
		    stub=NONE,
		    entities=EE.bind(ev,M.TYCent BT.ulistTycon,EE.empty),
		    properties = PropList.newHolder (),
		    rpath=IP.IPATH[S.strSymbol "uList"]
		  },
		access=A.nullAcc, prim= []
	      }
	 end

  (* Inline structure *)
    val inLine = let
          val bottom = T.POLYty{sign=[false], tyfun=T.TYFUN{arity=1, body=T.IBOUND 0}}
	  fun mkVarElement (offset, primBnd, (elems, primElems)) = let
		val s = S.varSymbol (PrimopBind.nameOf primBnd)
		val sp = M.VALspec{spec=PrimopBind.typeOf primBnd, slot=offset}
		val p = PrimopId.PrimE(PrimopId.Prim primBnd)
		in
		  ((s, sp)::elems, p::primElems)
		end
	  val (allElements, primList) = List.foldri mkVarElement ([], []) PrimopBindings.prims
	  val sigrec = {
		  stamp=ST.special "InlineSig",
		  name=NONE, closed=true,
		  fctflag=false,
		  elements=allElements,
		  typsharing=nil, strsharing=nil,
		  properties = PropList.newHolder (),  (* dbm: ??? *)
		  stub = NONE
		}
	  in
	    M.STR{
		sign = M.SIG sigrec,
		rlzn = {
		    stamp=ST.special "InlineStr",
		    stub=NONE,
		    entities=EE.empty,
		    properties = PropList.newHolder (),  (* dbm: ??? *)
		    rpath=IP.IPATH[S.strSymbol "InLine"]	(* FIXME: rename to "Inline" *)
		  },
		access = A.nullAcc,
		prim = primList
	      }
	  end

  (* priming structures: PrimTypes and Inline *)
    val nameofPT = S.strSymbol "PrimTypes"
    val nameofUL = S.strSymbol "UnrolledList"
    val nameofIL = S.strSymbol "InLine"	(* FIXME: rename to "Inline" *)

    val primEnv =
	  SE.bind(nameofIL,B.STRbind inLine,
	      SE.bind(nameofUL,B.STRbind uList,
		 SE.bind(nameofPT,B.STRbind primTypes,
		    MU.openStructure(SE.empty,primTypes))))

    val primEnv = let
	  val { hash, pickle, ... } =
		PickMod.pickleEnv (PickMod.INITIAL ModuleId.emptyTmap) primEnv
	  in
	    UnpickMod.unpickleEnv
	      (fn _ => (ModuleId.emptyTmap, fn () => "dummy"))
		(hash, pickle)
	  end

  end (* structure PrimEnv *)

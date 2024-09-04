(* to-extern.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Translation from compiler representations to the ASDL generated
 * IR used to pickle information about compilation units.
 *)

structure ToExtern : sig

    val xform : ??

  end = struct

    structure A = Access and XA = ExtAccess
    structure T = Types and XT = ExtTypes
    structure SP = SymPath and XSP = ExtSymPath
    structure IP = InvPath
    structure MI = ModuleId
    structure POI = PrimopId
    structure V = VarCon
    structure ED = EntPath.EvDict
    structure PS = PersStamps
    structure P = Primop
    structure M = Modules
    structure B = Bindings

    fun bug msg = ErrorMsg.impossible ("Extern: " ^ msg)

    val stampConverter = Stamps.newConverter ()

    fun stamp s = Stamps.Case stampConverter s {
	    fresh = ExtStamps.Fresh
	    global = ExtStamps.Global
	    special = ExtStamps.Special
	  }

    fun mkAccess { lvar, isLocalPid } = let
	  fun access (A.LVAR i) = XA.LVAR(lvar i)
	    | access (A.EXTERN pid) = XA.EXTERN pid
	    | access (A.PATH(A.EXTERN pid, i)) =
		if isLocalPid pid
		  then XA.LVAR i
		  else XA.PATH(XA.EXTERN pid, i)
	    | access (A.PATH(a, i)) = XA.PATH(access a, i)
	    | access A.NO_ACCESS = XA.NO_ACCESS
	  fun conrep A.UNTAGGED = XA.UNTAGGED
	    | conrep (A.TAGGED i) = XA.TAGGED i
	    | conrep A.TRANSPARENT = XA.TRANSPARENT
	    | conrep (A.CONSTANT i) = XA.CONSTANT i
	    | conrep A.REF = XA.REF
	    | conrep (A.EXN a) = XA.EXN(access a)
	    | conrep A.LISTCONS = XA.LISTCONS
	    | conrep A.LISTNIL = XA.LISTNIL
	    | conrep (A.SUSP NONE) = XA.SUSP NONE
	    | conrep (A.SUSP(SOME(a, b))) = XA.SUSP(SOME(access a, access b))
	  in
	    { access = access, conrep = conrep }
	  end

  end

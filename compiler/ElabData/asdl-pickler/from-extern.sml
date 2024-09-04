(* from-extern.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure FromExtern : sig

  end = struct

    structure A = Access and XA = ExtAccess
    structure T = Types and XT = ExtTypes

  (* `access lvar acc` converts from the external representation of a Access.access
   * value to the internal representation.  The first argument (`lvar`) is a function
   * for converting lvar IDs (i.e., integers) to internal lvars.
   *)
    fun access lvar = let
	  fun access' (XA.LVAR i) = A.LVAR(lvar i)
	    | access' (XA.EXTERN pid) = A.EXTERN pid
	    | access' (XA.PATH(acc, i)) = A.PATH(access' acc, i)
	    | access' XA.NO_ACCESS = A.NO_ACCESS
	  in
	    access'
	  end

    fun conrep lvar = let
	  val access = access lvar
	  fun cr XA.UNTAGGED => A.UNTAGGED
	    | cr (XA.TAGGED i) => A.TAGGED i
	    | cr XA.TRANSPARENT => A.TRANSPARENT
	    | cr (XA.CONSTANT i) => A.CONSTANT i
	    | cr XA.REF => A.REF
	    | cr (XA.EXN acc) => A.EXN (access acc)
	    | cr (XA.LISTCONS) => A.LISTCONS
	    | cr (XA.LISTNIL) => A.LISTNIL
	    | cr (XA.SUSP NONE) => A.SUSP NONE
	    | cr (XA.SUSP(SOME(a, b))) => A.SUSP(SOME(access a, access b))
	  in
	    cr
	  end

(* FIXME: should be the same type *)
    fun consig (XA.CSIG(n, m)) = A.CSIG(n, m)
      | consig XA.CNIL = A.CNIL

    fun mkEnvUnpickler {globalPid, ...} = let
	  fun stamp (Fresh lv) = Stamps.global { pid = globalPid(), cnt = lv }
	    | stamp (Global arg) = Stamps.global arg
	    | stamp (Special arg) = Stamps.special arg
	  in
	  end (* mkEnvUnpickler *)

  end

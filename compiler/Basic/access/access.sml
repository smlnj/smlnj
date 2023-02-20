(* access.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Access : ACCESS =
  struct

    structure LV = LambdaVar
    structure EM = ErrorMsg
    structure PS = PersStamps
    structure S = Symbol

    fun bug msg = EM.impossible("Bugs in Access: "^msg)

    type lvar = LV.lvar
    type persstamp = PS.persstamp

  (* How to find the dynamic value corresponding to a variable. *)
    datatype access
      = LVAR of lvar			(* defined in current compilation unit *)
      | EXTERN of persstamp		(* defined in another compilation unit *)
      | PATH of access * int
      | NO_ACCESS			(* defined in special builtin structure *)

  (* How to decide the data representations for data constructors. *)
    datatype conrep
      = UNTAGGED                        (* a pointer *)
      | TAGGED of int                   (* a pointer; 1st field is the tag *)
      | TRANSPARENT                     (* singleton dcon datatype *)
      | CONSTANT of int                 (* should be Int.int *)
      | REF
      | EXN of access
      | SUSP of (access * access) option
      | LISTCONS
      | LISTNIL

  (* See ElabData/types/basictypes.sml for examples
   *
   * FLINT/cps/switch.sml uses consig during representation analysis
   * CLAIM: for consig CSIG(m,n), the number of datacons of the datatype is m+n?
   *)
    datatype consig
      = CSIG of int * int               (* # dcon tagged, # untagged *)
      | CNIL

  (****************************************************************************
   *                    UTILITY FUNCTIONS ON ACCESS                           *
   ****************************************************************************)

  (*  shortened print name for pid *)
    fun pidShortName pid =
	let val s = PS.toHex pid
	    val n = size s
	in String.extract (s, size s - 5, NONE)
	end

  (** printing the access *)
    fun accessToString (LVAR i) = concat["LVAR(", LV.prLvar i, ")"]
      | accessToString (PATH(a,i)) = concat["PATH(", accessToString a, ",", Int.toString i, ")"]
      | accessToString (EXTERN pid) = concat["EXTERN(.", pidShortName pid, ")"]
      | accessToString (NO_ACCESS) = "NO_ACCESS"

  (** printing the conrep *)
    fun conrepToString UNTAGGED = "UT"
      | conrepToString (TAGGED i) = concat["TG(", Int.toString i, ")"]
      | conrepToString TRANSPARENT = "TN"
      | conrepToString (CONSTANT i) = concat["CN(", Int.toString i, ")"]
      | conrepToString REF = "RF"
      | conrepToString (EXN acc) = concat["EXN(" ^ accessToString acc, ")"]
      | conrepToString LISTCONS = "LC"
      | conrepToString LISTNIL = "LN"
      | conrepToString (SUSP _) = "SS"

  (** printing the data sign *)
    fun consigToString (CSIG(i,j)) = concat["B", Int.toString i, "U", Int.toString j]
      | consigToString CNIL = "CNIL"

  (** testing if a conrep is an exception or not *)
    fun isExn (EXN _) = true
      | isExn _ = false

  (** fetching a component out of a structure access *)
    fun selAcc (NO_ACCESS, _) = NO_ACCESS (* bug  "Selecting from a NO_ACCESS !" *)
      | selAcc (p, i) = PATH(p, i)

  (** duplicating an access variable *)
    fun dupAcc (v, mkv) = LVAR(mkv(LV.lvarSym(v)))

    fun namedAcc (s, mkv) = LVAR(mkv(SOME s))
    fun newAcc (mkv) = LVAR (mkv(NONE))
    fun extAcc pid  = EXTERN pid
    val nullAcc = NO_ACCESS

    fun accLvar (LVAR v) = SOME v
      | accLvar _ = NONE

  end (* structure Access *)

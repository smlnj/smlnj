(* pplty.sml
 *
 * (c) 2006 SML/NJ Fellowship
 *
 * Prettyprinting functions for PLambda types tkind, tyc, lty,
 * using the new PrettyPrint library
 *
 *)

signature PPLTY =
sig

  val fflagToString : Lty.fflag -> string
  val fmtTKind : int -> Lty.tkind -> PrettyPrint.format
  val fmtTyc : int -> Lty.tyc -> PrettyPrint.format
  val fmtLty : int -> Lty.lty -> PrettyPrint.format
  val fmtTkindEnv : int -> Lty.tkindEnv -> PrettyPrint.format

  val ppTKind : int -> Lty.tkind -> unit
  val ppTyc : int -> Lty.tyc -> unit
  val ppLty : int -> Lty.lty -> unit

  val tkindITag : Lty.tkindI -> string
  val tycITag   : Lty.tycI   -> string
  val ltyITag   : Lty.ltyI   -> string

end

structure PPLty : PPLTY =
struct

local
  structure LT = Lty
  structure PT = PrimTyc
  structure PP = PrettyPrint

  open PrettyPrint

  val lineWidth = Control.Print.lineWidth
  val say = Control.Print.say
in

(* These should be defined via Control.FLINT *)
val dtPrintNames : bool ref = FLINT_Control.printDTNames  (* default true *)
val printIND : bool ref = FLINT_Control.printINDltys (* default false *)

(* fflagToString : LT.fflag -> string *)
fun fflagToString fflag =
    (case fflag
       of LT.FF_FIXED => "f"
	| LT.FF_VAR bb =>
	    (case bb
	       of (true, true) => "rr"
		| (true, false) => "rc"
		| (false, true) => "cr"
		| (false, false) => "cc" ))

(* teBinderTag : LT.teBinder -> string *)
fun teBinderTag (LT.Lamb _) = "<teBinder.Lamb>"
  | teBinderTag (LT.Beta _) = "<teBinder.Beta>"

(* tkindITag : LT.tkindI -> string *)
fun tkindITag LT.TK_MONO    = "<tkindI.TK_MONO>"
  | tkindITag LT.TK_BOX     = "<tkindI.TK_BOX>"
  | tkindITag (LT.TK_SEQ _) = "<tkindI.TK_SEQ>"
  | tkindITag (LT.TK_FUN _) = "<tkindI.TK_FUN>"

(* tycITag : LT.tycI -> string *)
fun tycITag (LT.TC_DVAR _) = "<tycI.TC_DVAR_>"
  | tycITag (LT.TC_NVAR _) = "<tycI.TC_NVAR>"
  | tycITag (LT.TC_PRIM _) = "<tycI.TC_PRIM>"
  | tycITag (LT.TC_FN _)   = "<tycI.TC_FN>"
  | tycITag (LT.TC_APP _)  = "<tycI.TC_APP>"
  | tycITag (LT.TC_SEQ _)  = "<tycI.TC_SEQ>"
  | tycITag (LT.TC_PROJ _) = "<tycI.TC_PROJ>"
  | tycITag (LT.TC_SUM _)  = "<tycI.TC_SUM>"
  | tycITag (LT.TC_FIX _)  = "<tycI.TC_FIX>"
  | tycITag (LT.TC_TUPLE _)  = "<tycI.TC_TUPLE>"
  | tycITag (LT.TC_ARROW _)  = "<tycI.TC_ARROW>"
  | tycITag (LT.TC_PARROW _) = "<tycI.TC_PARROW>"
  | tycITag (LT.TC_BOX _)  = "<tycI.TC_BOX>"
  | tycITag (LT.TC_WRAP _) = "<tycI.TC_WRAP>"
  | tycITag (LT.TC_CONT _) = "<tycI.TC_CONT>"
  | tycITag (LT.TC_IND _)  = "<tycI.TC_IND>"
  | tycITag (LT.TC_ENV _)  = "<tycI.TC_ENV>"

(* ltyITag : LT.ltyI -> string *)
fun ltyITag (LT.LT_TYC _)  = "<ltyI.LT_TYC>"
  | ltyITag (LT.LT_STR _)  = "<ltyI.LT_STR>"
  | ltyITag (LT.LT_FCT _)  = "<ltyI.LT_FCT>"
  | ltyITag (LT.LT_POLY _) = "<ltyI.LT_POLY>"
  | ltyITag (LT.LT_CONT _) = "<ltyI.LT_CONT>"
  | ltyITag (LT.LT_IND _)  = "<ltyI.LT_IND>"
  | ltyITag (LT.LT_ENV _)  = "<ltyI.LT_ENV>"


(* formatting utilities *)

(* fmtSeq : format list -> format *)
(* packed, comma separated formatting of a format list -- no enclosing brackets or parentheses *)
val fmtSeq = psequence comma

(* fmtTKind : int -> tkind -> format
 * Format a hashconsed representation of the kind *)
fun fmtTKind pd (tk : LT.tkind) =
    if pd < 1 then text (tkindITag (LT.tk_out tk)) else
    let val fmtTKind' = fmtTKind (pd-1)
	fun fmtTKindI(LT.TK_MONO) = text "M"
	  | fmtTKindI(LT.TK_BOX) = text "B"
	  | fmtTKindI(LT.TK_FUN (argTkinds, resTkind)) =
	      (* resTkind may be a TK_SEQ wrapping some tkinds
	       * These are produced by Elaborate/modules/instantiate.sml
	       *)
	     parens (hcat [list (map fmtTKind' argTkinds), text "=>", fmtTKind' resTkind])
	  | fmtTKindI(LT.TK_SEQ tkinds) =
	     pcat [text "KSEQ", list (map (fmtTKind (pd-1)) tkinds)]
     in fmtTKindI (LT.tk_out tk)
    end (* fmtTKind *)

fun tycEnvFlatten tycenv =
    (case LT.teDest(tycenv)
       of NONE => []
        | SOME (elem, rest) => elem::tycEnvFlatten(rest))

(* fmtKeFrame : int -> Lty.tkind list -> PP.format *)
fun fmtKeFrame pd ks = list (map (fmtTKind pd) ks)

(* fmtKindEnv : int -> Lty.tkindEnv -> PP.format
 * used (once) in FLINT/kernel/ltykindchk.sml *)
fun fmtTkindEnv pd kenv =
    if pd < 1
    then text "<tkenv>"
    else list (map (fmtKeFrame (pd-1)) kenv)

(* fmtTEBinder : int -> LT.teBinder -> format *)
fun fmtTEBinder pd (binder: LT.teBinder) =
    if pd < 1 then text (teBinderTag binder) else
    (case binder
      of LT.Lamb (level, ks) =>
	 pcat [PP.ccat [text "L", integer level, colon], fmtKeFrame (pd-1)  ks]
       | LT.Beta (level, args, ks) =>
	 pcat [PP.ccat [text "B", integer level],
		 parens (fmtSeq [tuple (map (fmtTyc (pd-1)) args), fmtKeFrame (pd-1) ks])])
     (* function fmtTEBinder *)

(* fmtTyc : int -> LT.tyc -> format *)
and fmtTyc pd (tyc : LT.tyc) =
    (* FLINT variables are represented using deBruijn indices *)
    if pd < 1 then text (tycITag (LT.tc_out tyc)) else
    let (* partially applied versions of functions *)
	val fmtTKind' = fmtTKind (pd-1)
	val fmtTyc' = fmtTyc (pd-1)

	fun fmtTycI (LT.TC_DVAR(index, cnt)) =
            (* deBruijn type variable:
	     * -- index is a deBruijn index set in elabmod.sml/instantiate.sml
	     * -- cnt is computed in instantiate.sml sigToInst or
	          alternatively may be the IBOUND index in a polytype body ??? *)
	    label "DTV:" (fmtSeq [integer index, integer cnt])
	  | fmtTycI (LT.TC_NVAR tvar) =
            (* Named type variable; tvar = lvar [= int] *)
	    label "NTV:" (text (LambdaVar.prLvar tvar))
	  | fmtTycI (LT.TC_PRIM primtyc) =
	    label "PRIM:" (text (PT.pt_print primtyc))
	  | fmtTycI (LT.TC_FN (argTkinds, resultTyc)) =
	    label "TCFN:" (pcat [list (map fmtTKind' argTkinds), fmtTyc' resultTyc])
	  | fmtTycI (LT.TC_APP (contyc, tys)) =
	    label "TCAP:" (tryFlat (fmtSeq [fmtTyc' contyc, list (map fmtTyc' tys)]))
	  | fmtTycI (LT.TC_SEQ tycs) =
	    label "SEQ:" (psequence comma (map fmtTyc' tycs))
	  | fmtTycI (LT.TC_PROJ (tyc', index)) =
	    label "PROJ:" (fmtSeq [fmtTyc' tyc', integer index])
	  | fmtTycI (LT.TC_SUM tycs) =
	    label "SUM" (psequence comma (map fmtTyc' tycs))
	    (* TC_FIX is a recursive datatype constructor
	       from a (mutually-)recursive family *)
	  | fmtTycI (LT.TC_FIX {family={size,names,gen,params},index}) =
            if !dtPrintNames
	    then text (Vector.sub (names,index))
            else label "FIX:"
		   (hvcat
		     [hvcat
		       [label "size:" (integer size),
			label "index:" (integer index),
			label "gen:" (fmtTyc' gen)],
		      label "prms:" (list (map fmtTyc' params))])
	  | fmtTycI (LT.TC_BOX tyc) =
	      label "BOX:" (fmtTyc' tyc)
	  | fmtTycI (LT.TC_TUPLE tycs) =
	      PP.braces (PP.psequence PP.comma (map fmtTyc' tycs))
	  | fmtTycI (LT.TC_ARROW (fflag, argTycs, resTycs)) =  (* was "AR ..." *)
	    (* fflag records the calling convention: either FF_FIXED or FF_VAR *)
	      parens
		(pcat
		   [hcat [list (map fmtTyc' argTycs), text "->", text (fflagToString fflag)],
		    indent 2 (list (map fmtTyc' resTycs))])
	    (* According to ltykernel.sml comment, this arrow tyc is not used *)
	  | fmtTycI (LT.TC_PARROW (argTyc, resTyc)) =  (* was "PAR ..." *)
	      parens
		(pcat
	           [hcat [lparen, (fmtTyc' argTyc), text "-->"],
		    indent 2 (fmtTyc' resTyc)])
	  | fmtTycI (LT.TC_WRAP tyc) =
	      label "WRAP:" (fmtTyc' tyc)
	  | fmtTycI (LT.TC_CONT tycs) =
	      label "CONT:" (tuple (map fmtTyc' tycs))
	  | fmtTycI (LT.TC_IND (tyc, tycI)) =
              if !printIND
              then label "IND" (pcat [fmtTyc' tyc, comma, fmtTycI tycI])
              else fmtTyc' tyc
	  | fmtTycI (LT.TC_ENV (tyc, ol, nl, tenv)) =  (* (over) simplified Nadathur closure *)
	      label "ENV:" (fmtTyc' tyc)

     in fmtTycI (LT.tc_out tyc)
    end (* fmtTyc *)


fun fmtTycEnv pd (tycEnv : LT.tycEnv) =
    if pd < 1 then text "<tycEnv>" else
    label "TycEnv:" (list (map (fmtTEBinder (pd-1)) (tycEnvFlatten tycEnv)))
    (* fmtTycEnv *)


fun fmtLty pd (lty: LT.lty) =
    if pd < 1 then text (ltyITag (LT.lt_out lty)) else
    let val fmtTKind' = fmtTKind (pd-1)
	val fmtLty' = fmtLty (pd-1)

        fun fmtLtyI (LT.LT_TYC tc) =
              label "TYC:" (fmtTyc pd tc)
          | fmtLtyI (LT.LT_STR ltys) =
              label "STR:" (list (map fmtLty' ltys))
          | fmtLtyI (LT.LT_FCT (args,res)) =
	      label "FCT:" (fmtSeq [list (map fmtLty' args), list (map fmtLty' res)])
          | fmtLtyI (LT.LT_POLY (ks,ltys)) =
	      label "POL:" (fmtSeq [list (map fmtTKind' ks), list (map fmtLty' ltys)])
          | fmtLtyI (LT.LT_CONT ltys) =
	      label "CONT:" (list (map fmtLty' ltys))
          | fmtLtyI (LT.LT_IND(nt,ot)) =
              if !printIND
	      then label "IND" (fmtSeq [fmtLty' nt, fmtLtyI ot])
              else fmtLty pd nt
	  | fmtLtyI (LT.LT_ENV (lty, ol, nl, tenv)) =
	      label "LT_ENV:"
	        (fmtSeq [ccat [text "ol=", integer ol],
		         ccat [text "nl=", integer nl],
			 fmtLty' lty,
			 list (map (fmtTEBinder (pd-1)) (tycEnvFlatten tenv))])

     in fmtLtyI (LT.lt_out lty)
    end (* fmtLty *)

fun ppTKind pdepth (tkind: Lty.tkind) =
    render (fmtTKind pdepth tkind, say, !lineWidth)

fun ppTyc pdepth (tyc: Lty.tyc) =
    render (fmtTyc pdepth tyc, say, !lineWidth)

fun ppLty pdepth (lty: Lty.lty) =
    render (fmtLty pdepth lty, say, !lineWidth)

end (* top local *)
end (* structure PPLty *)

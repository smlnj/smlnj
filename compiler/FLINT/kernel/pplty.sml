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
  val fmtTKind : int -> Lty.tkind -> Formatting.format
  val fmtTyc : int -> Lty.tyc -> Formatting.format
  val fmtLty : int -> Lty.lty -> Formatting.format
  val fmtTkindEnv : int -> Lty.tkindEnv -> Formatting.format

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
  structure PP = Formatting
  structure PF = PrintFormat

  val lineWidth = PrintControl.lineWidth
  val say = PrintControl.say
in

(* These should be defined via Control.FLINT *)
val dtPrintNames : bool ref = FlintControl.printDTNames  (* default true *)
val printIND : bool ref = FlintControl.printINDltys (* default false *)

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
val fmtSeq = PP.psequence PP.comma

(* fmtTKind : int -> tkind -> format
 * Format a hashconsed representation of the kind *)
fun fmtTKind pd (tk : LT.tkind) =
    if pd < 1 then PP.text (tkindITag (LT.tk_out tk)) else
    let val fmtTKind' = fmtTKind (pd-1)
	fun fmtTKindI(LT.TK_MONO) = PP.text "M"
	  | fmtTKindI(LT.TK_BOX) = PP.text "B"
	  | fmtTKindI(LT.TK_FUN (argTkinds, resTkind)) =
	      (* resTkind may be a TK_SEQ wrapping some tkinds
	       * These are produced by Elaborate/modules/instantiate.sml
	       *)
	     PP.parens (PP.hblock [PP.list (map fmtTKind' argTkinds), PP.text "=>", fmtTKind' resTkind])
	  | fmtTKindI(LT.TK_SEQ tkinds) =
	     PP.pblock [PP.text "KSEQ", PP.list (map (fmtTKind (pd-1)) tkinds)]
     in fmtTKindI (LT.tk_out tk)
    end (* fmtTKind *)

fun tycEnvFlatten tycenv =
    (case LT.teDest(tycenv)
       of NONE => []
        | SOME (elem, rest) => elem::tycEnvFlatten(rest))

(* fmtKeFrame : int -> Lty.tkind list -> PP.format *)
fun fmtKeFrame pd ks = PP.list (map (fmtTKind pd) ks)

(* fmtKindEnv : int -> Lty.tkindEnv -> PP.format
 * used (once) in FLINT/kernel/ltykindchk.sml *)
fun fmtTkindEnv pd kenv =
    if pd < 1
    then PP.text "<tkenv>"
    else PP.list (map (fmtKeFrame (pd-1)) kenv)

(* fmtTEBinder : int -> LT.teBinder -> format *)
fun fmtTEBinder pd (binder: LT.teBinder) =
    if pd < 1 then PP.text (teBinderTag binder) else
    (case binder
      of LT.Lamb (level, ks) =>
	 PP.pblock [PP.cblock [PP.text "L", PP.integer level, PP.colon], fmtKeFrame (pd-1)  ks]
       | LT.Beta (level, args, ks) =>
	 PP.pblock [PP.cblock [PP.text "B", PP.integer level],
		 PP.parens (fmtSeq [PP.tuple (map (fmtTyc (pd-1)) args), fmtKeFrame (pd-1) ks])])
     (* function fmtTEBinder *)

(* fmtTyc : int -> LT.tyc -> format *)
and fmtTyc pd (tyc : LT.tyc) =
    (* FLINT variables are represented using deBruijn indices *)
    if pd < 1 then PP.text (tycITag (LT.tc_out tyc)) else
    let (* partially applied versions of functions *)
	val fmtTKind' = fmtTKind (pd-1)
	val fmtTyc' = fmtTyc (pd-1)

	fun fmtTycI (LT.TC_DVAR(index, cnt)) =
            (* deBruijn type variable:
	     * -- index is a deBruijn index set in elabmod.sml/instantiate.sml
	     * -- cnt is computed in instantiate.sml sigToInst or
	          alternatively may be the IBOUND index in a polytype body ??? *)
	    PP.label "DTV:" (fmtSeq [PP.integer index, PP.integer cnt])
	  | fmtTycI (LT.TC_NVAR tvar) =
            (* Named type variable; tvar = lvar [= int] *)
	    PP.label "NTV:" (PP.text (LambdaVar.prLvar tvar))
	  | fmtTycI (LT.TC_PRIM primtyc) =
	    PP.label "PRIM:" (PP.text (PT.pt_print primtyc))
	  | fmtTycI (LT.TC_FN (argTkinds, resultTyc)) =
	    PP.label "TCFN:" (PP.pblock [PP.list (map fmtTKind' argTkinds), fmtTyc' resultTyc])
	  | fmtTycI (LT.TC_APP (contyc, tys)) =
	    PP.label "TCAP:" (PP.tryFlat (fmtSeq [fmtTyc' contyc, PP.list (map fmtTyc' tys)]))
	  | fmtTycI (LT.TC_SEQ tycs) =
	    PP.label "SEQ:" (PP.psequence PP.comma (map fmtTyc' tycs))
	  | fmtTycI (LT.TC_PROJ (tyc', index)) =
	    PP.label "PROJ:" (PP.psequence PP.comma [fmtTyc' tyc', PP.integer index])
	  | fmtTycI (LT.TC_SUM tycs) =
	    PP.label "SUM" (PP.psequence PP.comma (map fmtTyc' tycs))
	    (* TC_FIX is a recursive datatype constructor
	       from a (mutually-)recursive family *)
	  | fmtTycI (LT.TC_FIX {family={size,names,gen,params},index}) =
            if !dtPrintNames
	    then PP.text (Vector.sub (names,index))
            else PP.label "FIX:"
		   (PP.hvblock
		     [PP.hvblock
		       [PP.label "size:" (PP.integer size),
			PP.label "index:" (PP.integer index),
			PP.label "gen:" (fmtTyc' gen)],
		      PP.label "prms:" (PP.list (map fmtTyc' params))])
	  | fmtTycI (LT.TC_BOX tyc) =
	      PP.label "BOX:" (fmtTyc' tyc)
	  | fmtTycI (LT.TC_TUPLE tycs) =
	      PP.braces (PP.psequence PP.comma (map fmtTyc' tycs))
	  | fmtTycI (LT.TC_ARROW (fflag, argTycs, resTycs)) =  (* was "AR ..." *)
	    (* fflag records the calling convention: either FF_FIXED or FF_VAR *)
	      PP.parens
		(PP.pblock
		   [PP.hblock [PP.list (map fmtTyc' argTycs), PP.text "->", PP.text (fflagToString fflag)],
		    PP.indent 2 (PP.list (map fmtTyc' resTycs))])
	    (* According to ltykernel.sml comment, this arrow tyc is not used *)
	  | fmtTycI (LT.TC_PARROW (argTyc, resTyc)) =  (* was "PAR ..." *)
	      PP.parens
		(PP.pblock
	           [PP.hblock [fmtTyc' argTyc, PP.text "-->"],
		    PP.indent 2 (fmtTyc' resTyc)])
	  | fmtTycI (LT.TC_WRAP tyc) =
	      PP.label "WRAP:" (fmtTyc' tyc)
	  | fmtTycI (LT.TC_CONT tycs) =
	      PP.label "CONT:" (PP.tuple (map fmtTyc' tycs))
	  | fmtTycI (LT.TC_IND (tyc, tycI)) =
              if !printIND
              then PP.label "IND:" (PP.pblock [fmtTyc' tyc, PP.comma, fmtTycI tycI])
              else fmtTyc' tyc
	  | fmtTycI (LT.TC_ENV (tyc, ol, nl, tenv)) =  (* (over) simplified Nadathur closure *)
	      PP.label "ENV:" (fmtTyc' tyc)

     in fmtTycI (LT.tc_out tyc)
    end (* fmtTyc *)


fun fmtTycEnv pd (tycEnv : LT.tycEnv) =
    if pd < 1 then PP.text "<tycEnv>" else
    PP.label "TycEnv:" (PP.list (map (fmtTEBinder (pd-1)) (tycEnvFlatten tycEnv)))
    (* fmtTycEnv *)


fun fmtLty pd (lty: LT.lty) =
    if pd < 1 then PP.text (ltyITag (LT.lt_out lty)) else
    let val fmtTKind' = fmtTKind (pd-1)
	val fmtLty' = fmtLty (pd-1)

        fun fmtLtyI (LT.LT_TYC tc) =
              PP.label "TYC:" (fmtTyc pd tc)
          | fmtLtyI (LT.LT_STR ltys) =
              PP.label "STR:" (PP.list (map fmtLty' ltys))
          | fmtLtyI (LT.LT_FCT (args,res)) =
	      PP.label "FCT:" (fmtSeq [PP.list (map fmtLty' args), PP.list (map fmtLty' res)])
          | fmtLtyI (LT.LT_POLY (ks,ltys)) =
	      PP.label "POL:" (fmtSeq [PP.list (map fmtTKind' ks), PP.list (map fmtLty' ltys)])
          | fmtLtyI (LT.LT_CONT ltys) =
	      PP.label "CONT:" (PP.list (map fmtLty' ltys))
          | fmtLtyI (LT.LT_IND(nt,ot)) =
              if !printIND
	      then PP.label "IND:" (fmtSeq [fmtLty' nt, fmtLtyI ot])
              else fmtLty pd nt
	  | fmtLtyI (LT.LT_ENV (lty, ol, nl, tenv)) =
	      PP.label "LT_ENV:"
	        (fmtSeq [PP.cblock [PP.text "ol=", PP.integer ol],
		         PP.cblock [PP.text "nl=", PP.integer nl],
			 fmtLty' lty,
			 PP.list (map (fmtTEBinder (pd-1)) (tycEnvFlatten tenv))])

     in fmtLtyI (LT.lt_out lty)
    end (* fmtLty *)

fun ppTKind pdepth (tkind: Lty.tkind) =
    PF.render (fmtTKind pdepth tkind, !lineWidth)

fun ppTyc pdepth (tyc: Lty.tyc) =
    PF.render (fmtTyc pdepth tyc, !lineWidth)

fun ppLty pdepth (lty: Lty.lty) =
    PF.render (fmtLty pdepth lty, !lineWidth)

end (* top local *)
end (* structure PPLty *)

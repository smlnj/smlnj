(* pplty.sml
 *
 * (c) 2006 SML/NJ Fellowship
 *
 * Pretty Printer for PLambda types using the new SMLNJ-lib new pretty printer
 *
 *)

signature PPLTY =
sig
  (* printing flags *)
  val dtPrintNames : bool ref
  val printIND : bool ref

  val ppList : PrettyPrint.stream ->
               {sep: string, pp : PrettyPrint.stream -> 'a -> unit} ->
               'a list -> unit
  val ppFflag : PrettyPrint.stream -> Lty.fflag -> unit
  val ppTKind : int -> PrettyPrint.stream -> Lty.tkind -> unit
  val ppTyc : int -> PrettyPrint.stream -> Lty.tyc -> unit
  val ppLty : int -> PrettyPrint.stream -> Lty.lty -> unit
end

structure PPLty (* : PPLTY *) =
struct

local
  structure LT = Lty
  structure PT = PrimTyc
  structure PP = PrettyPrint
  open PPUtil
in

val dtPrintNames : bool ref = ref true
val printIND : bool ref = ref false

fun ppSeq ppstrm {sep: string, pp : PP.stream -> 'a -> unit} (list: 'a list) =
    ppSequence ppstrm
      {sep = fn ppstrm => (PP.string ppstrm sep;
			   PP.break ppstrm {nsp=1, offset=0}),
       style = INCONSISTENT,
       pr = pp}
      list

fun ppList ppstrm {sep: string, pp : PP.stream -> 'a -> unit} (list: 'a list) =
    ppClosedSequence ppstrm
      {front = fn ppstrm => (PP.string ppstrm "["),
       back = fn ppstrm => (PP.string ppstrm "]"),
       sep = PPUtil.sepWithCut sep,
       style = INCONSISTENT,
       pr = pp}
      list

(* ppFflag : PP.stream -> Lty.fflag -> unit *)
fun ppFflag ppstrm fflag =
    let val fflagString =
	    (case fflag
	      of LT.FF_FIXED => "[f]"
	       | LT.FF_VAR bb =>
		   (case bb
		     of (true, true) => "[rr]"
		      | (true, false) => "[rc]"
		      | (false, true) => "[cr]"
		      | (false, false) => "[cc]" ))
     in PP.string ppstrm fflagString
    end

(* ppTKind : tkind -> unit
 * Print a hashconsed representation of the kind *)
fun ppTKind pd ppstrm (tk : LT.tkind) =
    if pd < 1 then pps ppstrm "<tk>" else
    let val {openHOVBox, closeBox, pps, ...} = en_pp ppstrm
        val ppTKind' = ppTKind (pd-1) ppstrm
	val ppList' = ppList ppstrm
	fun ppTKindI(LT.TK_MONO) = pps "M"
	  | ppTKindI(LT.TK_BOX) = pps "B"
	  | ppTKindI(LT.TK_FUN (argTkinds, resTkind)) =
	      (* resTkind may be a TK_SEQ wrapping some tkinds
	       * These are produced by Elaborate/modules/instantiate.sml
	       *)
	     (openHOVBox 1;
	       pps "(";
	       ppList' {sep=",", pp=ppTKind (pd-1)} argTkinds;
	       pps "=>"; ppTKind' resTkind;
	       pps ")";
	      closeBox())
	  | ppTKindI(LT.TK_SEQ tkinds) =
	     (openHOVBox 1;
	       pps "KSEQ";
	       ppList' {sep=",", pp=ppTKind (pd-1)} tkinds;
	      closeBox())
     in ppTKindI (LT.tk_out tk)
    end (* ppTKind *)

fun tycEnvFlatten(tycenv) =
    (case LT.teDest(tycenv)
       of NONE => []
        | SOME(elem, rest) => elem::tycEnvFlatten(rest))

fun ppKeFrame pd ppstrm ks =
    ppList ppstrm {sep=",", pp=ppTKind pd} ks

fun ppKindEnv pd ppstrm kenv =
    if pd < 1 then pps ppstrm "<tkenv>" else
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
     in openHOVBox 1;
        ppList ppstrm {sep=",",pp=ppKeFrame (pd-1)} kenv;
        closeBox ()
    end

fun ppTEBinder pd ppstrm (binder: LT.teBinder) =
    if pd < 1 then pps ppstrm "<teBinder>" else
    let val {openHOVBox, closeBox, pps, ppi, ...} = en_pp ppstrm
    in openHOVBox 1;
       (case binder
         of LT.Lamb (level, ks) =>
            (pps "L"; ppi level;
             pps ": ";
             ppKeFrame (pd-1) ppstrm ks)
          | LT.Beta (level, args, ks) =>
            (pps "B"; ppi level; pps "(";
             ppList ppstrm {sep=",", pp=ppTyc (pd-1)} args;
             pps ": ";
             ppKeFrame (pd-1) ppstrm ks;
             pps ")"));
       closeBox()
    end (* function ppTEBinder *)

and ppTyc pd ppstrm (tycon : LT.tyc) =
    (* FLINT variables are represented using deBruijn indices *)
    if pd < 1 then pps ppstrm "<tyc>" else
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, break, ...} =
            en_pp ppstrm

        (* partially applied versions of functions *)
	val ppList' : {pp: PP.stream -> 'a -> unit, sep: string}
                      -> 'a list -> unit =
              fn x => ppList ppstrm x
            (* eta-expand (ppList ppstrm) to avoid value restriction *)

	val ppTKind' = ppTKind (pd-1) ppstrm
	val ppTyc' = ppTyc (pd-1) ppstrm

	fun ppTycI (LT.TC_VAR(depth, cnt)) =
            (* deBruijn type variable *)
	    (pps "DTV(";
	     (* depth is a deBruijn index set in elabmod.sml/instantiate.sml *)
	     pps (DebIndex.di_print depth);
	     pps ",";
	     (* cnt is computed in instantiate.sml sigToInst or
	        alternatively may be the IBOUND index in a polytype body ??? *)
	     ppi cnt;
	     pps ")")
	  | ppTycI (LT.TC_NVAR tvar) =
            (* Named type variable; tvar = lvar [= int] *)
	    (pps "NTV(v"; pps(LambdaVar.prLvar tvar); pps ")")
	  | ppTycI (LT.TC_PRIM primtycon) =
	    (pps "PRIM(";
	     pps (PT.pt_print primtycon);
	     pps ")")
	  | ppTycI (LT.TC_FN (argTkinds, resultTyc)) =
	    (openHOVBox 1;
	     pps "TCFN(";
	     ppList' {sep=",", pp=ppTKind (pd-1)} argTkinds;
	     pps ",";
	     break {nsp=1,offset=0};
	     ppTyc' resultTyc;
	     pps ")";
	     closeBox())
	  | ppTycI (LT.TC_APP(contyc, tys)) =
	    (openHOVBox 0;
	     pps "TCAP(";
             PP.openHVBox ppstrm (PP.Rel 0);
	     ppTyc' contyc;
	     pps ","; break {nsp=1,offset=0};
	     ppList' {sep=",", pp=ppTyc (pd-1)} tys;
	     pps ")";
             closeBox();
	     closeBox())
	  | ppTycI (LT.TC_SEQ tycs) =
	    (openHOVBox 1;
	     pps "SEQ(";
	     ppList' {sep=",", pp=ppTyc (pd-1)} tycs;
	     pps ")";
	     closeBox())
	  | ppTycI (LT.TC_PROJ(tycon, index)) =
	    (openHOVBox 1;
	     pps "PROJ(";
	     ppTyc' tycon;
	     pps ",";
	     break {nsp=1,offset=0};
	     pps (Int.toString index);
	     pps ")";
	     closeBox())
	  | ppTycI (LT.TC_SUM(tycs)) =
	    (pps "SUM(";
	     ppList' {sep=",", pp=ppTyc (pd-1)} tycs;
	     pps ")")
	    (* TC_FIX is a recursive datatype constructor
	       from a (mutually-)recursive family *)
	  | ppTycI (LT.TC_FIX{family={size,names,gen,params},index}) =
            if !dtPrintNames then pps (Vector.sub(names,index))
            else
	    (openHOVBox 0;
              pps "FIX(";
              openHVBox 0;
               pps "size = "; ppi size; break {nsp=1,offset=0};
               pps "index = "; ppi index; break {nsp=1,offset=0};
               pps "gen = ";
               openHOVBox 2;
                ppTyc' gen;
               closeBox ();
               break {nsp=1,offset=0};
               pps "prms = ";
               openHOVBox 2;
                ppList' {sep = ",", pp = ppTyc (pd-1)} params;
               closeBox ();
               pps ")";
              closeBox();
	     closeBox())
	  | ppTycI (LT.TC_BOX tyc) =
	    (pps "BOX(";
	     ppTyc' tyc;
	     pps ")")
	  | ppTycI (LT.TC_TUPLE tycs) =
	    (ppClosedSequence ppstrm
                {front = (fn s => PP.string s "{"),
                 sep = PPUtil.sepWithCut ",",
                 back = (fn s => PP.string s "}"),
                 pr = ppTyc (pd-1),
                 style = INCONSISTENT}
	        tycs)
	  | ppTycI (LT.TC_ARROW (fflag, argTycs, resTycs)) =
	    (* fflag records the calling convention: either FF_FIXED or FF_VAR *)
	    (pps "AR"; ppFflag ppstrm fflag; pps "(";
             openHOVBox 0;
	     ppList' {sep=",", pp=ppTyc (pd-1)} argTycs;
	     pps ",";
	     break {nsp=1,offset=0};
	     ppList' {sep=",", pp=ppTyc (pd-1)} resTycs;
             closeBox ();
	     pps ")")
	    (* According to ltykernel.sml comment, this arrow tyc is not used *)
	  | ppTycI (LT.TC_PARROW (argTyc, resTyc)) =
	    (pps "PAR(";
	     ppTyc' argTyc;
	     pps ",";
	     break {nsp=1,offset=0};
	     ppTyc' resTyc;
	     pps ")")
	  | ppTycI (LT.TC_WRAP tyc) =
	    (pps "TC_WRAP(";
	     ppTyc' tyc;
	     pps ")")
	  | ppTycI (LT.TC_CONT tycs) =
	    (pps "CONT(";
	     ppList' {sep=", ", pp=ppTyc (pd-1)} tycs;
	     pps ")")
	  | ppTycI (LT.TC_IND (tyc, tycI)) =
            if !printIND then
              (openHOVBox 1;
               pps "IND(";
               openHOVBox 0;
               ppTyc' tyc;
               pps ",";
               break {nsp=1,offset=0};
               ppTycI tycI;
               closeBox();
               pps ")";
               closeBox())
            else ppTyc' tyc
	  | ppTycI (LT.TC_ENV (tyc, ol, nl, tenv)) =
	    (openHVBox 1;
	     pps "ENV(";
	     pps "ol=";
	     pps (Int.toString ol);
	     pps ", ";
	     pps "nl=";
	     pps (Int.toString nl);
	     pps ",";
	     break {nsp=1,offset=0};
	     ppTyc' tyc;
	     pps ",";
	     break {nsp=1,offset=0};
	     ppList' {sep=",", pp=ppTEBinder (pd-1)} (tycEnvFlatten tenv);
             pps ")";
	     closeBox())
    in ppTycI (LT.tc_out tycon)
    end (* ppTyc *)


fun ppTycEnv pd ppstrm (tycEnv : LT.tycEnv) =
    if pd < 1 then pps ppstrm "<tycEnv>" else
    let val {openHOVBox, closeBox, pps, ...} = en_pp ppstrm
     in openHOVBox 1;
	 pps "TycEnv(";
	 ppList ppstrm {sep=", ", pp=ppTEBinder (pd-1)} (tycEnvFlatten tycEnv);
	 pps ")";
	closeBox()
    end (* ppTycEnv *)


fun ppLty pd ppstrm (lty: LT.lty) =
    if pd < 1 then pps ppstrm "<tyc>" else
    let val {openHOVBox, openHVBox, openVBox, closeBox, pps, ppi, break, newline} =
            en_pp ppstrm
	val ppList' : {pp:PP.stream -> 'a -> unit, sep: string} -> 'a list -> unit =
              fn x => ppList ppstrm x
	       (* eta-expansion of ppList to avoid value restriction *)

	val ppTKind' = ppTKind (pd-1) ppstrm
	val ppLty' = ppLty (pd-1) ppstrm

        fun ppLtyI (LT.LT_TYC tc) =
            (pps "TYC("; ppTyc pd ppstrm tc; pps ")")
          | ppLtyI (LT.LT_STR ltys) =
            (pps "STR("; ppList' {sep=",",pp=ppLty (pd-1)} ltys; pps ")")
          | ppLtyI (LT.LT_FCT (args,res)) =
            (pps "FCT("; ppList' {sep=",",pp=ppLty (pd-1)} args; pps ",";
             break {nsp=1,offset=0};
             ppList' {sep=",",pp=ppLty (pd-1)} res; pps ")")
          | ppLtyI (LT.LT_POLY (ks,ltys)) =
	    (openHOVBox 1;
	     pps "POL(";
	     ppList' {sep=",", pp=ppTKind (pd-1)} ks;
	     pps ",";
	     break {nsp=1,offset=0};
	     ppList' {sep=",",pp=ppLty (pd-1)} ltys;
	     pps ")";
	     closeBox())
          | ppLtyI (LT.LT_CONT ltys) =
            (pps "CONT("; ppList' {sep=",",pp=ppLty (pd-1)} ltys; pps ")")
          | ppLtyI (LT.LT_IND(nt,ot)) =
            if !printIND then
              (pps "IND(";
               openHOVBox 0;
               ppLty' nt; pps ",";
               break {nsp=1,offset=0};
               ppLtyI ot;
               closeBox();
               pps ")")
            else ppLty pd ppstrm nt
	  | ppLtyI (LT.LT_ENV (lty, ol, nl, tenv)) =
	    (openHVBox 1;
	     pps "LT_ENV(";
	     pps "ol=";
	     pps (Int.toString ol);
	     pps ", ";
	     pps "nl=";
	     pps (Int.toString nl);
	     pps ",";
	     break {nsp=1,offset=0};
	     ppLty' lty;
	     pps ",";
	     break {nsp=1,offset=0};
	     ppList' {sep=",", pp=ppTEBinder (pd-1)} (tycEnvFlatten tenv);
             pps ")";
	     closeBox())
    in ppLtyI (LT.lt_out lty)
    end (* ppLty *)

end (* top local *)
end (* structure PPLty *)

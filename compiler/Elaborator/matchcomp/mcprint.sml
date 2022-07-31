(* mcprint.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* pretty printing for (revised old) match compiler (MC) internal structures *)

structure MCPrint =
struct

local
   structure PP = PrettyPrint
   structure PU = PPUtil
   structure LV = LambdaVar
   structure V = Variable
   structure AS = Absyn
   structure AU = AbsynUtil
   structure P = Paths
   structure MU = MCUtil
   open MCCommon

   open PP PPUtil
in

fun bug msg = ErrorMsg.impossible ("MCPrint: " ^ msg)

(* debugMsg : bool ref -> string -> unit *)
fun debugMsg flag (msg: string) =
    if !flag
    then with_default_pp
	  (fn ppstrm =>
	    (openHVBox ppstrm (PP.Rel 0);
	     PP.string ppstrm msg;
	     closeBox ppstrm;
	     newline ppstrm;
	     PP.flushStream ppstrm))
    else ()

(* debugPrint : bool ref -> string * (PP.stream -> 'a -> unit) * 'a -> unit *)
fun debugPrint flag (msg: string, printfn: PP.stream -> 'a -> unit, subject: 'a) =
    if !flag
    then with_default_pp
	  (fn ppstrm =>
	    (openHVBox ppstrm (PP.Rel 0);
	     PP.string ppstrm msg;
	     newline ppstrm;
	     PP.nbSpace ppstrm 2;
	     openHVBox ppstrm (PP.Rel 0);
	     printfn ppstrm subject;
	     closeBox ppstrm;
	     newline ppstrm;
	     closeBox ppstrm;
	     PP.flushStream ppstrm))
    else ()

fun ppCon ppstrm (con : AS.con) : unit =
    PP.string ppstrm (AU.conToString con)

fun ppPath ppstrm (path: P.path) =
    PP.string ppstrm (P.pathToString path)

fun ppList ppstrm ppfn elems =
    ppClosedSequence ppstrm
      {front = (fn strm => PP.string strm "["),
       back =  (fn strm => PP.string strm "]"),
       sep = (fn strm => PP.string strm ", "),
       pr = ppfn,
       style = CONSISTENT}
      elems

fun ppOption ppstrm ppfn elemOp =
    case elemOp
      of NONE => PP.string ppstrm "<<>>"
       | SOME e => (PP.string ppstrm "<< "; ppfn ppstrm e; PP.string ppstrm " >>")

fun ppSign ppstrm sign =
    (case sign
      of Access.CSIG(n,m) =>
	 (PP.openHBox ppstrm;
	  PP.string ppstrm "CSIG(";
	  PP.string ppstrm (Int.toString n);
	  PP.string ppstrm ",";
	  PP.string ppstrm (Int.toString m);
	  PP.string ppstrm ")";
	  PP.closeBox ppstrm)
       | Access.CNIL => PP.string ppstrm "CNIL")

fun ppVarRules ppstrm varRules =
    let fun pprule ppstrm ruleno =
	     PP.string ppstrm (Int.toString ruleno);
    in PU.ppSequence ppstrm
	   {sep = (fn ppstrm => PP.break ppstrm {nsp=1,offset=0}),
	    pr = pprule,
	    style = PU.INCONSISTENT}
	   (RS.toList varRules)
    end

fun ppRuleset ppstrm ruleset =
    let val rulesList = RS.listItems ruleset
    in PP.openHBox ppstrm;
        PU.pps ppstrm "{";
        PU.ppSequence ppstrm
	  {sep = (fn ppstrm => PU.pps ppstrm ","),
	   pr = (fn ppstrm => fn r => PU.pps ppstrm (Int.toString r)),
	   style = PU.INCONSISTENT}
	  rulesList;
        PU.pps ppstrm "}";
       PP.closeBox ppstrm (* openHBox *)
    end

(* ppSubcase : ppstrm -> (ppstrm -> 'a -> unit) -> subcase -> unit *)
fun ppSubcase ppstrm ppcase subcase =
    (case subcase
      of CONST => PP.string ppstrm "CONST"
       | DCARG thing => ppcase ppstrm thing
       | VELEMS elems => 
	    (PP.openHOVBox ppstrm (PP.Abs 0);
	     PP.openHBox ppstrm;
             PP.string ppstrm "VELEMS";
	     PP.closeBox ppstrm;
	     ppElems ppstrm ppcase elems;
	     PP.closeBox ppstrm))

and ppElems ppstrm ppcase elems =
    (PP.openVBox ppstrm (PP.Abs 3);
     PU.ppvseq ppstrm 0 "" ppcase elems;
     PP.closeBox ppstrm)

(* ppProtoAndor : ppstrm -> protoAndor -> unit *)
(* pretty printer for protoAndor nodes *)
fun ppProtoAndor ppstrm =
    let fun ppNode ppstrm (ANDp {varRules, children}) =
	    (PP.openHOVBox ppstrm (PP.Abs 0);
	     PP.openHBox ppstrm;
             PP.string ppstrm "ANDp";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppVarRules ppstrm varRules;
	     PP.closeBox ppstrm;
	     ppAndChildren ppstrm children;
	     PP.closeBox ppstrm)
	  | ppNode ppstrm (ORp {varRules, sign, cases}) =
	    (PP.openHOVBox ppstrm (PP.Abs 0);
             PP.openHBox ppstrm;
	     PP.string ppstrm "ORp";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppVarRules ppstrm varRules;
	     PP.break ppstrm {nsp=1,offset=0};
	     ppSign ppstrm sign;
	     PP.closeBox ppstrm;
	     ppProtoVariants ppstrm cases;
	     PP.closeBox ppstrm)
	  | ppNode ppstrm (VARp {varRules}) =
	    (PP.openHBox ppstrm;
	     PP.string ppstrm "VARp";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppVarRules ppstrm varRules;
	     PP.closeBox ppstrm)
	  | ppNode ppstrm (WCp) = PP.string ppstrm "WCp"

	and ppAndChildren ppstrm nodes =
	    (PP.openVBox ppstrm (PP.Abs 3);
	     PU.ppvseq ppstrm 0 "" ppNode nodes;
	     PP.closeBox ppstrm)

	and ppProtoVariants ppstrm variants =
	    (PP.openVBox ppstrm (PP.Abs 3);
	     PU.ppvseq ppstrm 0 "" ppProtoVariant variants;
	     PP.closeBox ppstrm)

	and ppProtoVariant ppstrm (con, rules, subcase) =
	    (PP.openHBox ppstrm (* (PP.Abs 0) *);
	     PP.string ppstrm (AU.conToString con);
	     PP.break ppstrm {nsp=1,offset=0};
	     ppRuleset ppstrm rules;
	     PP.break ppstrm {nsp=1,offset=0};
	     ppSubcase ppstrm ppProtoAndor subcase;
	     PP.closeBox ppstrm)

    in ppNode ppstrm
    end  (* fun ppSimpleAndor *)

(* ppAndor : ppstrm -> andor -> unit *)
(*  pretty printer for AND-OR nodes
 *  could develop a "path" while printing the andor tree *)
fun ppAndor ppstrm =
    let fun ppNode ppstrm (AND {id, children}) =
	    (PP.openHOVBox ppstrm (PP.Abs 0);
	     PP.openHBox ppstrm;
             PP.string ppstrm "AND";
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm (Int.toString id);
	     PP.closeBox ppstrm;
	     ppAndChildren ppstrm children;
	     PP.closeBox ppstrm)
	  | ppNode ppstrm (OR {id, path, sign, defaults, cases}) =
	    (PP.openHOVBox ppstrm (PP.Abs 0);
             PP.openHBox ppstrm;
	     PP.string ppstrm "OR";
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm (Int.toString id);
	     PP.break ppstrm {nsp=1,offset=0};
	     ppPath ppstrm path;
	     PP.break ppstrm {nsp=1,offset=0};
	     ppRuleset ppstrm defaults;
	     PP.break ppstrm {nsp=1,offset=0};
	     ppSign ppstrm sign;
	     PP.closeBox ppstrm; (* openHBox *)
	     ppVariants ppstrm cases;
	     PP.closeBox ppstrm) (* openHOVBox *)
	  | ppNode ppstrm (VAR {id}) =
	    (PP.openHBox ppstrm;
	     PP.string ppstrm "VAR";
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm (Int.toString id);
	     PP.closeBox ppstrm)
	  | ppNode ppstrm WC = PP.string ppstrm "WC"

	and ppAndChildren ppstrm nodes =
	    (PP.openVBox ppstrm (PP.Abs 3);
	     PU.ppvseq ppstrm 0 "" ppNode nodes;
	     PP.closeBox ppstrm)

	and ppVariants ppstrm variants =
	    (PP.openVBox ppstrm (PP.Abs 3);
	     PU.ppvseq ppstrm 0 "" ppVariant variants;
	     PP.closeBox ppstrm)

	and ppVariant ppstrm (con, rules, subcase) =
	    (PP.openHBox ppstrm (* (PP.Abs 0) *);
	     PP.string ppstrm (AU.conToString con);
	     PP.break ppstrm {nsp=1,offset=0};
	     ppSubcase ppstrm ppAndor subcase;
	     PP.closeBox ppstrm)

     in ppNode ppstrm
    end (* fun ppAndor *)

(* ppDectree : ppstrm -> decTree -> unit *)
val ppDectree =
    let fun ppDec ppstrm (SWITCH {id, path, sign, cases, defaultOp, live}) =
            (PP.openHBox ppstrm;
	     PP.string ppstrm "SWITCH";
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm (Int.toString id);
	     PP.break ppstrm {nsp=1,offset=0};
	     ppPath ppstrm path;
	     PP.break ppstrm {nsp=1,offset=0};
	     ppSign ppstrm sign;
	     PP.break ppstrm {nsp=1,offset=0};
	     ppSwitch ppstrm (cases, defaultOp);
	     PP.closeBox ppstrm)
	  | ppDec ppstrm (RHS ruleno) =
	    (PP.openHBox ppstrm;
	     PP.string ppstrm "RHS";
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm (Int.toString ruleno);
	     PP.closeBox ppstrm)
	  | ppDec ppstrm (FAIL) =
	    (PP.openHBox ppstrm;
	     PP.string ppstrm "FAIL";
	     PP.closeBox ppstrm)
	and ppSwitch ppstrm (cases,defaultOp) =
            (PP.openVBox ppstrm (PP.Abs 3);
	     PU.ppvseq ppstrm 0 "" ppCase cases;
	     (case defaultOp
	        of SOME dectree =>
          	     (PP.cut ppstrm;
		      PP.openHOVBox ppstrm (PP.Abs 0);
	              PP.string ppstrm "*";
		      PP.break ppstrm {nsp=1,offset=0};
		      ppDec ppstrm dectree;
		      PP.closeBox ppstrm)
		 | NONE => ());
	     PP.closeBox ppstrm)
	and ppCase ppstrm (con, decTree) =
	    (PP.openHBox ppstrm;
	     PP.string ppstrm (AU.conToString con);
	     PP.break ppstrm {nsp=1,offset=0};
	     ppDec ppstrm decTree;
	     PP.closeBox ppstrm)
     in ppDec
    end (* ppDectree *)

(* ppRule : ppstream -> Absyn.pat * Absyn.exp -> unit *)
(* print absyn rule *)
fun ppRule ppstrm (pat, exp) =
    (PP.openHBox ppstrm;
       PPAbsyn.ppPat StaticEnv.empty ppstrm (pat, 100);
       PP.string ppstrm " => ";
       PP.openHOVBox ppstrm (PP.Abs 3);
         PPAbsyn.ppExp (StaticEnv.empty, NONE) ppstrm (exp, 100);
       PP.closeBox ppstrm;  (* openHOVBox *)
     PP.closeBox ppstrm)  (* openHBox *)

fun ppMatch ppstrm match =
    (PP.openVBox ppstrm (PP.Abs 3);
       PU.ppvseq ppstrm 0 "" ppRule match;
     PP.closeBox ppstrm)

end (* top local *)
end (* structure PPMatchComp *)

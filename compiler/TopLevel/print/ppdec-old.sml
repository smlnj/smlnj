(* ppdec.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PPDEC =
sig
  val ppDec : Environment.environment -> PrettyPrint.stream
                -> (Absyn.dec * LambdaVar.lvar list) -> unit
end (* signature PPDEC *)

structure PPDec : PPDEC =
struct

local
  structure S = Symbol
  structure SP = SymPath
  structure IP = InvPath
  structure LV = LambdaVar
  structure A = Access
  structure V = Variable
  structure AS = Absyn
  structure T = Types
  structure M = Modules
  structure PP = PrettyPrint
  structure PU = PPUtil
  structure PT = PPType
  structure PO = PPObj

  (* debugging *)
  val debugging = Control.pddebugging
  fun say msg = (Control.Print.say msg; Control.Print.flush ())
  fun saynl msg = (Control.Print.say msg; Control.Print.say "\n"; Control.Print.flush ())
  fun saysnl msgs = saynl (concat msgs)
  fun dbsaynl msg = if !debugging then saynl msg else ()
  fun dbsaysnl msgs = if !debugging then saysnl msgs else ()

  fun bug msg = ErrorMsg.impossible ("PPDec: "^msg)
in

type object = Unsafe.Object.object

val signatures = Control.Print.signatures
val printOpens = Control.Print.printOpens
val printDepth = Control.Print.printDepth
val anonSym = S.strSymbol "<anonymousSig>"
val anonFsym = S.fctSymbol "<anonymousFsig>"

fun pplist_nl ppstrm pr =
  let fun pp [] = ()
        | pp [el] = pr el
        | pp (el::rst) = (pr el; PP.newline ppstrm; pp rst)
   in pp
  end

fun C f x y = f y x;

fun xtract (v, pos) = Unsafe.Object.nth (v, pos)

fun ppDec ({static,dynamic}: Environment.environment)
          (ppstrm: PP.stream) (dec: Absyn.dec, exportLvars) =
   let val dec = (* pruneDec *) dec

       fun isExport (x : LV.lvar) =
	   List.exists (fn y => LV.same(y,x)) exportLvars

       val pps = PP.string ppstrm
       fun sp () = PP.space ppstrm 1
       fun nbSp () = PP.nbSpace ppstrm 1

	(* trueValType : SP.path * T.ty -> T.ty
	 *  trueValType: get the type of the bound variable from static environment,
	 *  since the stamps in the absyn haven't been converted by the pickler,
	 *  defaulting to ty is the binding of sym (where path = [sym]) is not
         *  a VALvar, otherwise returns the type of the bound VALvar. *)
       fun trueValType (path: SP.path, defaultTy: T.ty) =
	   (case path
	      of SymPath.SPATH [sym] =>
		   (case Lookup.lookIdSymOp (static, sym)
		      of SOME (AS.VAR v) =>
			   (case v
			      of V.VALvar{typ,...} => !typ
			       | V.OVLDvar{name,...} =>
				   (saysnl ["### trueValType: OVLDvar: "^Symbol.name name];
				    defaultTy)
			       | (V.ERRORvar) => bug "trueValType: ERRORvar\n")
		       | SOME (AS.CON (T.DATACON{name,typ,...})) =>
			   bug ("trueValType: DATACON " ^ Symbol.name name)
		       | SOME (AS.ERRORid) => bug ("trueValType[ERRORid]: " ^ Symbol.name sym)
		       | NONE => bug ("trueValType[unbound]: " ^ Symbol.name sym))
	       | _ => bug "trueValType: not singleton path")

       fun trueTycon (path: IP.path) =
	    let val err = fn _ => fn _ => fn _ => (bug "trueTycon: unbound ")
	     in case Lookup.lookTyc(static,ConvertPaths.invertIPath(path),err)
		  of T.DEFtyc x => SOME x
		   | _ => NONE
	    end

       fun isLazyBogus (SymPath.SPATH path) =
	       case rev(String.explode (Symbol.name(List.last path)))
                of #"$":: #","::_ => true
                 | _ => false

       (* ppVar: V.variable -> unit *)
       fun ppVar (V.VALvar{path, access, typ=ref ty, prim, ...}) =
             if isLazyBogus path then () else
	       (dbsaysnl [">>> ppVar", SP.toString path];
		PP.openHVBox ppstrm (PP.Rel 0);
	          PP.openHOVBox ppstrm (PP.Rel 2);
	            PP.openHBox ppstrm;
	              PP.string ppstrm "val"; sp ();
		      PP.string ppstrm (SymPath.toString path); sp();
		      pps "=";
		    PP.closeBox ppstrm;
		    sp ();
		    (case access
		       of A.LVAR lv =>  (* access is expected to be an LVAR *)
			    (case StaticEnv.look (static, SymPath.last path)
			      of Bindings.VALbind(V.VALvar{access = A.PATH (A.EXTERN pid, pos),
							   ...}) =>
				  if isExport lv  (* is it "exported"? *)
				  then (case DynamicEnv.look dynamic pid
					 of SOME objv =>
					     let val obj = xtract (objv, pos)
					     in PO.ppObj static ppstrm (obj, ty, !printDepth);
						sp (); pps ":"; nbSp();
						PT.ppType static ppstrm (trueValType (path,ty))
					     end
					  | NONE => bug "ppVar: objv")
				   else (PP.string ppstrm "<hidden>";
					 sp (); pps ":"; sp();
					 PT.ppType static ppstrm ty)
				| _ => PP.string ppstrm "<hidden>"
			     (* end case *))
		        | _ => bug "ppVar"
		        (* end case *));
	          PP.closeBox ppstrm;
	          PP.newline ppstrm;
	        PP.closeBox ppstrm)
          | ppVar _ = ()

       fun ppVb (AS.VB{pat,...}) =
	 let fun ppBind(pat) =
	           case pat
		    of AS.VARpat v => ppVar v
		     | AS.RECORDpat{fields,...} => app (ppBind o #2) fields
		     | AS.VECTORpat(pats,_) => app ppBind pats
		     | AS.APPpat(_,_,pat) => ppBind pat
		     | AS.CONSTRAINTpat(pat,_) => ppBind pat
		     | AS.LAYEREDpat(pat1,pat2) => (ppBind pat1; ppBind pat2)
                     | AS.ORpat(p1, _) => ppBind p1
		     | _ => ()
	  in ppBind pat
	 end

       and ppRvb (AS.RVB{var, ...}) = ppVar var

       and ppTb (T.DEFtyc dt) =
	   let val {path, tyfun = T.TYFUN {arity,body},...} =
		   getOpt (trueTycon (#path dt), dt)
	   in
	       PP.openHVBox ppstrm (PP.Rel 0);
	       PP.openHOVBox ppstrm (PP.Rel 2);
	       PP.string ppstrm "type";
	       PT.ppFormals ppstrm arity;
	       PP.break ppstrm {nsp=1,offset=0};
	       PU.ppSym ppstrm (InvPath.last path);
	       PP.string ppstrm " =";
	       PP.break ppstrm {nsp=1,offset=0};
	       PT.ppType static ppstrm body;
	       PP.closeBox ppstrm;
	       PP.newline ppstrm;
	       PP.closeBox ppstrm
	   end
	 | ppTb _ = bug "ppTb:nonDEFtyc"

	and ppAbsTyc (T.GENtyc { path, arity, eq, ... }) =
	    (case !eq
	       of T.ABS =>
		    (PP.openHVBox ppstrm (PP.Rel 0);
		     PP.openHOVBox ppstrm (PP.Rel 2);
		     PP.string ppstrm "type";
		     PT.ppFormals ppstrm arity;
		     PP.break ppstrm {nsp=1,offset=0};
		     PU.ppSym ppstrm (InvPath.last path);
		     PP.closeBox ppstrm;
		     PP.newline ppstrm;
		     PP.closeBox ppstrm)
	       | _ =>
		    (PP.openHVBox ppstrm (PP.Rel 0);
		     PP.openHOVBox ppstrm (PP.Rel 2);
		     PP.string ppstrm "type";
		     PT.ppFormals ppstrm arity;
		     PP.break ppstrm {nsp=1,offset=0};
		     PU.ppSym ppstrm (InvPath.last path);
		     PP.closeBox ppstrm;
		     PP.newline ppstrm;
		     PP.closeBox ppstrm))
          | ppAbsTyc _ = bug "ppAbsTyc:tycKind"

	and ppDataTyc (T.GENtyc{path, arity,
				kind = T.DATATYPE{index, freetycs,
						  family={members, ...},...},
				... }) =
	    let fun ppDcons nil = ()
		  | ppDcons (first::rest) =
		    let fun ppDcon ({name,domain,rep}) =
			    (PU.ppSym ppstrm name;
			     case domain
			      of SOME dom =>
			         (PP.string ppstrm " of ";
				  PT.ppDconDomain (members,freetycs)
					       static ppstrm dom)
			       | NONE => ())
		    in
			PP.string ppstrm "= "; ppDcon first;
			app (fn d => (PP.break ppstrm {nsp=1,offset=0};
				      PP.string ppstrm "| "; ppDcon d))
			    rest
		    end
		val {tycname,dcons,...} = Vector.sub(members,index)
	    in
		PP.openHVBox ppstrm (PP.Rel 0);
		PP.openHVBox ppstrm (PP.Rel 0);
		PP.string ppstrm "datatype";
		PT.ppFormals ppstrm arity;
		PP.string ppstrm " ";
		PU.ppSym ppstrm (InvPath.last path);
		PP.break ppstrm {nsp=1,offset=2};
		PP.openHVBox ppstrm (PP.Rel 0);
		ppDcons dcons;
		PP.closeBox ppstrm;
		PP.closeBox ppstrm;
		PP.newline ppstrm;
		PP.closeBox ppstrm
	    end
	  | ppDataTyc _ = bug "unexpected case in ppDataTyc"

	and ppEb(AS.EBgen{exn=T.DATACON{name,...},etype,...}) =
	      (PP.openHVBox ppstrm (PP.Rel 0);
	       PP.openHOVBox ppstrm (PP.Rel 2);
	       PP.string ppstrm "exception ";
	       PU.ppSym ppstrm name;
	       case etype
		 of NONE => ()
		  | SOME ty' =>
		           (PP.string ppstrm " of";
			    PP.break ppstrm {nsp=1,offset=0};
			    PT.ppType static ppstrm ty');
	       PP.closeBox ppstrm;
 	       PP.newline ppstrm;
	       PP.closeBox ppstrm)

	  | ppEb (AS.EBdef{exn=T.DATACON{name,...}, edef=T.DATACON{name=dname,...}}) =
	      (PP.openHVBox ppstrm (PP.Rel 0);
	       PP.openHOVBox ppstrm (PP.Rel 2);
	       PP.string ppstrm "exception ";
	       PU.ppSym ppstrm name;
	       PP.string ppstrm " =";
	       PP.break ppstrm {nsp=1,offset=0};
	       PU.ppSym ppstrm dname;
	       PP.closeBox ppstrm;
 	       PP.newline ppstrm;
	       PP.closeBox ppstrm)

	and ppStrb (AS.STRB{name, str, ...}) = (
	      PP.openHVBox ppstrm (PP.Abs 0);
		PP.openHBox ppstrm;
		  PP.string ppstrm "structure";
		  PP.space ppstrm 1;
		  PU.ppSym ppstrm name;
		  PP.space ppstrm 1;
		  PP.string ppstrm ":";
		  PP.space ppstrm 1;
		PP.closeBox ppstrm;
		PPModules.ppStructure ppstrm static (str, !signatures);
	        PP.newline ppstrm;
	      PP.closeBox ppstrm)

	and ppFctb (AS.FCTB{name, fct, ...}) =
	    (PP.openHVBox ppstrm (PP.Abs 0);
	      pps "functor ";
	      PU.ppSym ppstrm name;
	      case fct of
		  M.FCT { sign, ... } =>
		    PPModules.ppFunsig ppstrm static (sign, !signatures)
		| _ => pps " : <sig>";  (* blume: cannot (?) happen *)
	      PP.newline ppstrm;
	    PP.closeBox ppstrm)

        and ppSigb sign =
	    let val name = case sign
                            of M.SIG { name, ... } => getOpt (name, anonSym)
                             | _ => anonSym
             in (PP.openHVBox ppstrm (PP.Abs 0);
		   PP.openHVBox ppstrm (PP.Abs 0);
   	             PP.openHBox ppstrm;
		       PP.string ppstrm "signature"; PP.space ppstrm 1;
		       PU.ppSym ppstrm name; PP.space ppstrm 1;
		       PP.string ppstrm "="; PP.space ppstrm 1;
		     PP.closeBox ppstrm;
(*	           PP.break ppstrm {nsp=1,offset=2}; *)
	           PPModules.ppSignature ppstrm static (sign, !signatures);
	          PP.closeBox ppstrm;
	         PP.newline ppstrm;
	         PP.closeBox ppstrm)
            end

        and ppFsigb fsig =
	    let val name = case fsig
                            of M.FSIG{kind=SOME s, ...} => s
                             | _ => anonFsym

	     in (PP.openHVBox ppstrm (PP.Abs 0);
	         pps "funsig "; PU.ppSym ppstrm name;
	         PPModules.ppFunsig ppstrm static (fsig, !signatures);
	         PP.newline ppstrm;
	         PP.closeBox ppstrm)
            end

	and ppFixity{fixity,ops} =
	    (PP.openHVBox ppstrm (PP.Rel 0);
	     PP.openHVBox ppstrm (PP.Rel 0);
	     PP.string ppstrm (Fixity.fixityToString fixity);
	     PU.ppSequence ppstrm {sep=C PP.break {nsp=1,offset=0},
			                  pr=PU.ppSym,
			                  style=PU.INCONSISTENT}
	                          ops;
	     PP.closeBox ppstrm;
	     PP.newline ppstrm;
	     PP.closeBox ppstrm)

	and ppOpen(pathStrs) =
	    if !printOpens
	    then (PP.openHVBox ppstrm (PP.Rel 0);
		   app (fn (path,str) =>
			 PPModules.ppOpen ppstrm static (path, str, !signatures))
		       pathStrs;
		  PP.closeBox ppstrm)
	    else (PP.openHVBox ppstrm (PP.Rel 0);
		  PP.openHVBox ppstrm (PP.Rel 0);
		  PP.string ppstrm "open ";
		  PU.ppSequence ppstrm
		     {sep=C PP.break {nsp=1,offset=0},
		      pr=(fn ppstrm => fn (path,_) =>
			   PP.string ppstrm (SymPath.toString path)),
		      style=PU.INCONSISTENT}
		     pathStrs;
		  PP.closeBox ppstrm;
		  PP.newline ppstrm;
		  PP.closeBox ppstrm)

	and ppVARSEL (var1, var2, index) = ppVar var1

	and ppDec0 dec =
	    (PT.resetPPType();
	     case dec
	      of AS.VALdec vbs => app ppVb vbs
	       | AS.VALRECdec rvbs => app ppRvb rvbs
	       | AS.DOdec _ => ()
	       | AS.TYPEdec tbs => app ppTb tbs
	       | AS.DATATYPEdec{datatycs,withtycs} =>
		   (app ppDataTyc datatycs;
		    app ppTb withtycs)
	       | AS.ABSTYPEdec{abstycs,withtycs,body} =>
		   (app ppAbsTyc abstycs;
		    app ppTb withtycs;
		    ppDec0 body)
	       | AS.EXCEPTIONdec ebs => app ppEb ebs
	       | AS.STRdec strbs => app ppStrb strbs
	       | AS.FCTdec fctbs => app ppFctb fctbs
	       | AS.SIGdec sigbs => app ppSigb sigbs
	       | AS.FSIGdec fsigbs => app ppFsigb fsigbs
	       | AS.LOCALdec(decIn,decOut) => ppDec0 decOut
	       | AS.SEQdec decs =>
		  (case decs
		     of AS.OPENdec pathStrs :: rest =>
			 ppOpen pathStrs
                      | _ => app ppDec0 decs)
	       | AS.FIXdec fixd => ppFixity fixd
	       | AS.OVLDdec _ =>
                   (PP.string ppstrm "overload"; PP.newline ppstrm)
	       | AS.OPENdec pathStrs => ppOpen pathStrs
	       | AS.MARKdec(dec,_) => ppDec0 dec
	       | AS.VARSELdec(v1,v2,i) => ppVARSEL (v1,v2,i))

     in PP.openHVBox ppstrm (PP.Rel 0);
	ppDec0 dec;
	PP.closeBox ppstrm;
	PP.flushStream ppstrm
    end (* end ppDec *)

end (* top local *)
end (* structure PPDec *)

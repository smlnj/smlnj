(* ppdec.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PPDEC =
sig
  val fmtDec : Environment.environment -> (Absyn.dec * LambdaVar.lvar list) -> NewPP.format
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
  structure PP = NewPP
  structure PPU = NewPPUtil
  structure PPT = PPType
  structure PPS = PPSymbols
  structure PPP = PPSymPaths
  structure PPO = PPObj

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
val anonSigName = S.strSymbol "<anonymousSig>"
val anonFsigName = S.fctSymbol "<anonymousFsig>"

fun extract (v, pos) = Unsafe.Object.nth (v, pos)

fun fmtDec ({static,dynamic}: Environment.environment)
           (dec: Absyn.dec, exportLvars) =
   let fun isExport (x : LV.lvar) =
	   List.exists (fn y => LV.same(y,x)) exportLvars

	(* trueValType : SP.path * T.ty -> T.ty
	 *  trueValType: get the type of the bound variable from the static environment,
	 *  since the stamps in the absyn haven't been converted by the pickler,
	 *  defaulting to ty is the binding of sym (where path = [sym]) is not
         *  a VALvar, otherwise returns the type of the bound VALvar. *)
       fun trueValType (path: SP.path, defaultTy: T.ty) =
	   (case path
	      of SP.SPATH [sym] =>
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
	     in case Lookup.lookTyc(static, ConvertPaths.invertIPath(path), err)
		  of T.DEFtyc x => SOME x
		   | _ => NONE
	    end

       (* isLazyBogus : SP.path -> bool *)
       fun isLazyBogus (SP.SPATH path) =
	       case rev(String.explode (Symbol.name(List.last path)))
                of #"$":: #","::_ => true
                 | _ => false

       (* fmtVar: V.variable -> PP.format *)
       fun fmtVar (V.VALvar{path, access, typ=ref ty, prim, ...}) =
             if isLazyBogus path then PP.empty else
	       (dbsaysnl [">>> fmtVar", SP.toString path];
		(PP.hcat
	           (PP.hblock [PP.text "val", PPP.fmtSymPath path, PP.equal],
		     (case access
		        of A.LVAR lv =>  (* access is expected to be an LVAR *)
			    (case StaticEnv.look (static, SP.last path)
			      of Bindings.VALbind(V.VALvar{access = A.PATH (A.EXTERN pid, pos), ...}) =>
				  if isExport lv  (* is it "exported"? *)
				  then (case DynamicEnv.look dynamic pid
					 of SOME objv =>
					     let val obj = extract (objv, pos)
					     in PP.hblock
						  [PPO.fmtObj static (obj, ty, !printDepth),
						   PP.colon,
						   PPT.fmtType static (trueValType (path,ty))]
					     end
					  | NONE => bug "fmtVar: objv")
				   else PP.hblock [PP.text "<hidden>", PP.colon, PPT.fmtType static ty]
				| _ => PP.text "<hidden>"
			     (* end case *))
		        | _ => bug "fmtVar"
		        (* end case *)))))
         | fmtVar _ = PP.empty

       (* fmtVb : AS.vb -> PP.format *)
       fun fmtVb (AS.VB{pat,...}) =
	 let fun fmtBind(pat) =
	           case pat
		    of AS.VARpat v => fmtVar v
		     | AS.RECORDpat{fields,...} => PP.hblock (map (fmtBind o #2) fields)
		     | AS.VECTORpat(pats,_) => PP.hblock (map fmtBind pats)
		     | AS.APPpat(_,_,pat) => fmtBind pat
		     | AS.CONSTRAINTpat(pat,_) => fmtBind pat
		     | AS.LAYEREDpat(pat1,pat2) => PP.hblock [fmtBind pat1, fmtBind pat2]
                     | AS.ORpat(p1, _) => fmtBind p1
		     | _ => PP.empty
	  in fmtBind pat
	 end

       (* fmtRvb : AS.rvb -> PP.format *)
       and fmtRvb (AS.RVB{var, ...}) = fmtVar var

       and fmtTb (T.DEFtyc dt) =
	   let val {path, tyfun = T.TYFUN {arity,body},...} =
		   getOpt (trueTycon (#path dt), dt)
	    in PP.hblock
	         [PP.text "type", PPT.fmtFormals arity, PPS.fmtSym (InvPath.last path), PP.equal,
		  PPT.fmtType static body]
	   end
	 | fmtTb _ = bug "fmtTb:nonDEFtyc"

	and fmtAbsTyc (T.GENtyc { path, arity, eq, kind, ... }) =
	    (case kind
	       of T.ABSTRACT _ =>
		     PP.hblock
		       [PP.text "type", PPT.fmtFormals arity, PPS.fmtSym (InvPath.last path)]
		| _ =>  (* same! *)
		     PP.hblock
		       [PP.text "type", PPT.fmtFormals arity, PPS.fmtSym (InvPath.last path)])
          | fmtAbsTyc _ = bug "fmtAbsTyc:tycKind"

        (* fmtDataTyc : T.tycon -> PP.format *)
        (* REQUIRED: tycon is DATATYPE *)
	and fmtDataTyc (T.GENtyc {path, arity,
				  kind = T.DATATYPE{index, freetycs,
					  	    family = {members, ...},...},
				  ... }) =
	    let fun fmtDcons nil = PP.empty
		  | fmtDcons dcons =
		    let fun fmtDcon ({name,domain,rep}) =
			    PP.hcat
			      (PPS.fmtSym name,
			       case domain
			         of SOME dom =>
			              PP.hcat
					(PP.text "of",
				         PPT.fmtDconDomain (members,freetycs) static dom)
				  | NONE => PP.empty)
		     in PP.hcat (PP.equal, PP.hsequence (PP.text " |") (map fmtDcon dcons))
		    end
		val {dcons, ...} = Vector.sub(members,index)
	     in PP.hblock
		  [PP.text "datatype", PPT.fmtFormals arity, PPS.fmtSym (InvPath.last path),
		   fmtDcons dcons]
	    end
	  | fmtDataTyc _ = bug "unexpected case in fmtDataTyc"

        (* fmtEb : AS.eb -> PP.format *)
	and fmtEb(AS.EBgen{exn=T.DATACON{name,...},etype,...}) =
	      PP.hblock
	        [PP.text "exception", PPS.fmtSym name,
		 case etype
		   of NONE => PP.empty
		    | SOME ty' => PP.hcat (PP.text " of", PPT.fmtType static ty')]

	  | fmtEb (AS.EBdef{exn=T.DATACON{name,...}, edef=T.DATACON{name=dname,...}}) =
	      PP.hblock [PP.text "exception", PPS.fmtSym name, PP.equal, PPS.fmtSym dname]

	and fmtStrb (AS.STRB{name, str, ...}) =
	      PP.pcat
		(PP.hblock [PP.text "structure", PPS.fmtSym name, PP.colon], 
		 PP.softIndent 2 (PPModules.fmtStructure static (str, !signatures)))

	and fmtFctb (AS.FCTB{name, fct, ...}) =
	    (PP.pcat
	       (PP.hblock [PP.text "functor", PPS.fmtSym name, PP.colon],
	        case fct
		  of M.FCT { sign, ... } =>
		       PP.softIndent 2 (PPModules.fmtFunsig static (sign, !signatures))
		   | _ => PP.text "<sig>"))  (* blume: cannot (?) happen *)

        and fmtSigb sign =
	    let val name = case sign
                             of M.SIG {name, ...} => getOpt (name, anonSigName)
                              | _ => anonSigName
             in PP.hblock
		  [PP.text "signature", PPS.fmtSym name, PP.equal,
		   PPModules.fmtSignature static (sign, !signatures)]
            end

        and fmtFsigb fsig =
	    let val name = case fsig
                            of M.FSIG{kind=SOME s, ...} => s
                             | _ => anonFsigName

	     in PP.hblock
	          [PP.text "funsig", PPS.fmtSym name, PP.equal,
	           PPModules.fmtFunsig static (fsig, !signatures)]
            end

	and fmtFixity {fixity,ops} =
	      PP.hcat (PP.text (Fixity.fixityToString fixity),
		       PP.hsequence PP.empty (map PPS.fmtSym ops))

	and fmtOpen(pathStrs) =
	    if !printOpens
	    then PP.vblock
		   (map (fn (path,str) =>
			    PPModules.fmtOpen static (path, str, !signatures))
			pathStrs)
	    else PP.hcat 
		   (PP.text "open",
		    PP.psequence PP.empty (map (fn (path,_) => PPP.fmtSymPath path) pathStrs))

	and fmtVARSEL (var1, var2, index) = fmtVar var1

	and fmtDec0 dec =
	    (PPT.resetPPType();
	     case dec
	      of AS.VALdec vbs => PP.vblock (map fmtVb vbs)
	       | AS.VALRECdec rvbs => PP.vblock (map fmtRvb rvbs)
	       | AS.DOdec _ => PP.empty
	       | AS.TYPEdec tbs => PP.vblock (map fmtTb tbs)
	       | AS.DATATYPEdec{datatycs,withtycs} =>
		   PP.vcat
		     (PP.vblock (map fmtDataTyc datatycs),
		      PP.vblock (map fmtTb withtycs))
	       | AS.ABSTYPEdec {abstycs, withtycs, body} =>
		   PP.vblock
		     [PP.vblock (map fmtAbsTyc abstycs),
		      PP.vblock (map fmtTb withtycs),
		      fmtDec0 body]
	       | AS.EXCEPTIONdec ebs => PP.vblock (map fmtEb ebs)
	       | AS.STRdec strbs => PP.vblock (map fmtStrb strbs)
	       | AS.FCTdec fctbs => PP.vblock (map fmtFctb fctbs)
	       | AS.SIGdec sigbs => PP.vblock (map fmtSigb sigbs)
	       | AS.FSIGdec fsigbs => PP.vblock (map fmtFsigb fsigbs)
	       | AS.LOCALdec(decIn,decOut) => fmtDec0 decOut
	       | AS.SEQdec decs => (* DBM ??? *)
		  (case decs
		     of AS.OPENdec pathStrs :: rest =>
			 fmtOpen pathStrs
                      | _ => PP.vblock (map fmtDec0 decs))
	       | AS.FIXdec fixd => fmtFixity fixd
	       | AS.OVLDdec _ => (PP.text "overload")
	       | AS.OPENdec pathStrs => fmtOpen pathStrs
	       | AS.MARKdec (dec, _) => fmtDec0 dec
	       | AS.VARSELdec (v1, v2, i) => fmtVARSEL (v1,v2,i))  (* DBM ??? *)

     in fmtDec0 dec
    end (* end fmtDec *)

end (* top local *)
end (* structure PPDec *)

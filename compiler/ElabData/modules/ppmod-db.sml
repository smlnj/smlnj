(* ppmod-db.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* Modified to use SML/NJ Lib PP. [dbm, 7/30/03]).
 * Debugging version of module printing showing internals;
 * roughly equivalent to the old ppmod with internals true [DBM, 2021/12/12].
 * [DBM, 2022.09.26] Converted to NewPrettyPrint pretty printer library *)

signature PPMOD_DB =
sig

  val fmtSignature: StaticEnv.staticEnv -> Modules.Signature * int -> NewPrettyPrint.format

  val fmtStructure: StaticEnv.staticEnv -> Modules.Structure * int -> NewPrettyPrint.format

  val fmtOpen: StaticEnv.staticEnv -> SymPath.path * Modules.Structure * int -> NewPrettyPrint.format
									    
  val fmtStructureName : StaticEnv.staticEnv -> Modules.Structure -> NewPrettyPrint.format

  val fmtFunsig : StaticEnv.staticEnv -> Modules.fctSig * int -> NewPrettyPrint.format

  val fmtBinding: StaticEnv.staticEnv -> Symbol.symbol * Bindings.binding * int -> NewPrettyPrint.format

  (* module internals *)
  val fmtElement : Modules.entityEnv option * StaticEnv.staticEnv
		  -> Symbol.symbol * Modules.spec * int -> NewPrettyPrint.format

  val fmtEntity : StaticEnv.staticEnv -> Modules.entity * int -> NewPrettyPrint.format

  val fmtEntityEnv : StaticEnv.staticEnv -> Modules.entityEnv * int -> NewPrettyPrint.format

  val fmtFunctor : StaticEnv.staticEnv -> Modules.Functor * int -> NewPrettyPrint.format

  val fmtEnv : Symbol.symbol list option * StaticEnv.staticEnv
	      -> StaticEnv.staticEnv * int -> NewPrettyPrint.format

end (* signature PPMOD *)


structure PPModules_DB : PPMOD_DB =
struct

local
  structure S = Symbol
  structure SP = SymPath
  structure IP = InvPath
  structure PN = PathName
  structure A = Access
  structure T = Types
  structure TU = TypesUtil
  structure BT = BasicTypes
  structure V = Variable
  structure AS = Absyn
  structure EP = EntPath
  structure M = Modules
  structure MU = ModuleUtil
  structure B = Bindings
  structure SE = StaticEnv
  structure EE = EntityEnv
  structure LU = Lookup

  structure PP = NewPrettyPrint
  structure PPS = PPSymbols
  structure PPP = PPSymPaths
  structure PPT = PPType

  fun bug msg = ErrorMsg.impossible("PPModules: "^msg)

  val fmtType = PPT.fmtType
  val fmtTycon = PPT.fmtTycon
  val fmtTyfun = PPT.fmtTyfun
  val fmtFormals = PPT.fmtFormals

  fun viblock (formats: PP.format list) = PP.breakIndent 2 (PP.vblock formats)

in

val resultId = S.strSymbol "<resultStr>"

(* strToEnv : M.Signature * EE.entityEnv -> SE.staticEnv *)
fun strToEnv (M.SIG {elements,...},entities) =
    let fun bindElem ((sym,spec), env) =
	    case spec
              of M.TYCspec{entVar,...} =>
		  let val tyc = EE.lookTycEnt(entities,entVar)
		   in SE.bind(sym,B.TYCbind tyc,env)
		  end
	       | M.STRspec{entVar,sign,...} =>
		  let val strEnt = EE.lookStrEnt(entities,entVar)
		   in SE.bind(sym,B.STRbind(M.STR{sign=sign,rlzn=strEnt,
						  access=A.nullAcc,
						  prim=[]}),
			      env)
		  end
	       | M.CONspec{spec=dcon, ...} => SE.bind(sym,B.CONbind dcon,env)
	       | _ => env
     in foldl bindElem SE.empty elements
    end
  | strToEnv _ = SE.empty

(* sigToEnv : M.Signature -> SE.staticEnv *)
fun sigToEnv(M.SIG {elements,...}) =
    let fun bindElem ((sym,spec), env) =
	  (case spec
            of M.TYCspec{info=M.RegTycSpec{spec,...},...} =>
                SE.bind(sym,B.TYCbind spec,env)
             | M.TYCspec{info=M.InfTycSpec{name,arity},...} =>
                let val tyc =
                        T.GENtyc{stamp=Stamps.special "x", arity=arity,
                                 eq=ref(T.UNDEF), kind=T.FORMAL, stub=NONE,
                                 path=InvPath.extend(InvPath.empty,name)}
                in SE.bind(sym,B.TYCbind tyc,env)
                end
	     | M.STRspec{sign,slot,def,entVar=ev} =>
		 SE.bind(sym,B.STRbind(M.STRSIG{sign=sign,entPath=[ev]}),env)
	     | M.CONspec{spec=dcon, ...} => SE.bind(sym,B.CONbind dcon,env)
	     | _ => env)
     in foldl bindElem SE.empty elements
    end
  | sigToEnv _ = bug "sigToEnv"

(* filter out non-exception data constructors, since they should not be printed (again) *)
fun removeDCons elements = List.filter
      (fn (_,M.CONspec{spec=T.DATACON{rep=A.EXN _,...},...}) => true
	| (_,M.CONspec{spec=dcon,...}) => false
	| _ => true)
      elements

(* fmtEntVar : EP.entVar -> PP.format *)
fun fmtEntVar entVar = PP.text (EntPath.entVarToString entVar)

(* fmtEntPath : EP.entPath -> PP.format *)
fun fmtEntPath entPath = PP.text (EntPath.entPathToString entPath)

(* fmtTycExp : (M.tycExp * int) -> PP.format *)
fun fmtTycExp (tycExp,depth) =
    if depth <= 0 then PP.text "<tycExp>" else
    (case tycExp
       of M.VARtyc ep =>      PP.hcat (PP.text "TE.V:", fmtEntPath ep)
        | M.CONSTtyc tycon => PP.hcat (PP.text "TE.C:", fmtTycon SE.empty tycon)
        | M.FORMtyc tycon =>  PP.hcat (PP.text "TE.FM:", fmtTycon SE.empty tycon))

(* fmtStructureName : SE.staticEnv -> M.Structure -> PP.format *)
fun fmtStructureName env (str as M.STR {rlzn, ...}) =
      let val rpath = #rpath rlzn
	  fun check str' = MU.eqOrigin(str',str)
	  fun look sym = SOME (LU.lookStr (env, sym, (fn _ => raise StaticEnv.Unbound)))
			   handle StaticEnv.Unbound => NONE
	  val (syms,found) = ConvertPaths.findPath (rpath, check, look)
	  val pathFmt = PPP.fmtSymPath (SymPath.SPATH syms)
      in if found
	 then pathFmt
	 else PP.cblock [PP.text "?", PP.period, pathFmt]
      end
  | fmtStructureName _ _ = bug "fmtStructureName"

(* fmtVariable : SE.staticEnv -> V.variable -> PP.format *)
fun fmtVariable env var =
    (case var
      of V.VALvar {path, access, typ, ...} =>
           PP.hblock [PPP.fmtSymPath path, PPVal.fmtAccess access, PP.colon, PPT.fmtType env (!typ)]
       | V.OVLDvar {name, variants} => PPS.fmtSym (name)
       | V.ERRORvar => PP.text "<ERRORvar>")

(* fmtConBinding : SE.staticEnv -> T.datacon -> PP.format *)
fun fmtConBinding env dcon =
    (case dcon
       of T.DATACON{name, typ, rep=A.EXN _, ...} =>
	    PP.hblock
	      [PP.text "exception", PPS.fmtSym name,
               if BasicTypes.isArrowType typ
               then PP.hcat (PP.text "of", fmtType env (BasicTypes.domain typ))
               else PP.empty]
	| T.DATACON {name,typ,...} =>
	    PP.hblock [PP.text "datacon ", PPS.fmtSym name, PP.colon, fmtType env typ])

(* fmtStructure : SE.staticEnv -> (M.Structure * int) -> PP.format *)
fun fmtStructure env (str, depth) =
      (case str
	 of M.STR { sign, rlzn as { entities, ... }, prim, ... } =>
              PP.vcat
	        (PP.text "STR",
		 viblock
		    [PP.label "sign:" (fmtSignature0 env (sign, SOME entities, depth-1)),
		     PP.label "rlzn:" (fmtStrEntity env (rlzn, depth-1)),
		     PP.label "prim:" (PPPrim.fmtStrPrimInfo prim)]) (* or PPPrim.fmtStrPrimInfo prim ? *)
			(* GK: This should be cleaned up soon so as to use a
			   fmtStrInfo that is an actual pretty printer conforming
			   to the pattern of the other pretty printers. *)
	  | M.STRSIG _ => PP.text "<strsig>"
	  | M.ERRORstr => PP.text "<error str>")

(* fmtElement : (M.enityEnv option * SE.staticEnv) 
	       -> (S.symbol * M.spec * int)
	       -> PP.format *)
and fmtElement (entityEnvOp, env) (sym, spec, depth) =
    (case spec
       of M.STRspec {sign, entVar, def, slot} =>
            PP.vblock
	      [PP.hblock [PP.text "structure", PPS.fmtSym sym,
		  	  PP.brackets (PP.label "entVar" (fmtEntVar entVar)),
			  PP.colon],
	       PP.indent 2
		 (case entityEnvOp
		    of NONE => fmtSignature0 env (sign, NONE, depth-1)
		     | SOME eenv =>
			 let val {entities,...} =
				 case EE.look(eenv,entVar)
				   of M.STRent e => e
				    | _ => bug "fmtElement:STRent"
			  in fmtSignature0 env (sign, SOME entities, depth-1)
			 end)]

	| M.FCTspec {sign, entVar, slot} =>
            PP.vblock
	      [PP.hblock
                 [PP.text "functor", PPS.fmtSym sym,
	          PP.brackets (PP.label "entVar" (fmtEntVar entVar)),
		  PP.colon],
               PP.indent 2 (fmtFunsig env (sign, depth-1))]

	| M.TYCspec {entVar, info} =>
	    (case info
	       of M.RegTycSpec{spec,repl,scope} =>
		   PP.vblock
		    [(case entityEnvOp
		       of NONE =>
			    if repl
			    then fmtReplBind env spec
			    else fmtTycBind env spec
			| SOME eenv =>
			    (case EE.look(eenv,entVar)
			      of M.TYCent tyc =>
				 if repl
				 then fmtReplBind env tyc
				 else fmtTycBind env tyc
			       | M.ERRORent => PP.text "<ERRORent>"
			       | _ => bug "fmtElements:TYCent")),
		    PP.label "entVar" (fmtEntVar entVar),
		    PP.label "scope" (PP.integer scope)]

		| M.InfTycSpec{name,arity} =>
		   PP.vcat
		    ((case entityEnvOp
			of NONE => PP.hblock [PP.text "type", fmtFormals arity, PPS.fmtSym name]
			 | SOME eenv =>
			     (case EE.look(eenv,entVar)
				of M.TYCent tyc => fmtTycBind env tyc
				 | M.ERRORent => PP.text "<ERRORent>"
				 | _ => bug "fmtElements:TYCent")),
		     PP.label "entVar" (fmtEntVar entVar)))

	| M.VALspec{spec=typ,...} =>
	    PP.hblock [PP.text "val", PPS.fmtSym sym, PP.colon, fmtType env typ]

	| M.CONspec{spec=dcon as T.DATACON{rep=A.EXN _,...}, ...} =>
	    fmtConBinding env dcon

	| M.CONspec{spec=dcon,...} => fmtConBinding env dcon
        (* end case *))
        (* end fmtElement *)

(* fmtSignature0 : SE.staticEnv -> (M.Signature * M.entityEnv option * int) -> PP.format *)
and fmtSignature0 env (sign, entityEnvOp, depth: int) =
    let val env = SE.atop(case entityEnvOp
			    of NONE => sigToEnv sign
			     | SOME entEnv => strToEnv(sign,entEnv),
			  env)

	fun fmtConstraints (variety, constraints : M.sharespec list) =
 	      PP.vblock
		(map (fn paths =>
		         PP.pblock
			   [PP.text "sharing ", PP.text variety,
		            PP.psequence PP.equal (map PPP.fmtSymPath paths)])
		      constraints)

     in if depth <= 0 then PP.text "<sig>" else
        (case sign
	   of M.SIG {stamp, name, elements, typsharing, strsharing, ...} =>
		let (* Filter out ordinary dcons that do not print in fmtElements
		       for element printing so that we do not print the spurious
		       newline. We still use the unfiltered elements
		       for determining whether the sig ... end should be
		       multiline even with just one datatype. [DBM ???] *)
		    val nonConsElems = removeDCons elements
		 in PP.vcat 
		      (PP.text "Signature.SIG:",
		       viblock
			 [PP.label "stamp:" (PP.text (Stamps.toShortString stamp)),
			  PP.label "label:"
			    (case name
			       of NONE => PP.text "ANONYMOUS"
				| SOME sym => PP.hcat (PP.text "NAMED", PPS.fmtSym sym)),
			  (case elements
			     of nil => PP.empty
			      | _ => PP.label "elements:"
				       (viblock
					  (map (fn (sym, spec) =>
						   fmtElement (entityEnvOp, env) (sym, spec, depth))
					       nonConsElems))),
			  (case strsharing
			     of nil => PP.empty
			      | _ => PP.label "strsharing:" (fmtConstraints ("",strsharing))),
			  (case typsharing
			     of nil => PP.empty
			      | _ => PP.label "tycsharing:" (fmtConstraints ("type ",typsharing)))])
		end
	    | M.ERRORsig => PP.text "<error sig>")
    end (* end fmtSignature0 *)

(* fmtFunsig : SE.staticEnv -> (M.fctSig * int) -> PP.format *)
and fmtFunsig env (sign, depth) =
    let fun trueBodySig (orig as M.SIG { elements =
					 [(sym, M.STRspec { sign, ... })],
					 ... }) =
	    if Symbol.eq (sym, resultId) then sign else orig
	  | trueBodySig orig = orig
    in if depth<=0 then PP.text "<fctsig>" else
       (case sign
	  of M.FSIG {paramsig, paramvar, paramsym, bodysig, ...} =>
               PP.vcat
		 (PP.text "Funsig.FSIG:",
		  viblock
		    [PP.label "psig:" (fmtSignature0 env (paramsig, NONE, depth-1)),
		     PP.label "pvar:" (PP.text (EntPath.entVarToString paramvar)),
		     PP.label "psym:"
			(case paramsym
			  of NONE => PP.text "<anonymous>"
			   | SOME sym => PPS.fmtSym sym),
		    PP.label "bsig:" (fmtSignature0 env (bodysig, NONE, depth-1))])
	   | M.ERRORfsig => PP.text "<error fsig>")
    end

(* fmtStrEntity : SE.staticEnv -> (M.strEntity * int) -> PP.format *)
and fmtStrEntity env ({stamp, entities, properties, rpath, stub}: M.strEntity, depth) =
      if depth <= 1
      then PP.text "<structure entity>"
      else PP.vcat
	       (PP.text "strEntity:",
		viblock
		  [PP.label "rpath:" (PPP.fmtInvPath rpath),
		   PP.label "stamp:" (PP.text (Stamps.toShortString stamp)),
		   PP.label "entities:" (fmtEntityEnv env (entities, depth-1)),
		   PP.label "lambdaty:" PP.empty])

(* fmtFctEntity : SE.staticEnv -> (M.fctEntity * int) -> PP.format *)
and fmtFctEntity env ({stamp,closure,properties,tycpath,rpath,stub}: M.fctEntity, depth) =
    if depth <= 0
    then PP.text "<functor entity>"
    else PP.vcat
	   (PP.text "fctEntity:",
	    viblock
	      [PP.label "rpath:" (PPP.fmtInvPath rpath),
	       PP.label "stamp:" (PP.text (Stamps.toShortString stamp)),
	       PP.label "closure:" (fmtClosure (closure,depth-1)),
	       PP.label "lambdaty:" PP.empty,
	       PP.label "tycpath:" (PP.text "--tycpath formatting not implemented--")])

(* fmtFunctor : SE.staticEnv -> (M.Functor * int) -> PP.format *)
and fmtFunctor env (fct, depth) =
    if depth <= 0 then PP.text "<functor>" else
    (case fct
       of (M.FCT { sign, rlzn, ... }) =>
	    PP.vcat
              (PP.text "Functor.FCT:",
	       viblock
	         [PP.label "sign:" (fmtFunsig env (sign, depth-1)),
		  PP.label "rlzn:" (fmtFctEntity env (rlzn, depth-1))])
	| M.ERRORfct => PP.text "Functor.ERROR")

(* fmtTycBind : SE.staticEnv -> T.tycon -> PP.format *)
and fmtTycBind (env: SE.staticEnv) (tyc: T.tycon) =
    let fun visibleDcons (tyc, dcons) =
	    let fun find ((actual as {name,rep,domain}) :: rest) =
		     (case LU.lookIdSymOp (env, name)
		        of SOME(AS.CON dcon) =>
		           (* test whether the datatypes of actual and found
			    * constructor agree *)
			    (case TU.dataconTyc dcon
			       of tyc1 as T.GENtyc _ =>
				  (* the expected form in structures *)
				     if TU.eqTycon(tyc,tyc1)
				     then dcon :: find rest
				     else find rest
				| T.PATHtyc _ => dcon :: find rest
				  (* the expected form in signatures;
				     we won't check visibility [dbm] *)
				| tycon =>
				  (* something's weird *)
				   (PP.printFormatNL
				      (PP.vblock
				         [PP.text "fmtTycBind failure",
					  PP.label "looking for:" (fmtTycon env tyc),
					  PP.label "found:" (fmtTycon env tycon)]);
				    find rest))
			| NONE => find rest)
		  | find [] = []
	     in find dcons
	    end
     in PP.hcat (PP.text "type", fmtTycon env tyc)  (* fmtTycon should be internals version *)
    end (* fmtTycBind *)

(* fmtReplBind : SE.staticEnv -> T.tycon -> PP.format *)
and fmtReplBind (env: SE.staticEnv) (tyc: T.tycon) =
    (case tyc
       of T.DEFtyc{tyfun=T.TYFUN{body=T.CONty(rightTyc,_),...},path,...} =>
	   (* [GK 5/4/07] Does this case ever occur? All datatype
	      replication tycs are GENtycs after elaboration *)
	      PP.hblock
		[PP.text "datatype", PPS.fmtSym (PN.getTycNameIP path), PP.equal,
		 PP.text "datatype", fmtTycon env rightTyc]
	 | (tyc as T.GENtyc{stamp, arity, eq, kind, path, stub}) =>
	      PP.hblock
		[PP.text "datatype", PPS.fmtSym (PN.getTycNameIP path), PP.equal, fmtTycBind env tyc]
	 | T.PATHtyc _ => ErrorMsg.impossible "<replbind:PATHtyc>"
	 | T.RECtyc _ => ErrorMsg.impossible "<replbind:RECtyc>"
	 | T.FREEtyc _ => ErrorMsg.impossible "<replbind:FREEtyc>"
	 | _ => ErrorMsg.impossible "fmtReplBind")

(* fmtEntity : SE.staticEnv -> (M.entity * int) -> int *)
and fmtEntity env (entity, depth) =
    case entity
      of M.TYCent tycon => fmtTycon env tycon
       | M.STRent strEntity => fmtStrEntity env (strEntity, depth-1)
       | M.FCTent fctEntity => fmtFctEntity env (fctEntity, depth-1)
       | M.ERRORent => PP.text "ERRORent"

(* fmtEntityEnv : SE.staticEnv -> (M.entityEnv * int) -> PP.format *)
and fmtEntityEnv env (entEnv, depth) =
    if depth <= 0 then PP.text "<entityEnv>" else
    let fun fmt (entVar,entity) =
	    PP.vcat (PP.hcat (PP.text (EntPath.entVarToString entVar), PP.colon),
		     PP.indent 2 (fmtEntity env (entity, depth-1)))
     in PP.vblock (map fmt (EE.toList entEnv))
    end

(* fmtEntDec : (M.entityDec * int) -> PP.format *)
and fmtEntDec (entDec, depth) =
    if depth <= 0 then PP.text "<entDec>" else
    (case entDec
       of M.TYCdec(entVar,tycExp) =>
	   PP.label "ED.TYC:"
	     (PP.hcat (fmtEntVar entVar, fmtTycExp (tycExp,depth-1)))
	| M.STRdec(entVar,strExp,sym) =>
	   PP.label "ED.STR:"
	     (PP.hblock [PPS.fmtSym sym, fmtEntVar entVar, fmtStrExp (strExp,depth-1)])
	| M.FCTdec(entVar,fctExp) =>
	   PP.label "ED.FCT:"
	     (PP.hcat (fmtEntVar entVar, fmtFctExp (fctExp,depth-1)))
	| M.SEQdec entityDecs =>
	   PP.vcat
	     (PP.text "ED.SEQ:",
	      PP.indent 2 (PP.vblock (map (fn entDec => fmtEntDec (entDec,depth)) entityDecs)))
	| M.LOCALdec (entityDecL,entityDecB) => PP.text "ED.LOCAL:"
	| M.ERRORdec => PP.text "ED.ERROR:"
	| M.EMPTYdec => PP.text "ED.EMPTY:")

(* fmtStrExp : (M.strExp * int) -> PP.format *)
and fmtStrExp (strExp,depth) =
    if depth <= 0 then PP.text "<strExp>" else
    case strExp
      of M.VARstr ep => PP.label "SE.VAR" (fmtEntPath ep)
       | M.CONSTstr { stamp, rpath, ... } => PP.label "SE.CONST:" (PPP.fmtInvPath rpath)
       | M.STRUCTURE {stamp,entDec} => PP.label "SE.STRUCTURE:" (fmtEntDec (entDec,depth-1))
       | M.APPLY (fctExp,strExp) =>
	   PP.vcat
	     (PP.text "SE.APPLY:",
	      viblock
	        [PP.label "fct:" (fmtFctExp (fctExp, depth-1)),
		 PP.label "arg:" (fmtStrExp (strExp, depth-1))])
       | M.LETstr (entDec,strExp) =>
	  PP.vcat
            (PP.text "SE.LET:",
	     viblock
	       [PP.label "let:" (fmtEntDec (entDec,depth-1)),
		PP.label "in:" (fmtStrExp (strExp, depth-1))])
       | M.ABSstr (sign,strExp) =>
          PP.vcat
            (PP.text "SE.ABS:",
             viblock
	       [PP.label "sign:" (PP.text "<omitted>"),
		PP.label "strExp:" (fmtStrExp (strExp, depth-1))])
       | M.CONSTRAINstr {boundvar,raw,coercion} =>
          PP.vcat
            (PP.text "SE.CONSTRAIN:",
             viblock
               [PP.label "boundvar:" (fmtEntVar boundvar),
		PP.label "source:" (fmtStrExp (raw, depth-1)),
		PP.label "target:" (fmtStrExp (coercion, depth-1))])
       | M.FORMstr(sign) => PP.text "SE.FORM:"

(* fmtFctExp : M.fctExp * int -> PP.format *)
and fmtFctExp (fctExp,depth) =
    if depth <= 0 then PP.text "<fctExp>" else
    case fctExp
      of M.VARfct ep => PP.label "FE.VARfct" (fmtEntPath ep)
       | M.CONSTfct {rpath, ...} => PP.label "FE.CONSTfct" (PP.text (InvPath.toString rpath))
       | M.LAMBDA_TP {param, body, ...} =>
           PP.vcat
	     (PP.text "FE.LAMBDA_TP:",
	      viblock
                [PP.label "param:" (fmtEntVar param),
		 PP.label "body:" (fmtStrExp (body, depth-1))])
       | M.LAMBDA {param, body} =>
           PP.vcat
       	     (PP.text "FE.LAMBDA:",
	      viblock
	        [PP.label "param:" (fmtEntVar param),
		 PP.label "body:" (fmtStrExp (body, depth-1))])
       | M.LETfct (entDec,fctExp) =>
           PP.vcat
             (PP.text "FE.LETfct:",
	      viblock
		[PP.label "decl:" (fmtEntDec (entDec,depth-1)),
		 PP.label "body:" (fmtFctExp (fctExp, depth-1))])

(* fmtClosure : M.fctClosure * int -> PP.format *)
and fmtClosure (M.CLOSURE {param, body, env}, depth) =
      PP.vcat
	(PP.text "CLOSURE:",
	 viblock
	   [PP.label "param:" (fmtEntVar param),
	    PP.label "body:" (fmtStrExp (body, depth-1)),
	    PP.label "env:" (fmtEntityEnv SE.empty (env, depth-1))])

(* fmtBinding : SE.staticEnv -> (S.symbol * B.binding * int) -> PP.format
 *  assumes no newline is needed before pping *)
and fmtBinding (env: SE.staticEnv) (name: S.symbol, binding:B.binding, depth:int) =
    case binding
      of B.VALbind var => PP.hcat (PP.text "val", fmtVariable env var)
       | B.CONbind con => fmtConBinding env con
       | B.TYCbind tycon => fmtTycBind env tycon
       | B.SIGbind sign =>
           PP.pcat (PP.hblock [PP.text "signature", PPS.fmtSym name, PP.equal], 
		    PP.indent 4 (fmtSignature0 env (sign, NONE, depth)))

       | B.FSGbind fs =>
           PP.hblock [PP.text "funsig", PPS.fmtSym name, fmtFunsig env (fs, depth)]

       | B.STRbind str =>
           PP.hblock [PP.text "structure", PPS.fmtSym name, PP.colon, fmtStructure env (str, depth)]

       | B.FCTbind fct =>
           PP.hblock [PP.text "functor", PPS.fmtSym name, PP.colon, PP.text "<sig>"]
           (* DBM -- should print the signature! *)

       | B.FIXbind fixity =>
	   PP.hcat (PP.text (Fixity.fixityToString fixity), PPS.fmtSym name)

(* fmtEnv : (S.symbol list option * SE.staticEnv) -> (SE.staticEnv * int) -> PP.format
   format an environment in the context of the top environment.
   The environment must either be for a signature or be absolute (i.e.
   all types and structures have been interpreted) *)
(* Note: I make a preliminary pass over bindings to remove
         invisible ConBindings -- Konrad.
	 and invisible structures too -- PC *)
and fmtEnv (boundsyms, topenv) (env, depth) =
    let val bindings =
	    case boundsyms
	      of NONE => SE.sort env
	       | SOME syms => foldr (fn (x,bs) =>
				        ((x,SE.look(env,x))::bs
				         handle SE.Unbound => bs))
				[] syms
	val env' = StaticEnv.atop (env,topenv)
	fun fmtb (name, binding) = fmtBinding env' (name, binding, depth)
     in PP.vblock (map fmtb bindings)
    end

(* fmtOpen : SE.staticEnv -> (SP.path * M.Structure * int) -> PP.format *)
fun fmtOpen env (path, str, depth) =
      PP.vcat
	(PP.label "opening" (PPP.fmtSymPath path),
	 if depth < 1 then PP.empty
	 else (case str
		 of M.STR { sign, rlzn as {entities,...}, ... } =>
			(case sign
			   of M.SIG {elements = [],...} => PP.text "<empty sig>"
			    | M.SIG {elements,...} =>
				let fun fmtElem (sym, spec) =
					fmtElement (SOME entities, SE.atop(sigToEnv sign, env))
						   (sym, spec, depth)
				 in PP.vblock (map fmtElem (removeDCons elements))
				end
			    | M.ERRORsig => PP.text "<ERRORsig>")
		  | M.ERRORstr => PP.text "<ERRORstr>"
		  | M.STRSIG _ => bug "fmtOpen"))

(* fmtSignature : SE.staticEnv -> (M.Signature * int) -> PP.format *)
fun fmtSignature (env: SE.staticEnv) (sign: M.Signature, depth: int) =
    fmtSignature0 env (sign, NONE, depth)

end (* local *)
end (* structure PPModules *)

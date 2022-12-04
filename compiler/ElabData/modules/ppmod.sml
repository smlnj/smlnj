(* ppmod.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* 1. modified to use SML/NJ Lib PP. [DBM, 2003/7/30])
 * 2. "internals" printing removed; should be moved to a separate module [DBM, 2021/12/09] *)

signature PPMOD =
sig

  val fmtSignature:  StaticEnv.staticEnv -> Modules.Signature * int -> NewPrettyPrint.format

  val fmtStructure:  StaticEnv.staticEnv -> Modules.Structure * int -> NewPrettyPrint.format

  val fmtOpen:  StaticEnv.staticEnv -> SymPath.path * Modules.Structure * int -> NewPrettyPrint.format

  val fmtStructureName :  StaticEnv.staticEnv -> Modules.Structure -> NewPrettyPrint.format

  val fmtFunsig :  StaticEnv.staticEnv -> Modules.fctSig * int -> NewPrettyPrint.format

  val fmtBinding:  StaticEnv.staticEnv -> Symbol.symbol * Bindings.binding * int -> NewPrettyPrint.format

end (* signature PPMOD *)


structure PPModules : PPMOD =
struct

local
  structure S = Symbol
  structure SP = SymPath
  structure IP = InvPath
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
  structure PPV = PPVal

  fun bug msg = ErrorMsg.impossible("PPModules: "^msg)

  val fmtTycon = PPType.fmtTycon
  val fmtTyfun = PPType.fmtTyfun
  val fmtFormals = PPType.fmtFormals

in

val resultId = S.strSymbol "<resultStr>"

(* strToEnv : M.Signature * EE.entityEnv -> SE.staticEnv *)
fun strToEnv (M.SIG {elements,...}, entities) =
    let fun bindElem ((sym, spec), env) =
	    case spec
              of M.TYCspec {entVar, ...} =>
		  let val tyc = EE.lookTycEnt (entities, entVar)
		   in SE.bind (sym, B.TYCbind tyc, env)
		  end
	       | M.STRspec {entVar, sign, ...} =>
		  let val strEnt = EE.lookStrEnt (entities, entVar)
		   in SE.bind (sym,
			       B.STRbind (M.STR {sign = sign, rlzn = strEnt,
					         access = A.nullAcc,
					         prim = nil}),
			       env)
		  end
	       | M.CONspec {spec = dcon, ...} => SE.bind (sym, B.CONbind dcon, env)
	       | _ => env
     in foldl bindElem SE.empty elements
    end
  | strToEnv _ = SE.empty

(* sigToEnv : M.Signature -> SE.staticEnv *)
fun sigToEnv(M.SIG {elements,...}) =
    let fun bindElem ((sym,spec), env) =
	  (case spec
            of M.TYCspec {info = M.RegTycSpec {spec,...}, ...} =>
                SE.bind(sym, B.TYCbind spec, env)
             | M.TYCspec {info = M.InfTycSpec {name, arity}, ...} =>
                let val tyc =
                        T.GENtyc {stamp = Stamps.special "x", arity = arity,
                                  eq = ref(T.UNDEF), kind = T.FORMAL, stub = NONE,
                                  path = IP.extend (IP.empty,name)}
                 in SE.bind (sym, B.TYCbind tyc, env)
                end
	     | M.STRspec {sign, slot, def, entVar=ev} =>
		 SE.bind (sym, B.STRbind (M.STRSIG {sign = sign, entPath = [ev]}), env)
	     | M.CONspec {spec = dcon, ...} => SE.bind (sym, B.CONbind dcon, env)
	     | _ => env)
     in foldl bindElem SE.empty elements
    end
  | sigToEnv _ = bug "sigToEnv"

(* visibleConBinding (is_ppable_conBinding) and visibleBindings (all_ppable_bindings)
 * are currently not used!  (used in ppEnv in ppmod-new.sml)

(* visibleConBinding : T.datacon * SE.staticEnv -> bool
 * Support for a hack to make sure that non-visible ConBindings don't
 * cause spurious blank lines when pp-ing signatures.
 *)
fun visibleConBinding (T.DATACON{rep=A.EXN _, ...}, _) = true
  | visibleConBinding (dcon,env) =
      let exception Hidden
	  val visibleDconTyc =
	        let val tyc = TU.dataconTyc dcon
		 in (TU.equalTycon
		      (LU.lookTyc
			 (env,
			  (case TU.tycPath tyc
			     of SOME ipath => PN.getTycNameIP ipath  (* PN = PathName *)
			      | NONE => bug "visibleConBinding"),
			  fn _ => raise Hidden),
		       tyc)
		       handle Hidden => false)
		end
       in not visibleDconTyc
      end

(* visibleBindings : (S.symbol * B.binding) list -> SE.staticEnv -> bool list *)
fun visibleBindings alist env =
    List.filter (fn (name, B.CONbind con) => visibleConBinding(con,env)
                  | b => true)
                alist
*)

fun printableSpec (M.CONspec {spec = T.DATACON {rep, ...}, ...}) =
      (case rep of A.EXN _ => true | _ => false)
  | printableSpec _ = true

(* filter out non-exception data constructors, since they should not be printed *)
fun removeDCons elements =
    List.filter (fn (_, spec) => printableSpec spec) elements

(* fmtEntVar : EP.entVar -> PP.format *)
fun fmtEntVar entVar =
    PP.text (EntPath.entVarToString entVar)

(* fmtEntPath : EP.entPath -> PP.format *)
fun fmtEntPath entPath =
    PP.text (EntPath.entPathToString entPath)

(* fmtTycExp : (M.tycExp * int) -> PP.format *)
fun fmtTycExp (tycExp, depth) =
    if depth <= 0 then PP.text "<tycExp>" else
    case tycExp
      of M.VARtyc ep =>
	  PP.hcat (PP.text "TE.V:", fmtEntPath ep)
       | M.CONSTtyc tycon =>
	  PP.hcat (PP.text "TE.C:", fmtTycon SE.empty tycon)
       | M.FORMtyc tycon =>
	  PP.hcat (PP.text "TE.FM:", fmtTycon SE.empty tycon)

(* fmtStructureName : SE.staticEnv -> M.Structure -> PP.format *)
fun fmtStructureName env (str as M.STR {rlzn, ...}) =
      let val rpath = #rpath rlzn
	  fun check str' = MU.eqOrigin(str',str)
	  fun look sym = SOME (LU.lookStr (env, sym, (fn _ => raise StaticEnv.Unbound)))
			   handle StaticEnv.Unbound => NONE
	  val (syms,found) = ConvertPaths.findPath (rpath, check, look)
	  val pathFmt = PPP.fmtSymPath (SP.SPATH syms)
       in if found then pathFmt
	  else PP.ccat (PP.text "?", pathFmt)
      end
  | fmtStructureName _ _ = bug "ppStructureName"

(* fmtStructure : SE.staticEnv -> M.Structure * int -> PP.format *)
fun fmtStructure env (str, depth) =
      (case str
	 of M.STR { sign, rlzn as { entities, ... }, prim, ... } =>
	     (case sign
		of M.SIG { name = SOME sym, ... } =>
		     ((if MU.eqSign
			  (sign, LU.lookSig (env,sym,(fn _ => raise SE.Unbound)))
		       then PPS.fmtSym sym
		       else (PPS.fmtSym sym; PP.text "?"))
		      handle SE.Unbound => (PPS.fmtSym sym; PP.text "?"))
		 | M.SIG { name = NONE, ... } =>
		     if depth <= 1
		     then PP.text "<sig>"
		     else fmtSignature0 (sign, env, depth-1, (* true, *) SOME entities)
		 | M.ERRORsig => PP.text "<error sig>")
	  | M.STRSIG _ => PP.text "<strsig>"
	  | M.ERRORstr => PP.text "<error str>")

(* fmtElement : (SE.staticEnv * int * M.enityEnv option)
	       -> (S.symbol * M.spec)
	       -> PP.format *)
and fmtElement (env, depth, entityEnvOp) (sym, spec) =
    (case spec
       of M.STRspec {sign, entVar, def, slot} =>
	    PP.pcat
              (PP.hblock [PP.text "structure", PPS.fmtSym sym, PP.colon],
	       PP.softIndent 2 
		 (case entityEnvOp
		    of NONE => fmtSignature0 (sign, env, depth-1, NONE)
		     | SOME eenv =>
			 let val {entities,...} =
				 case EE.look(eenv,entVar)
				   of M.STRent e => e
				    | _ => bug "fmtElement:STRent"
			  in fmtSignature0 (sign, env, depth-1, SOME entities)
			 end
		    (* end case *)))

	| M.FCTspec {sign, entVar, slot} =>
            PP.pcat
	      (PP.hblock [PP.text "functor ", PPS.fmtSym sym, PP.colon],
	       PP.softIndent 2 (fmtFunsig env (sign, depth-1)))

	| M.TYCspec {entVar, info} =>
	   (case info
	      of M.RegTycSpec{spec,repl,scope} =>
		   (case entityEnvOp
		      of NONE =>
			   if repl
			   then fmtReplBind (spec,env)
			   else fmtTycBind (spec,env)
		       | SOME eenv =>
			   (case EE.look(eenv,entVar)
			     of M.TYCent tyc =>
				  if repl
				  then fmtReplBind (tyc,env)
				  else fmtTycBind (tyc,env)
			      | M.ERRORent => PP.text "<ERRORent>"
			      | _ => bug "fmtElements:TYCent 1"))
	       | M.InfTycSpec{name,arity} =>
		   (case entityEnvOp
		      of NONE => PP.hblock [PP.text "type", fmtFormals arity, PPS.fmtSym name]
		       | SOME eenv =>
			   (case EE.look(eenv,entVar)
			      of M.TYCent tyc => fmtTycBind (tyc,env)
			       | M.ERRORent => PP.text "<ERRORent>"
			       | _ => bug "fmtElements:TYCent 2")))


	| M.VALspec {spec, ...} =>
	    PP.hblock [PP.text "val", PPS.fmtSym sym, PP.colon, PPT.fmtType env spec]

	| M.CONspec {spec = dcon as T.DATACON{rep=A.EXN _, ...}, ...} =>
            (* exception constructor *)
	    PPV.fmtConBinding (env, dcon)

	| M.CONspec _ => PP.empty
	    (* We don't print ordinary, non-exception, data constructor elements
	     * because they are printed with their datatype. *)
        (* end case *))
        (* end fmtElement *)

(* fmtSignature0 : M.Signature * SE.staticEnv * int * M.entityEnv option -> PP.format *)
and fmtSignature0 (sign, env, depth: int, entityEnvOp) =
    let val env = SE.atop (case entityEnvOp
			     of NONE => sigToEnv sign
			      | SOME entEnv => strToEnv(sign,entEnv),
			   env)

	fun fmtConstraints (variety: PP.format, constraints : M.sharespec list) =
	      PP.vblock
		(map 
		  (fn paths =>
		        PP.hcat
			  (PP.hcat (PP.text "sharing ", variety),
			   PP.psequence (PP.text " =") (map PPP.fmtSymPath paths)))
		  constraints)

     in if depth <= 0 then PP.text "<sig>"
	else case sign
	       of M.SIG {stamp, name, elements, typsharing, strsharing, ...} =>
		  let (* Filter out ordinary dcons that do not print in fmtElements
			 for element printing so that we do not print the spurious
			 newline. We still use the unfiltered elements
			 for determining whether the sig ... end should be
			 multiline even with just one datatype. [DBM ???] *)
		     val nonConsElems = removeDCons elements

		     fun big (_, M.STRspec _) = true
		       | big (_, M.TYCspec{info=M.RegTycSpec
						    {spec=T.GENtyc{kind=T.DATATYPE _, ...},
						     ...},
					   ...}) = true
		       | big _  = not (null typsharing) orelse not (null strsharing)

		     fun fmtsig () =
			   PP.vblock
			     [PP.text "sig",
			      PP.hardIndent 2 
				(PP.vblock
				   (List.concat
				     [map (fmtElement (env,depth,entityEnvOp)) nonConsElems,
				      case strsharing
					of nil => nil
					 | _ => [fmtConstraints (PP.empty, strsharing)],
				      case typsharing
					of nil => nil
					 | _ => [fmtConstraints (PP.text "type", typsharing)]])),
			      PP.text "end"]

		   in case nonConsElems
			of nil => PP.text "sig end"
			 | [elem] =>
			     if big elem then fmtsig ()
			     else PP.hblock
				    [PP.text "sig",
				     fmtElement (env, depth, entityEnvOp) elem,
				     PP.text "end"]
			 | _ => fmtsig ()
		  end (* let -- binding: nonConsElems, big, fmtsig *)
		      
		| M.ERRORsig => PP.text "<error sig>"
    end (* end fmtSignature0 *)

(* fmtFunsig : SE.staticEnv -> M.fctSig * int -> PP.format *)
and fmtFunsig env (sign, depth) =
    let fun trueBodySig (orig as M.SIG { elements = [(sym, M.STRspec { sign, ... })], ... }) =
	      if Symbol.eq (sym, resultId) then sign else orig
	  | trueBodySig orig = orig
     in if depth<=0 then PP.text "<fctsig>"
	else case sign
	       of M.FSIG {paramsig, paramvar, paramsym, bodysig, ...} =>
		  let val trueBodySig =
			  (case bodysig
			     of M.SIG {elements = [(sym, M.STRspec { sign, ... })], ... } =>
			        if Symbol.eq (sym, resultId) then sign else bodysig
			      | _ => bodysig)
		   in PP.pblock
			[PP.parens
			  (PP.hblock
			    [case paramsym
			       of SOME x => PPS.fmtSym x
				| _ => PP.text "<param>",
			     PP.colon,
			     fmtSignature0 (paramsig, env, depth-1, NONE)]),
			 PP.colon,
			 fmtSignature0 (trueBodySig, env, depth-1, NONE)]
		  end
		| M.ERRORfsig => PP.text "<ERRORfsig>"
    end (* fun fmtFunsig *)

(* fmtTycBind : T.tycon * SE.staticEnv -> PP.format *)
and fmtTycBind (tyc, env) =
    let fun visibleDcons (tyc, dcons) =
	    let fun find ((actual as {name,rep,domain}) :: rest) =
		     (case LU.lookIdSymOp (env, name)
		        of SOME(AS.CON dcon) =>
		           (* test whether the datatypes of actual and found constructor agree *)
			    (case TU.dataconTyc dcon
			       of tyc1 as T.GENtyc _ =>
				  (* the expected form in structures *)
				     if TU.eqTycon(tyc,tyc1)
				     then dcon :: find rest
				     else find rest
				| T.PATHtyc _ => dcon :: find rest
				  (* the expected form in signatures;
				     we won't check visibility [dbm] *)
				| tycon => bug "fmtTycBind..visibleDcons..find")
			| NONE => find rest)
		  | find [] = []
	     in find dcons
	    end
	fun fmtDcon (T.DATACON {name, typ,...}) =
	    PP.hcat
	      (PPS.fmtSym name,
	       let val typ = 
		       (case typ
			  of (T.POLYty{tyfun=T.TYFUN{body,...},...}) => body
			   | _ => typ)
		in if BT.isArrowType typ
		   then PP.hcat (PP.text "of", PPT.fmtType env (BT.domain typ))
		   else PP.empty
	       end)
    in case tyc
         of T.GENtyc { path, arity, eq, kind, ... } =>
	      let val tycNameFmt : PP.format = PPP.fmtTycName path
	       in case kind
		   of T.ABSTRACT _ =>
			(* abstype *)
			PP.hblock [PP.text "type", PPT.fmtFormals arity, tycNameFmt]
		    | T.DATATYPE {index, family = {members, ...}, ...} =>
			(* ordinary datatype *)
			let val {dcons,...} = Vector.sub(members,index)
			    val visdcons = visibleDcons(tyc,dcons)
			    val incomplete = length visdcons < length dcons
			in PP.hcat
			     (PP.hblock [PP.text "datatype", PPT.fmtFormals arity, tycNameFmt, PP.equal],
			      case visdcons
				of nil => PP.text "..."
				 | first :: rest =>
				   PP.pblock
				     (fmtDcon first ::
				      (map (fn dcon => PP.hcat (PP.text " |", fmtDcon dcon)) rest
				       @ [if incomplete then PP.text "..." else PP.empty])))
			end
		    | _ =>
			PP.hblock [if EqTypes.isEqTycon tyc then PP.text "eqtype" else PP.text "type",
				   PPT.fmtFormals arity, tycNameFmt]
	      end
	  | T.DEFtyc {path, tyfun=T.TYFUN{arity,body}, ...} =>
	      PP.hblock
		[PP.text "type", PPT.fmtFormals arity, PPP.fmtTycName path, PP.equal,
		 PPT.fmtType env body]
	  | T.ERRORtyc => PP.text "ERRORtyc"
	  | T.PATHtyc _ => PP.hcat (PP.text "PATHtyc:", fmtTycon env tyc)
	  | tycon => PP.hcat (PP.text "strange tycon: ", fmtTycon env tycon)
    end (* fun fmtTycBind *)

(* fmtReplBind : T.tycon * SE.staticEnv -> PP.format *)
(* ??? Needs revision? See comment in DEFtyc case. *)
and fmtReplBind (tycon, env) =
    (case tycon
       of T.GENtyc{stamp, arity, eq, kind, path, stub} =>
	   PP.hblock
	     [PP.text "datatype", PPP.fmtTycName path, PP.equal, fmtTycBind (tycon, env)]
        | T.DEFtyc{tyfun=T.TYFUN{body=T.CONty(rightTyc,_),...},path,...} =>  (* ??? *)
	   (* [GK 5/4/07] Does this case ever occur? All datatype
	      replication tycs are GENtycs after elaboration *)
	   PP.hblock
             [PP.text "datatype", PPP.fmtTycName path, PP.equal,
              PP.text "datatype", fmtTycon env rightTyc]
	| T.PATHtyc _ => ErrorMsg.impossible "<replbind:PATHtyc>"
	| T.RECtyc _  => ErrorMsg.impossible "<replbind:RECtyc>"
	| T.FREEtyc _ => ErrorMsg.impossible "<replbind:FREEtyc>"
	| _ => ErrorMsg.impossible "fmtReplBind")

(* fmtBinding : SE.staticEnv ->  (S.symbol * B.binding * int) -> PP.format *)
and fmtBinding (env: SE.staticEnv) (name: S.symbol, binding: B.binding, depth: int) =
    case binding
      of B.VALbind var => PP.hcat (PP.text "val", PPV.fmtVarTyped (env,var))
       | B.CONbind con => PPV.fmtConBinding (env, con)
       | B.TYCbind tycon => fmtTycBind (tycon, env)
       | B.SIGbind sign =>
	   PP.hblock [PP.text "signature", PPS.fmtSym name, PP.equal,
		      fmtSignature0 (sign, env, depth, (* true, *) NONE)]
       | B.FSGbind fs =>
	   PP.hblock [PP.text "funsig ", PPS.fmtSym name, PP.equal, fmtFunsig env (fs, depth)]
       | B.STRbind str =>
	   PP.hblock [PP.text "structure", PPS.fmtSym name, PP.colon, fmtStructure env (str, depth)]
       | B.FCTbind fct =>
	   (case fct
	     of M.FCT {sign, ...} =>
		  PP.hblock [PP.text "functor", PPS.fmtSym name, PP.colon, fmtFunsig env (sign, depth)]
	      | M.ERRORfct => PP.empty)
       | B.FIXbind fixity =>
	   PP.hcat (PP.text (Fixity.fixityToString fixity), PPS.fmtSym name)

(* fmtOpen : SE.staticEnv -> (SP.path * M.Structure * int) -> PP.format *)
fun fmtOpen env (path, str, depth) =
      PP.vcat
	(PP.hcat (PP.text "opening ", PPP.fmtSymPath path),
	 if depth <= 0 then PP.empty
	 else (case str
		 of M.STR { sign, rlzn as {entities,...}, ... } =>
		      (case sign
			 of M.SIG {elements = [],...} => PP.empty
			  | M.SIG {elements,...} =>
			      let fun fmtElem elem =
				      fmtElement (SE.atop(sigToEnv sign, env),
						  depth, SOME entities)
						 elem
			       in PP.vblock (map fmtElem (removeDCons elements))
			      end
			  | M.ERRORsig => PP.empty)
		  | M.ERRORstr => PP.empty
		  | M.STRSIG _ => bug "ppOpen"))

(* fmtSignature : SE.staticEnv -> (M.Signature * int) -> PP.format *)
fun fmtSignature env (sign, depth) = fmtSignature0 (sign, env, depth, NONE)

end (* local *)
end (* structure PPModules *)

(* ppmod.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* 1. modified to use SML/NJ Lib PP. [DBM, 2003/7/30])
 * 2. "internals" printing removed; should be moved to a separate module [DBM, 2021/12/09] *)

signature PPMOD =
sig

  val fmtSignature:  StaticEnv.staticEnv -> Modules.Signature * int -> NewPP.format

  val fmtStructure:  StaticEnv.staticEnv -> Modules.Structure * int -> NewPP.format

  val fmtOpen:  StaticEnv.staticEnv -> SymPath.path * Modules.Structure * int -> NewPP.format

  val fmtStructureName :  StaticEnv.staticEnv -> Modules.Structure -> NewPP.format

  val fmtFunsig :  StaticEnv.staticEnv -> Modules.fctSig * int -> NewPP.format

  val fmtBinding:  StaticEnv.staticEnv -> Symbol.symbol * Bindings.binding * int -> NewPP.format

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

  structure PP = NewPP
  structure PPU = NewPPUtil

  fun bug msg = ErrorMsg.impossible("PPModules: "^msg)

  val pps = PP.string
  val ppType = PPType.ppType
  val ppTycon = PPType.ppTycon
  val ppTyfun = PPType.ppTyfun
  val ppFormals = PPType.ppFormals

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
                                  path = InvPath.extend(InvPath.empty,name)}
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
			     of SOME ipath => SP.SPATH[IP.last ipath]
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
fun ppTycExp (tycExp, depth) =
    if depth <= 0 then PP.text "<tycExp>" else
    case tycExp
      of M.VARtyc ep =>
	  PP.concat [PP.text "TE.V", PP.colon, fmtEntPath ep]
       | M.CONSTtyc tycon =>
	  PP.concat [PP.text "TE.C", PP.colon, fmtTycon SE.empty tycon]
       | M.FORMtyc tycon =>
	  PP.concat [PP.text "TE.FM", PP.colon, fmtTycon SE.empty tycon]

(* fmtStructureName : SE.staticEnv -> M.Structure -> PP.format *)
fun fmtStructureName env (str as M.STR {rlzn, ...}) =
      let val rpath = #rpath rlzn
	  fun check str' = MU.eqOrigin(str',str)
	  fun look sym = SOME (LU.lookStr (env, sym, (fn _ => raise StaticEnv.Unbound)))
			   handle StaticEnv.Unbound => NONE
	  val (syms,found) = ConvertPaths.findPath (rpath, check, look)
	  val pathFmt = PPU.fmtSymPath (SP.SPATH syms)
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
		       then PPU.fmtSym sym
		       else (PPU.fmtSym sym; PP.text "?"))
		      handle SE.Unbound => (PPU.fmtSym sym; PP.text "?"))
		 | M.SIG { name = NONE, ... } =>
		     if depth <= 1
		     then PP.text "<sig>"
		     else ppSignature0 (sign, env, depth-1, true, SOME entities)
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
              (PP.hblock [PP.text "structure", PPU.fmtSym sym, PP.colon],
	       PP.softIndent (2, 
		 (case entityEnvOp
		    of NONE => fmtSignature0 (sign, env, depth-1, true, NONE)
		     | SOME eenv =>
			 let val {entities,...} =
				 case EE.look(eenv,entVar)
				  of M.STRent e => e
				   | _ => bug "fmtElement:STRent"
			  in fmtSignature0 (sign, env, depth-1, true, SOME entities)
			 end
		    (* end case *))))

	| M.FCTspec {sign, entVar, slot} =>
            PP.pcat
	      (PP.hblock [PP.text "functor ", PPU.fmtSym sym, PP.colon],
	       PP.softIndent (2, fmtFunsig env (sign, depth-1)))

	| M.TYCspec{entVar,info} =>
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
			      | _ => bug "fmtElements:TYCent"))
	       | M.InfTycSpec{name,arity} =>
		   (case entityEnvOp
		      of NONE => PP.hblock [PP.text "type", fmtFormals arity, PPU.fmtSym name]
		       | SOME eenv =>
			   (case EE.look(eenv,entVar)
			      of M.TYCent tyc => fmtTycBind (tyc,env)
			      | M.ERRORent => PP.text "<ERRORent>"
			      | _ => bug "fmtElements:TYCent"))


	| M.VALspec{spec=typ,...} =>
	    PP.hblock [PP.text "val", PPU.fmtSym sym, PP.colon, fmtType env (typ)]

	| M.CONspec{spec=dcon as T.DATACON{rep=A.EXN _,...}, ...} =>
            (* exception constructor *)
	    PPV.fmtConBinding (dcon,env)

	| M.CONspec{spec=dcon,...} => PP.empty
	    (* don't print ordinary data constructor,
	     * because it was printed with its datatype *)
        (* end case *))
        (* end fmtElement *)

(* fmtSignature0 : M.Signature * SE.staticEnv * int * M.entityEnv option -> PP.format *)
and ppSignature0 (sign, env, depth: int, entityEnvOp) =
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
			   PP.sequence {alignment = PP.P, sep = (PP.text " =")}
			     (map PPU.fmtSymPath paths)))
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
			     [PP.text "sig"
			      PP.hardIndent 2 
				 (PP.vblock
				    (List.concat
				      [map (fmtElement (env,depth,entityEnvOp)) nonConsElems,
				       case strsharing
					 of nil => nil
					  | _ => [fmtConstraints (PP.empty, strsharing)],
				       case typsharing
					 of nil => nil
					  | _ => [fmtConstraints (PP.text "type", typsharing)]]))
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
    let fun trueBodySig (orig as 
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
			       of SOME x => PPU.fmtSym x
				| _ => PP.text "<param>",
			     PP.colon,
			     ppSignature0 (paramsig, env, depth-1, NONE)]),
			 PP.colon,
			 ppSignature0 (trueBodySig, env, depth-1, NONE)]
		| M.ERRORfsig => PP.text "<error fsig>"
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
	fun ppDcon (T.DATACON {name, typ,...}) =
	    PP.hcat
	      (PPU.fmtSym name,
	       let val typ = 
		       (case typ
			  of (T.POLYty{tyfun=T.TYFUN{body,...},...}) => body
			   | _ => typ)
		in if BT.isArrowType typ
		   then PP.hcat (PP.text "of", ppType env (BT.domain typ))
		   else ()
	       end)
    in case tyc
         of T.GENtyc { path, arity, eq, kind, ... } =>
	      (case kind
		 of T.ABSTRACT _ =>
		      (* abstype *)
		      PP.hblock [PP.text "type", ppFormals arity, PPU.fmtSym (IP.last path)]
		  | T.DATATYPE {index, family = {members, ...}, ...} =>
		      (* ordinary datatype *)
		      let val {dcons,...} = Vector.sub(members,index)
			  val visdcons = visibleDcons(tyc,dcons)
			  val incomplete = length visdcons < length dcons
		      in PP.hcat
			   (PP.hblock
			      [PP.text "datatype", ppFormals arity, PPU.fmtSym (IP.last path),
			       PP.equal],
			    case visdcons
			      of nil => PP.text "..."
			       | first :: rest =>
				 PP.pblock
				   (fmtDatacon first ::
				    map (fn dcon => PP.hcat (PP.text " |", fmtDatacon dcon))
				      rest
				    @ (if incomplete then PP.text "..." else PP.empty)))
		      end
		  | _ =>
		      PP.hblock [if EqTypes.isEqTycon tyc then PP.text "eqtype" else PP.text "type",
				 ppFormals arity;
				 PPU.fmtSym (IP.last path)])
	  | T.DEFtyc {path, tyfun=T.TYFUN{arity,body}, ...} =>
	      PP.hblock
		[PP.text "type", fmtFormals arity, PPU.fmtSym (InvPath.last path), PP.equal,
		 fmtType env body]
	  | T.ERRORtyc => PP.text "ERRORtyc"
	  | T.PATHtyc _ => PP.hcat (PP.text "PATHtyc:", fmtTycon env tyc)
	  | tycon => PP.hcat (PP.text "strange tycon: ", fmtTycon env tycon)
    end (* fun fmtTycBind *)

(* ppReplBind : PP.stream -> T.tycon * SE.staticEnv -> unit *)
and ppReplBind =
    fn (T.DEFtyc{tyfun=T.TYFUN{body=T.CONty(rightTyc,_),...},path,...}, env) =>
	   (* [GK 5/4/07] Does this case ever occur? All datatype
	      replication tycs are GENtycs after elaboration *)
	   (PP.openHOVBox (PP.Abs 2);
            PP.text "datatype"; break {nsp=1,offset=0};
            PPU.fmtSym (IP.last path);
            PP.text " ="; break {nsp=1,offset=0};
            PP.text "datatype"; break {nsp=1,offset=0};
            ppTycon env rightTyc;
            PP.closeBox)
	 | (tyc as T.GENtyc{stamp, arity, eq, kind, path, stub}, env) =>
	   (PP.openHOVBox (PP.Abs 2);
	    PP.text "datatype"; PP.break {nsp=1,offset=0};
	    PPU.fmtSym (IP.last path);
	    PP.text " ="; PP.break {nsp=1,offset=0};
	    fmtTycBind (tyc, env);
	    PP.closeBox)
	 | (T.PATHtyc _, _) => ErrorMsg.impossible "<replbind:PATHtyc>"
	 | (T.RECtyc _, _) => ErrorMsg.impossible "<replbind:RECtyc>"
	 | (T.FREEtyc _, _) => ErrorMsg.impossible "<replbind:FREEtyc>"
	 | _ => ErrorMsg.impossible "ppReplBind"

(* ppBinding : PP.stream -> SE.staticEnv ->  (S.symbol * B.binding * int) -> unit
 *  assumes no newline is needed before pping *)
and ppBinding (env: SE.staticEnv) (name: S.symbol, binding: B.binding, depth: int) =
    case binding
      of B.VALbind var => PP.hcat (PP.text "val", PPV.fmtVarTyped (env,var))
       | B.CONbind con => ppConBinding (con,env)
       | B.TYCbind tycon => fmtTycBind (tycon,env)
       | B.SIGbind sign =>
	  (PP.openHVBox (PP.Abs 0);
	    PP.text "signature ";
	    PPU.fmtSym name; PP.text " =";
	    PP.break {nsp=1,offset=2};
	    ppSignature0 (sign,env,depth,true,NONE);
	   PP.closeBox)
       | B.FSGbind fs =>
	   (PP.openHVBox (PP.Abs 2);
	     PP.text "funsig "; PPU.fmtSym name;
	     fmtFunsig env (fs, depth);
	    PP.closeBox)
       | B.STRbind str => (
          PP.openHVBox (PP.Abs 0);
	    PP.openHBox;
	      PP.text "structure";
	      PP.space 1;
              PPU.fmtSym name;
	      PP.space 1;
	      PP.text ":";
	      PP.space 1;
            PP.closeBox;
	    ppStructure env (str, depth);
	   PP.closeBox)
       | B.FCTbind fct =>
	  (PP.openHVBox (PP.Abs 0);
	    PP.text "functor ";
	    PPU.fmtSym name;
	    PP.text " : <sig>";  (* DBM -- should print the signature *)
	   PP.closeBox)
       | B.FIXbind fixity =>
	  (PP.text (Fixity.fixityToString fixity); PPU.fmtSym name)

(* ppOpen : PP.stream -> SE.staticEnv -> (SP.path * M.Structure * int) -> unit *)
fun ppOpen env (path, str, depth) =
      (PP.openVBox (PP.Abs 0);
         PP.openVBox (PP.Abs 2);
           PP.openHBox;
	     PP.text "opening ";
             PP.text (SymPath.toString path);
           PP.closeBox;
	   if depth < 1 then ()
	   else (case str
		   of M.STR { sign, rlzn as {entities,...}, ... } =>
			(case sign
			   of M.SIG {elements = [],...} => ()
			    | M.SIG {elements,...} =>
				let fun fmtElem elem =
					(PP.cut;
					 fmtElement (SE.atop(sigToEnv sign, env),
						    depth, SOME entities)
						  elem)
				in PP.openVBox (PP.Abs 0);
				     List.app fmtElem (removeDCons elements);
				   PP.closeBox
				end
			      | M.ERRORsig => ())
		    | M.ERRORstr => ()
		    | M.STRSIG _ => bug "ppOpen");
         PP.closeBox;
       PP.cut;  (* generates final newline (?) *)
       PP.closeBox)

(* ppSignature : PP.stream -> SE.staticEnv -> (M.Signature * int) -> unit *)
fun ppSignature env (sign, depth) =
    ppSignature0 (sign,env,depth,true,NONE)

end (* local *)
end (* structure PPModules *)

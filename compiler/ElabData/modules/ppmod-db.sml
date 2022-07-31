(* ppmod-db.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* Modified to use SML/NJ Lib PP. [dbm, 7/30/03]).
 * Debugging version of module printing showing internals;
 * roughly equivalent to the old ppmod with internals true [DBM, 2021/12/12]. *)

signature PPMOD_DB =
sig

  val ppSignature: PrettyPrint.stream -> StaticEnv.staticEnv
		   -> Modules.Signature * int -> unit

  val ppStructure: PrettyPrint.stream -> StaticEnv.staticEnv
		   -> Modules.Structure * int -> unit

  val ppOpen: PrettyPrint.stream -> StaticEnv.staticEnv
              -> SymPath.path * Modules.Structure * int -> unit
									    
  val ppStructureName : PrettyPrint.stream -> StaticEnv.staticEnv
			-> Modules.Structure -> unit

  val ppFunsig : PrettyPrint.stream -> StaticEnv.staticEnv
		 -> Modules.fctSig * int -> unit

  val ppBinding: PrettyPrint.stream -> StaticEnv.staticEnv
		 -> Symbol.symbol * Bindings.binding * int -> unit

  (* module internals *)
  val ppElement : PrettyPrint.stream -> Modules.entityEnv option * StaticEnv.staticEnv
		   -> Symbol.symbol * Modules.spec * int -> unit

  val ppEntity : PrettyPrint.stream -> StaticEnv.staticEnv
                 -> Modules.entity * int -> unit

  val ppEntityEnv : PrettyPrint.stream -> StaticEnv.staticEnv
		    -> Modules.entityEnv * int -> unit

  val ppFunctor : PrettyPrint.stream -> StaticEnv.staticEnv
		  -> Modules.Functor * int -> unit

  val ppEnv : PrettyPrint.stream -> Symbol.symbol list option * StaticEnv.staticEnv
	      -> StaticEnv.staticEnv * int -> unit

end (* signature PPMOD *)


structure PPModules_DB : PPMOD_DB =
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

  structure PP = PrettyPrint
  structure PU = PPUtil
  open PrettyPrint PPUtil

  fun bug msg = ErrorMsg.impossible("PPModules: "^msg)

  val pps = PP.string
  val ppType = PPType.ppType
  val ppTycon = PPType.ppTycon
  val ppTyfun = PPType.ppTyfun
  val ppFormals = PPType.ppFormals

in

val resultId = S.strSymbol "<resultStr>"

(* strToEnv : M.Signature * EE.entityEnv -> SE.staticEnv *)
fun strToEnv(M.SIG {elements,...},entities) =
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

(* is_ppable_ConBinding : T.datacon * SE.staticEnv -> bool
 * Support for a hack to make sure that non-visible ConBindings don't
 * cause spurious blank lines when pp-ing signatures.
 *)
fun is_ppable_ConBinding (T.DATACON{rep=A.EXN _, ...}, _) = true
  | is_ppable_ConBinding (dcon,env) =
      let exception Hidden
	  val visibleDconTyc =
	        let val tyc = TU.dataconTyc dcon
		 in (TU.equalTycon
		      (LU.lookTyc
			 (env,
			  (case TU.tycPath tyc
			     of SOME ipath => SP.SPATH[IP.last ipath]
			      | NONE => bug "is_ppable_ConBinding"),
			  fn _ => raise Hidden),
		       tyc)
		       handle Hidden => false)
		end
       in true (* (!internals orelse not visibleDconTyc) *)
      end

(* all_ppable_bindings : (S.symbol * B.binding) list -> SE.staticEnv -> bool list *)
fun all_ppable_bindings alist env =
    List.filter (fn (name,B.CONbind con) => is_ppable_ConBinding(con,env)
                  | b => true)
                alist

(* filter out non-exception data constructors, since they should not be printed *)
fun removeDCons elements = List.filter
      (fn (_,M.CONspec{spec=T.DATACON{rep=A.EXN _,...},...}) => true
	| (_,M.CONspec{spec=dcon,...}) => false
	| _ => true)
	elements

(* ppEntVar : PP.stream  -> EP.entVar -> unit *)
fun ppEntVar ppstrm entVar =
    pps ppstrm (EntPath.entVarToString entVar)

(* ppEntPath : PP.stream -> EP.entPath -> unit *)
fun ppEntPath ppstrm entPath =
    pps ppstrm (EntPath.entPathToString entPath)

(* ppTycExp : PP.stream -> (M.tycExp * int) -> unit *)
fun ppTycExp ppstrm (tycExp,depth) =
    if depth <= 0 then pps ppstrm "<tycExp>" else
    case tycExp
      of M.VARtyc ep =>
	  (pps ppstrm "TE.V:"; break ppstrm {nsp=1,offset=1};
	   ppEntPath ppstrm ep)
       | M.CONSTtyc tycon =>
	  (pps ppstrm "TE.C:"; break ppstrm {nsp=1,offset=1};
	   ppTycon SE.empty ppstrm tycon)
       | M.FORMtyc tycon =>
	  (pps ppstrm "TE.FM:"; break ppstrm {nsp=1,offset=1};
	   ppTycon SE.empty ppstrm tycon)

(* ppStructureName : PP.stream -> SE.staticEnv -> M.Structure -> unit *)
fun ppStructureName ppstrm env (str as M.STR {rlzn, ...}) =
      let val rpath = #rpath rlzn
	  fun check str' = MU.eqOrigin(str',str)
	  fun look sym = SOME (LU.lookStr (env, sym, (fn _ => raise StaticEnv.Unbound)))
			   handle StaticEnv.Unbound => NONE
	  val (syms,found) = ConvertPaths.findPath (rpath, check, look)
       in PP.string ppstrm
	    (if found then SP.toString(SP.SPATH syms)
	     else "?" ^ (SP.toString(SP.SPATH syms)))
      end
  | ppStructureName _ _ _ = bug "ppStructureName"

(* ppVariable : PP.stream -> V.variable -> unit *)
fun ppVariable ppstrm =
    let fun ppV(V.VALvar{path,access,typ,prim,btvs}, env:StaticEnv.staticEnv) =
	      (PP.openHVBox ppstrm (PP.Abs 0);
	        PP.string ppstrm (SP.toString path);
	        PPVal.ppAccess ppstrm access;
	        PP.string ppstrm " : "; ppType env ppstrm (!typ);
	       PP.closeBox ppstrm)
	  | ppV (V.OVLDvar {name,variants},env) =
	      (PP.openHVBox ppstrm (PP.Abs 0);
	       ppSym ppstrm (name);
	       PP.closeBox ppstrm)
	  | ppV(V.ERRORvar,_) = PP.string ppstrm "<ERRORvar>"
     in ppV
    end

(* ppConBinding : PP.stream -> T.datacon -> unit *)
fun ppConBinding ppstrm =
    let fun ppCon (T.DATACON{name, typ, rep=A.EXN _, ...}, env) =
	      (PP.openHOVBox ppstrm (PP.Abs 4);
	       pps ppstrm "exception "; ppSym ppstrm name;
               if BasicTypes.isArrowType typ then
                  (pps ppstrm " of "; ppType env ppstrm (BasicTypes.domain typ))
               else ();
	       PP.closeBox ppstrm)
	  | ppCon (con as T.DATACON{name,typ,...},env) =
 	      (PP.openHOVBox ppstrm (PP.Abs 4);
 	       pps ppstrm "datacon ";
	       ppSym ppstrm name; pps ppstrm " : ";
 	       ppType env ppstrm typ;
 	       PP.closeBox ppstrm)
     in ppCon
    end

(* ppStructure : PP.stream -> SE.staticEnv -> (M.Structure * int) -> unit *)
fun ppStructure ppstrm env (str, depth) =
      (case str
	 of M.STR { sign, rlzn as { entities, ... }, prim, ... } =>
	     (PP.openHVBox ppstrm (PP.Abs 2);
	      PP.string ppstrm "STR";
	      PU.nl_indent ppstrm 2;
	      PP.openHVBox ppstrm (PP.Abs 0);
	      PP.string ppstrm "sign:";
	      PP.break ppstrm {nsp=1,offset=2};
	      ppSignature0 ppstrm env (sign, SOME entities, depth-1);
	      PP.newline ppstrm;
	      PP.string ppstrm "rlzn:";
	      PP.break ppstrm {nsp=1,offset=2};
	      ppStrEntity ppstrm env (rlzn, depth-1);
	      PP.newline ppstrm;
	      PP.string ppstrm "prim:";
	      PP.break ppstrm {nsp=1,offset=2};
			(* GK: This should be cleaned up soon so as to use a
			   ppStrInfo that is an actual pretty printer conforming
			   to the pattern of the other pretty printers.
			PrimopId.ppStrInfo prim; *)
	      PPPrim.ppStrPrimInfo ppstrm prim;
	      PP.closeBox ppstrm;
	      PP.closeBox ppstrm)
	  | M.STRSIG _ => PP.string ppstrm "<strsig>"
	  | M.ERRORstr => PP.string ppstrm "<error str>")

(* ppElement : PP.stream
               -> (M.enityEnv option * SE.staticEnv) 
	       -> (S.symbol * M.spec * int)
	       -> unit *)
and ppElement ppstrm (entityEnvOp, env) (sym, spec, depth) =
    (case spec
       of M.STRspec{sign,entVar,def,slot} =>
	   (openHVBox ppstrm (PP.Abs 0);
	     pps ppstrm "structure ";
	     ppSym ppstrm sym; pps ppstrm " :";
	     break ppstrm {nsp=1,offset=2};
	     openHVBox ppstrm (PP.Abs 0);
	      case entityEnvOp
		of NONE => ppSignature0 ppstrm env (sign, NONE, depth-1)
		 | SOME eenv =>
		    let val {entities,...} =
			    case EE.look(eenv,entVar) of
				M.STRent e => e
			      | _ => bug "ppElement:STRent"
		     in ppSignature0 ppstrm env
			  (sign, SOME entities, depth-1)
		    end;
	      newline ppstrm;
	      pps ppstrm "entVar: ";
	      pps ppstrm (EntPath.entVarToString entVar);
	     closeBox ppstrm;
	    closeBox ppstrm)

	| M.FCTspec{sign,entVar,slot} =>
	   (openHVBox ppstrm (PP.Rel 0);
	     pps ppstrm "functor ";
	     ppSym ppstrm sym; pps ppstrm " :";
	     break ppstrm {nsp=1,offset=2};
	     openHVBox ppstrm (PP.Rel 0);
	      ppFunsig ppstrm env (sign, depth-1);
	      newline ppstrm;
	      pps ppstrm "entVar: ";
	      pps ppstrm (EntPath.entVarToString entVar);
	     closeBox ppstrm;
	    closeBox ppstrm)

	| M.TYCspec{entVar,info} =>
	   (case info
	      of M.RegTycSpec{spec,repl,scope} =>
		 (openHVBox ppstrm (PP.Rel 0);
		   case entityEnvOp
		    of NONE =>
		       if repl then
			   ppReplBind ppstrm (spec,env)
		       else ppTycBind ppstrm (spec,env)
		     | SOME eenv =>
		       (case EE.look(eenv,entVar)
			 of M.TYCent tyc =>
			    if repl then
				ppReplBind ppstrm (tyc,env)
			    else ppTycBind ppstrm (tyc,env)
			  | M.ERRORent => pps ppstrm "<ERRORent>"
			  | _ => bug "ppElements:TYCent");
		   newline ppstrm;
		   pps ppstrm "entVar: ";
		   pps ppstrm (EntPath.entVarToString entVar);
		   newline ppstrm;
		   pps ppstrm "scope: ";
		   pps ppstrm (Int.toString scope);
		  closeBox ppstrm)
	       | M.InfTycSpec{name,arity} =>
		 (openHVBox ppstrm (PP.Rel 0);
		   case entityEnvOp
		     of NONE =>
			 (pps ppstrm "type";
			  ppFormals ppstrm arity;
			  pps ppstrm " ";
			  ppSym ppstrm name)
		      | SOME eenv =>
			 (case EE.look(eenv,entVar)
			    of M.TYCent tyc =>
				 ppTycBind ppstrm (tyc,env)
			     | M.ERRORent => pps ppstrm "<ERRORent>"
			     | _ => bug "ppElements:TYCent");
		   newline ppstrm;
		   pps ppstrm "entVar: ";
		   pps ppstrm (EntPath.entVarToString entVar);
		  closeBox ppstrm))

	| M.VALspec{spec=typ,...} =>
	   (openHOVBox ppstrm (PP.Rel 4);
	      pps ppstrm "val ";
	      ppSym ppstrm sym;
	      pps ppstrm " : ";
	      ppType env ppstrm (typ);
	    closeBox ppstrm)

	| M.CONspec{spec=dcon as T.DATACON{rep=A.EXN _,...}, ...} =>
	   ppConBinding ppstrm (dcon,env)

	| M.CONspec{spec=dcon,...} => ppConBinding ppstrm (dcon,env)
        (* end case *))
        (* end ppElement *)

(* ppSignature0 : PP.stream -> SE.staticEnv
                  -> (M.Signature * M.entityEnv option *  int) -> unit *)
and ppSignature0 ppstrm env (sign, entityEnvOp, depth: int) =
    let val env = SE.atop(case entityEnvOp
			    of NONE => sigToEnv sign
			     | SOME entEnv => strToEnv(sign,entEnv),
			  env)
	fun ppConstraints (variety, constraints : M.sharespec list) =
	      (PP.openHVBox ppstrm (PP.Abs 0);
		ppvseq ppstrm 0 ""
		 (fn ppstrm => fn paths =>
		     (PP.openHOVBox ppstrm (PP.Abs 2);
		       PP.string ppstrm "sharing "; PP.string ppstrm variety;
		       ppSequence ppstrm
			{sep=(fn ppstrm =>
				 (PP.string ppstrm " =";
				  PP.break ppstrm {nsp=1,offset=0})),
			 pr=(fn ppstrm => (fn sympath =>
				PP.string ppstrm (SymPath.toString sympath))),
			 style=INCONSISTENT}
			paths;
		      PP.closeBox ppstrm))
		 constraints;
	       PP.closeBox ppstrm)
     in if depth <= 0
	then PP.string ppstrm "<sig>"
	else case sign
	       of M.SIG {stamp,name,elements,typsharing,strsharing,...} =>
		    let
			(* Filter out ordinary dcons that do not print in ppElements
			   for element printing so that we do not print the spurious
			   newline. We still use the unfiltered elements
			   for determining whether the sig ... end should be
			   multiline even with just one datatype. [DBM ???] *)
		       val nonConsElems = removeDCons elements
		     in PP.openHVBox ppstrm (PP.Abs 0);
			PP.string ppstrm "SIG:";
			PP.openVBox ppstrm (PP.Abs 2);
			PP.cut ppstrm;
			PP.string ppstrm "stamp: ";
			PP.string ppstrm (Stamps.toShortString stamp);
			PP.cut ppstrm;
			PP.string ppstrm "name: ";
			case name
			  of NONE => PP.string ppstrm "ANONYMOUS"
			   | SOME p => (PP.string ppstrm "NAMED "; ppSym ppstrm p);
			case elements
			  of nil => ()
			   | _ => (PP.cut ppstrm;
				   PP.string ppstrm "elements:";
				   PU.ppvseq ppstrm 2 ""
				     (fn ppstrm =>
					 (fn (sym, spec) =>
					     ppElement ppstrm (entityEnvOp, env)
						       (sym, spec, depth)))
				     nonConsElems);
			case strsharing
			  of nil => ()
			   | _ => (PP.newline ppstrm;
				   PP.string ppstrm "strsharing:";
				   PU.nl_indent ppstrm 2;
				   ppConstraints ("",strsharing));
			case typsharing
			  of nil => ()
			   | _ => (PP.newline ppstrm;
				   PP.string ppstrm "tycsharing:";
				   PU.nl_indent ppstrm 2;
				   ppConstraints ("type ",typsharing));
			PP.closeBox ppstrm;
			PP.closeBox ppstrm
		    end (* let -- binding nonConsElems *)
		| M.ERRORsig => PP.string ppstrm "<error sig>"
    end (* end ppSignature0 *)

(* ppFunsig : PP.stream -> SE.staticEnv -> (M.fctSig * int) -> unit *)
and ppFunsig ppstrm env (sign, depth) =
    let val {openHVBox, openHOVBox,closeBox,pps,ppi,break,newline,...} = PU.en_pp ppstrm
	fun trueBodySig (orig as M.SIG { elements =
					 [(sym, M.STRspec { sign, ... })],
					 ... }) =
	    if Symbol.eq (sym, resultId) then sign else orig
	  | trueBodySig orig = orig
     in if depth<=0 then pps "<fctsig>"
	else case sign
	       of M.FSIG {paramsig,paramvar,paramsym,bodysig, ...} =>
		   (openHVBox 0;
		    pps "FSIG:";
		    nl_indent ppstrm 2;
		    openHVBox 0;
		    pps "psig: ";
		    ppSignature0 ppstrm env (paramsig, NONE, depth-1);
		    newline();
		    pps "pvar: ";
		    pps (EntPath.entVarToString paramvar);
		    newline();
		    pps "psym: ";
		    (case paramsym
		      of NONE => pps "<anonymous>"
		       | SOME sym => ppSym ppstrm sym);
		    newline();
		    pps "bsig: ";
		    ppSignature0 ppstrm env (bodysig, NONE, depth-1);
		    closeBox();
		    closeBox())
		| M.ERRORfsig => pps "<error fsig>"
    end

(* ppStrEntity : PP.stream -> SE.staticEnv -> (M.entity * int) -> unit *)
and ppStrEntity ppstrm env (e, depth) =
    let val {stamp,entities,properties,rpath,stub} = e
	val {openHVBox, openHOVBox,closeBox,pps,ppi,break,newline,...} = PU.en_pp ppstrm
     in if depth <= 1
	then pps "<structure entity>"
	else (openHVBox 0;
	       pps "strEntity:";
	       nl_indent ppstrm 2;
	       openHVBox 0;
		pps "rpath: ";
		pps (IP.toString rpath);
		newline();
		pps "stamp: ";
		pps (Stamps.toShortString stamp);
		newline();
		pps "entities:";
		nl_indent ppstrm 2;
		ppEntityEnv ppstrm env (entities, depth-1);
		newline();
		pps "lambdaty:";
		nl_indent ppstrm 2;
	       closeBox ();
	      closeBox ())
    end

(* ppFctEntity : PP.stream -> SE.staticEnv -> (M.entity * int) -> unit *)
and ppFctEntity ppstrm env (e, depth) =
    let val {stamp,closure,properties,tycpath,rpath,stub} = e
	val {openHVBox,openHOVBox,closeBox,pps,ppi,break,newline,...} = PU.en_pp ppstrm
    in if depth <= 1
	then pps "<functor entity>"
	else (openHVBox 0;
	       pps "fctEntity:";
	       nl_indent ppstrm 2;
	       openHVBox 0;
		pps "rpath: ";
		pps (IP.toString rpath);
		newline();
		pps "stamp: ";
		pps (Stamps.toShortString stamp);
		newline();
		pps "closure:";
		break{nsp=1,offset=2};
		ppClosure ppstrm (closure,depth-1);
		newline();
		pps "lambdaty:";
		break{nsp=1,offset=2};
		pps "tycpath:";
		break{nsp=1,offset=2};
		pps "--printing of tycpath not implemented yet--";
	       closeBox ();
	      closeBox ())
    end

(* ppFunctor : PP.stream -> SE.staticEnv -> (M.Functor * int) -> unit *)
and ppFunctor ppstrm env (fct, depth) =
    (case fct
       of (M.FCT { sign, rlzn, ... }) =>
	    if depth <= 1
	    then pps ppstrm "<functor>"
	    else (PP.openHVBox ppstrm (PP.Abs 0);
		    pps ppstrm "sign:";
		    nl_indent ppstrm 2;
		    ppFunsig ppstrm env (sign, depth-1);
		    newline ppstrm;
		    pps ppstrm "rlzn:";
		    nl_indent ppstrm 2;
		    ppFctEntity ppstrm env (rlzn, depth-1);
		  PP.closeBox ppstrm)
	| M.ERRORfct => pps ppstrm "<error functor>")

(* ppTycBind : PP.stream -> (T.tycon * SE.staticEnv) -> unit *)
and ppTycBind ppstrm (tyc,env) =
    let val {openHVBox, openHOVBox,closeBox,pps,ppi,break,newline,...} = PU.en_pp ppstrm
        fun visibleDcons(tyc,dcons) =
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
				    (openHVBox 0;
				     pps "ppTycBind failure: ";
				     newline ();
				     ppTycon env ppstrm tyc;
				     newline ();
				     ppTycon env ppstrm tycon;
				     newline ();
				     closeBox ();
				     find rest))
			| NONE => find rest)
		  | find [] = []
	     in find dcons
	    end
     in (openHVBox 0;
	 pps "type "; ppTycon env ppstrm tyc;  (* ppTycon should be internals version *)
	 closeBox())
    end (* ppTycBind *)

(* ppReplBind : PP.stream -> T.tycon * SE.staticEnv -> unit *)
and ppReplBind ppstrm =
    let val {openHVBox, openHOVBox,closeBox,pps,ppi,break,newline,...} = PU.en_pp ppstrm
     in fn (T.DEFtyc{tyfun=T.TYFUN{body=T.CONty(rightTyc,_),...},path,...},
	    env) =>
	   (* [GK 5/4/07] Does this case ever occur? All datatype
	      replication tycs are GENtycs after elaboration *)
	   (openHOVBox 2;
            pps "datatype"; break{nsp=1,offset=0};
            ppSym ppstrm (IP.last path);
            pps " ="; break{nsp=1,offset=0};
            pps "datatype"; break{nsp=1,offset=0};
            ppTycon env ppstrm rightTyc;
            closeBox ())
	 | (tyc as T.GENtyc{stamp, arity, eq, kind, path, stub}, env) =>
	   (openHOVBox 2;
	    pps "datatype"; break{nsp=1,offset=0};
	    ppSym ppstrm (IP.last path);
	    pps " ="; break{nsp=1,offset=0};
	    ppTycBind ppstrm (tyc, env);
	    closeBox())
	 | (T.PATHtyc _, _) => ErrorMsg.impossible "<replbind:PATHtyc>"
	 | (T.RECtyc _, _) => ErrorMsg.impossible "<replbind:RECtyc>"
	 | (T.FREEtyc _, _) => ErrorMsg.impossible "<replbind:FREEtyc>"
	 | _ => ErrorMsg.impossible "ppReplBind"
    end (* fun ppReplBind *)

(* ppEntity : PP.stream -> SE.staticEnv -> (M.entity * int) -> int *)
and ppEntity ppstrm env (entity, depth) =
    case entity
      of M.TYCent tycon => ppTycon env ppstrm tycon
       | M.STRent strEntity => ppStrEntity ppstrm env (strEntity, depth-1)
       | M.FCTent fctEntity => ppFctEntity ppstrm env (fctEntity, depth-1)
       | M.ERRORent => pps ppstrm "ERRORent"

(* ppEntityEnv : PP.stream -> SE.staticEnv -> (M.entityEnv * int) -> unit *)
and ppEntityEnv ppstrm env (entEnv, depth) =
    if depth <= 1
    then pps ppstrm "<entityEnv>"
    else (ppvseq ppstrm 2 ""
	      (fn ppstrm => fn (entVar,entity) =>
		let val {openHVBox,openHOVBox,closeBox,pps,ppi,break,newline,...} =
			 PU.en_pp ppstrm
		 in openHVBox 2;
		     pps (EntPath.entVarToString entVar);
		     pps ":";
		     nl_indent ppstrm 2;
		     ppEntity ppstrm env (entity, depth-1);
		     newline();
		    closeBox()
		end)
	  (EE.toList entEnv))

(* ppEntDec : PP.stream -> (M.entityDec * int) -> unit *)
and ppEntDec ppstrm (entDec,depth) =
    if depth <= 0 then pps ppstrm "<entDec>"
    else case entDec
	  of M.TYCdec(entVar,tycExp) =>
	      (pps ppstrm "ED.T: ";
	       ppEntVar ppstrm entVar; break ppstrm {nsp=1,offset=1};
	       ppTycExp ppstrm (tycExp,depth-1))
	   | M.STRdec(entVar,strExp,sym) =>
	      (pps ppstrm "ED.S: ";
	       ppEntVar ppstrm entVar; break ppstrm {nsp=1,offset=1};
	       ppStrExp ppstrm (strExp,depth-1); break ppstrm {nsp=1,offset=1};
	       ppSym ppstrm sym)
	   | M.FCTdec(entVar,fctExp) =>
	      (pps ppstrm "ED.F: ";
	       ppEntVar ppstrm entVar; break ppstrm {nsp=1,offset=1};
	       ppFctExp ppstrm (fctExp,depth-1))
	   | M.SEQdec entityDecs =>
	      ppvseq ppstrm 0 ""
	        (fn ppstrm => fn entDec => ppEntDec ppstrm (entDec,depth))
		entityDecs
	   | M.LOCALdec(entityDecL,entityDecB) => pps ppstrm "ED.L:"
	   | M.ERRORdec => pps ppstrm "ED.ER:"
	   | M.EMPTYdec => pps ppstrm "ED.EM:"

(* ppStrExp : PP.stream -> (M.strExp * int) -> unit *)
and ppStrExp ppstrm (strExp,depth) =
    if depth <= 0 then pps ppstrm "<strExp>" else
    case strExp
      of M.VARstr ep =>
	  (PP.string ppstrm "SE.V:"; PP.break ppstrm {nsp=1,offset=1};
           ppEntPath ppstrm ep)
       | M.CONSTstr { stamp, rpath, ... } =>
	 (PP.string ppstrm "SE.C:"; PP.break ppstrm {nsp=1,offset=1};
	  PP.string ppstrm (InvPath.toString rpath))
       | M.STRUCTURE{stamp,entDec} =>
	  (PP.string ppstrm "SE.S:"; PP.break ppstrm {nsp=1,offset=1};
	   ppEntDec ppstrm (entDec,depth-1))
       | M.APPLY(fctExp,strExp) =>
	  (PP.openHVBox ppstrm (PP.Rel 0);
	    PP.string ppstrm "SE.AP:"; PP.break ppstrm {nsp=1,offset=1};
	    PP.openHVBox ppstrm (PP.Rel 0);
	     PP.string ppstrm "fct:"; ppFctExp ppstrm (fctExp, depth-1);
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "arg:"; ppStrExp ppstrm (strExp, depth-1);
	    closeBox ppstrm;
	   closeBox ppstrm)
       | M.LETstr(entDec,strExp) =>
	  (PP.openHVBox ppstrm (PP.Rel 0);
           PP.string ppstrm "SE.L:"; PP.break ppstrm {nsp=1,offset=1};
           PP.openHVBox ppstrm (PP.Rel 0);
	   PP.string ppstrm "let:"; ppEntDec ppstrm (entDec,depth-1);
           PP.break ppstrm {nsp=1,offset=0};
           PP.string ppstrm "in:"; ppStrExp ppstrm (strExp, depth-1);
           closeBox ppstrm;
	   closeBox ppstrm)
       | M.ABSstr(sign,strExp) =>
          (PP.openHVBox ppstrm (PP.Rel 0);
           PP.string ppstrm "SE.AB:"; PP.break ppstrm {nsp=1,offset=1};
            PP.openHVBox ppstrm (PP.Rel 0);
	     PP.string ppstrm "sign: <omitted>";
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "sexp:"; ppStrExp ppstrm (strExp, depth-1);
	    closeBox ppstrm;
	   closeBox ppstrm)
       | M.CONSTRAINstr{boundvar,raw,coercion} =>
          (PP.openHVBox ppstrm (PP.Rel 0);
           PP.string ppstrm "SE.CO:"; PP.break ppstrm {nsp=1,offset=1};
            PP.openHVBox ppstrm (PP.Rel 0);
             ppEntVar ppstrm boundvar; PP.break ppstrm {nsp=1,offset=1};
	     PP.string ppstrm "src:"; ppStrExp ppstrm (raw, depth-1);
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "tgt:"; ppStrExp ppstrm (coercion, depth-1);
	    closeBox ppstrm;
	   closeBox ppstrm)
       | M.FORMstr(sign) => PP.string ppstrm "SE.FM:"

(* ppFctExp : PP.stream -> M.fctExp * int -> unit *)
and ppFctExp ppstrm (fctExp,depth) =
    if depth <= 0 then PP.string ppstrm "<fctExp>" else
    case fctExp
      of M.VARfct ep =>
	  (PP.string ppstrm "FE.V:"; ppEntPath ppstrm ep)
       | M.CONSTfct { rpath, ... } =>
	 (PP.string ppstrm "FE.C:";
	  PP.string ppstrm (InvPath.toString rpath))
       | M.LAMBDA_TP {param, body, ...} =>
	  (PP.openHVBox ppstrm (PP.Rel 0);
	    PP.string ppstrm "FE.LP:"; PP.break ppstrm {nsp=1,offset=1};
	    PP.openHVBox ppstrm (PP.Rel 0);
	     PP.string ppstrm "par:"; ppEntVar ppstrm param;
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "bod:"; ppStrExp ppstrm (body, depth-1);
	    PP.closeBox ppstrm;
	   PP.closeBox ppstrm)
       | M.LAMBDA {param, body} =>
	  (PP.openHVBox ppstrm (PP.Rel 0);
	    PP.string ppstrm "FE.L:"; PP.break ppstrm {nsp=1,offset=1};
	    PP.openHVBox ppstrm (PP.Rel 0);
	     PP.string ppstrm "par:"; ppEntVar ppstrm param;
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "bod:"; ppStrExp ppstrm (body, depth-1);
	    PP.closeBox ppstrm;
	   PP.closeBox ppstrm)
       | M.LETfct (entDec,fctExp) =>
          (PP.openHVBox ppstrm (PP.Rel 0);
            PP.string ppstrm "FE.LT:"; PP.break ppstrm {nsp=1,offset=1};
            PP.openHVBox ppstrm (PP.Rel 0);
  	     PP.string ppstrm "let:"; ppEntDec ppstrm (entDec,depth-1);
             PP.break ppstrm {nsp=1,offset=0};
             PP.string ppstrm "in:"; ppFctExp ppstrm (fctExp, depth-1);
            PP.closeBox ppstrm;
	   PP.closeBox ppstrm)

(* ppClosure : PP.stream -> M.fctClosure * int -> unit *)
and ppClosure ppstrm (M.CLOSURE{param,body,env},depth) =
      (PP.openHVBox ppstrm (PP.Abs 0);
	 PP.string ppstrm "CL:";
	 PP.break ppstrm {nsp=1,offset=1};
	 PP.openHVBox ppstrm (PP.Abs 0);
	   PP.string ppstrm "param: ";
	   ppEntVar ppstrm param;
	   newline ppstrm;
	   PP.string ppstrm "body: ";
	   ppStrExp ppstrm (body,depth-1);
	   newline ppstrm;
	   PP.string ppstrm "env: ";
	   ppEntityEnv ppstrm SE.empty (env, depth-1);
	 PP.closeBox ppstrm;
       PP.closeBox ppstrm)

(* ppBinding : PP.stream -> SE.staticEnv -> (S.symbol * B.binding * int) -> unit
 *  assumes no newline is needed before pping *)
and ppBinding ppstrm (env: SE.staticEnv) (name: S.symbol, binding:B.binding, depth:int) =
    case binding
      of B.VALbind var => (pps ppstrm "val "; ppVariable ppstrm (var,env))
       | B.CONbind con => ppConBinding ppstrm (con,env)
       | B.TYCbind tycon => ppTycBind ppstrm (tycon,env)
       | B.SIGbind sign =>
	  (PP.openHVBox ppstrm (PP.Abs 0);
	    PP.string ppstrm "signature ";
	    ppSym ppstrm name; PP.string ppstrm " =";
	    PP.break ppstrm {nsp=1,offset=2};
	    ppSignature0 ppstrm env (sign, NONE, depth);
	   PP.closeBox ppstrm)
       | B.FSGbind fs =>
	  (PP.openHVBox ppstrm (PP.Abs 2);
	     PP.string ppstrm "funsig "; ppSym ppstrm name;
	     ppFunsig ppstrm env (fs, depth);
	   PP.closeBox ppstrm)
       | B.STRbind str =>
	  (PP.openHVBox ppstrm (PP.Abs 0);
	    PP.string ppstrm "structure "; ppSym ppstrm name;
	    PP.string ppstrm " :";
	    PP.break ppstrm {nsp=1,offset=2};
	    ppStructure ppstrm env (str, depth);
	   PP.closeBox ppstrm)
       | B.FCTbind fct =>
	  (PP.openHVBox ppstrm (PP.Abs 0);
	    PP.string ppstrm "functor ";
	    ppSym ppstrm name;
	    PP.string ppstrm " : <sig>";  (* DBM -- should print the signature *)
	   PP.closeBox ppstrm)
       | B.FIXbind fixity =>
	  (pps ppstrm (Fixity.fixityToString fixity); ppSym ppstrm name)

(* ppEnv : PP.stream -> (S.symbol list option * SE.staticEnv) -> (SE.staticEnv * int)
           -> unit
   pp an environment in the context of the top environment.
   The environment must either be for a signature or be absolute (i.e.
   all types and structures have been interpreted) *)
(* Note: I make a preliminary pass over bindings to remove
         invisible ConBindings -- Konrad.
	 and invisible structures too -- PC *)
and ppEnv ppstrm (boundsyms, topenv) (env, depth) =
    let val bindings =
	    case boundsyms
	      of NONE => SE.sort env
	       | SOME l => foldr (fn (x,bs) =>
				    ((x,SE.look(env,x))::bs
				     handle SE.Unbound => bs))
				[] l
	val pp_env = StaticEnv.atop(env,topenv)
     in ppSequence ppstrm
	  {sep=newline,
	   pr=(fn ppstrm => fn (name,binding) =>
	          ppBinding ppstrm pp_env (name, binding, depth)),
	   style=CONSISTENT}
	  (all_ppable_bindings bindings pp_env)
    end

(* ppOpen : PP.stream -> SE.staticEnv -> (SP.path * M.Structure * int) -> unit *)
fun ppOpen ppstrm env (path, str, depth) =
      (PP.openVBox ppstrm (PP.Abs 0);
         PP.openVBox ppstrm (PP.Abs 2);
           PP.openHBox ppstrm;
	     PP.string ppstrm "opening ";
             PP.string ppstrm (SymPath.toString path);
           PP.closeBox ppstrm;
	   if depth < 1 then ()
	   else (case str
		   of M.STR { sign, rlzn as {entities,...}, ... } =>
			(case sign
			   of M.SIG {elements = [],...} => ()
			    | M.SIG {elements,...} =>
				let fun ppElem (sym, spec) =
					(PP.cut ppstrm;
					 ppElement ppstrm
					   (SOME entities, SE.atop(sigToEnv sign, env))
					   (sym, spec, depth))
				in PP.openVBox ppstrm (PP.Abs 0);
				     List.app ppElem (removeDCons elements);
				   PP.closeBox ppstrm
				end
			      | M.ERRORsig => ())
		    | M.ERRORstr => ()
		    | M.STRSIG _ => bug "ppOpen");
         PP.closeBox ppstrm;
       PP.cut ppstrm;  (* generates final newline (?) *)
       PP.closeBox ppstrm)

(* ppSignature : PP.stream -> SE.staticEnv -> (M.Signature * int) -> unit *)
fun ppSignature (ppstrm: PP.stream) (env: SE.staticEnv) (sign: M.Signature, depth: int) =
    ppSignature0 ppstrm env (sign, NONE, depth)

end (* local *)
end (* structure PPModules *)

(* ppmod.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* 1. modified to use SML/NJ Lib PP. [DBM, 2003/7/30])
 * 2. "internals" printing removed; should be moved to a separate module [DBM, 2021/12/09] *)

signature PPMOD =
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

end (* signature PPMOD *)


structure PPModules : PPMOD =
struct

local
  structure S = Symbol
  structure SP = SymPath
  structure IP = InvPath
  structure A = Access
  (* structure II = InlInfo *)
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
       in not visibleDconTyc
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
	  | ppCon (con as T.DATACON{name,typ,...},env) = ()
     in ppCon
    end

(* ppStructure : PP.stream -> SE.staticEnv -> M.Structure * int -> unit *)
fun ppStructure ppstrm env (str, depth) =
      (case str
	 of M.STR { sign, rlzn as { entities, ... }, prim, ... } =>
	     (case sign
		of M.SIG { name = SOME sym, ... } =>
		     ((if MU.eqSign
			  (sign, LU.lookSig (env,sym,(fn _ => raise SE.Unbound)))
		       then ppSym ppstrm sym
		       else (ppSym ppstrm sym; PP.string ppstrm "?"))
		      handle SE.Unbound => (ppSym ppstrm sym; PP.string ppstrm "?"))
		 | M.SIG { name = NONE, ... } =>
		     if depth <= 1
		     then PP.string ppstrm "<sig>"
		     else ppSignature0 ppstrm (sign, env, depth-1, true, SOME entities)
		 | M.ERRORsig => PP.string ppstrm "<error sig>")
	  | M.STRSIG _ => PP.string ppstrm "<strsig>"
	  | M.ERRORstr => PP.string ppstrm "<error str>")

(* ppElement : (SE.staticEnv * int * M.enityEnv option)
               -> PP.stream
	       -> (S.symbol * M.spec)
	       -> unit *)
and ppElement (env,depth,entityEnvOp) ppstrm (sym, spec) =
    (case spec
       of M.STRspec{sign,entVar,def,slot} => (
	    openHVBox ppstrm (PP.Abs 0);
              openHBox ppstrm;
	        pps ppstrm "structure";
                space ppstrm 1;
	        ppSym ppstrm sym;
                space ppstrm 1;
                pps ppstrm ":";
                space ppstrm 1;
             closeBox ppstrm;
	     openHVBox ppstrm (PP.Abs 0);
               case entityEnvOp
                of NONE => ppSignature0 ppstrm (sign,env,depth-1,true,NONE)
                 | SOME eenv => let
                     val {entities,...} =
                             case EE.look(eenv,entVar)
                              of M.STRent e => e
                               | _ => bug "ppElement:STRent"
                     in
                       ppSignature0 ppstrm (sign,env,depth-1,true,SOME entities)
                     end
                (* end case *);
              closeBox ppstrm;
	    closeBox ppstrm)

	| M.FCTspec{sign,entVar,slot} =>
	   (openHVBox ppstrm (PP.Rel 0);
	     pps ppstrm "functor ";
	     ppSym ppstrm sym; pps ppstrm " :";
	     break ppstrm {nsp=1,offset=2};
	     openHVBox ppstrm (PP.Rel 0);
	      ppFunsig ppstrm env (sign, depth-1);
	     closeBox ppstrm;
	    closeBox ppstrm)

	| M.TYCspec{entVar,info} =>
	   (case info
	      of M.RegTycSpec{spec,repl,scope} =>
		 (openHVBox ppstrm (PP.Rel 0);
		   (case entityEnvOp
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
			      | _ => bug "ppElements:TYCent"));
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

	| M.CONspec{spec=dcon,...} => ()
	    (* don't print ordinary data constructor,
	     * because it was printed with its datatype *)
        (* end case *))
        (* end ppElement *)

(* ppSignature0 : PP.stream
                  -> (M.Signature * SE.staticEnv * int * bool * M.entityEnv option)
                  -> unit *)
and ppSignature0 ppstrm (sign, env, depth: int, cut: bool, entityEnvOp) =
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
			       (PP.string ppstrm " ="; PP.break ppstrm {nsp=1,offset=0})),
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
		  let (* Filter out ordinary dcons that do not print in ppElements
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
		     fun ppsig () =
			   let val offset = if cut then PP.Abs 2 else PP.Abs 2
			    in (PP.openVBox ppstrm offset;  (* was PP.Abs 2? *)
				 if cut then PP.cut ppstrm else ();
				 PP.string ppstrm "sig";
				 PP.openVBox ppstrm offset;  (* was PP.Abs 2? *)
				   PP.cut ppstrm;
				   PU.ppvseqNoBox ppstrm
					(ppElement (env,depth,entityEnvOp))
					nonConsElems;
				   case strsharing
				     of nil => ()
				      | _ => ((* PP.cut ppstrm; *)
					      ppConstraints("",strsharing));
				   case typsharing
				     of nil => ()
				      | _ => ((* PP.cut ppstrm; *)
					      ppConstraints("type ",typsharing));
				 PP.closeBox ppstrm;
				 PP.cut ppstrm;
				 PP.string ppstrm "end";
			       PP.closeBox ppstrm)
			   end
		   in case nonConsElems
			of nil => PP.string ppstrm "sig end"
			 | [elem] =>
			   if big elem then ppsig ()
			   else (PP.openHVBox ppstrm (PP.Abs 2);
				 PP.string ppstrm "sig";
				 PP.break ppstrm {nsp=1, offset=0};
				 ppElement (env, depth, entityEnvOp) ppstrm elem;
				 PP.break ppstrm {nsp=1, offset=0};
				 PP.string ppstrm "end";
				 PP.closeBox ppstrm)
			 | _ => ppsig ()
		  end (* let -- binding nonConsElems, big, ppsig *)
		| M.ERRORsig => PP.string ppstrm "<error sig>"
    end (* end ppSignature0 *)

(* ppFunsig : PP.stream -> SE.staticEnv -> M.fctSig * int -> unit *)
and ppFunsig ppstrm env (sign, depth) =
    let fun trueBodySig (orig as M.SIG { elements =
					 [(sym, M.STRspec { sign, ... })],
					 ... }) =
	    if Symbol.eq (sym, resultId) then sign else orig
	  | trueBodySig orig = orig
     in if depth<=0 then pps ppstrm "<fctsig>"
	else case sign
	       of M.FSIG {paramsig,paramvar,paramsym,bodysig, ...} =>
		    (PP.openHVBox ppstrm (PP.Rel 0);  (* HVBox <-> VBox ? *)
		     PP.openHVBox ppstrm (PP.Rel 0);
		     pps ppstrm "(";
		     case paramsym
		       of SOME x => pps ppstrm (S.name x)
			| _ => pps ppstrm "<param>";
		     pps ppstrm ": ";
		     ppSignature0 ppstrm (paramsig,env,depth-1,false,NONE);
		     pps ppstrm ") :";
		     PP.closeBox ppstrm;
		     PP.break ppstrm {nsp=1,offset=0};  (* PP.cut ppstrm; ? *)
		     ppSignature0 ppstrm (trueBodySig bodysig,env,depth-1,true,NONE);
		     PP.closeBox ppstrm)
		| M.ERRORfsig => pps ppstrm "<error fsig>"
    end (* fun ppFunsig *)

(* ppTycBind : PP.stream -> (T.tycon * SE.staticEnv) -> unit *)
and ppTycBind ppstrm (tyc,env) =
    let fun visibleDcons(tyc,dcons) =
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
				| tycon => bug "ppTycBind..visibleDcons..find")
			| NONE => find rest)
		  | find [] = []
	     in find dcons
	    end
	fun stripPoly(T.POLYty{tyfun=T.TYFUN{body,...},...}) = body
	  | stripPoly ty = ty
	fun ppDcon (T.DATACON{name,typ,...}) =
	    (ppSym ppstrm name;
	     let val typ = stripPoly typ
	      in if BT.isArrowType typ
		 then (pps ppstrm " of "; ppType env ppstrm (BT.domain typ))
		 else ()
	     end)
    in case tyc
         of T.GENtyc { path, arity, eq, kind, ... } =>
	      (case (!eq, kind)
		 of (T.ABS, _) =>
		      (* abstype *)
		      (PP.openHVBox ppstrm (PP.Abs 0);
		       pps ppstrm "type";
		       ppFormals ppstrm arity;
		       pps ppstrm " ";
		       ppSym ppstrm (IP.last path);
		       PP.closeBox ppstrm)
		  | (_, T.DATATYPE{index,family={members,...},...}) =>
		      (* ordinary datatype *)
		      let val {dcons,...} = Vector.sub(members,index)
			  val visdcons = visibleDcons(tyc,dcons)
			  val incomplete = length visdcons < length dcons
		       in PP.openHVBox ppstrm (PP.Abs 0);
			  pps ppstrm "datatype";
			  ppFormals ppstrm arity;
			  pps ppstrm " ";
			  ppSym ppstrm (IP.last path);
			  case visdcons
			    of nil => pps ppstrm " = ..."
			     | first :: rest =>
				(PP.break ppstrm {nsp=1,offset=2};
				 PP.openHVBox ppstrm (PP.Abs 0);
				  pps ppstrm "= "; ppDcon first;
				  app (fn d => (PP.break ppstrm {nsp=1,offset=0};
						pps ppstrm "| "; ppDcon d))
				      rest;
				  if incomplete
				  then (PP.break ppstrm {nsp=1,offset=0};
					pps ppstrm "... ")
				  else ();
				 PP.closeBox ppstrm);
			 PP.closeBox ppstrm
		     end
		| _ =>
		  (PP.openHVBox ppstrm (PP.Abs 0);
		   if EqTypes.isEqTycon tyc
		   then pps ppstrm "eqtype"
		   else pps ppstrm "type";
		   ppFormals ppstrm arity;
		   pps ppstrm " ";
		   ppSym ppstrm (IP.last path);
		   PP.closeBox ppstrm))
	  | T.DEFtyc{path,tyfun=T.TYFUN{arity,body},...} =>
		(PP.openHOVBox ppstrm (PP.Abs 2);
		 pps ppstrm "type";
		 ppFormals ppstrm arity;
		 PP.break ppstrm {nsp=1,offset=0};
		 ppSym ppstrm (InvPath.last path);
		 pps ppstrm " =";
		 PP.break ppstrm {nsp=1,offset=0};
		 ppType env ppstrm body;
		 PP.closeBox ppstrm)
	  | T.ERRORtyc =>
		(pps ppstrm "ERRORtyc")
	  | T.PATHtyc _ =>
		(pps ppstrm "PATHtyc:";
		 ppTycon env ppstrm tyc)
	  | tycon =>
		(pps ppstrm "strange tycon: ";
		 ppTycon env ppstrm tycon)
    end (* fun ppTycBind *)

(* ppReplBind : PP.stream -> T.tycon * SE.staticEnv -> unit *)
and ppReplBind ppstrm =
    fn (T.DEFtyc{tyfun=T.TYFUN{body=T.CONty(rightTyc,_),...},path,...}, env) =>
	   (* [GK 5/4/07] Does this case ever occur? All datatype
	      replication tycs are GENtycs after elaboration *)
	   (PP.openHOVBox ppstrm (PP.Abs 2);
            pps ppstrm "datatype"; break ppstrm {nsp=1,offset=0};
            ppSym ppstrm (IP.last path);
            pps ppstrm " ="; break ppstrm {nsp=1,offset=0};
            pps ppstrm "datatype"; break ppstrm {nsp=1,offset=0};
            ppTycon env ppstrm rightTyc;
            PP.closeBox ppstrm)
	 | (tyc as T.GENtyc{stamp, arity, eq, kind, path, stub}, env) =>
	   (PP.openHOVBox ppstrm (PP.Abs 2);
	    pps ppstrm "datatype"; PP.break ppstrm {nsp=1,offset=0};
	    ppSym ppstrm (IP.last path);
	    pps ppstrm " ="; PP.break ppstrm {nsp=1,offset=0};
	    ppTycBind ppstrm (tyc, env);
	    PP.closeBox ppstrm)
	 | (T.PATHtyc _, _) => ErrorMsg.impossible "<replbind:PATHtyc>"
	 | (T.RECtyc _, _) => ErrorMsg.impossible "<replbind:RECtyc>"
	 | (T.FREEtyc _, _) => ErrorMsg.impossible "<replbind:FREEtyc>"
	 | _ => ErrorMsg.impossible "ppReplBind"

(* ppBinding : PP.stream -> SE.staticEnv ->  (S.symbol * B.binding * int) -> unit
 *  assumes no newline is needed before pping *)
and ppBinding ppstrm (env: SE.staticEnv) (name: S.symbol, binding: B.binding, depth: int) =
    case binding
      of B.VALbind var => (pps ppstrm "val "; ppVariable ppstrm (var,env))
       | B.CONbind con => ppConBinding ppstrm (con,env)
       | B.TYCbind tycon => ppTycBind ppstrm (tycon,env)
       | B.SIGbind sign =>
	  (PP.openHVBox ppstrm (PP.Abs 0);
	    PP.string ppstrm "signature ";
	    ppSym ppstrm name; PP.string ppstrm " =";
	    PP.break ppstrm {nsp=1,offset=2};
	    ppSignature0 ppstrm (sign,env,depth,true,NONE);
	   PP.closeBox ppstrm)
       | B.FSGbind fs =>
	   (PP.openHVBox ppstrm (PP.Abs 2);
	     PP.string ppstrm "funsig "; ppSym ppstrm name;
	     ppFunsig ppstrm env (fs, depth);
	    PP.closeBox ppstrm)
       | B.STRbind str => (
          PP.openHVBox ppstrm (PP.Abs 0);
	    PP.openHBox ppstrm;
	      PP.string ppstrm "structure";
	      PP.space ppstrm 1;
              ppSym ppstrm name;
	      PP.space ppstrm 1;
	      PP.string ppstrm ":";
	      PP.space ppstrm 1;
            PP.closeBox ppstrm;
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
				let fun ppElem elem =
					(PP.cut ppstrm;
					 ppElement (SE.atop(sigToEnv sign, env),
						    depth, SOME entities)
						  ppstrm elem)
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
fun ppSignature ppstrm env (sign, depth) =
    ppSignature0 ppstrm (sign,env,depth,true,NONE)

end (* local *)
end (* structure PPModules *)

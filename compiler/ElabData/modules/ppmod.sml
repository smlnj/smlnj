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
    List.filter (fn (name, B.CONbind con) => is_ppable_ConBinding(con,env)
                  | b => true)
                alist

(* filter out non-exception data constructors, since they should not be printed *)
fun removeDCons elements = List.filter
      (fn (_, M.CONspec {spec = T.DATACON {rep = A.EXN _, ...}, ...}) => true
	| (_, M.CONspec {spec = dcon, ...}) => false
	| _ => true)
	elements

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

(* ppStructure : PP.stream -> SE.staticEnv -> M.Structure * int -> unit *)
fun ppStructure env (str, depth) =
      (case str
	 of M.STR { sign, rlzn as { entities, ... }, prim, ... } =>
	     (case sign
		of M.SIG { name = SOME sym, ... } =>
		     ((if MU.eqSign
			  (sign, LU.lookSig (env,sym,(fn _ => raise SE.Unbound)))
		       then ppSym sym
		       else (ppSym sym; PP.string "?"))
		      handle SE.Unbound => (ppSym sym; PP.string "?"))
		 | M.SIG { name = NONE, ... } =>
		     if depth <= 1
		     then PP.string "<sig>"
		     else ppSignature0 (sign, env, depth-1, true, SOME entities)
		 | M.ERRORsig => PP.string "<error sig>")
	  | M.STRSIG _ => PP.string "<strsig>"
	  | M.ERRORstr => PP.string "<error str>")

(* ppElement : (SE.staticEnv * int * M.enityEnv option)
               -> PP.stream
	       -> (S.symbol * M.spec)
	       -> unit *)
and ppElement (env,depth,entityEnvOp) (sym, spec) =
    (case spec
       of M.STRspec{sign,entVar,def,slot} => (
	    openHVBox (PP.Abs 0);
              openHBox;
	        PP.text "structure";
                space 1;
	        ppSym sym;
                space 1;
                PP.text ":";
                space 1;
             closeBox;
	     openHVBox (PP.Abs 0);
               case entityEnvOp
                of NONE => ppSignature0 (sign,env,depth-1,true,NONE)
                 | SOME eenv => let
                     val {entities,...} =
                             case EE.look(eenv,entVar)
                              of M.STRent e => e
                               | _ => bug "ppElement:STRent"
                     in
                       ppSignature0 (sign,env,depth-1,true,SOME entities)
                     end
                (* end case *);
              closeBox;
	    closeBox)

	| M.FCTspec{sign,entVar,slot} =>
	   (openHVBox (PP.Rel 0);
	     PP.text "functor ";
	     ppSym sym; PP.text " :";
	     break {nsp=1,offset=2};
	     openHVBox (PP.Rel 0);
	      ppFunsig env (sign, depth-1);
	     closeBox;
	    closeBox)

	| M.TYCspec{entVar,info} =>
	   (case info
	      of M.RegTycSpec{spec,repl,scope} =>
		 (openHVBox (PP.Rel 0);
		   (case entityEnvOp
		      of NONE =>
			   if repl then
			       ppReplBind (spec,env)
			   else ppTycBind (spec,env)
		       | SOME eenv =>
			   (case EE.look(eenv,entVar)
			     of M.TYCent tyc =>
				if repl then
				    ppReplBind (tyc,env)
				else ppTycBind (tyc,env)
			      | M.ERRORent => PP.text "<ERRORent>"
			      | _ => bug "ppElements:TYCent"));
		  closeBox)
	       | M.InfTycSpec{name,arity} =>
		 (openHVBox (PP.Rel 0);
		   case entityEnvOp
		     of NONE =>
			 (PP.text "type";
			  ppFormals arity;
			  PP.text " ";
			  ppSym name)
		      | SOME eenv =>
			 (case EE.look(eenv,entVar)
			    of M.TYCent tyc =>
				 ppTycBind (tyc,env)
			     | M.ERRORent => PP.text "<ERRORent>"
			     | _ => bug "ppElements:TYCent");
		  closeBox))

	| M.VALspec{spec=typ,...} =>
	   (openHOVBox (PP.Rel 4);
	      PP.text "val ";
	      ppSym sym;
	      PP.text " : ";
	      ppType env (typ);
	    closeBox)

	| M.CONspec{spec=dcon as T.DATACON{rep=A.EXN _,...}, ...} =>
	   ppConBinding (dcon,env)

	| M.CONspec{spec=dcon,...} => ()
	    (* don't print ordinary data constructor,
	     * because it was printed with its datatype *)
        (* end case *))
        (* end ppElement *)

(* ppSignature0 : PP.stream
                  -> (M.Signature * SE.staticEnv * int * bool * M.entityEnv option)
                  -> unit *)
and ppSignature0 (sign, env, depth: int, cut: bool, entityEnvOp) =
    let val env = SE.atop(case entityEnvOp
			    of NONE => sigToEnv sign
			     | SOME entEnv => strToEnv(sign,entEnv),
			  env)
	fun ppConstraints (variety, constraints : M.sharespec list) =
	      (PP.openHVBox (PP.Abs 0);
		ppvseq 0 ""
		 (fn => fn paths =>
		     (PP.openHOVBox (PP.Abs 2);
		       PP.string "sharing "; PP.string variety;
		       ppSequence
			{sep=(fn =>
			       (PP.string " ="; PP.break {nsp=1,offset=0})),
			 pr=(fn => (fn sympath =>
				PP.string (SymPath.toString sympath))),
			 style=INCONSISTENT}
			paths;
		      PP.closeBox))
		 constraints;
	       PP.closeBox)
     in if depth <= 0
	then PP.string "<sig>"
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
			    in (PP.openVBox offset;  (* was PP.Abs 2? *)
				 if cut then PP.cut else ();
				 PP.string "sig";
				 PP.openVBox offset;  (* was PP.Abs 2? *)
				   PP.cut;
				   PU.ppvseqNoBox
					(ppElement (env,depth,entityEnvOp))
					nonConsElems;
				   case strsharing
				     of nil => ()
				      | _ => ((* PP.cut; *)
					      ppConstraints("",strsharing));
				   case typsharing
				     of nil => ()
				      | _ => ((* PP.cut; *)
					      ppConstraints("type ",typsharing));
				 PP.closeBox;
				 PP.cut;
				 PP.string "end";
			       PP.closeBox)
			   end
		   in case nonConsElems
			of nil => PP.string "sig end"
			 | [elem] =>
			   if big elem then ppsig ()
			   else (PP.openHVBox (PP.Abs 2);
				 PP.string "sig";
				 PP.break {nsp=1, offset=0};
				 ppElement (env, depth, entityEnvOp) elem;
				 PP.break {nsp=1, offset=0};
				 PP.string "end";
				 PP.closeBox)
			 | _ => ppsig ()
		  end (* let -- binding nonConsElems, big, ppsig *)
		| M.ERRORsig => PP.string "<error sig>"
    end (* end ppSignature0 *)

(* ppFunsig : PP.stream -> SE.staticEnv -> M.fctSig * int -> unit *)
and ppFunsig env (sign, depth) =
    let fun trueBodySig (orig as M.SIG { elements =
					 [(sym, M.STRspec { sign, ... })],
					 ... }) =
	    if Symbol.eq (sym, resultId) then sign else orig
	  | trueBodySig orig = orig
     in if depth<=0 then PP.text "<fctsig>"
	else case sign
	       of M.FSIG {paramsig,paramvar,paramsym,bodysig, ...} =>
		    (PP.openHVBox (PP.Rel 0);  (* HVBox <-> VBox ? *)
		     PP.openHVBox (PP.Rel 0);
		     PP.text "(";
		     case paramsym
		       of SOME x => PP.text (S.name x)
			| _ => PP.text "<param>";
		     PP.text ": ";
		     ppSignature0 (paramsig,env,depth-1,false,NONE);
		     PP.text ") :";
		     PP.closeBox;
		     PP.break {nsp=1,offset=0};  (* PP.cut; ? *)
		     ppSignature0 (trueBodySig bodysig,env,depth-1,true,NONE);
		     PP.closeBox)
		| M.ERRORfsig => PP.text "<error fsig>"
    end (* fun ppFunsig *)

(* ppTycBind : PP.stream -> (T.tycon * SE.staticEnv) -> unit *)
and ppTycBind (tyc,env) =
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
	    (ppSym name;
	     let val typ = stripPoly typ
	      in if BT.isArrowType typ
		 then (PP.text " of "; ppType env (BT.domain typ))
		 else ()
	     end)
    in case tyc
         of T.GENtyc { path, arity, eq, kind, ... } =>
	      (case (!eq, kind)
		 of (T.ABS, _) =>
		      (* abstype *)
		      (PP.openHVBox (PP.Abs 0);
		       PP.text "type";
		       ppFormals arity;
		       PP.text " ";
		       ppSym (IP.last path);
		       PP.closeBox)
		  | (_, T.DATATYPE{index,family={members,...},...}) =>
		      (* ordinary datatype *)
		      let val {dcons,...} = Vector.sub(members,index)
			  val visdcons = visibleDcons(tyc,dcons)
			  val incomplete = length visdcons < length dcons
		       in PP.openHVBox (PP.Abs 0);
			  PP.text "datatype";
			  ppFormals arity;
			  PP.text " ";
			  ppSym (IP.last path);
			  case visdcons
			    of nil => PP.text " = ..."
			     | first :: rest =>
				(PP.break {nsp=1,offset=2};
				 PP.openHVBox (PP.Abs 0);
				  PP.text "= "; ppDcon first;
				  app (fn d => (PP.break {nsp=1,offset=0};
						PP.text "| "; ppDcon d))
				      rest;
				  if incomplete
				  then (PP.break {nsp=1,offset=0};
					PP.text "... ")
				  else ();
				 PP.closeBox);
			 PP.closeBox
		     end
		| _ =>
		  (PP.openHVBox (PP.Abs 0);
		   if EqTypes.isEqTycon tyc
		   then PP.text "eqtype"
		   else PP.text "type";
		   ppFormals arity;
		   PP.text " ";
		   ppSym (IP.last path);
		   PP.closeBox))
	  | T.DEFtyc{path,tyfun=T.TYFUN{arity,body},...} =>
		(PP.openHOVBox (PP.Abs 2);
		 PP.text "type";
		 ppFormals arity;
		 PP.break {nsp=1,offset=0};
		 ppSym (InvPath.last path);
		 PP.text " =";
		 PP.break {nsp=1,offset=0};
		 ppType env body;
		 PP.closeBox)
	  | T.ERRORtyc =>
		(PP.text "ERRORtyc")
	  | T.PATHtyc _ =>
		(PP.text "PATHtyc:";
		 ppTycon env tyc)
	  | tycon =>
		(PP.text "strange tycon: ";
		 ppTycon env tycon)
    end (* fun ppTycBind *)

(* ppReplBind : PP.stream -> T.tycon * SE.staticEnv -> unit *)
and ppReplBind =
    fn (T.DEFtyc{tyfun=T.TYFUN{body=T.CONty(rightTyc,_),...},path,...}, env) =>
	   (* [GK 5/4/07] Does this case ever occur? All datatype
	      replication tycs are GENtycs after elaboration *)
	   (PP.openHOVBox (PP.Abs 2);
            PP.text "datatype"; break {nsp=1,offset=0};
            ppSym (IP.last path);
            PP.text " ="; break {nsp=1,offset=0};
            PP.text "datatype"; break {nsp=1,offset=0};
            ppTycon env rightTyc;
            PP.closeBox)
	 | (tyc as T.GENtyc{stamp, arity, eq, kind, path, stub}, env) =>
	   (PP.openHOVBox (PP.Abs 2);
	    PP.text "datatype"; PP.break {nsp=1,offset=0};
	    ppSym (IP.last path);
	    PP.text " ="; PP.break {nsp=1,offset=0};
	    ppTycBind (tyc, env);
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
       | B.TYCbind tycon => ppTycBind (tycon,env)
       | B.SIGbind sign =>
	  (PP.openHVBox (PP.Abs 0);
	    PP.string "signature ";
	    ppSym name; PP.string " =";
	    PP.break {nsp=1,offset=2};
	    ppSignature0 (sign,env,depth,true,NONE);
	   PP.closeBox)
       | B.FSGbind fs =>
	   (PP.openHVBox (PP.Abs 2);
	     PP.string "funsig "; ppSym name;
	     ppFunsig env (fs, depth);
	    PP.closeBox)
       | B.STRbind str => (
          PP.openHVBox (PP.Abs 0);
	    PP.openHBox;
	      PP.string "structure";
	      PP.space 1;
              ppSym name;
	      PP.space 1;
	      PP.string ":";
	      PP.space 1;
            PP.closeBox;
	    ppStructure env (str, depth);
	   PP.closeBox)
       | B.FCTbind fct =>
	  (PP.openHVBox (PP.Abs 0);
	    PP.string "functor ";
	    ppSym name;
	    PP.string " : <sig>";  (* DBM -- should print the signature *)
	   PP.closeBox)
       | B.FIXbind fixity =>
	  (PP.text (Fixity.fixityToString fixity); ppSym name)

(* ppOpen : PP.stream -> SE.staticEnv -> (SP.path * M.Structure * int) -> unit *)
fun ppOpen env (path, str, depth) =
      (PP.openVBox (PP.Abs 0);
         PP.openVBox (PP.Abs 2);
           PP.openHBox;
	     PP.string "opening ";
             PP.string (SymPath.toString path);
           PP.closeBox;
	   if depth < 1 then ()
	   else (case str
		   of M.STR { sign, rlzn as {entities,...}, ... } =>
			(case sign
			   of M.SIG {elements = [],...} => ()
			    | M.SIG {elements,...} =>
				let fun ppElem elem =
					(PP.cut;
					 ppElement (SE.atop(sigToEnv sign, env),
						    depth, SOME entities)
						  elem)
				in PP.openVBox (PP.Abs 0);
				     List.app ppElem (removeDCons elements);
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

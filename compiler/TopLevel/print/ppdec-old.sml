(* ppdec.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PPDEC =
sig
  val ppDec : Environment.environment -> PrettyPrint.stream
                -> (Absyn.dec * LambdaVar.lvar list) -> unit
  val debugging : bool ref
end (* signature PPDEC *)

structure PPDec : PPDEC =
struct

local
  structure S = Symbol
  structure IP = InvPath
  structure M = Modules
  structure V = Variable
  structure AS = Absyn
  structure PP = PrettyPrint
  structure PU = PPUtil
open Types Variable Modules Bindings Fixity Absyn
PrettyPrint PPUtil PPType PPObj Access
in

(* debugging *)
val say = Control.Print.say
val debugging = ref false
fun debugmsg (msg: string) =
if !debugging then (say msg; say "\n") else ()

fun bug msg = ErrorMsg.impossible("PPDec: "^msg)


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

exception OVERLOAD

fun ppDec ({static,dynamic,...}: Environment.environment)
(ppstrm: PP.stream) (dec: Absyn.dec, exportLvars) =
let val dec = (* pruneDec *) dec

fun isExport (x : LambdaVar.lvar, []) = false
| isExport (x, a::r) = if x = a then true else isExport(x, r)

val pps = PP.string ppstrm
fun sp () = PP.space ppstrm 1
fun nbSp () = PP.nbSpace ppstrm 1
(* trueValType: get the type of the bound variable from static,
since the stamps in the absyn haven't been converted by the pickler *)
fun trueValType path =
let val err = fn _ => fn _ => fn _ => (bug "trueValType: unbound")
in case path
of SymPath.SPATH[id] =>
(case Lookup.lookValSym(static,id,err)
   of AS.VAR(V.VALvar{typ,...}) => !typ
    | AS.VAR(V.OVLDvar{name,...}) =>
(print ("#trueValType: OVLDvar"^Symbol.name name^"\n");
raise OVERLOAD)
| AS.VAR(V.ERRORvar) =>
bug "trueValType: ERRORvar\n"
| AS.CON(DATACON{name,typ,...}) =>
bug ("trueValType: DATACON"^Symbol.name name^"\n"))
| _ => bug "trueValType: not singleton path"
end

fun trueTycon (path: IP.path) =
let val err = fn _ => fn _ => fn _ => (bug "trueTycon: unbound ")
in case Lookup.lookTyc(static,ConvertPaths.invertIPath(path),err)
of DEFtyc x => SOME x
| _ => NONE
end

fun isLazyBogus (SymPath.SPATH path) =
case rev(String.explode (Symbol.name(List.last path)))
of #"$":: #","::_ => true
| _ => false

fun ppVar (VALvar{path, access, typ=(t0 as ref ty), prim, ...}) =
if isLazyBogus path then ()
else (
openHVBox ppstrm (PP.Rel 0);
openHOVBox ppstrm (PP.Rel 2);
PP.openHBox ppstrm;
pps "val"; sp (); pps (SymPath.toString path); sp(); pps "=";
PP.closeBox ppstrm;
sp ();
case access
of LVAR lv => (case StaticEnv.look (static, SymPath.last path)
of VALbind(VALvar{access=PATH (EXTERN pid, pos), ...}) =>
if isExport(lv, exportLvars)
then let
val SOME objv = DynamicEnv.look dynamic pid
val obj = xtract (objv, pos)
in
ppObj static ppstrm (obj, ty, !printDepth);
sp (); pps ":"; nbSp();
ppType static ppstrm (
trueValType path handle OVERLOAD => ty)
end
else (
PP.string ppstrm "<hidden>";
sp (); pps ":"; sp();
ppType static ppstrm ty)
| _ => PP.string ppstrm "<hidden>"
(* end case *))
| _ => ErrorMsg.impossible "ppDec.ppVar"
(* end case *);
closeBox ppstrm;
PP.newline ppstrm;
closeBox ppstrm)
| ppVar _ = ()

fun ppVb (VB{pat,...}) =
let fun ppBind(pat) =
case pat
of VARpat v => ppVar v
| RECORDpat{fields,...} => app (ppBind o #2) fields
| VECTORpat(pats,_) => app ppBind pats
| APPpat(_,_,pat) => ppBind pat
| CONSTRAINTpat(pat,_) => ppBind pat
| LAYEREDpat(pat1,pat2) => (ppBind pat1; ppBind pat2)
| ORpat(p1, _) => ppBind p1
| _ => ()
in ppBind pat
end

and ppRvb (RVB{var, ...}) = ppVar var

and ppTb (DEFtyc dt) =
let val {path,tyfun=TYFUN{arity,body},...} =
getOpt (trueTycon (#path dt), dt)
in
openHVBox ppstrm (PP.Rel 0);
openHOVBox ppstrm (PP.Rel 2);
PP.string ppstrm "type";
ppFormals ppstrm arity;
break ppstrm {nsp=1,offset=0};
ppSym ppstrm (InvPath.last path);
PP.string ppstrm " =";
break ppstrm {nsp=1,offset=0};
ppType static ppstrm body;
closeBox ppstrm;
PP.newline ppstrm;
closeBox ppstrm
end
| ppTb _ = bug "ppTb:DEFtyc"

and ppAbsTyc (GENtyc { path, arity, eq, ... }) =
(case !eq of
ABS =>
(openHVBox ppstrm (PP.Rel 0);
openHOVBox ppstrm (PP.Rel 2);
PP.string ppstrm "type";
ppFormals ppstrm arity;
break ppstrm {nsp=1,offset=0};
ppSym ppstrm (InvPath.last path);
closeBox ppstrm;
PP.newline ppstrm;
closeBox ppstrm)
| _ =>
(openHVBox ppstrm (PP.Rel 0);
openHOVBox ppstrm (PP.Rel 2);
PP.string ppstrm "type";
ppFormals ppstrm arity;
break ppstrm {nsp=1,offset=0};
ppSym ppstrm (InvPath.last path);
closeBox ppstrm;
PP.newline ppstrm;
closeBox ppstrm))
| ppAbsTyc _ = bug "unexpected case in ppAbsTyc"

and ppDataTyc (GENtyc { path, arity,
kind = DATATYPE{index, freetycs,
family={members, ...},...},
... }) =
let fun ppDcons nil = ()
| ppDcons (first::rest) =
let fun ppDcon ({name,domain,rep}) =
(ppSym ppstrm name;
case domain
of SOME dom =>
(PP.string ppstrm " of ";
ppDconDomain (members,freetycs)
static ppstrm dom)
| NONE => ())
in
PP.string ppstrm "= "; ppDcon first;
app (fn d => (break ppstrm {nsp=1,offset=0};
PP.string ppstrm "| "; ppDcon d))
rest
end
val {tycname,dcons,...} = Vector.sub(members,index)
in
openHVBox ppstrm (PP.Rel 0);
openHVBox ppstrm (PP.Rel 0);
PP.string ppstrm "datatype";
ppFormals ppstrm arity;
PP.string ppstrm " ";
ppSym ppstrm (InvPath.last path);
break ppstrm {nsp=1,offset=2};
openHVBox ppstrm (PP.Rel 0);
ppDcons dcons;
closeBox ppstrm;
closeBox ppstrm;
PP.newline ppstrm;
closeBox ppstrm
end
| ppDataTyc _ = bug "unexpected case in ppDataTyc"

and ppEb(EBgen{exn=DATACON{name,...},etype,...}) =
(openHVBox ppstrm (PP.Rel 0);
openHOVBox ppstrm (PP.Rel 2);
PP.string ppstrm "exception ";
ppSym ppstrm name;
case etype
of NONE => ()
| SOME ty' =>
(PP.string ppstrm " of";
break ppstrm {nsp=1,offset=0};
ppType static ppstrm ty');
closeBox ppstrm;
PP.newline ppstrm;
closeBox ppstrm)

| ppEb (EBdef{exn=DATACON{name,...}, edef=DATACON{name=dname,...}}) =
(openHVBox ppstrm (PP.Rel 0);
openHOVBox ppstrm (PP.Rel 2);
PP.string ppstrm "exception ";
ppSym ppstrm name;
PP.string ppstrm " =";
break ppstrm {nsp=1,offset=0};
ppSym ppstrm dname;
closeBox ppstrm;
PP.newline ppstrm;
closeBox ppstrm)

and ppStrb (STRB{name, str, ...}) = (
PP.openHVBox ppstrm (PP.Abs 0);
PP.openHBox ppstrm;
PP.string ppstrm "structure";
PP.space ppstrm 1;
ppSym ppstrm name;
PP.space ppstrm 1;
PP.string ppstrm ":";
PP.space ppstrm 1;
PP.closeBox ppstrm;
PPModules.ppStructure ppstrm (str,static,!signatures);
PP.newline ppstrm;
PP.closeBox ppstrm)

and ppFctb (FCTB{name, fct, ...}) =
(openHVBox ppstrm (PP.Rel 0);
pps "functor ";
ppSym ppstrm name;
case fct of
M.FCT { sign, ... } =>
PPModules.ppFunsig ppstrm (sign, static, !signatures)
| _ => pps " : <sig>";  (* blume: cannot (?) happen *)
PP.newline ppstrm;
closeBox ppstrm)

and ppSigb sign =
let val name = case sign
of M.SIG { name, ... } => getOpt (name, anonSym)
| _ => anonSym

in (openHVBox ppstrm (PP.Rel 0);
openHVBox ppstrm (PP.Rel 0);
pps "signature "; ppSym ppstrm name; pps " =";
break ppstrm {nsp=1,offset=2};
PPModules.ppSignature ppstrm (sign,static,!signatures);
closeBox ppstrm;
PP.newline ppstrm;
closeBox ppstrm)
end

and ppFsigb fsig =
let val name = case fsig
of M.FSIG{kind=SOME s, ...} => s
| _ => anonFsym

in (openHVBox ppstrm (PP.Rel 0);
pps "funsig "; ppSym ppstrm name;
PPModules.ppFunsig ppstrm (fsig,static,!signatures);
PP.newline ppstrm;
closeBox ppstrm)
end

and ppFixity{fixity,ops} =
(openHVBox ppstrm (PP.Rel 0);
openHVBox ppstrm (PP.Rel 0);
PP.string ppstrm (Fixity.fixityToString fixity);
PU.ppSequence ppstrm {sep=C break {nsp=1,offset=0},
pr=PU.ppSym,
style=INCONSISTENT}
ops;
closeBox ppstrm;
PP.newline ppstrm;
closeBox ppstrm)

and ppOpen(pathStrs) =
if !printOpens
then (openHVBox ppstrm (PP.Rel 0);
app (fn (path,str) =>
PPModules.ppOpen ppstrm (path,str,static,!signatures))
pathStrs;
closeBox ppstrm)
else (openHVBox ppstrm (PP.Rel 0);
openHVBox ppstrm (PP.Rel 0);
PP.string ppstrm "open ";
ppSequence ppstrm {sep=C break {nsp=1,offset=0},
pr=(fn ppstrm => fn (path,_)
=> (PP.string ppstrm (SymPath.toString path))),
style=INCONSISTENT}
pathStrs;
closeBox ppstrm;
PP.newline ppstrm;
closeBox ppstrm)

and ppDec0 dec =
case (resetPPType(); dec)
of VALdec vbs => app ppVb vbs
| VALRECdec rvbs => app ppRvb rvbs
| DOdec _ => ()
| TYPEdec tbs => app ppTb tbs
| DATATYPEdec{datatycs,withtycs} =>
(app ppDataTyc datatycs;
app ppTb withtycs)
| ABSTYPEdec{abstycs,withtycs,body} =>
(app ppAbsTyc abstycs;
app ppTb withtycs;
ppDec0 body)
| EXCEPTIONdec ebs => app ppEb ebs
| STRdec strbs => app ppStrb strbs
| FCTdec fctbs => app ppFctb fctbs
| SIGdec sigbs => app ppSigb sigbs
| FSIGdec fsigbs => app ppFsigb fsigbs
| LOCALdec(decIn,decOut) => ppDec0 decOut
| SEQdec decs =>
(case decs
of OPENdec pathStrs :: rest =>
ppOpen pathStrs
| _ => app ppDec0 decs)
| FIXdec fixd => ppFixity fixd
| OVLDdec _ =>
(PP.string ppstrm "overload"; PP.newline ppstrm)
| OPENdec pathStrs => ppOpen pathStrs
| MARKdec(dec,_) => ppDec0 dec

in openHVBox ppstrm (PP.Rel 0);
ppDec0 dec;
closeBox ppstrm;
flushStream ppstrm
end

end (* local *)
end (* structure PPDec *)

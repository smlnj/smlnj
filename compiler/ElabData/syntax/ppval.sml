(* ppval.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* modified to use smlnj-lib/PrettyPrint. [DBM, 2023.3.8]) *)

signature PPVAL =
sig
  val fmtAccess :       Access.access -> Formatting.format
  val fmtConrep :       Access.conrep -> Formatting.format
  val fmtDatacon :      Types.datacon -> Formatting.format
  val fmtDataconTyped : StaticEnv.staticEnv * Types.datacon -> Formatting.format
  val fmtDataconDebug : StaticEnv.staticEnv * Types.datacon -> Formatting.format
  val fmtConBinding :   StaticEnv.staticEnv * Types.datacon -> Formatting.format
  val fmtVar :          Variable.var -> Formatting.format
  val fmtVarTyped :     StaticEnv.staticEnv * Variable.var -> Formatting.format
  val fmtVarDebug :     StaticEnv.staticEnv * Variable.var -> Formatting.format
end (* signature PPVAL *)

structure PPVal : PPVAL =
struct

local
  structure PP = Formatting
  structure PPS = PPSymbols
  structure PPP = PPSymPaths
  structure PPT = PPType
  structure IP = InvPath
  structure TU = TypesUtil
  structure LU = Lookup
  structure A = Access
  structure LV = LambdaVar
  structure V = Variable
  structure T = Types
  structure SE = StaticEnv
in

val internals = ElabDataControl.varconInternals

exception PPVAL of string

fun fmtAccess a = PP.brackets (PP.text (A.accessToString a))

fun fmtConrep rep = PP.text (A.conrepToString rep)

fun fmtConsig csig = PP.text (A.consigToString csig)

fun fmtField (field : string, value : PP.format) =
    PP.hblock [PP.text field, PP.text "=", value]

fun fmtPrim prim = PP.text "<prim>"

fun fmtDatacon (T.DATACON {name, rep, ...} : T.datacon) =
    PP.cblock [PPS.fmtSym name,
	     case rep
	       of A.EXN acc => if !internals then fmtAccess acc else PP.empty
		| _ => PP.empty]

fun fmtDataconTyped (env: SE.staticEnv, T.DATACON{name, typ, ...}) =
    PP.pblock [PPS.fmtSym name, PP.colon, PPT.fmtType env typ]

fun fmtDataconDebug (env: SE.staticEnv, T.DATACON {name, rep, const, typ, sign, lazyp}) =
    PP.hblock
      [PP.text "DATACON",
	(PP.braces
	   (PP.vblock
	      [fmtField ("name", PPS.fmtSym name),
	       fmtField ("const", PP.bool const),
	       fmtField ("typ", PPT.fmtType env typ),
	       fmtField ("lazyp", PP.bool lazyp),
	       fmtField ("conrep", fmtConrep rep),
	       fmtField ("sign", PP.brackets (fmtConsig sign))]))]

(* fmtConBinding : SE.staticEnv * T.datacon -> PP.format
 * used in PPModule *)
fun fmtConBinding (env : SE.staticEnv, dcon: T.datacon) =
    (case dcon
       of T.DATACON{name, typ, rep=A.EXN _, ...} =>  (* exception constructor case *)
	    PP.pblock
	     [PP.text "exception",
	      PPS.fmtSym name,
	      if BasicTypes.isArrowType typ
              then PP.hblock [PP.text "of", PPT.fmtType env (BasicTypes.domain typ)]
              else PP.empty]
	| con =>      (* ordinary datacon case *)
	    let exception Hidden
		val visible =
		      let val tyc = TU.dataconTyc con
		       in (case TU.tycPath tyc
		             of NONE => raise (PPVAL "fmtConBinding: is hidden?")
			      | SOME (IP.IPATH syms) =>
				  (case syms
				     of nil => raise (PPVAL "fmtConBinding: null rpath")
				      | tycName :: _ => 
					  (TypesUtil.equalTycon
					     (LU.lookTyc (env, SymPath.SPATH [tycName],
							  fn _ => raise Hidden),
					      tyc))))
			  handle Hidden => false
		      end
	     in if visible orelse !internals
	        then PP.hblock [PP.text "con", fmtDatacon con]
	        else PP.empty
	    end)

fun fmtVar (V.VALvar {access,path,...}) =
       PP.cblock [PPP.fmtSymPath path,
		if !internals
		then (case access
		       of A.LVAR lvar => PP.cblock [PP.period, PP.text (LV.toString lvar)]
			| _ => fmtAccess access)
		else PP.empty]
  | fmtVar (V.OVLDvar {name,...}) = PPS.fmtSym name
  | fmtVar (V.ERRORvar) = PP.text "<errorvar>"

fun fmtVarTyped (env : SE.staticEnv, variable : V.var) =
    (case variable
       of V.VALvar {btvs, path, access, typ, prim} =>
	    PP.pblock
	      [PPP.fmtSymPath path,
	       if !internals then fmtAccess access else PP.empty,
	       PP.colon,
	       PPT.fmtType env (!typ)]
	| V.OVLDvar {name, ...} => PPS.fmtSym name
	| V.ERRORvar => PP.text "<ERRORvar>")

fun fmtVarDebug (env: SE.staticEnv, var: V.var) =
    (case var
      of V.VALvar {access,path, btvs, typ,prim} =>
	 PP.hblock
	   [PP.text "VALvar",
	    PP.braces
	      (PP.vblock
	         [fmtField ("access", fmtAccess access),
                  fmtField ("prim", fmtPrim prim),
		  fmtField ("path", PPP.fmtSymPath path),
		  fmtField ("typ", PPT.fmtType env (!typ))])]
       | V.OVLDvar {name,variants} =>
	 PP.hblock   
	   [PP.text "OVLDvar",
	    PP.braces (fmtField ("name", PPS.fmtSym (name)))]
       | V.ERRORvar => PP.text "<ERRORvar>")

end (* local *)
end (* structure PPVal *)

(* pptype.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* modified to use SML/NJ Lib PP. [dbm, 7/30/03]) *)

signature PPTYPE =
sig

  val typeFormals : int -> string list
  val tyvarToString : Types.tyvar -> string

  (* formatting functions *)
  val fmtTycon : StaticEnv.staticEnv -> Types.tycon -> PrettyPrint.format
  val fmtType  : StaticEnv.staticEnv -> Types.ty -> PrettyPrint.format
  val fmtTyfun : StaticEnv.staticEnv -> Types.tyfun -> PrettyPrint.format
  val fmtFormals : int -> PrettyPrint.format
  val fmtDconDomain : (Types.dtmember vector * Types.tycon list)
                      -> StaticEnv.staticEnv
                      -> Types.ty
		      -> PrettyPrint.format

  (* actual printing functions *)
  val ppTycon : StaticEnv.staticEnv -> Types.tycon -> unit
  val ppType  : StaticEnv.staticEnv -> Types.ty -> unit
  val ppTyfun : StaticEnv.staticEnv -> Types.tyfun -> unit
  val ppDconDomain : (Types.dtmember vector * Types.tycon list)
                     -> StaticEnv.staticEnv
                     -> Types.ty
		     -> unit

  val resetPPType : unit -> unit
  (* DBM: try to avoid the global state that requires this reset function *)

end (* signature PPTYPE *)

structure PPType : PPTYPE =
struct

local
  structure S = Symbol
  structure SP = SymPath
  structure IP = InvPath
  structure BT = BasicTypes
  structure T = Types
  structure TU = TypesUtil
  structure SE = StaticEnv
  structure PP = PrettyPrint
  structure PPS = PPSymbols
  open Types
in

val debugging = ElabDataControl.tpdebugging
val unalias = ElabDataControl.typeUnalias

fun bug s = ErrorMsg.impossible ("PPType: " ^ s)

fun say msg = (Control_Print.say msg; Control_Print.flush ())
fun saynl msg = (Control_Print.say (msg ^ "\n"); Control_Print.flush ())
fun saysnl msgs = saynl (concat msgs)
fun dbsaynl msg = if !debugging then saynl msg else ()
fun dbsaysnl msgs = if !debugging then saysnl msgs else ()

fun C f x y = f y x

val internals = ElabDataControl.typesInternals

val unitRPath = IP.extend (IP.empty, Symbol.tycSymbol "unit")  (* [<unit>] *)

(* boundTyvarName : int -> string
 * boundTyvarName translates a type variable represented as
 * an "index" (int) into a one- or two-character string from the sequence
 * "a", "b", ..., "z", "aa", "ab", ..., "az", "ba", ... "zz" *)
fun boundTyvarName (k: int) =
    let val base = Char.ord #"a"
     in if k < 26
	then String.str (Char.chr (base + k))
	else implode[Char.chr(Int.div(k,26) + base),
                     Char.chr(Int.mod(k,26) + base)]
    end

(* metaTyvarName' : int -> string
 * metaTyvarName' translates a meta type variable represented as
 * an "index" (int) into a one- or two-character string from the descending sequence
 * "Z", "Y", "X", ... *)
fun metaTyvarName' (k: int) =
    let val base = Char.ord #"Z" (* use reverse order for meta vars *)
     in if k < 26
	then String.str (Char.chr (base - k))
	else implode [Char.chr (base - (Int.div(k,26))),
                      Char.chr (base - (Int.mod(k,26)))]
    end

(* typeFormals : int -> string list *)
fun typeFormals n = List.tabulate (n, boundTyvarName)

local  (* WARNING -- compiler global variables. Imposes reset requirement. *)
  val count = ref (~1)
  val metaTyvars = ref (nil: tyvar list)
in
  fun metaTyvarName(tv: tyvar) =
      let fun find([],_) =
	        (metaTyvars := tv::(!metaTyvars);
		 count := !count+1;
		 !count)
	    | find(tv'::rest,k) =
	        if tv = tv'
		then !count - k
		else find(rest,k+1)
       in metaTyvarName' (find(!metaTyvars,0))
      end

  fun resetPPType() = (count := ~1; metaTyvars := [])
end

(* tyvarInternals : string * int option -> string *)
fun tyvarInternals (annotation, depthOp) =
    if !internals
    then String.concat
           (annotation::
	    (case depthOp
	       of SOME depth => ["[",(Int.toString depth),"]"]
	        | NONE => nil))
    else ""

(* sourcesToString : (string * 'a) list -> string *)
fun sourcesToString [(name,_)] = Symbol.name name
  | sourcesToString ((name,_)::rest) =
      String.concat [Symbol.name name, ",", sourcesToString rest]
  | sourcesToString nil = bug "sourcesToString"

(* tyvarHead : bool -> string *)
fun tyvarHead true = "''"
  | tyvarHead false = "'"

(* tyvarToString : tyvar -> string *)
fun tyvarToString (tyvar) =
    let fun prKind info =
	    case info
	     of INSTANTIATED(VARty(tyvar)) => tyvarToString tyvar
	      | INSTANTIATED _ => "[INSTANTIATED]"
	      | OPEN{depth,eq,kind} =>
		String.concat
		  [tyvarHead eq,
		   metaTyvarName tyvar,
		   tyvarInternals(case kind of META => ".M" | FLEX _ => ".F", SOME depth) ]
	      | UBOUND{name,depth,eq} =>
		String.concat
		  [tyvarHead eq, Symbol.name name, tyvarInternals(".U",SOME depth)]
	      | OVLDV{sources,eq} =>
		concat[tyvarHead eq, metaTyvarName tyvar,
		       "[OL(",sourcesToString sources,")]"]
	      | OVLDI sources =>
		concat["'",metaTyvarName tyvar, "[INT]"]
	      | OVLDW sources =>
		concat["'",metaTyvarName tyvar, "[WORD]"]
	      | LBOUND{depth,eq,index} =>
		concat[tyvarHead eq, metaTyvarName tyvar,
		       "[LB(", Int.toString depth, ".", Int.toString index, ")]"]
    in
	prKind (!tyvar)
    end

(* tyckindTag : T.tyckind -> string *)
fun tyckindTag (kind: T.tyckind) =
    case kind
      of PRIMITIVE => "P" | FORMAL => "F"
       | FLEXTYC _ => "X" | ABSTRACT _ => "A" | TEMP => "T"
       | DATATYPE {stripped,...} => if stripped then "DS" else "D"

(* fmtTyckind : T.tyckind -> format *)
fun fmtTyckind tyckind =
    PP.text (tyckindTag tyckind)

(* ppkind : T.tyckind -> unit *)
fun ppkind tyckind =
    PP.printFormat (fmtTyckind tyckind)

(* effectivePath : IP.path * T.tycon * SE.statenv -> string *)
fun effectivePath(path: IP.path, tyc: T.tycon, env: SE.staticEnv) : string =
    let val _ = dbsaysnl [">>> effectivePath: ", IP.toString path]
	fun find (path: IP.path, tycon: T.tycon) =
            let fun check tycon' = TU.equalTycon (tycon', tycon)
		fun look sympath =
		    SOME (Lookup.lookTyc (env, sympath, (fn _ => raise SE.Unbound)))
		    handle SE.Unbound => NONE
	     in ConvertPaths.findPath (path, check, look)
	    end
	fun search (path, tyc) =
	    let val (suffix, found) = find (path,tyc)
	     in if found then (suffix, true)
		else if not (!unalias) then (suffix, false)
		else case TU.unWrapDef1 tyc
		       of SOME tyc' =>
			   (case TU.tycPath tyc'
			      of SOME path' =>
				  let val x as (suffix', found') = search(path',tyc')
				   in if found' then x
				      else (suffix, false)
				  end
			       | NONE => (suffix, false))
			| NONE => (suffix, false)
	    end
	val (suffix,found) = search(path,tyc)
	val name = SP.toString(SP.SPATH suffix)
     in if found
	then name
	else "?."^name
    end

val arrowStamp = BT.arrowStamp

fun strength(ty) =
    case ty
      of VARty(ref(INSTANTIATED ty')) => strength(ty')
       | CONty(tycon, args) =>
	   (case tycon
 	      of GENtyc { stamp, kind = PRIMITIVE, ... } =>
		 if Stamps.eq(stamp,arrowStamp) then 0 else 2
	       | RECORDtyc (_::_) =>  (* excepting type unit *)
		 if Tuples.isTUPLEtyc(tycon) then 1 else 2
	       | _ => 2)
       | MARKty(ty, region) => strength ty
       | _ => 2

(* eqpropTag : T.eqprop -> string *)
fun eqpropTag (p: T.eqprop) =
    case p
      of NO => "NO"
       | YES => "YES"
       | IND => "IND"
       | OBJ => "OBJ"
       | DATA => "DATA"
       | ABS => "ABS"
       | UNDEF => "UNDEF"

(* fmtEqprop : T.eqprop -> PP.format *)
(* NOT USED! *)
fun fmtEqprop (p: T.eqprop) = PP.text (eqpropTag p)

(* fmtInvPath : InvPath.path -> PP.format *)
fun fmtInvPath (ipath: InvPath.path) =
    PP.text (SymPath.toString (ConvertPaths.invertIPath ipath))

(* formatBool : bool -> PP.format *)
(* bool values formated as "true" and "false" *)
fun formatBool (b: bool) = PP.text (Bool.toString b)

(* fmtBool : bool -> PP.format *)
(* bool values formated as "t" and "f" *)
fun fmtBool (b: bool) = PP.text (case b of true => "t" | false => "f")

val arrow = PP.text "->"
val langle = PP.text "<"
val rangle = PP.text ">"

(* NOTE: will need to reintroduce "internals" printing for GENtyc and PATHtyc tycons *)

(* fmtTycon1 : SE.staticEnv -> (T.dtmember vector * T.tycon list) option -> T.tycon -> PP.format *)
fun fmtTycon1 env (membersOp: (T.dtmember vector * T.tycon list) option) =
    let fun fmtTyc (tyc as GENtyc { path, stamp, eq, kind, ... }) =
	      PP.text (effectivePath (path,tyc,env))

	  | fmtTyc (tyc as DEFtyc {path, strict, tyfun=TYFUN{body,...}, ...}) =
	      PP.text (effectivePath (path,tyc,env))

	  | fmtTyc (RECORDtyc labels) =
	      PP.braces (PP.psequence PP.comma (map PPS.fmtSym labels))

          | fmtTyc (RECtyc n) =
              (case membersOp
                of SOME (members,_) =>
                     let val {tycname,dcons,...} = Vector.sub (members,n)
                      in PPS.fmtSym tycname
                     end
                 | NONE =>
		     PP.enclose {front = langle, back = rangle}
		       (PP.hcat [PP.text "RECtyc ", PP.integer n)])

          | fmtTyc (FREEtyc n) =
              (case membersOp
                of SOME (_, freetycs) =>
                    let val tyc = (List.nth(freetycs, n) handle _ =>
                                    bug "unexpected freetycs in fmtTyc")
                     in fmtTyc tyc
                    end
                 | NONE =>
		     PP.enclose {front = langle, back = rangle}
		       (PP.hcat [PP.text "FREEtyc ", PP.integer n)])

 	  | fmtTyc (tyc as PATHtyc {arity, entPath, path}) =
	      PP.text (SP.toString (ConvertPaths.stripPath path))

	  | fmtTyc ERRORtyc = PP.text "<ERRORtyc>"
     in fmtTyc
    end


(* fmtType1 : SE.staticEnv
              -> (T.ty * T.polysign * (T.dtmember vector * T.tycon list) option
              -> PP.format *)
fun fmtType1 env (ty: ty, sign: T.polysign,
                  membersOp: (T.dtmember vector * T.tycon list) option) : PP.format =
    let val unitFmt = PP.text (effectivePath (unitRPath, RECORDtyc [], env))
	fun fmtAtomTy (ty, str: int) =
	    let val fmt = fmtTy ty
	    in if strength ty <= str then PP.parens fmt else fmt
	    end
	and fmtTy ty =
	    case ty
	      of VARty(ref(INSTANTIATED ty')) => fmtTy(ty')
	       | VARty(tv) => fmtTyvar tv
	       | IBOUND n =>
		   let val eq = List.nth(sign,n)
		                handle Subscript => false
		    in PP.text (tyvarHead eq ^ boundTyvarName n)
		   end
	       | CONty(tycon, args) =>
		   let fun otherwise () =
			     PP.pcat [fmtTypeArgs args, fmtTycon1 env membersOp tycon]
		    in case tycon
		         of GENtyc { stamp, kind, ... } =>
			      (case kind
				 of PRIMITIVE =>
				      if Stamps.eq(stamp,arrowStamp)
				      then case args
					    of [domain,range] =>
					       let val domainFmt = fmtAtomTy (domain, 0)
						   val rangeFmt = fmtTy range
					       in PP.pcat [PP.hcat [domainFmt, arrow],
							   PP.indent 2 rangeFmt]
					       end
					     | _ => bug "CONty:arity"
				      else PP.hcat [fmtTypeArgs args, fmtTycon1 env membersOp tycon]
				  | _ => otherwise ())
			  | RECORDtyc labels =>
			     if Tuples.isTUPLEtyc(tycon)
			     then fmtTUPLEty args
			     else fmtRECORDty (labels, args)
			  | _ => otherwise ()
		   end
	       | POLYty{sign,tyfun=TYFUN{arity,body}} =>  (* dropped internals variant *)
                   fmtType1 env (body,sign, membersOp)
	       | MARKty(ty, region) => fmtTy ty
	       | WILDCARDty => PP.text "_"
	       | UNDEFty => PP.text "<UNDEFty>"

	and fmtTypeArgs [] = PP.empty
	  | fmtTypeArgs [ty] = fmtAtomTy (ty, 1)
	  | fmtTypeArgs tys = PP.tuple (map fmtTy tys)

	and fmtTUPLEty [] = unitFmt
	  | fmtTUPLEty tys =
	      let fun formatter ty = fmtAtomTy (ty, 1)
	       in PP.psequence (PP.text " *") (map formatter tys)
	      end

	and fmtField (lab,ty) = PP.hcat [PP.ccat [PPS.fmtSym lab, PP.colon], fmtTy ty]

	and fmtRECORDty (nil, nil) = unitFmt
              (* this case should not occur *)
	  | fmtRECORDty (labels, args) =
	      PP.braces (PP.psequence PP.comma (ListPair.map fmtField (labels, args)))

	and fmtTyvar (tv as (ref info) : tyvar) : PP.format =
	    let val printname = tyvarToString tv
	     in case info
		  of OPEN{depth,eq,kind} =>
		       (case kind
			  of FLEX fields =>
			      (case fields
				 of [] => PP.braces (PP.empty)
				  | fields =>
				      PP.braces
				        (PP.pcat
					   [PP.psequence PP.comma (map fmtField fields),
					    PP.semicolon,
					    PP.text printname]))
			   | _ => PP.text printname)
		   | _ => PP.text printname
	    end

     in fmtTy ty
    end  (* fmtType1 *)

and fmtType (env: SE.staticEnv) (ty: T.ty) : PP.format =
    fmtType1 env (ty, [], NONE)

(* fmtDconDomain : (Types.dtmember vector * Types.tycon list) -> StaticEnv.staticEnv -> Types.ty -> PrettyPrint.format *)
fun fmtDconDomain members (env: SE.staticEnv) (ty:T.ty) : PP.format =
    fmtType1 env (ty, [], SOME members)

fun fmtTycon env tycon = fmtTycon1 env NONE tycon

fun fmtTyfun env (TYFUN{arity,body}) =
    PP.hcat
       [PP.text "TYFUN",
	PP.braces (PP.hsequence PP.comma
		     [PP.label "arity" (PP.integer arity),
		      PP.label "body" (fmtType env body)])]

(* fmtFormals : int -> PP.format *)
fun fmtFormals n =
    let val formals = map (fn s => PP.text ("'" ^ s)) (typeFormals n)
     in case formals
 	  of nil => PP.empty
	   | [x] => x
  	   | fmts => PP.tuple fmts
    end

(* NOT USED!
(* fmtDataconsWithTypes : SE.staticEnv -> T.tycon -> PP.format *)
fun fmtDataconsWithTypes env tycon =
    (case tycon
       of GENtyc { kind = DATATYPE dt, ... } =>
	  let val {index, freetycs, family = {members,...}, ...} = dt
	      val {dcons, ...} = Vector.sub (members, index)
	   in PP.vcat
	        (map (fn {name, domain, ...} =>
			 PP.hcat [PP.ccat [PPS.fmtSym name, PP.colon],
				  case domain
				    of SOME ty =>
					 fmtType1 env (ty,[],SOME (members,freetycs))
				     | NONE => PP.text "CONST"])
		     dcons)
	  end
	| _ = bug "ppDataconTypes")
*)

(* ppTycon : SE.staticEnv -> T.tycon -> unit *)
fun ppTycon (env: SE.staticEnv) (tycon: T.tycon) = PP.printFormat (fmtTycon env tycon)

(* ppType : SE.staticEnv -> T.ty -> unit *)
fun ppType (env: SE.staticEnv) (ty: T.ty) = PP.printFormat (fmtType env ty)

(* ppTyfun : SE.staticEnv -> T.tyfun -> unit *)
fun ppTyfun (env: SE.staticEnv) (tyfun : T.tyfun) = PP.printFormat (fmtTyfun env tyfun)

(* ppDconDomain : (Types.dtmember vector * Types.tycon list) -> StaticEnv.staticEnv -> Types.ty -> unit *)
fun ppDconDomain members (env: SE.staticEnv) (ty:T.ty) : unit =
    PP.printFormat (fmtDconDomain members env ty)

end (* toplevel local *)
end (* structure PPType *)

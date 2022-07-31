(* FLINT/trans/generate.sml *)
(* based on revised "old" match compiler *)

(* generation of "code" (in the form of PLambda.lexp) from decision trees (type dectree) *)

structure Generate = 
struct

local
  structure T = Types
  structure BT = BasicTypes
  structure LV = LambdaVar
  structure A = Access
  structure V = Variable
  structure AS = Absyn
  structure AU = AbsynUtil
  structure EU = ElabUtil
  structure PO = Primop
  structure P = Paths
  structure MC = MCCommon
  structure MU = MCUtil
  structure PP = PrettyPrint

  open MCCommon

  val debugging = MCControl.mcdebugging
  val stats = MCControl.mcstats

  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())
  fun says strings = say (concat strings)
  fun saysnl strings = saynl (concat strings)

  fun dbsay msg =
      if !debugging
      then (say msg; newline())
      else ()
  fun dbsays msgs = dbsay (concat msgs)

  fun bug msg = ErrorMsg.impossible ("Generate: " ^ msg)

  fun ppAndor andor =
      PP.with_default_pp
	  (fn ppstrm =>
	      (PP.string ppstrm "andor:\n";
	       MCPrint.ppAndor ppstrm andor;
	       PP.newline ppstrm))

  fun ppDecisionTree dectree =
      PP.with_default_pp
	  (fn ppstrm =>
	      (PP.string ppstrm "dectree:\n";
	       MCPrint.ppDectree ppstrm dectree;
	       PP.newline ppstrm))

  fun ppExp (exp, msg) =
      PP.with_default_pp
          (fn ppstrm =>
	      (PP.string ppstrm msg;
	       PPAbsyn.ppExp (StaticEnv.empty, NONE) ppstrm (exp, 100);
	       PP.newline ppstrm))

  fun ppDec (dec, msg) =
      PP.with_default_pp
          (fn ppstrm =>
	      (PP.string ppstrm msg;
	       PPAbsyn.ppDec (StaticEnv.empty, NONE) ppstrm (dec, 100);
	       PP.newline ppstrm))

  fun ppPat pat =
      PP.with_default_pp(fn ppstrm => PPAbsyn.ppPat StaticEnv.empty ppstrm (pat, 20))

  fun ppVar var =
      PP.with_default_pp(fn ppstrm => PPVal.ppVar ppstrm var)

  fun ppType msg ty =
      PP.with_default_pp
	(fn ppstrm => (PP.string ppstrm (msg^": "); PPType.ppType StaticEnv.empty ppstrm ty))

  fun timeIt x = TimeIt.timeIt (!stats) x
			       
  (* intCon : int -> AS.con *)
  fun intCon n = AS.INTcon {ival = IntInf.fromInt n, ty = Target.defaultIntSz}

in

(* How are singleton constructor patterns, and in particular the
 * "special" ones like *ref* and *susp*?  In the general case we generate a single
 * datacon "deconstructor" expressed as a single-variant SWITCH in these cases.
 * It is possible that the single variant dies and this reduces to a SWITCH
 * with empty cases and a default (see MC Notes, sec 41.2).
 *
 * The special cases (ref,susp) are handled in genSwitch (along with switch on
 * intinf integers). Otherwise, MC.SWITCH translates almost directly to Plambda.SWITCH.
 *)

(* -------------------------------------------------------------------------------- *)
(* generate: main match "code generation" function
 * top level code generating function for matches (formerly MCCode.genCode) *)

(* rules: we can assume the rhs exp has been (match-) translated *)
type rule = Absyn.pat * AS.exp
type mvar = V.var

(* generate : MC.decTree * ruleMap * ruleset * failInfo * T.ty -> AS.exp * mvar *)
fun generate (decTree: MC.dectree, ruleMap: Preprocessing.ruleMap, allRules: ruleset,
	      failExnOp : T.datacon option, rhsTy: T.ty) : AS.exp * V.var =
    let val _ = dbsay ">> generate"

    (* relativeExp : (var: V.var) * (suffix: P.link list) -> exp *)
    fun relativeExp (var: V.var, suffix: P.link list) =
	let fun wrapSuffix (nil, exp) = exp
	      | wrapSuffix (P.PI n::rest, exp) = wrapSuffix (rest, AS.RSELECTexp(exp, n))
	      | wrapSuffix (P.VI (n, elemTy)::rest, exp) =
		  wrapSuffix (rest, AS.VSELECTexp (exp, elemTy, n))  (* exp : elemTy vector *)
	      | wrapSuffix (suffix, exp) =
		(saynl ("relativeExp:wrapSuffix suffix: " ^ P.pathToString suffix);
		 bug "wrapSuffix")
	    val suffix' = 
		case suffix
		 of P.DC(AS.VLENcon _):: _ => tl suffix
		  | _ => suffix
	in wrapSuffix (suffix', EU.varToExp var)
	end

    (* pathToExp : MU.pathenv -> P.path -> AS.exp *)
    fun pathToExp pathenv path =
(*	(saysnl ["pathToExp:path = ", P.pathToString path]; *)
	case MU.lookPath (pathenv, path)
	  of NONE => bug "pathToExp"
	   | SOME(mvar, suffix) => relativeExp (mvar, suffix)

    (* genRHS : ruleno * pathenv -> PL.lexp
     *  invoking the rule RHS function (fvar ~ abstracted rhs) on record of mvars corresponding to
     *    lhs pattern variables (which could be the empty record).
     *  The case where there are no pvars in the pattern is handled by default case in body. *)
    fun genRHS (ruleno: MC.ruleno, pathenv: MU.pathenv) =
	  let val (varPaths, fvar, _) = ruleMap ruleno
	       val argLexp =
	           case varPaths
		     of [path] => pathToExp pathenv path  (* single pvar pattern *)
		      | _ => AU.TUPLEexp (map (pathToExp pathenv) varPaths)
			 (* for multiple pvars, _or none_ *)
	   in AS.APPexp(EU.varToExp fvar, argLexp)
	   end

    val rootvar = MU.mkMvar ()
    val pathenv0 = MU.bindPath (MU.emptyPathenv, P.rootpath, rootvar)

    fun isVector ((AS.VLENcon _, _) :: _) = true
      | isVector _ = false

(* requirements:
 *  -- need to be able to determine if an MC.SWITCH is a vector-length switch.
 *     Look at the first con in the cases list to see if it is VLENcon.
 *  -- for a vector-length switch, check if the andor node is bound to an mvar in the pathenv
 *     (lookPath (node-path, pathenv) = SOME (mvar, nil) (empty suffix).  If so, this mvar
 *     serves as the "name" of the vector value, and is used as the decon vector for each
 *     vector-length case. Case VLENcon cons are replaced by INTcons.
 *     If the node is not mapped to an mvar by pathenv (suffix not nil), then have to introduce
 *     a fresh mvar to denote the vector; it will be LET-bound to the subject expression
 *     created by relativeExp (mvar, suffix) and then used as the decon variable for all the
 *     cases. This fresh mvar is not in the pathEnv! Yet! But it will be added to the pathenv used
 *     to generate code for the vector elements.*)

    (* genDecTree: decTree * pathenv -> AS.exp
     *   pathenvs are grown by adding new path --> var bindings at:
     *     (1) each non-constant datacon case for the decon[datacon] of the value
     *     (2) each vector switch node in the dectree, unless that node's is already mapped by pathenv,
     *         the new var denotes the vector value in this case
     *   In case (1), the new var is also added to the srule (as SOME var) to be incorporated into
     *   the PL.DATAcon in Translate..conToCon (and eventually (where?) bound to the decon[datacon] of
     *   the corresponding value).
     *)
    fun genDecTree (decTree, pathenv) =
	(case decTree
	   of MC.SWITCH {path, sign, cases, defaultOp,   ...} =>
		let val subject =  (* exp denoting switch scrutinee (subject) value *)
			(case MU.lookPath (pathenv, path)
			  of NONE => bug "genDecTree:SWITCH: no mvar bound at prefix of this OR path"
			   | SOME (baseMvar, suffix) => relativeExp (baseMvar, suffix))
		 in (case cases
		       of nil => bug "genDecTree: null cases"
		        | (AS.VLENcon (_, elemTy), _) :: _ =>  (* vector-length switch *)
			    let val (vecmvar, newvar) =
				    (case subject
				       of AS.VARexp (ref mvar, _) => (mvar, false)  (* null suffix, mvar bound to vector *)
					| _ => (MU.mkMvar (), true))  (* ow, create a new mvar to denote vector *)
					     (* if switch subject is a vector, check if subject exp is an mvar
					      * (e.g. rootvar). If not, we need to bind the subject exp to a fresh mvar
					      * and use that mvar as the the decon-bound variable (in this case
					      * bound to the vector) in each case. This new mvar will be bound to
					      * path in a modified pathenv. *)
				 fun genVCase (AS.VLENcon(n,_), decTree) =
				     (* this is where VLENcon cons are converted to INTcon. dcvarOp is NONE because
				      *  the mvar designating the vector serves this purpose. *)
				       let val newPathenv =
					       if newvar (* vecmvar is a new mvar to be bound to vector *)
					       then MU.bindPath (pathenv, path, vecmvar)
					       else pathenv
					in AS.SRULE (intCon n, NONE, genDecTree(decTree, pathenv))
				       end
				   | genVCase _ = bug "genDecTree:genVCase: not a vector-length case"
				 val switchCases = map genVCase cases
				 val default' =
				     (case defaultOp
				       of NONE => bug "genDecTree: vector switch with no default"
					| SOME dectree => genDecTree (dectree, pathenv))
			     in if newvar
				then AS.LETVexp (vecmvar, subject,  (* let-binding vector variable *)
						 AS.VSWITCHexp (EU.varToExp vecmvar, elemTy, switchCases, default'))
				else AS.VSWITCHexp (subject, elemTy, switchCases, default')
			    end
			| _ => (* other, non-vector, switches *)
			    let fun genCase (con, decTree) =
				    if AU.constantCon con
				    then AS.SRULE (con, NONE, genDecTree(decTree, pathenv))
					     (* saysnl ["genDecTree.genCase:con = ", conToString con, "\n"] *)
				    else let (* val _ = saysnl ["genDecTree.genCase:con = ", AU.conToString con] *)
					     val deconMvar = MU.mkMvar() (* new decon variable denoting decon[con] result *)
					     val newPathenv = MU.bindPath (pathenv, P.addLink(P.DC con, path), deconMvar)
					       (* the new path binding designates the decon for this case *)
					  in AS.SRULE (con, SOME deconMvar, genDecTree(decTree, newPathenv))
					 end
				val defaultOp' = Option.map (fn dt => genDecTree (dt, pathenv)) defaultOp
			     in AS.SWITCHexp (subject, map genCase cases, defaultOp')
				  (* not a vector length switch *)
			    end)
		end
            | MC.FAIL =>
		let val failExp = 
			(case failExnOp
			  of NONE => EU.varToExp rootvar
			   | SOME datacon => AS.CONexp(datacon, nil))
		 in AS.RAISEexp (failExp, rhsTy)
		end
	    | MC.RHS ruleno => genRHS (ruleno, pathenv)
	        (* dispatch to (i.e. call) appropriate rhs function for this ruleno, with argument _expressions_
                 * corresponding to the pvars in the lhs pattern *)
	(* end case *))
        (* end genDecTree *)

    val matchExp = genDecTree (decTree, pathenv0)

 in (matchExp, rootvar)
end (* fun generate *)

end (* top local *)
end (* structure Generate *)

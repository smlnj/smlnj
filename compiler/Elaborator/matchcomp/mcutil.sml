(* FLINT/match/mcutil.sml; revmc *)

structure MCUtil = (* needs signature *)
struct

local
  structure AS = Absyn
  structure LV = LambdaVar
  structure TU = TypesUtil
  structure V = Variable
  structure P = Paths
  structure MC = MCCommon
  open MCCommon

  fun bug s = ErrorMsg.impossible ("MCUtil: " ^ s)
  fun say msg = (Control_Print.say msg; Control_Print.flush ())
  fun says msgs = say (concat msgs)
  fun saynl msg = (say (msg^"\n"))
  fun saysnl msgs = (saynl (concat msgs))
  fun newline () = say "\n"

in

(* getId : andor -> nodeId option *)
fun getId (AND{id, ...}) = SOME id
  | getId (OR{id, ...}) = SOME id
  | getId (VAR{id, ...}) = SOME id
  | getId WC = NONE

(* getPath : andor -> path option *)
fun getPath (OR{path,...}) = SOME path
  | getPath _ = NONE

(* -------------------------------------------------------------------------------- *)
(* patterns *)

(* mkRECORDpat : AS.pat -> AS.pat list -> AS.pat
 *  1st arg must be a nonflex record pat
 *  USED(1): Preprocess (preprocess.sml) *)
fun mkRECORDpat (AS.RECORDpat{fields, flex=false, typ}) pats =
      AS.RECORDpat {flex = false, typ = typ,
                 fields = ListPair.map (fn((id,_),p)=>(id,p)) (fields, pats)}
  | mkRECORDpat (AS.RECORDpat{flex=true,...}) _ =
      bug "mkRECORDpat - flex record"
  | mkRECORDpat _ _ = bug "mkRECORDpat - non-record"

type varpaths = (V.var * P.path) list  (* association list mapping var to path *)

(* varPaths : pat -> varpaths
 *  produces a "varpaths" ((var, path) alist) mapping each variable in the pattern
 *  to the path identifying its _unique_ location in the pattern.
 *  REQUIRES: pat is OR-free, so variable occurrences (& hence their paths) are unique. *)
fun varPaths (pat: AS.pat) =
    let fun scan (AS.VARpat v, rpath) = [(v, P.rpathToPath rpath)]
	  | scan (AS.CONSTRAINTpat(pat,_), rpath) = scan(pat, rpath)
	  | scan (AS.LAYEREDpat(pat1, pat2), rpath) =
	      scan(pat1, rpath) @ scan(pat2, rpath)
	  | scan (AS.APPpat(dcon,tvs,pat), rpath) =
	      scan(pat, P.DC(AS.DATAcon(dcon,tvs)) :: rpath)
	  | scan (AS.RECORDpat{fields,...}, rpath) =
	      let fun foldFields (n, (lap,pat), varpaths) =
		      scan (pat, P.addLinkR (P.PI n, rpath)) @ varpaths
	      in List.foldri foldFields nil fields
	      end
	  | scan (AS.VECTORpat(pats,ty), rpath) =
	    let fun foldElement (n, pat, varpaths) =
		    scan (pat, P.addLinkR (P.VI (n,ty), rpath)) @ varpaths
	       in List.foldri foldElement nil pats
	      end
	  | scan (AS.MARKpat (pat,_), rpath) = scan (pat, rpath) (* MARKpat is tranparent *)
	  | scan (AS.ORpat _, _) = bug "varPaths:scan - unexpected OR pattern"
	  | scan _ = nil
    in scan (pat, nil)
    end

(* lookupVar : (V.var * path) list -> valvar -> path
 *   alist lookup of a var in a "pathenv" alist produced by varPaths *)
fun lookupVar ((V.VALvar{path=p2,...}, path)::rest) (v as V.VALvar{path=p1,...}) =
      if SymPath.equal(p1,p2) then path else lookupVar rest v  (* p1, p2 will have length 1 *)
  | lookupVar [] _ = bug "lookupVar unbound"
  | lookupVar _ _ = bug "lookupVar unexpected arg"

(* bindingPaths : V.var list -> AS.pat -> path list
 * ASSUME: variables in var list are the pattern variables, in some particular order.
 * That order will be maintained by the resulting path list for rhs linkage. *)
fun bindingPaths variables pat =
    map (lookupVar (varPaths pat)) variables

(* -------------------------------------------------------------------------------- *)
(* mvars *)

(* mvar : vars used as administrative or destructuring variables to designate intermediate
 *  values during a match *)
type mvar = V.var (* == int *)

(* mkMvar : unit -> V.var *)
fun mkMvar () = V.lvarToVar (LV.mkLvar ())

(* -------------------------------------------------------------------------------- *)
(* pathenv: path -> mvar environments *)

(* pathenv is a (path, mvar) alist sorted by path length (longest first) *)
type pathenv = (P.path * mvar) list

val emptyPathenv = nil

(* lookPath : pathenv * P.path -> (mvar * P.link list) option
 *  looks for the longest prefix of path in pathenv and returns the associated mvar and the (shortest) suffix.
 *  the shortest suffix (hence longest prefix) will be unique if it exists
 *  ASSERT: the suffix contains only index links (PI, VI) *)
fun lookPath (nil,_) = NONE
  | lookPath (pathenv, path) =
    let fun look (nil, best) = best
	  | look ((p,m)::rest, best) =
	    (case P.suffix (p, path)
	      of NONE => look(rest,best) (* p is not a prefix of path *)
	       | SOME suf =>
		   let val len = length suf
		    in case best
			 of NONE => look (rest, SOME(len, suf, m))
			  | SOME(len', suf', m') =>
			      if len < len'
			      then look (rest, SOME (len, suf, m)) (* bug: can't happen *)
			      else best (* can't hope for shorter suffix, since pathenv paths are getting shorter *)
		   end)
    in case look(pathenv, NONE)
	 of NONE => NONE
	  | SOME (_, suf, m) => SOME(m, suf)
    end

(* bindPath : pathenv * P.path * mvar -> pathenv *)
fun bindPath (nil, path, mvar) = [(path, mvar)]
  | bindPath (pathenv as ((p,m)::rest), path, mvar) = 
      if P.pathLength path < P.pathLength p
      then (p,m) :: bindPath (rest, path, mvar)
      else (path, mvar) :: pathenv

end (* top local *)
end (* structure MCUtil *)

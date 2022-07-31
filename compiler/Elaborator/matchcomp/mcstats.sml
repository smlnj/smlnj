(* mcstats.sml *)

(* collection of match compiler statistics *)

structure MCStats =
struct

local

  structure LV = LambdaVar
  open MCCommon
  structure RS = RuleSet

  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())
  fun says strings = saynl (concat strings)

  fun incr (r: int ref) = r := !r + 1

  val stats : bool ref = MCControl.mcstats

in

(* andor stats *)

type andorStats =
     {numAND : int, numOR : int, numVAR : int, numTotal : int}

val andorStats : andorStats option ref = ref NONE

fun collectAndorStats (andor: andor) : unit =
    let val countAND = ref 0
	val countOR = ref 0
	val countVAR = ref 0
        fun scanTree (VAR _) = incr countVAR
	  | scanTree (AND {children,...}) =
	    (incr countAND;
	     app scanTree children)
	  | scanTree (OR {cases,...}) =
	    (incr countOR;
	     app scanSubcase (map #3 cases))
	  | scanTree WC = ()
	and scanSubcase CONST = ()
	  | scanSubcase (VELEMS elems) = app scanTree elems
	  | scanSubcase (DCARG andor) = scanTree andor
        val _ = scanTree andor
	val numAND = !countAND
	val numOR = !countOR
	val numVAR = !countVAR
	val total = numAND + numOR + numVAR
     in scanTree andor;
        andorStats := SOME {numAND = numAND, numOR = numOR, numVAR = numVAR, numTotal = total}
    end


(* dectree stats *)

structure NodeMap = IntRedBlackMap

type dectreeStats =
     {rulesUsed : RS.set,
      numSWITCH : int,
      numRHS : int,
      numFAIL : int,
      numTotal : int,
      choiceDist : int NodeMap.map}

val dectreeStats : dectreeStats option ref = ref NONE

(* collectDectreeStats : dectree * int -> decTreeStats
 *  returns the set of all rules used in the dectree, maintaining ordering
 *  (because union operation does). The boolean value indicates presence of FAIL,
 *  signalling that the rules are non-exhaustive. *)
fun collectDectreeStats (dectree: dectree): unit =
    let val rules = ref RS.empty
	val countFAIL = ref 0
	val countRHS = ref 0
	val countSWITCH = ref 0
	val choiceDist = ref NodeMap.empty
	fun scanTree (RHS n) = (incr countRHS; rules := RS.add (!rules, n)) 
	  | scanTree FAIL = incr countFAIL
	  | scanTree (SWITCH {id, cases, defaultOp, ...}) =
	    (incr countSWITCH;
	     choiceDist :=
		let val nmap = !choiceDist
		    val newcount =
			case NodeMap.find (nmap, id)
			 of NONE => 1
			  | SOME k => k+1
		in NodeMap.insert(nmap, id, newcount)
		end;
	     app scanTree (map #2 cases);
	     Option.app scanTree defaultOp)
    in scanTree dectree;
       dectreeStats :=
         SOME {rulesUsed = !rules,
	       numFAIL = !countFAIL,
	       numRHS = !countRHS,
	       numSWITCH = !countSWITCH,
	       numTotal = !countSWITCH + !countRHS + !countFAIL,
	       choiceDist = !choiceDist}
    end

val initialLvar : LV.lvar ref = ref (LV.nextLvar ())
val finalLvar : LV.lvar ref = ref (LV.nextLvar ())

val caseThreshold = 0

fun reportStats () =
    case (!andorStats, !dectreeStats)
      of (SOME {numAND, numOR, numVAR, numTotal},
	  SOME {rulesUsed, numSWITCH, numFAIL, numRHS, ...}) =>
	 if numSWITCH > caseThreshold then
	     (saynl "andor stats:";
	      says ["  total = ", Int.toString numTotal];
	      says ["  #AND  = ", Int.toString numAND];
	      says ["  #OR   = ", Int.toString numOR];
	      says ["  #VAR  = ", Int.toString numVAR]; newline ();
	      saynl "dectree stats:";
	      says ["  #SWITCH = ", Int.toString numSWITCH];
	      says ["  #RHS    = ", Int.toString numRHS];
	      says ["  #FAIL   = ", Int.toString numFAIL]; newline ();
	      saynl "lvars:";
	      says ["  initial = ", Int.toString (LV.toId(!initialLvar) + 1)];
	      says ["  final   = ", Int.toString (LV.toId(!finalLvar))];
	      says ["  total   = ", Int.toString (LV.diff(!initialLvar, !finalLvar))]; newline ())
	 else ()
       | _ => ()


end (* top local *)
end (* structure MCStats *)

(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* pflatten.sml *)

structure PFlatten : PFLATTEN =
struct

local
  structure LT = Lty
  structure LD = LtyDef
  structure LK = LtyKernel
  structure LV = LambdaVar
  structure F = FLINT
  structure FU = FlintUtil
in

type lexp = FLINT.lexp
type value = FLINT.value
type lvar = LambdaVar.lvar

fun bug s = ErrorMsg.impossible ("Pflatten:" ^ s)
val mkv = LambdaVar.mkLvar
val say = Control.Print.say

(*****************************************************************************
 *                 FUNCTIONS USED BY PLAMBDA TO FLINT NORMALIZATION          *
 *****************************************************************************)
(* recursively turn cooked types into raw when possible ??? *)

fun v_punflattenGen ltys = 
  (fn (lv, lexp) => 
     let val lvs = map (fn _ => mkv()) ltys 
      in (lvs, F.RECORD(FU.rk_tuple, map F.VAR lvs, lv, lexp)) 
     end) 

fun v_pflattenGen ltys =
  (fn v => 
     let val lvs = map (fn _ => mkv()) ltys 
      in (map (fn v => F.VAR v) lvs, 
          fn lexp => 
             #1 (foldl (fn (lv, (lexp, field)) => 
	                  (F.SELECT(v, field, lv, lexp), field+1)) 
	          (lexp, 0) lvs)) 
     end) 

val v_punflattenDef = fn (lv, lexp) => ([lv], lexp) 
val v_pflattenDef = fn v => ([v], fn lexp => lexp) 


(* punflatten: (lvar * F.lexp) -> (lvar list * F.lexp)
 *   turn `lexp' from an expression expecting a single value bound to `lvar'
 *   to an expression expecting multiple values to be bound to `lvar list'.
 *   It seems generally more convenient to choose the `lvar list' inside
 *   bundlefn than outside.
 * pflatten: value -> (value list * (lexp -> lexp))
 *   expand `value' into its flattened `value list' around `lexp'.
 *   The `value list' might be required in order to construct the
 *   `lexp' argument, which explains the fact that `value' and `lexp'
 *   are passed in two steps. *)

fun t_pflatten (lty : LT.lty) = LK.lt_autoflat lty

fun v_punflatten (lty : LT.lty) = 
  let val x as (_, ltys, flag) = LK.lt_autoflat lty
   in (x, if flag then v_punflattenGen ltys else v_punflattenDef)
  end

fun v_pflatten   (lty : LT.lty) = 
  let val x as (_, ltys, flag) = LK.lt_autoflat lty
   in (x, if flag then v_pflattenGen ltys else v_pflattenDef)
  end


(*****************************************************************************
 *                 FUNCTIONS USED BY FLINT TYPE SPECIALIZATION               *
 *****************************************************************************)

fun v_unflattenGen ltys = 
  (fn ([lv], lexp) => 
         let val lvs = map (fn _ => mkv()) ltys 
          in (lvs, F.RECORD(FU.rk_tuple,
                            map F.VAR lvs, lv, lexp)) 
         end
    | _ => bug "unexpected case in v_unflattenGen")

fun v_flattenGen ltys =
  (fn [v] => 
        let val lvs = map (fn _ => mkv()) ltys 
         in (map (fn x => F.VAR x) lvs, 
             fn lexp => 
                #1 (foldl (fn (lv, (lexp, field)) => 
	                  (F.SELECT(v, field, lv, lexp), field+1)) 
	            (lexp, 0) lvs)) 
        end 
    | _ => bug "unexpected case in v_flattenGen")

val v_unflattenDef = fn (vs, lexp) => (vs, lexp) 
val v_flattenDef = fn vs => (vs, fn lexp => lexp) 

fun t_flatten ([flty], false) = LK.lt_autoflat flty
  | t_flatten (fltys, true) = (true, fltys, false)
  | t_flatten _ = bug "unexpected case in ltc_flat"

fun v_unflatten ([flty], false) = 
      let val x as (_, fltys, flag) = LK.lt_autoflat flty
       in (x, if flag then v_unflattenGen fltys else v_unflattenDef)
      end
  | v_unflatten (fltys, false) = ((true, fltys, false), v_unflattenDef)
  | v_unflatten (fltys, true) = ((true, fltys, false), v_unflattenDef)

fun v_flatten ([flty], false) = 
      let val x as (_, fltys, flag) = LK.lt_autoflat flty
       in (x, if flag then v_flattenGen fltys else v_flattenDef)
      end
  | v_flatten (fltys, false) = ((true, fltys, false), v_flattenDef)
  | v_flatten (fltys, true) = ((true, fltys, false), v_flattenDef)


(*****************************************************************************
 *                 FUNCTIONS USED BY FLINT REPRESENTATION ANALYSIS           *
 *****************************************************************************)

(* NOTE: the implementation of v_coerce should be consistent with that
   of v_flattenGen and v_unflattenGen *)
fun v_coerce (wflag, nftcs, oftcs) =         
    let val nlen = length nftcs
	val olen = length oftcs
     in if nlen = olen
	then (oftcs, NONE)
	else if (nlen = 1) andalso ((olen > 1) orelse (olen = 0))
	then ([LD.tcc_tuple oftcs],
	      if wflag then
		  let val v = mkv()
		  in SOME (fn vs => 
			      ([F.VAR v], 
			       fn le => F.RECORD(FU.rk_tuple, vs, v, le)))
		  end
	      else SOME (v_flattenGen (map LD.ltc_tyc oftcs)))
	else bug "unexpected case in v_coerce"
    end (* function v_coerce *)

end (* local *)
end (* structure PFlatten *)


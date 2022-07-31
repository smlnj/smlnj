(* recover.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* recover the type information of a closed FLINT program *)
signature RECOVER =
sig

  val recover : (FLINT.prog * bool) -> (FLINT.value -> Lty.lty)

end (* signature RECOVER *)

structure Recover : RECOVER =
struct

local
  structure LT = Lty
  structure LD = LtyDef
  structure LB = LtyBasic
  structure LE = LtyExtern
  structure DI = DebIndex
  structure LV = LambdaVar
  structure PL = PLambda
  open FLINT
in

fun bug s = ErrorMsg.impossible ("Recover: "^s)

(* ltInst : LT.lty * LT.tyc list -> LT.lty *)
fun ltInst (lt, ts) =
    (case LE.lt_inst(lt, ts)
       of [x] => x
        | _ => bug "ltInst")

(** these two functions are applicable to the types of primops and data
    constructors only (ZHONG) *)
(* arglty : LT.lty * LT.tyc list -> LT.lty *)
fun arglty (lt, ts) =
  let val (_, atys, _) = LD.ltd_arrow(ltInst(lt, ts))
   in case atys of [x] => x
                 | _ => bug "arglty"
  end

(* reslty : LT.lty * LT.tyc list -> LT.lty *)
fun reslty (lt, ts) =
  let val (_, _, rtys) = LD.ltd_arrow(ltInst(lt, ts))
   in case rtys
        of [x] => x
         | _ => bug "reslty"
  end

structure T = LV.Tbl

fun recover (fdec: prog, postRep : bool) =
    let exception RecoverLty
	val ltyTable : LT.lty T.hash_table = T.mkTable(32, RecoverLty)
	fun lookup v =
	    T.lookup ltyTable v
	    handle RecoverLty => bug ("recover/get " ^ LV.toString v)
	val addv = T.insert ltyTable
	fun addvs vts = app addv vts
	fun getlty (VAR v) = lookup v
	  | getlty (INT{ty, ...}) = LB.ltc_num ty
	  | getlty (WORD{ty, ...}) = LB.ltc_num ty
	  | getlty (REAL _) = LB.ltc_real
	  | getlty (STRING _) = LB.ltc_string

	val lt_nvar_cvt = LE.lt_nvar_cvt_gen()

        (* loop : depth -> lexp -> lty list *)
        fun loop e =
	    let fun lpv u = getlty u
		fun lpvs vs = map lpv vs

		fun lpd (fk, f, vts, e) =
		  (addvs vts; addv (f, LE.ltc_fkfun(fk, map #2 vts, lpe e)))

		and lpds (fds as ((fk as {isrec=SOME _, ...},_,_,_)::_)) =
		      let fun h ((fk as {isrec=SOME (rts,_), ...},
				 f, vts, _) : fundec) =
				addv(f, LE.ltc_fkfun(fk, map #2 vts, rts))
			    | h _ = bug "unexpected case in lpds"
			  val _ = app h fds
		       in app lpd fds
		      end
		  | lpds [fd] = lpd fd
		  | lpds _ = bug "unexpected case 2 in lpds"

		and lpc (PL.DATAcon((_,_,lt), ts, v), e) =
		      (addv (v, arglty(lt, ts)); lpe e)
		  | lpc (_, e) = lpe e

		and lpe (RET vs) = lpvs vs
		  | lpe (LET(vs, e1, e2)) =
		      (addvs (ListPair.zip(vs, lpe e1)); lpe e2)
		  | lpe (FIX(fdecs, e)) = (lpds fdecs; lpe e)
		  | lpe (APP(u, vs)) =
		      let val u' = lpv u
		      in (#2(LE.ltd_fkfun u')
			  handle LD.DeconExn =>
			   (print "\nError Application:\n";
			    PrintFlint.printLexp (APP(u, vs));
			    raise LD.DeconExn))
		      end
		  | lpe (TFN((tfk, v, tvks, e1), e2)) =
		      (addv(v, LE.lt_nvpoly(tvks, loop e1));
		       lpe e2)
		  | lpe (TAPP(v, ts)) = LE.lt_inst (lpv v, ts)
		  | lpe (RECORD(rk,vs,v,e)) =
		      (addv (v, LE.ltc_rkind(rk, lpvs vs)); lpe e)
		  | lpe (SELECT(u,i,v,e)) =
		      (addv (v, LE.ltd_rkind(lpv u, i)); lpe e)
		  | lpe (CON((_,_,lt),ts,_,v,e)) =
		      (addv (v, reslty(lt, ts)); lpe e)
		  | lpe (SWITCH(_, _, ces, e)) =
		      let val lts = map lpc ces
		       in case e of NONE => hd lts
				  | SOME e => lpe e
		      end
		  | lpe (RAISE (_, lts)) = lts
		  | lpe (HANDLE(e, _)) = lpe e
		  | lpe (BRANCH(p, _, e1, e2)) =
		      let val _ = lpe e1
		       in lpe e2
		      end
		  | lpe (PRIMOP((_,Primop.WCAST, lt, []), _, v, e)) =
		      if postRep
		      then (case LD.ltd_fct lt
			      of ([_],[r]) => (addv(v, r); lpe e)
			       | _ => bug "unexpected case for WCAST")
		      else bug "unexpected primop WCAST in recover"
		  | lpe (PRIMOP((_,_,lt,ts), _, v, e)) =
		      (addv (v, reslty (lt, ts)); lpe e)

	     in lpe e handle LD.DeconExn => (print "\nWhole Expr:\n";
					     PrintFlint.printLexp e; bug "ltd decon")
	    end (* function loop *)

	val (fkind, f, vts, e) = fdec
	val _ = addvs vts
	val atys = map #2 vts
	(* val _ = PrintFlint.printLexp e *)
	val rtys = loop e
	val _ = addv (f, LE.ltc_fkfun(fkind, atys, rtys))

     in getlty
    end (* function recover *)

end (* local *)
end (* structure Recover *)

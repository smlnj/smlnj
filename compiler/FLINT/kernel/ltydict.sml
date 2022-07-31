(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* ltydict.sml *)

signature LTYDICT =
sig 
  type tyc = Lty.tyc
  type lty = Lty.lty

  val tmemo_gen : {tcf: (tyc -> 'a) -> (tyc -> 'a),
                   ltf: ((tyc -> 'a) * (lty -> 'b)) -> (lty -> 'b)} 
                  -> {tc_map: tyc -> 'a, lt_map: lty -> 'b}

  val wmemo_gen : {tc_wmap : ((tyc -> 'a) * (tyc -> 'a)) -> (tyc -> 'a),
                   tc_umap : ((tyc -> 'a) * (tyc -> 'a)) -> (tyc -> 'a),
                   lt_umap : ((tyc -> 'a) * (lty -> 'b)) -> (lty -> 'b)}
                  -> {tc_wmap : tyc -> 'a,
                      tc_umap : tyc -> 'a, 
                      lt_umap : lty -> 'b,
                      cleanup : unit -> unit}

end (* signature LTYDICT *)

structure LtyDict : LTYDICT = 
struct 

type tyc = Lty.tyc
type lty = Lty.lty

structure TcDict = RedBlackMapFn(struct
                                   type ord_key = tyc
				   val compare = Lty.tc_cmp
			         end)

structure LtDict = RedBlackMapFn(struct
                                   type ord_key = lty
				   val compare = Lty.lt_cmp
			         end)

fun tmemo_gen {tcf, ltf} =
  let val m1 = ref (TcDict.empty)
      val m2 = ref (LtDict.empty)

      fun tc_look t = 
        (case TcDict.find(!m1, t)
          of SOME t' => t'
           | NONE => 
               let val x = (tcf tc_look) t
                   val _ = (m1 := TcDict.insert(!m1, t, x))
                in x
               end)

      and lt_look t = 
        (case LtDict.find(!m2, t)
          of SOME t' => t'
           | NONE => 
               let val x = ltf (tc_look, lt_look) t
                   val _ = (m2 := LtDict.insert(!m2, t, x))
                in x
               end)
   in {tc_map=tc_look, lt_map=lt_look}
  end (* tmemo_gen *)

fun wmemo_gen {tc_wmap, tc_umap, lt_umap} = 
  let val m1 = ref (TcDict.empty)
      val m2 = ref (TcDict.empty)
      val m3 = ref (LtDict.empty)

      fun tcw_look t = 
        (case TcDict.find(!m1, t)
          of SOME t' => t'
           | NONE => 
               let val x = (tc_wmap (tcw_look, tcu_look)) t
                   val _ = (m1 := TcDict.insert(!m1, t, x))
                in x
               end)

      and tcu_look t = 
        (case TcDict.find(!m2, t)
          of SOME t' => t'
           | NONE => 
               let val x = (tc_umap (tcu_look, tcw_look)) t
                   val _ = (m2 := TcDict.insert(!m2, t, x))
                in x
               end)

      and ltu_look t = 
        (case LtDict.find(!m3, t)
          of SOME t' => t'
           | NONE => 
               let val x = lt_umap (tcu_look, ltu_look) t
                   val _ = (m3 := LtDict.insert(!m3, t, x))
                in x
               end)

      fun cleanup () = ()
   in {tc_wmap=tcw_look, tc_umap=tcu_look, lt_umap=ltu_look, cleanup=cleanup}
  end (* wmemo_gen *)

end (* structure LtyDict *)

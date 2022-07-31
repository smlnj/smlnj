(*
 * Compute typed liveness information for garbage collection safety
 *)
functor GCLiveness
  (structure IR : MLRISC_IR
   structure GC : GC_TYPE
   structure InsnProps : INSN_PROPERTIES
      sharing InsnProps.I = IR.I
  ) : GC_LIVENESS =
struct

  structure IR  = IR
  structure C   = IR.I.C
  structure CFG = IR.CFG
  structure GC  = GC
  structure G   = Graph
  structure GCTypeMap = GCTypeMap(GC)
  structure R = GCTypeMap
  structure A = Array

  structure Liveness =
      Dataflow
         (structure CFG = CFG
          type domain  = R.typemap
          val  forward = false
          val  bot     = R.empty
          val  ==      = R.==
          val join     = R.joins
          type dataflow_info = 
                (C.cell -> GC.gctype) * 
                { liveIn : R.typemap, liveOut : R.typemap } A.array
          fun mk(gcmap,regs) =
              R.fromList(map (fn r => (r,gcmap r)) regs)

          fun liveOut(gcmap,b as CFG.BLOCK{id,...}) = 
          let val cellset = CFG.liveOut(b)
              val regs    = C.CellSet.toCellList cellset
          in  mk(gcmap,regs)
          end

          val defUseR = InsnProps.defUse C.GP
          val defUseF = InsnProps.defUse C.FP

          fun scan(gcmap,CFG.BLOCK{insns,...}) = 
          let fun loop([],def,use) = (def,use)
                | loop(i::is,def,use) =
                  let val (d1,u1) = defUseR i 
                      val (d2,u2) = defUseF i 
                      val d = mk(gcmap,d1 @ d2)
                      val u = mk(gcmap,u1 @ u2)
                      (* val _ = print("d="^R.toString d^" ")
                      val _ = print("u="^R.toString u^"\n")
                      val _ = print("use-d="^R.toString(R.kill(use,d))^"\n")*)
                      val use = R.gen(R.kill(use,d),u)
                      val def = R.kill(R.gen(def,d),u)
                      (*val _ = print("def="^R.toString def^" ")
                      val _ = print("use="^R.toString use^"\n") *)
                  in  loop(is,def,use) 
                  end
          in  loop(!insns,R.empty,R.empty) end

          fun prologue (_,(gcmap,_)) (b,b') =
          let val (def,use) = scan(gcmap,b')
              val liveOut   = liveOut(gcmap,b')
          in  (* print("Liveout("^Int.toString b^")="^R.toString liveOut^"\n");
              print("def("^Int.toString b^")="^R.toString def^"\n");
              print("use("^Int.toString b^")="^R.toString use^"\n"); *)
              { input    = liveOut,
                output   = R.gen(R.kill(liveOut,def),use),
                transfer = fn liveOut => R.gen(R.kill(liveOut,def),use)
              }
          end
          fun epilogue (_,(_,table)) 
              {node=(b,_), input=liveOut, output=liveIn } = 
               ((* print("Livein("^Int.toString b^")="^R.toString liveIn^"\n");
                print("Liveout("^Int.toString b^")="^R.toString liveOut^"\n");*)
                A.update(table,b,{liveIn=liveIn,liveOut=liveOut})
               ) 
         )

  fun liveness (IR as G.GRAPH cfg) = 
  let val an = !(CFG.annotations IR)
      val table = A.array(#capacity cfg (),{liveIn=R.empty,liveOut=R.empty})
      fun gclookup(C.CELL{an, ...}) = #lookup GC.GC_TYPE (!an)
      val _ = Liveness.analyze(IR,(gclookup,table))
  in  table
  end

end

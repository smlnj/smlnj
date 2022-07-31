(*
 * This module is responsible for propagating gc type information.
 *)
functor GCTyping
   (structure IR : MLRISC_IR
    structure GCProps : GC_PROPERTIES
    structure GCMap : GC_MAP
    structure Props : INSN_PROPERTIES
       sharing GCMap.GC = GCProps.GC
       sharing IR.I = GCProps.I = Props.I
   ) : GC_TYPING =
struct

   structure IR  = IR
   structure CFG = IR.CFG
   structure GC  = GCProps.GC
   structure G   = Graph
   structure An  = Annotations 

   fun gcTyping(IR as G.GRAPH cfg) =
       case #get GCMap.GCMAP (CFG.getAnnotations IR)
       of NONE => IR (* no gc map; do nothing *)
        | SOME gcmap =>
       let val lookup = Intmap.map gcmap
           val add    = Intmap.add gcmap
           fun update(dst,ty) = 
               (lookup dst; ()) handle _ => add(dst,ty)
           fun move(dst,src) = 
               (lookup dst; ()) handle _ => 
                   (add(dst,lookup src) handle _ => ())
           val prop = GCProps.propagate {lookup=lookup,update=update} 
           fun process(b,CFG.BLOCK{insns,...}) = 
           let fun scan [] = ()
                 | scan(i::is) =
                   (case Props.instrKind i of
                      Props.IK_COPY =>
                        let val (dst,src) = Props.moveDstSrc i
                            fun copy(d::ds,s::ss) = (move(d,s); copy(ds,ss))
                              | copy _ = ()
                        in  copy(dst,src)
                        end
                    | _ => prop i handle _ => ();
                    scan is
                   )
           in  scan(rev(!insns))
           end
       in  #forall_nodes cfg process;
           IR
       end 

end

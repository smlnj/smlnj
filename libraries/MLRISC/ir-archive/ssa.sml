(*
 * SSA placement module.  This is the algorithm from Cytron et al.'s
 * TOPLAS paper.  This module is kept generic so that we can also use it
 * to compute sparse evaluation graphs, factored redef/use chains (of Wolfe)
 * etc.
 *
 * This implementation uses Sreedhar et al.'s DJ-graph to compute
 * the iterated dominance frontier, which should be slightly faster
 * than the default implementation.
 *
 * For the stack of renamed variables, we use the scheme proposed
 * by Briggs, Cooper, Harvey and Simpson in Software Practice & Experience
 * 1988.  (Actually we don't)
 *
 * -- Allen
 *)

functor StaticSingleAssignmentForm
   (Dom : DOMINATOR_TREE) : STATIC_SINGLE_ASSIGNMENT_FORM =
struct
   structure Dom     = Dom
   structure G       = Graph
   structure A       = Array

   type var = int
   type phi = var * var * var list
   type renamer = {defs : var list, uses: var list} ->
                  {defs : var list, uses: var list}
   type copy    = {dst : var list, src : var list} -> unit

   structure DJ = DJGraph(Dom)  

   fun app f = 
   let fun g [] = ()
         | g (x::xs) = (f x; g xs)
   in  g end

   (*
    * Place join nodes at the iterated dominance frontier of def_sites(v)
    * that is live.
    *)
   fun place_joins (Dom as G.GRAPH dom) 
       { max_var=V, defs, is_live } =
   let val N           = #capacity dom ()
       val G.GRAPH cfg = Dom.cfg Dom
       val def_sites   = A.array(V,[]) (* indexed by var *)
       val phis        = A.array(N,[]) (* indexed by block id *)

       (* compute the def sites of all variables *)
       val _ = #forall_nodes cfg
                (fn (n,block) =>
                   app (fn v => A.update(def_sites,v,n::A.sub(def_sites,v)))
                       (defs(n,block))
                )
       (* compute phi placements for a variable *)
       val IDFs = DJ.IDFs Dom
       fun place_phi(v,[])        = ()
         | place_phi(v,def_sites) = 
           let fun place_all [] = ()
                 | place_all(Y::Ys) = 
                   (if is_live(v,Y) then
                       A.update(phis,Y,(v,v,[])::A.sub(phis,Y))
                    else (); 
                    place_all Ys)
           in   place_all (IDFs def_sites)
           end

       val _ = A.appi place_phi (def_sites,0,NONE)
   in  phis
   end

   (*
    *  Rename variables and compute the ssa form
    *)
   fun compute_ssa (Dom as G.GRAPH dom) 
          { max_var=V, defs, is_live, rename_stmt, insert_phi, rename_var } =
   let val N           = #capacity dom ()
       val G.GRAPH cfg = Dom.cfg Dom
       val [ENTRY]     = #entries dom ()
       val phis        = place_joins Dom {max_var=V,defs=defs,is_live=is_live}
       val stacks      = A.array(V,[])  (* indexed by var *)
       val in_edges    = A.array(N,[])

           (* Lookup the current renaming of v *)
       fun lookup v = 
           case A.sub(stacks,v) of 
             v'::_ => v'
           | _     => v

           (* Retract one entry of v *)
       fun pop v = case A.sub(stacks,v) of _::l => A.update(stacks,v,l)

       fun search X =
       let val X' = #node_info cfg X
           val old_defs = ref []

           fun rename_use v = 
           if v < 0 then v
           else
           let val vs = A.sub(stacks,v)
               val v' = case vs of v'::_ => v' | _ => v
           in  v'
           end

           fun rename_uses [] = []
             | rename_uses (v::vs) = rename_use v::rename_uses vs

               (* rename a definition of v *)
           fun rename_def v =
           let val v' = rename_var v
               val vs = A.sub(stacks,v)
           in  A.update(stacks,v,v'::vs);
               old_defs := v :: !old_defs;
               v'
           end

           fun rename_defs [] = []
             | rename_defs (v::vs) = rename_def v::rename_defs vs

           fun copy_def(v,v') =
               (A.update(stacks,v,v'::A.sub(stacks,v));
                old_defs := v :: !old_defs)

               (* parallel copy *)
           fun copy {dst,src} =
               ListPair.app copy_def (dst,rename_uses src)

               (* rename statement of the form defs := uses in block X 
                * We must rename the uses first!!! 
                *)
           fun rename {defs,uses} =
           let val uses' = rename_uses uses
               val defs' = rename_defs defs
           in  {defs=defs',uses=uses'}
           end

               (* rename the definition of phi functions *) 
           fun rename_phi_def X =
           let val X_phis = A.sub(phis,X)
               fun rn [] = []
                 | rn((v',v,uses)::rest) = (v',rename_def v,uses)::rn rest
               val X_phis = rn X_phis
           in  A.update(phis,X,X_phis) 
           end

               (* rename the uses of phi functions *) 
           fun rename_phi_use X =
           let val out_edges = #out_edges cfg X
               fun rename_phi_of_Y (e as (X,Y,_)) =
               let val Y_phis = A.sub(phis,Y)
                   fun insert_uses [] = []
                     | insert_uses((v',v,uses)::rest) = 
                         (v',v,rename_use v'::uses)::insert_uses rest
               in  A.update(in_edges,Y,e::A.sub(in_edges,Y));
                   A.update(phis,Y,insert_uses Y_phis)
               end
           in  app rename_phi_of_Y out_edges
           end

       in
           rename_phi_def X;
           rename_stmt {rename=rename,copy=copy} (X,X');    
           rename_phi_use X;
           app search (#succ dom X);
           app pop (!old_defs)
       end
       
          (* place phis *) 
       fun place_phi (B as (b,_)) = 
            insert_phi{block=B,in_edges=A.sub(in_edges,b),phis=A.sub(phis,b)}
           
   in
       search ENTRY;
       #forall_nodes cfg place_phi
   end
                         
end


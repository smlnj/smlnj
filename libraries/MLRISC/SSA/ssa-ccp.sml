(* 
 * Conditional Constant Propagation
 *)
functor SSACCP(CCP : SSA_CONDITIONAL_CONSTANT_PROPAGATION) : SSA_OPTIMIZATION =
struct
   structure SSA = CCP.SSA
   structure CF  = CCP.CF
   structure A   = Array

   type flowgraph = SSA.ssa

   val name = "conditional constant propagation"

   fun error msg = MLRiscErrorMsg.error("SSACCP",msg)

   fun run SSA =
   let val valueMap   = CCP.condConstProp SSA 
       val defSiteTbl = SSA.defSiteTbl SSA
       val showVal    = SSA.showVal SSA
       val showOp     = SSA.showOp SSA
       val showVN     = CF.showVN SSA
   in  A.appi (fn (v,vn) =>
         if vn <> CF.bot andalso vn <> CF.top then
            (let val i = A.sub(defSiteTbl,v)
             in  print(showVal v^" "^showOp i^" = "^showVN vn^"\n")
             end 
            )
         else ())
         (valueMap, 0, NONE);
       SSA
   end
end

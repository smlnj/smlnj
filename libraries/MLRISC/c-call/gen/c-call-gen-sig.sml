signature C_CALL_GEN =
  sig

    structure T : MLTREE
    structure SA : STAGED_ALLOCATION
      where type reg_id = T.reg
      where type loc_kind = CLocKind.loc_kind

    datatype c_arg 
      = ARG of T.rexp	
	  (* rexp specifies integer or pointer; if the 
           * corresponding parameter is a C struct, then 
	   * this argument is the address of the struct. 
	   *)
      | FARG of T.fexp
	  (* fexp specifies floating-point argument *)

    (* write a C argument (non aggregate) to a machine location
     *   - arg is the argument data
     *   - off is an offset into the argument data
     *   - loc is the machine location
     *   - stms is the accumulator of machine instructions
     *)
    val writeLoc : c_arg -> (T.rexp * SA.loc * T.stm list) -> T.stm list
  (* write C arguments to parameter locations; also return any used GPRs and FPRs *)
    val writeLocs : (c_arg list * SA.loc list list) -> (T.stm list * T.reg list * (int * T.reg) list)

  (* read from a machine location *)
    val readLoc : (SA.loc * (T.mlrisc list * T.stm list)) -> (T.mlrisc list * T.stm list) 
  (* read from some machine locations *)
    val readLocs : SA.loc list -> (T.mlrisc list * T.stm list)

  end

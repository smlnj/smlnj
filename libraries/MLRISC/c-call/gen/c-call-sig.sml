signature C_CALL = 
  sig

    structure T : MLTREE
    structure Gen : C_CALL_GEN
      where T = T

    datatype c_arg = datatype Gen.c_arg

    val layout : CType.c_proto -> {
	    argLocs : Gen.SA.loc list list,	        (* argument/parameter assignment; nested lists are for passing structs *)
	    argMem : {szb : int, align : int},	        (* memory requirements for stack-allocated *)
						        (* arguments; this value can be passed to *)
						        (* the paramAlloc callback. *)
	    resLocs : Gen.SA.loc list,	                (* result location *)
	    structRetLoc : {szb : int, align : int} option
	  }

  (* translate a C function call with the given argument list into
   * a MLRISC statement list.  The arguments are as follows:
   *
   *	name			-- an expression that speficies the function.
   *	proto			-- the function's prototype
   *	paramAlloc		-- this callback takes the size and alignment
   *				   constraints on the parameter-passing area
   *				   in the stack.  If it returns true, then the
   *				   space for the parameters is allocated by
   *				   client; otherwise genCall allocates the space.
   *    structRet		-- this callback takes the size and alignment
   *				   of space required for returning a struct
   *				   value.  It returns the address of the
   *				   reserved space.
   *	saveRestoreDedicated	-- this callback takes a list of registers
   *				   that the call kills and should return an
   *				   instruction sequence to save/restore any
   *				   registers that the client run-time model
   *				   expects to be preserved (e.g., allocation
   *				   pointers).
   *    callComment		-- if present, the comment string is attached
   *				   the CALL instruction as a COMMENT annotation.
   *    args			-- the arguments to the call.  The assumption is
   *				   that any required sign or zero extension has
   *				   already been done.
   *
   * The result of genCall is a mlrisc list specifying where the result
   * is and the MLRisc statements that implement the calling sequence.
   * Functions with void return type have no result, most others have
   * one result, but some conventions may flatten larger arguments into
   * multiple registers (e.g., a register pair for long long results).
   *
   * The implementation of genCall will return a statement sequence with the
   * following order:
   *
   *	<argument area allocation>
   *	<setup arguments>
   *	<save dedicated registers>
   *	<call C function>
   *	<restore dedicated registers>
   *	<free argument area>
   *	<copy result into fresh registers>
   *
   * WARNING: if the client's implementation of structRet uses the stack
   * pointer to address the struct-return area, then paramAlloc should always
   * handle allocating space for the parameter area (i.e., return true).
   *)
    val genCall : {
	    name  : Gen.T.rexp,
            proto : CType.c_proto,
	    paramAlloc : {szb : int, align : int} -> bool,
            structRet : {szb : int, align : int} -> Gen.T.rexp,
	    saveRestoreDedicated :
	      Gen.T.mlrisc list -> {save: Gen.T.stm list, restore: Gen.T.stm list},
	    callComment : string option, 
            args : Gen.c_arg list
	  } -> {
	    callseq : Gen.T.stm list,
	    result: Gen.T.mlrisc list
	  }

  (* Callee-save registers as defined in the C calling convention.  Note that
   * these do not include special registers (e.g., stack and frame-pointers)
   * that are preserved across calls.
   *)
    val calleeSaveRegs : Gen.T.reg list	(* C callee-save registers *)
    val calleeSaveFRegs : Gen.T.reg list	(* C callee-save floating-point registers *)

    val callerSaveRegs : Gen.T.reg list	(* C caller-save registers *)
    val callerSaveFRegs : Gen.T.reg list	(* C caller-save floating-point registers *)

  end (* C_CALL *)

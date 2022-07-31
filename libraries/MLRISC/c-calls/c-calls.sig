(* c-calls.sig
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *)

signature C_CALLS =
  sig

    structure T : MLTREE

    datatype c_arg 
      = ARG of T.rexp	
	  (* rexp specifies integer or pointer; if the 
           * corresponding parameter is a C struct, then 
	   * this argument is the address of the struct. 
	   *)
      | FARG of T.fexp
	  (* fexp specifies floating-point argument *)
      | ARGS of c_arg list
	  (* list of arguments corresponding to the contents of a C struct *)

  (* this constant is the offset from the caller's SP to the low-address of the
   * parameter area (see the paramAlloc callback below).
   *)
    val paramAreaOffset : int

  (* The MLRISC type that describes the natural size of integer arguments (i.e., what
   * small integers are promoted to).
   *)
    val naturalIntSz : T.ty

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
	    name  : T.rexp,
            proto : CTypes.c_proto,
	    paramAlloc : {szb : int, align : int} -> bool,
            structRet : {szb : int, align : int} -> T.rexp,
	    saveRestoreDedicated :
	      T.mlrisc list -> {save: T.stm list, restore: T.stm list},
	    callComment : string option,
            args : c_arg list
	  } -> {
	    callseq : T.stm list,
	    result: T.mlrisc list
	  }

  (* the location of arguments/parameters; offsets are given with respect to the
   * low end of the parameter area (see paramAreaOffset above).
   *)
    datatype arg_location
      = Reg of T.ty * T.reg * T.I.machine_int option
					(* integer/pointer argument in register *)
      | FReg of T.fty * T.reg * T.I.machine_int option
					(* floating-point argument in register *)
      | Stk of T.ty * T.I.machine_int	(* integer/pointer argument in parameter area *)
      | FStk of T.fty * T.I.machine_int	(* floating-point argument in parameter area *)
      | Args of arg_location list

    val layout : CTypes.c_proto -> {
	    argLocs : arg_location list,	(* argument/parameter assignment *)
	    argMem : {szb : int, align : int},	(* memory requirements for stack-allocated *)
						(* arguments; this value can be passed to *)
						(* the paramAlloc callback. *)
	    resLoc : arg_location option,	(* result location; NONE for void functions *)
	    structRetLoc : {szb : int, align : int} option
	  }

  (* Callee-save registers as defined in the C calling convention.  Note that
   * these do not include special registers (e.g., stack and frame-pointers)
   * that are preserved across calls.
   *)
    val calleeSaveRegs : T.reg list	(* C callee-save registers *)
    val calleeSaveFRegs : T.reg list	(* C callee-save floating-point registers *)

  end

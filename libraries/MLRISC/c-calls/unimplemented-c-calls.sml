(* unimplemented-c-calls.sml
 *
 *   A dummy (placeholder) "implementation" of the c-calls interface.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *)
functor UnimplementedCCallsFn
	    (structure T: MLTREE
	     val impossible: string -> 'a) :> C_CALLS where T = T =
struct
    structure T = T

    datatype c_arg 
      = ARG of T.rexp	
      | FARG of T.fexp
      | ARGS of c_arg list

    fun genCall _ = impossible "C-calls not implemented (genCall)"

    val paramAreaOffset = 0

    val naturalIntSz = 32

    datatype arg_location
      = Reg of T.ty * T.reg * T.I.machine_int option
      | FReg of T.fty * T.reg * T.I.machine_int option
      | Stk of T.ty * T.I.machine_int
      | FStk of T.fty * T.I.machine_int
      | Args of arg_location list

    fun layout _ = impossible "C-calls not implemented (layout)"

    val calleeSaveRegs : T.reg list = []
    val calleeSaveFRegs : T.reg list = []
end

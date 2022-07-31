
(*---------------------------------------------------------------------------
 * First, some front-end dependent stuff.  Typically, you only need
 * one instance of these things for each source language.
 *---------------------------------------------------------------------------*)

(*
 * User defined constant type.  Dummy for now.
 * In practice, you'll want to use this type to implement constants with
 * values that cannot be determined until final code generation, e.g.
 * stack frame offset.
 *)
structure UserConst =
struct
   type const = unit
   fun toString() = ""  
   fun hash() = 0w0  
   fun valueOf _ = 0
   fun == _ = true  
end

(*
 * Instantiate label expressions with respect to user defined constants.
 * This type is somewhat misnamed; it is used to represent constant 
 * expressions.
 *)
(* structure LabelExp = LabelExp(UserConst) *)

(*
 * User defined datatype for representing aliasing.   Dummy for now.
 * You'll need this to represent aliasing information. 
 *)
structure UserRegion =
struct
   type region = unit
   fun toString () = "" 
   val memory = ()
   val stack = ()
   val readonly = ()
   val spill = ()
end

(*
 * User defined datatype for representing pseudo assembly operators.
 * Dummy for now.
 *
 * You'll need this to represent assembler directives. 
 *)
structure UserPseudoOps =
struct
   type pseudo_op = unit  
   fun toString () = ""
   fun emitValue _ = ()
   fun sizeOf _ = 0
   fun adjustLabels _ = true
end


(*
 * Instruction stream datatype.
 * This is just a simple record type used by MLRISC to represent 
 * instruction streams.
 *)
(*structure Stream = InstructionStream(UserPseudoOps)*)

(*
 * Client defined extensions.  None for now.
 * You'll need this only if you need to extend the set of MLTREE operators
 *)
structure UserExtension =
struct

   type ('s,'r,'f,'c) sx = ('s,'r,'f,'c) AMD64InstrExt.sext
   type ('s,'r,'f,'c) rx = unit
   type ('s,'r,'f,'c) fx = unit
   type ('s,'r,'f,'c) ccx = unit

end

(*
 * This module controls how we handle user extensions.  Since we don't
 * have any yet.  This is just a bunch of dummy routines.
 *)
functor UserMLTreeExtComp
	    (    structure I : AMD64INSTR where T.Extension = UserExtension
    structure TS : MLTREE_STREAM where T = I.T
    structure CFG : CONTROL_FLOW_GRAPH where I = I and P = TS.S.P
   ) : MLTREE_EXTENSION_COMP =
struct
    structure T = TS.T
    structure TS = TS
    structure I = I
    structure CFG = CFG
    structure C = I.C

    structure CompInstrExt = AMD64CompInstrExt (
      structure I = I
      structure TS = TS
      structure CFG = CFG)

    type reducer =
	  (I.instruction,C.cellset,I.operand,I.addressing_mode,CFG.cfg) TS.reducer

    val compileSext = CompInstrExt.compileSext

    fun compileRext _ = raise Fail "AMD64CompExtFn.compileRext"
    fun compileFext _ = raise Fail "AMD64CompExtFn.compileFext"
    fun compileCCext _ = raise Fail "AMD64CompExtFn.compileCCext"
(*
   structure T = T
   structure I = I
   structure C = I.C
   type reducer =
     (I.instruction,C.cellset,I.operand,I.addressing_mode) T.reducer
   fun unimplemented _ = MLRiscErrorMsg.impossible "UserMLTreeExtComp"
   val compileSext  = unimplemented
   val compileRext  = unimplemented
   val compileFext  = unimplemented
   val compileCCext = unimplemented
*)
end


(* hppaRegAlloc.sml --- hppa integer and floating register allocator
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* Integer and floating register allocators are a partial application
 * of a curried functor. 
 *)

functor HppaRegAlloc(structure I : INSTRUCTIONS where C = HppaCells
		     structure P : INSN_PROPERTIES where I = I
		     structure F : FLOWGRAPH where I = I and P = P
		     structure Asm : INSTRUCTION_EMITTER where I = I and P = P) :
  sig
    functor IntRa (structure RaUser : RA_USER_PARAMS
		     where I = I
		     where type B.name = F.B.name) : RA
    functor FloatRa (structure RaUser : RA_USER_PARAMS
		     where I = I
		     where type B.name = F.B.name) : RA
   end=
struct

  structure C=I.C

  (* liveness analysis for general purpose registers *)
  structure RegLiveness = 
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.GP
	     val regSet = C.getCell C.GP
	     val cellset = C.updateCell C.GP)


  functor IntRa =
    RegAllocator
       (structure RaArch = struct
	   structure InsnProps = P
	   structure AsmEmitter = Asm
	   structure I = I
	   structure Liveness=RegLiveness

	   val defUse = P.defUse C.GP
	   val firstPseudoR = 32
	   val maxPseudoR = C.maxCell
	   val numRegs = C.numCell C.GP
	   val regSet = C.getCell C.GP
	end)

  (* liveness analysis for floating point registers *)
  structure FregLiveness = 
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.FP
	     val regSet = C.getCell C.FP
	     val cellset = C.updateCell C.FP)

  functor FloatRa =
    RegAllocator
       (structure RaArch = struct
	   structure InsnProps = P
	   structure AsmEmitter = Asm
	   structure I = I
	   structure Liveness=FregLiveness

 	   val defUse = P.defUse C.FP
	   val firstPseudoR = 64
	   val maxPseudoR = C.maxCell 
	   val numRegs = HppaCells.numCell C.FP
	   val regSet = C.getCell C.FP
	end)
end


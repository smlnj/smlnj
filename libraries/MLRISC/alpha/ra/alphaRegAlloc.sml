(* alphaRegAlloc.sml --- alpha integer and floating register allocator
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* Integer and floating register allocators are a partial application
 * of a curried functor. 
 *)



functor AlphaRegAlloc(structure I : INSTRUCTIONS where C = AlphaCells
			structure P : INSN_PROPERTIES where I = I
			structure F : FLOWGRAPH where I = I 
			structure Asm : INSTRUCTION_EMITTER 
			  where I = I and P = F.P) :
   sig
    functor IntRa (structure RaUser : RA_USER_PARAMS
		     where I = I
		     where type B.name = F.B.name
		     (* should be: where I = I -- bug 1205 *)) : RA
    functor FloatRa (structure RaUser : RA_USER_PARAMS
		       where I = I
		       where type B.name = F.B.name
		       (* should be: where I = I *)) : RA
   end =
struct
  structure C = I.C
    (* liveness analysis for general purpose registers *)
  structure RegLiveness =
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.GP
	     val regSet = C.getCell C.GP 
             val cellset = C.updateCell C.GP)


  (* integer register allocator *)
  functor IntRa = 
      RegAllocator
	 (structure RaArch = struct

	     structure InsnProps = P
	     structure AsmEmitter = Asm
	     structure I = I
	     structure Liveness=RegLiveness
	     val defUse = P.defUse C.GP
	     val firstPseudoR = 32
	     val maxPseudoR = AlphaCells.maxCell
	     val numRegs = AlphaCells.numCell AlphaCells.GP
	     val regSet = C.getCell C.GP 
	  end)



  (* liveness analysis for floating point registers *)
  structure FregLiveness = 
    Liveness(structure Flowgraph=F
	     structure Instruction=I
	     val defUse = P.defUse C.FP
	     val regSet = C.getCell C.FP 
             val cellset = C.updateCell C.FP)

  (* floating register allocator *)
  functor FloatRa = 
    RegAllocator
       (structure RaArch = struct

          structure InsnProps = P
	  structure AsmEmitter = Asm
	  structure Liveness=FregLiveness
	  structure I = I

	  val defUse = P.defUse C.FP
	  val firstPseudoR = 64
	  val maxPseudoR = AlphaCells.maxCell 
	  val numRegs = AlphaCells.numCell AlphaCells.FP
	  val regSet = C.getCell C.FP 
	end)
end

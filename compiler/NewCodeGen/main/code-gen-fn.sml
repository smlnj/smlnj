(* code-gen-fn.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Final machine code generation.  This module wraps up the post-FLINT phases
 * as a pass that takes the program in FLINT IR and produces the machine
 * code and literal data objects.
 *)

functor CodeGeneratorFn (MachSpec : MACH_SPEC) : CODE_GENERATOR =
  struct

    structure CPSGen = CPSCompFn (MachSpec)

    val architecture = MachSpec.architecture
    val abi_variant = NONE (* TODO: we should be able to get rid of this *)

    fun phase x = Stats.doPhase (Stats.makePhase x)

    fun compile {source, prog} = let
	(* CPS compilation *)
	  val {clusters, maxAlloc, data} = CPSGen.compile {source=source, prog=prog}
        (* convert to CFG IR *)
          val cfg = CPSGen.toCFG {
                  source = source, clusters = clusters, maxAlloc = maxAlloc
                }
        (* pickle the IR into a vector *)
          val pkl = CFGPickler.toBytes cfg
        (* invoke the LLVM code generator to generate machine code *)
          val code = CodeObj.generate {
                  target = MachSpec.llvmTargetName,
                  src = source,
                  pkl = pkl
                }
	  in
	    {code = code, data = data}
	  end

  end

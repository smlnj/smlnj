(* code-gen-fn.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Final machine code generation.  This module wraps up the post-FLINT phases
 * as a pass that takes the program in FLINT IR and produces the machine
 * code and literal data objects.
 *)

functor CodeGeneratorFn (MachSpec : MACH_SPEC) : CODE_GENERATOR =
  struct

    structure CPSGen = CPSCompFn (MachSpec)

    type 'a csegments = {
        code : 'a,
        lits : CodeObj.literals
      }

    val architecture = MachSpec.architecture

    fun phase x = Stats.doPhase (Stats.makePhase x)

    (* compile FLINT IR to the CFG IR *)
    fun compileToCFG {source, prog} = let
	  (* CPS compilation *)
	  val {clusters, maxAlloc, data} = CPSGen.compile {source=source, prog=prog}
          (* convert to CFG IR *)
          val cfg = CPSGen.toCFG {
                  source = source, clusters = clusters, maxAlloc = maxAlloc,
                  normalize = !Control.CG.normalizeCFG
                }
          in
            {code = cfg, lits = data}
          end

    (* compile CFG IR to native code by first converting the *)
    fun compileToNative code = CodeObj.generate {
            target = MachSpec.llvmTargetName,
            src = #srcFile code,
            pkl = CFGPickler.toBytes code,
            verifyLLVM = !Control.CG.verifyLLVM
          }

  end (* functor CodeGeneratorFn *)

/// \file mc-gen.hxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Wrapper class for the low-level machine-specific parts of the code generator
///
/// \author John Reppy
///

#ifndef _MC_GEN_HXX_
#define _MC_GEN_HXX_

#include "code-object.hxx"

#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Object/ObjectFile.h"

class mc_gen {
  public:

    mc_gen (llvm::LLVMContext &context, target_info const *target);

  // per-module initialization and finalization
    void beginModule (llvm::Module *module);
    void endModule ();

  // run the per-function optimizations over the functions of the module
    void optimize (llvm::Module *module);

  // dump the code to an output file
    void dumpCode (llvm::Module *module, std::string const & stem, bool asmCode = true) const;

  // compile the LLVM module to a SML code object
    std::unique_ptr<CodeObject> compile (llvm::Module *module);

  private:
    target_info const *_tgtInfo;
    std::unique_ptr<llvm::TargetMachine> _tgtMachine;
    std::unique_ptr<llvm::legacy::FunctionPassManager> _passMngr;

};

#endif // !_MC_GEN_HXX_

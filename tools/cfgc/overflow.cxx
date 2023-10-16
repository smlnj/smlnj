/// \file overflow.cxx
///
/// \copyright 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief This file contains the CodeBuffer methods that support
/// signaling an Overflow exception.  We factor them out of the
/// code-buffer.cxx file because they are fairly complicated.
///
/// \author John Reppy
///

#include "code-buffer.hxx"
#include "target-info.hxx"
#include "cfg.hxx" // for argument setup

#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"


// return the basic-block that contains the Overflow trap generator
// We also update the PHI nodes for the overflow basic block.
//
llvm::BasicBlock *code_buffer::getOverflowBB ()
{
    auto srcBB = this->_builder.GetInsertBlock ();
    int nArgs = this->_regInfo.numMachineRegs();

    if (this->_overflowBB == nullptr) {
	this->_overflowBB = this->newBB ("overflow");
	this->_builder.SetInsertPoint (this->_overflowBB);

      // allocate PHI nodes for the SML special registers.  This is necessary
      // to ensure that any changes to the ML state (e.g., allocation) that
      // happened before the arithmetic operation are correctly recorded
        this->_overflowPhiNodes.reserve (nArgs);
    	Args_t args;
	args.reserve (nArgs);
	for (int i = 0;  i < nArgs;  ++i) {
	    llvm::Type *ty;
	    if (this->_regInfo.machineReg(i)->id() <= sml_reg_id::STORE_PTR) {
		ty = this->objPtrTy;
	    } else {
		ty = this->mlValueTy;
	    }
	    auto phi = this->_builder.CreatePHI(ty, 0);
	    this->_overflowPhiNodes.push_back(phi);
	    args.push_back(phi);
	}

      // fetch the raise_overflow code address from the stack
	Value *raiseFn = _loadFromStack (this->_target->raiseOvflwOffset, "raiseOverflow");

      // call the raise_overflow function  We use a non-tail call here so that the return
      // address, which is in the faulting module, is pushed on the stack and made available
      // to the runtime system, which uses it to create the initial message in the exception
      // traceback list.
	auto call = this->_builder.CreateCall (
	    this->_raiseOverflowFnTy,
	    this->createBitCast(raiseFn, this->_raiseOverflowFnTy->getPointerTo()),
	    args);
	call->setCallingConv (llvm::CallingConv::JWA);
	call->setTailCallKind (llvm::CallInst::TCK_NoTail);

	this->_builder.CreateRetVoid ();

      // restore current basic block
	this->_builder.SetInsertPoint (srcBB);
    }

  // add PHI-node dependencies
    for (int i = 0;  i < nArgs;  ++i) {
	reg_info const *rInfo = this->_regInfo.machineReg(i);
	this->_overflowPhiNodes[i]->addIncoming(this->_regState.get (rInfo->id()), srcBB);
    }

    return this->_overflowBB;

} // code_buffer::getOverflowBB

// get the branch-weight meta data for overflow branches
//
llvm::MDNode *code_buffer::overflowWeights ()
{
  // we use 1/1000 as the probability of an overflow
    return this->branchProb(1);

} // code_buffer::overflowWeights

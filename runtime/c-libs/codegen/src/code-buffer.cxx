/// \file codegen.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief This file implements the methods for the `code_buffer` class
///
/// \author John Reppy
///

#include "code-buffer.hxx"
#include "target-info.hxx"
#include "mc-gen.hxx"
#include "cfg.hxx" // for argument setup

#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Verifier.h"

/* control the adding of symbolic names to some values for easier debugging */
#ifndef _DEBUG
#  define NO_NAMES
#endif

/* FIXME: for now, these are all zero, but we should do something else */
/* address spaces for various kinds of ML data that are necessarily disjoint */
#define ML_HEAP_ADDR_SP		0		// immutable heap objects
#define ML_REF_ADDR_SP		0		// mutable heap objects

/***** class code_buffer member functions *****/

code_buffer *code_buffer::create (target_info const * tgtInfo)
{
    code_buffer *buf = new code_buffer (tgtInfo);

    return buf;
}

code_buffer *code_buffer::create (std::string const & target)
{
    auto tgtInfo = target_info::infoForTarget (target);
    if (tgtInfo == nullptr) {
	return nullptr;
    }

    code_buffer *buf = new code_buffer (tgtInfo);

    return buf;
}

code_buffer::code_buffer (target_info const *target)
  : _target(target),
    _context(), _builder(this->_context),
    _gen(nullptr),
  // initialize the register info
    _regInfo(target),
    _regState(this->_regInfo)
{
    this->_gen = new mc_gen (this->_context, target),

  // initialize the standard types that we use
    this->i8Ty = llvm::IntegerType::get (this->_context, 8);
    this->i16Ty = llvm::IntegerType::get (this->_context, 16);
    this->i32Ty = llvm::IntegerType::get (this->_context, 32);
    this->i64Ty = llvm::IntegerType::get (this->_context, 64);
    this->f32Ty = Type::getPrimitiveType (this->_context, Type::FloatTyID);
    this->f64Ty = Type::getPrimitiveType (this->_context, Type::DoubleTyID);

    if (this->_target->wordSz == 32) {
	this->intTy = this->i32Ty;
	this->_wordSzB = 4;
    }
    else { // info.wordSz == 64
	this->intTy = this->i64Ty;
	this->_wordSzB = 8;
    }
    this->mlValueTy = this->intTy->getPointerTo ();
    this->objPtrTy = this->mlValueTy->getPointerTo ();
    this->bytePtrTy = this->i8Ty->getPointerTo (ML_HEAP_ADDR_SP);
    this->voidTy = Type::getVoidTy (this->_context);

  // "call-gc" types
    {
	int n = target->numCalleeSaves + 4;
	std::vector<Type *> gcTys = this->createParamTys (frag_kind::STD_FUN, n);
	for (int i = 0;  i < n;  ++i) {
	    gcTys.push_back (this->mlValueTy);
	}
	auto gcRetTy = llvm::StructType::create(gcTys, "gc_ret_ty");
	this->_gcFnTy = llvm::FunctionType::get(gcRetTy, gcTys, false);
    }

  // initialize the overflow block and raise_overflow function type
    {
      // the overflow block and raise_overflow have a minimal calling convention
      // that consists of just the hardware CMachine registers.  These are
      // necessary to ensure that the correct values are in place at the point
      // where the Overflow exception will be raised.
      //
	std::vector<Type *> tys;
	int nArgs = this->_regInfo.numMachineRegs();
	tys.reserve (nArgs);
	for (int i = 0;  i < nArgs;  ++i) {
	    if (this->_regInfo.machineReg(i)->id() <= sml_reg_id::STORE_PTR) {
		tys.push_back (this->objPtrTy);
	    } else {
		tys.push_back (this->mlValueTy);
	    }
	}
	this->_raiseOverflowFnTy = llvm::FunctionType::get(this->voidTy, tys, false);
	this->_overflowBB = nullptr;
    }

} // constructor

void code_buffer::beginModule (std::string const & src, int nClusters)
{
    this->_module = new llvm::Module (src, this->_context);

    this->_gen->beginModule (this->_module);

  // prepare the label-to-cluster map
    this->_clusterMap.clear();
    this->_clusterMap.reserve(nClusters);

  // clear the cached intrinsic functions
    this->_sadd32WO = nullptr;
    this->_ssub32WO = nullptr;
    this->_smul32WO = nullptr;
    this->_sadd64WO = nullptr;
    this->_ssub64WO = nullptr;
    this->_smul64WO = nullptr;
    this->_fabs32 = nullptr;
    this->_fabs64 = nullptr;
    this->_sqrt32 = nullptr;
    this->_sqrt64 = nullptr;
    this->_copysign32 = nullptr;
    this->_copysign64 = nullptr;
    this->_readReg = nullptr;
    this->_spRegMD = nullptr;

} // code_buffer::beginModule

void code_buffer::completeModule ()
{
}

void code_buffer::optimize ()
{
    this->_gen->optimize (this->_module);
}

void code_buffer::endModule ()
{
    this->_gen->endModule();
    delete this->_module;
}

void code_buffer::beginCluster (CFG::cluster *cluster, llvm::Function *fn)
{
    assert ((cluster != nullptr) && "undefined cluster");
    assert ((fn != nullptr) && "undefined function");

    this->_overflowBB = nullptr;
    this->_overflowPhiNodes.clear();
    this->_fragMap.clear();
    this->_curFn = fn;
    this->_curCluster = cluster;

} // code_buffer::beginCluster

void code_buffer::endCluster ()
{
} // code_buffer::endCluster

void code_buffer::beginFrag ()
{
    this->_vMap.clear();

} // code_buffer::beginFrag

llvm::Function *code_buffer::newFunction (
    llvm::FunctionType *fnTy,
    std::string const &name,
    bool isPublic)
{
    llvm::Function *fn = llvm::Function::Create (
	    fnTy,
	    isPublic ? llvm::GlobalValue::ExternalLinkage : llvm::GlobalValue::PrivateLinkage,
	    name,
	    this->_module);

  // set the calling convention to our "Jump-with-arguments" convention
    fn->setCallingConv (llvm::CallingConv::JWA);

  // assign attributes to the function
    fn->addFnAttr (llvm::Attribute::Naked);

    return fn;

}

// helper function to get the numbers of arguments/parameters for
// a fragment
code_buffer::arg_info code_buffer::_getArgInfo (frag_kind kind) const
{
    code_buffer::arg_info info;

    info.nExtra = this->_regInfo.numMachineRegs();

    switch (kind) {
      case frag_kind::STD_FUN:
	info.nUnused = 0;
	info.basePtr = 0;
	break;
      case frag_kind::STD_CONT:
      // standard continuations do not use the first two registers of
      // the JWA convention (STD_LINK and STD_CLOS).
	info.nUnused = 2;
	info.basePtr = 0;
	break;
      case frag_kind::KNOWN_FUN:
      case frag_kind::INTERNAL:
        assert (this->_curCluster != nullptr && "no current cluster defined");
	info.nUnused = 0;
	if (this->_regInfo.usesBasePtr()
	&& this->_curCluster->get_attrs()->get_needsBasePtr()) {
	  // we need an extra argument for threading the base pointer
/* TODO: for KNOWN_FUN, we might have to pass the basePtr in memory! */
	    info.basePtr = 1;
	}
	else {
	    info.basePtr = 0;
	}
	break;
    }

    return info;

}

llvm::FunctionType *code_buffer::createFnTy (frag_kind kind, std::vector<Type *> const & tys) const
{
    std::vector<Type *> allParams = this->createParamTys (kind, tys.size());

  // add the types from the function's formal parameters
    for (auto ty : tys) {
	allParams.push_back (ty);
    }

    return llvm::FunctionType::get (
	this->voidTy,
	llvm::ArrayRef<Type *>(allParams),
	false);

}

void code_buffer::_addExtraParamTys (std::vector<Type *> &tys, arg_info const &info) const
{
  // the parameter list starts with the special registers (i.e., alloc ptr, ...),
  //
    for (int i = 0;  i < info.nExtra;  ++i) {
	if (this->_regInfo.machineReg(i)->id() <= sml_reg_id::STORE_PTR) {
	    tys.push_back (this->objPtrTy);
	} else {
	    tys.push_back (this->mlValueTy);
	}
    }

    if (info.basePtr) {
	tys.push_back (this->intTy);
    }

}

std::vector<Type *> code_buffer::createParamTys (frag_kind kind, int n) const
{
    std::vector<Type *> tys;
    arg_info info = this->_getArgInfo (kind);

    tys.reserve (info.numArgs(n));

  // the parameter list starts with the special registers (i.e., alloc ptr, ...),
  //
    this->_addExtraParamTys (tys, info);

  // we give the unused registers the ML value type
    for (int i = 0;  i < info.nUnused;  ++i) {
	tys.push_back (this->mlValueTy);
    }

    return tys;

}

void code_buffer::_addExtraArgs (Args_t &args, arg_info const &info) const
{
  // seed the args array with the extra arguments
    for (int i = 0;  i < info.nExtra;  ++i) {
	args.push_back (this->_regState.get (this->_regInfo.machineReg(i)));
    }

    if (info.basePtr) {
	args.push_back (this->_regState.getBasePtr());
    }
}

Args_t code_buffer::createArgs (frag_kind kind, int n)
{
    Args_t args;
    arg_info info = this->_getArgInfo (kind);

    args.reserve (info.numArgs(n));

    this->_addExtraArgs (args, info);

  // we assign the unused argument registers the undefined value
    for (int i = 0;  i < info.nUnused;  ++i) {
	args.push_back (llvm::UndefValue::get(this->mlValueTy));
    }

    return args;
}

// setup the incoming arguments for a standard function entry
//
void code_buffer::setupStdEntry (CFG::attrs *attrs, CFG::frag *frag)
{
  // the order of incoming arguments is:
  //
  //	1. special registers: ALLOC_PTR, LIMIT_PTR, STORE_PTR, EXN_HNDLR, VAR_PTR
  //
  //    2. STD_LINK, STD_CLOS, STD_CONT
  //
  //	3. general purpose callee-saves (MISC0, MISC1, ...)
  //
  //	4. floating-point callee-saves (FPR0, FPR1, ...)
  //
  //    5. argument registers: STDARG, MISC{n}, MISC{n+1}, ... / FPR{m}, FPR{m+1}, ...
  //	   where "n" is the number of callee saves and "m" is the number of floating-point
  //	   callee-saves
  //
  // For continuations, the STD_LINK and STD_CLOS registers are undefined and do not
  // correspond to CFG parameters

    llvm::Function *fn = this->_curFn;

    arg_info info = this->_getArgInfo(frag->get_kind());

  // initialize the register state
    for (int i = 0, hwIx = 0;  i < reg_info::NUM_REGS;  ++i) {
	reg_info const *info = this->_regInfo.info(static_cast<sml_reg_id>(i));
	if (info->isMachineReg()) {
	    llvm::Argument *arg = this->_curFn->getArg(hwIx++);
#ifndef NO_NAMES
	    arg->setName (info->name());
#endif
	    this->_regState.set (info->id(), arg);
	}
	else { // stack-allocated register
	    this->_regState.set (info->id(), nullptr);
	}
    }

  // initialize the base pointer (if necessary)
    if (this->_regInfo.usesBasePtr()
    && this->_curCluster->get_attrs()->get_needsBasePtr()) {
      // get the index of the register that holds the base address of the cluster
	int baseIx = (frag->get_kind() == frag_kind::STD_FUN)
	  // STDLINK holds the function's address and is the first non-special argument.
	    ? this->_regInfo.numMachineRegs()
	  // STDCONT holds the function's address and is the third non-special argument.
	    : this->_regInfo.numMachineRegs() + 2;
      // get base address of cluster and cast to the native int type
	auto basePtr = this->createPtrToInt (this->_curFn->getArg(baseIx));
	this->_regState.setBasePtr (basePtr);
#ifndef NO_NAMES
	basePtr->setName ("basePtr");
#endif
    }
#ifdef _DEBUG
    else {
	this->_regState.clearBasePtr ();
    }
#endif

    std::vector<CFG::param *> params = frag->get_params();
    int baseIx = info.nExtra + info.nUnused;
    for (int i = 0;  i < params.size();  i++) {
	params[i]->bind (this, fn->getArg(baseIx + i));
    }

}

void code_buffer::setupFragEntry (CFG::frag *frag, std::vector<llvm::PHINode *> &phiNodes)
{
    assert (frag->get_kind() == frag_kind::INTERNAL && "not an internal fragment");

    arg_info info = this->_getArgInfo (frag->get_kind());

  // initialize the register state
    for (int i = 0;  i < info.nExtra;  ++i) {
	reg_info const *rInfo = this->_regInfo.machineReg(i);
	this->_regState.set (rInfo->id(), phiNodes[i]);
    }

    if (info.basePtr) {
      // we are using a base pointer
	this->_regState.setBasePtr (phiNodes[info.nExtra]);
    }

  // bind the formal parameters to the remaining PHI nodes
    std::vector<CFG::param *> params = frag->get_params();
    for (int i = 0;  i < params.size();  i++) {
	params[i]->bind (this, phiNodes[info.nExtra + info.basePtr + i]);
    }

} // code_buffer::setupFragEntry

llvm::Constant *code_buffer::createGlobalAlias (
    Type *ty,
    llvm::Twine const &name,
    llvm::Constant *v)
{
    auto alias = llvm::GlobalAlias::create (
	ty,
	0,
	llvm::GlobalValue::PrivateLinkage,
	name,
	v,
	this->_module);
    alias->setUnnamedAddr (llvm::GlobalValue::UnnamedAddr::Global);

    return alias;
}

llvm::Constant *code_buffer::labelDiff (llvm::Function *f1, llvm::Function *f2)
{
  // define an alias for the value `(lab - curFn)`
    return this->createGlobalAlias (
	this->intTy,
	f1->getName() + "_sub_" + f2->getName(),
	llvm::ConstantExpr::getIntToPtr(
	    llvm::ConstantExpr::getSub (
		llvm::ConstantExpr::getPtrToInt(f1, this->intTy),
		llvm::ConstantExpr::getPtrToInt(f2, this->intTy)),
	    this->mlValueTy));

}

llvm::Constant *code_buffer::blockDiff (llvm::BasicBlock *bb)
{
    return this->createGlobalAlias (
	this->intTy,
	bb->getName() + "_sub_" + this->_curFn->getName(),
	llvm::ConstantExpr::getIntToPtr(
	    llvm::ConstantExpr::getSub (
		llvm::ConstantExpr::getPtrToInt(this->blockAddr(bb), this->intTy),
		llvm::ConstantExpr::getPtrToInt(this->_curFn, this->intTy)),
	    this->mlValueTy));

}

Value *code_buffer::evalLabel (llvm::Function *fn)
{
    if (this->_target->hasPCRel) {
#ifdef XXX
      // the target supports PC-relative addressing, so we can directly
      // refer to the function's label as a value.
	return fn;
#endif
      // the target supports PC-relative addressing, but we still need to
      // create an alias for `(lab - 0)` to force computation of the PC relative address.
	return this->createGlobalAlias (
	    this->intTy,
	    fn->getName() + "_alias",
	    llvm::ConstantExpr::getIntToPtr(
		llvm::ConstantExpr::getSub (
		    llvm::ConstantExpr::getPtrToInt(fn, this->intTy),
		    llvm::Constant::getNullValue(this->intTy)),
		this->mlValueTy));
    }
    else {
	Value *basePtr = this->_regState.getBasePtr();

	assert ((basePtr != nullptr) && "basePtr is not defined");

      // compute basePtr + (lab - curFn)
	auto labAdr = this->_builder.CreateIntToPtr(
	    this->createAdd (basePtr, this->labelDiff (fn, this->_curFn)),
	    this->mlValueTy);
#ifndef NO_NAMES
	labAdr->setName ("L_" + fn->getName());
#endif
	return labAdr;
    }

} // code_buffer::evalLabel

void code_buffer::_initSPAccess ()
{
    assert ((this->_readReg == nullptr) && (this->_spRegMD == nullptr));
    this->_readReg = _getIntrinsic (llvm::Intrinsic::read_register, this->intTy);
    this->_spRegMD = llvm::MDNode::get (
	this->_context,
	llvm::MDString::get(this->_context, this->_target->spName));

}

// private function for loading a special register from memory
Value *code_buffer::_loadMemReg (sml_reg_id r)
{
    auto info = this->_regInfo.info(r);
    return this->_loadFromStack (info->offset(), info->name());

} // code_buffer::_loadMemReg

// private function for setting a special memory register
void code_buffer::_storeMemReg (sml_reg_id r, Value *v)
{
    auto info = this->_regInfo.info(r);
    auto stkAddr = this->stkAddr (v->getType()->getPointerTo(), info->offset());
    this->_builder.CreateAlignedStore (
	v,
	stkAddr,
	llvm::MaybeAlign (this->_wordSzB));

} // code_buffer::_storeMemReg

// utility function for allocating a record of ML values (pointers or
// tagged ints).
//
Value *code_buffer::allocRecord (Value *desc, Args_t const & args)
{
    assert (desc->getType() == this->mlValueTy && "descriptor should be ML Value");

    int len = args.size();
    Value *allocPtr = this->mlReg (sml_reg_id::ALLOC_PTR);

  // write object descriptor
    this->build().CreateAlignedStore (desc, allocPtr, llvm::MaybeAlign (this->_wordSzB));

  // initialize the object's fields
    for (int i = 1;  i <= len;  ++i) {
	this->build().CreateAlignedStore (
	    this->asMLValue (args[i-1]),
	    this->createGEP (allocPtr, i),
	    llvm::MaybeAlign (this->_wordSzB));
    }

  // compute the object's address and cast it to an ML value
    Value *obj = this->asMLValue (this->createGEP (allocPtr, 1));

  // bump the allocation pointer
    this->setMLReg (sml_reg_id::ALLOC_PTR, this->createGEP (allocPtr, len + 1));

    return obj;
}

void code_buffer::callGC (
    Args_t const & roots,
    std::vector<LambdaVar::lvar> const & newRoots)
{
    assert ((this->_gcFnTy->getNumParams() == roots.size())
	&& "arity mismatch in GC call");

  // get the address of the "call-gc" entry
    Value *callGCFn = this->_loadFromStack (this->_target->callGCOffset, "callGC");

  // call the garbage collector.  The return type of the GC is a struct
  // that contains the post-GC values of the argument registers
    auto call = this->_builder.CreateCall (
	this->_gcFnTy,
	this->createBitCast(callGCFn, this->_gcFnTy->getPointerTo()),
	roots);
    call->setCallingConv (llvm::CallingConv::JWA);
    call->setTailCallKind (llvm::CallInst::TCK_NoTail);

  // restore the register state from the return struct
    for (unsigned i = 0, hwIx = 0;  i < reg_info::NUM_REGS;  ++i) {
	reg_info const *info = this->_regInfo.info(static_cast<sml_reg_id>(i));
	if (info->isMachineReg()) {
	    auto reg = this->_builder.CreateExtractValue(call, { hwIx });
	    this->setMLReg (info->id(), reg);
	    hwIx++;
	}
    }

  // extract the new roots from the return struct
    unsigned ix = this->_regInfo.numMachineRegs();
    for (auto lv : newRoots) {
	this->insertVal (lv, this->_builder.CreateExtractValue(call, { ix++ }));
    }

} // code_buffer::callGC

// return branch-weight meta data, where `prob` represents the probability of
// the true branch and is in the range 1..999.
llvm::MDNode *code_buffer::branchProb (int prob)
{
    auto name = llvm::MDString::get(this->_context, "branch_weights");
    auto trueProb = llvm::ValueAsMetadata::get(this->i32Const(prob));
    auto falseProb = llvm::ValueAsMetadata::get(this->i32Const(1000 - prob));
    auto tpl = llvm::MDTuple::get(this->_context, {name, trueProb, falseProb});

    return tpl;

} // code_buffer::branchProb

// generate a type cast for an actual to formal transfer.
//
Value *code_buffer::castTy (Type *srcTy, Type *tgtTy, Value *v)
{
    if (tgtTy->isPointerTy()) {
	if (srcTy->isPointerTy()) {
	    return this->_builder.CreateBitCast(v, tgtTy);
	} else {
	    return this->_builder.CreateIntToPtr(v, tgtTy);
	}
    }
    else if (tgtTy->isIntegerTy() && srcTy->isPointerTy()) {
	return this->_builder.CreatePtrToInt(v, tgtTy);
    }
    else {
//llvm::dbgs() << "castTy (" << *srcTy << ", " << *tgtTy << ", -)\n";
	assert (false && "invalid type cast");
	return nullptr;
    }

} // code_buffer::castTy

llvm::Function *code_buffer::_getIntrinsic (llvm::Intrinsic::ID id, Type *ty) const
{
    return llvm::Intrinsic::getDeclaration (
	this->_module, id, llvm::ArrayRef<Type *>(ty));
}

std::unique_ptr<CodeObject> code_buffer::compile () const
{
    return this->_gen->compile (this->_module);
}

void code_buffer::dumpAsm () const
{
    this->_gen->dumpCode (this->_module, "-", true);
}

void code_buffer::dumpAsm (std::string const &stem) const
{
    this->_gen->dumpCode (this->_module, stem, true);
}

void code_buffer::dumpObj (std::string const &stem) const
{
    this->_gen->dumpCode (this->_module, stem, false);
}

// dump the current module to stderr
void code_buffer::dump () const
{
#if defined(_DEBUG) || defined(LLVM_ENABLE_DUMP)
    this->_module->dump();
#endif
}

// run the LLVM verifier on the module
bool code_buffer::verify () const
{
    return llvm::verifyModule (*this->_module, &llvm::dbgs());
}

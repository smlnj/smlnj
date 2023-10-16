///! \file cfg-codegen.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief This file holds the implementations of the `codegen` methods
/// for the CFG expression and statement types (defined in the `CFG` module).
///
/// \author John Reppy
///

#include "cfg.hxx"
#include "target-info.hxx"
//#include "codegen.hxx"

namespace CFG {

  /***** code generation for the `ty` type *****/

    Type *LABt::codegen (code_buffer * buf)
    {
	return buf->mlValueTy;

    } // LABt::codegen

    Type *PTRt::codegen (code_buffer * buf)
    {
	return buf->mlValueTy;

    } // PTRt::codegen

    Type *TAGt::codegen (code_buffer * buf)
    {
	return buf->intTy;

    } // TAGt::codegen

    Type *NUMt::codegen (code_buffer * buf)
    {
	return buf->iType (this->_v_sz);

    } // NUMt::codegen

    Type *FLTt::codegen (code_buffer * buf)
    {
	return buf->fType (this->_v_sz);

    } // FLTt::codegen

  // code generation for a vector of types
    static std::vector<Type *> genTypes (code_buffer * buf, std::vector<ty *> const &tys)
    {
	std::vector<Type *> llvmTys;
	llvmTys.reserve(tys.size());
	for (auto ty : tys) {
	    llvmTys.push_back (ty->codegen (buf));
	}
	return llvmTys;
    }

  /***** code generation for the `exp` type *****/

    Value *VAR::codegen (code_buffer * buf)
    {
	Value *v = buf->lookupVal (this->_v_name);
#ifdef _DEBUG
	if (v == nullptr) {
	    llvm::dbgs() << "VAR: " << this->_v_name << " is unbound\n";
	    assert (v && "unbound variable");
	}
#endif
	return v;

    } // VAR::codegen

    Value *LABEL::codegen (code_buffer * buf)
    {
	cluster *cluster = buf->lookupCluster (this->_v_name);

	assert (cluster && "Unknown cluster label");

	return buf->evalLabel (cluster->fn());

    } // LABEL::codegen

    Value *NUM::codegen (code_buffer * buf)
    {
	if (this->get_iv().getSign() < 0) {
	    return buf->iConst (this->get_sz(), this->get_iv().toInt64());
	} else {
	    return buf->uConst (this->get_sz(), this->get_iv().toUInt64());
	}

    } // NUM::codegen

    Value *LOOKER::codegen (code_buffer * buf)
    {
	Args_t args;
	for (auto it = this->_v_args.begin(); it != this->_v_args.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
	return this->_v_oper->codegen (buf, args);

    } // LOOKER::codegen

    Value *PURE::codegen (code_buffer * buf)
    {
	Args_t args;
	for (auto it = this->_v_args.begin(); it != this->_v_args.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
	return this->_v_oper->codegen (buf, args);

    } // PURE::codegen

    Value *SELECT::codegen (code_buffer * buf)
    {
	Value *adr = buf->createGEP (
	    buf->asObjPtr(this->_v_arg->codegen(buf)),
	    static_cast<int32_t>(this->_v_idx));
	return buf->createLoad (buf->mlValueTy, adr);

    } // SELECT::codegen

    Value *OFFSET::codegen (code_buffer * buf)
    {
	return buf->createGEP (
	    buf->asObjPtr(this->_v_arg->codegen(buf)),
	    static_cast<int32_t>(this->_v_idx));

    } // OFFSET::codegen

  /***** code generation for the `stm` type *****/

    void LET::codegen (code_buffer * buf)
    {
      // record mapping from the parameter to the compiled expression
	this->_v1->bind (buf, this->_v0->codegen(buf));
      // compile continuation
	this->_v2->codegen(buf);

    } // LET::codegen

    void ALLOC::codegen (code_buffer * buf)
    {
	Args_t args;
	for (auto it = this->_v1.begin(); it != this->_v1.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
	buf->insertVal (this->_v2, this->_v0->codegen(buf, args));

      // compile continuation
	this->_v3->codegen(buf);

    } // ALLOC::codegen

  // helper function for argument set up for APPLY and THROW
    inline Args_t SetupStdArgs (
	code_buffer * buf,
	llvm::FunctionType *fnTy,
	frag_kind fk,
	std::vector<exp *> const &cfgArgs)
    {
	Args_t args = buf->createArgs (fk, cfgArgs.size());

	int base = args.size(); // index of first user parameter
	for (int i = 0;  i < cfgArgs.size();  ++i) {
	    auto arg = cfgArgs[i]->codegen (buf);
	    auto paramTy = fnTy->getParamType(base + i);
	    if (paramTy != arg->getType()) {
		arg = buf->castTy(arg->getType(), paramTy, arg);
	    }
	    args.push_back (arg);
	}

	return args;

    } // SetupStdArgs

    void APPLY::codegen (code_buffer * buf)
    {
	frag_kind fk = frag_kind::STD_FUN;
	llvm::FunctionType *fnTy;
	Value *fn;
	LABEL *lab = (this->_v0->isLABEL() ? reinterpret_cast<LABEL *>(this->_v0) : nullptr);
	if (lab == nullptr) {
	    fnTy = buf->createFnTy (fk, genTypes (buf, this->_v2));
	    fn = buf->build().CreateBitCast(
		this->_v0->codegen (buf),
		fnTy->getPointerTo());
	} else {
	    cluster *f = buf->lookupCluster (lab->get_name());
	    assert (f && "APPLY of unknown cluster");
	    fk = f->entry()->get_kind();
	    fn = f->fn();
	    fnTy = f->fn()->getFunctionType();
	}

      // evaluate the arguments
	Args_t args = SetupStdArgs (buf, fnTy, fk, this->_v1);

	buf->createJWACall(fnTy, fn, args);

	buf->build().CreateRetVoid();

    } // APPLY::codegen

    void THROW::codegen (code_buffer * buf)
    {
	llvm::FunctionType *fnTy;
	Value *fn;
	LABEL *lab = (this->_v0->isLABEL() ? reinterpret_cast<LABEL *>(this->_v0) : nullptr);
	if (lab == nullptr) {
	    fnTy = buf->createFnTy (frag_kind::STD_CONT, genTypes (buf, this->_v2));
	    fn = buf->build().CreateBitCast(
		this->_v0->codegen (buf),
		fnTy->getPointerTo());
	} else {
	    cluster *f = buf->lookupCluster (lab->get_name());
	    assert (f && "THROW of unknown cluster");
	    fn = f->fn();
	    fnTy = f->fn()->getFunctionType();
	}

      // evaluate the arguments
	Args_t args = SetupStdArgs (buf, fnTy, frag_kind::STD_CONT, this->_v1);

	buf->createJWACall(fnTy, fn, args);

	buf->build().CreateRetVoid();

    } // THROW::codegen

    void GOTO::codegen (code_buffer * buf)
    {
	llvm::BasicBlock *srcBB = buf->getCurBB();
	frag *dstFrag = buf->lookupFrag (this->_v0);

	assert (dstFrag && (dstFrag->get_kind() == frag_kind::INTERNAL));

      // evaluate the arguments
	Args_t args = buf->createArgs (frag_kind::INTERNAL, this->_v1.size());
	for (auto arg : this->_v1) {
	    args.push_back (arg->codegen (buf));
	}

      // add outgoing values as incoming values to the destination's
      // phi nodes
	for (int i = 0;  i < args.size();  ++i) {
	  // make sure that the type match!
	    assert (args[i]);
	    Type *srcTy = args[i]->getType();
	    Type *tgtTy = dstFrag->paramTy(i);
	    if (srcTy != tgtTy) {
		dstFrag->addIncoming (i, buf->castTy(srcTy, tgtTy, args[i]), srcBB);
	    } else {
		dstFrag->addIncoming (i, args[i], srcBB);
	    }
	}

      // generate the control transfer; note that we need to do this *after*
      // updating the PHI nodes, since any type casts introduced for the PHI
      // nodes will be generated in the *source* block!
	buf->createBr (dstFrag->bb());

    } // GOTO::codegen

    void SWITCH::codegen (code_buffer * buf)
    {
      // evaluate the argument and truncate to 32 bits
	Value *arg = buf->createTrunc(buf->asInt(this->_v0->codegen(buf)), buf->i32Ty);

      // the number of non-default cases
	int nCases = this->_v1.size();

      // save the current block
	auto curBlk = buf->getCurBB();

      // as suggested by Matthew Fluet, we mark the default case as unreachable,
      // which has the effect of getting LLVM to treat the switch as being exhaustive.
	auto dfltBlk = buf->newBB("impossible");
	buf->setInsertPoint (dfltBlk);
	buf->build().CreateUnreachable();

      // create the switch in the current block
	buf->setInsertPoint (curBlk);
	llvm::SwitchInst *sw = buf->build().CreateSwitch(arg, dfltBlk, nCases);

      // add the cases to the switch
	for (int i = 0;  i < nCases;  i++) {
	    sw->addCase (buf->iConst(32, i), this->_v1[i]->bb());
	}

      // generate the code for the basic blocks
	reg_state saveRegs;
	buf->saveSMLRegState (saveRegs);
	for (auto it = this->_v1.begin();  it != this->_v1.end();  ++it) {
	    buf->restoreSMLRegState (saveRegs);
	    buf->setInsertPoint ((*it)->bb());
	    (*it)->codegen (buf);
	}

    } // SWITCH::codegen

    void BRANCH::codegen (code_buffer * buf)
    {
      // evaluate the test
	Args_t args;
	for (auto it = this->_v1.begin(); it != this->_v1.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
	Value *cond = this->_v0->codegen(buf, args);

      // generate the conditional branch
	if (this->_v2 == 0) {
	  // no branch prediction
	    buf->build().CreateCondBr(cond, this->_v3->bb(), this->_v4->bb());
	} else {
	    buf->build().CreateCondBr(
		cond,
		this->_v3->bb(), this->_v4->bb(),
		buf->branchProb (this->_v2));
	}

      // generate code for the true branch
	reg_state saveRegs;
	buf->saveSMLRegState (saveRegs);
	buf->setInsertPoint (this->_v3->bb());
	this->_v3->codegen (buf);

      // generate code for the false branch
	buf->restoreSMLRegState (saveRegs);
	buf->setInsertPoint (this->_v4->bb());
	this->_v4->codegen (buf);

    } // BRANCH::codegen

    void ARITH::codegen (code_buffer * buf)
    {
	Args_t args;
	for (auto it = this->_v1.begin(); it != this->_v1.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
      // record mapping from the parameter to the compiled expression
	this->_v2->bind (buf, this->_v0->codegen (buf, args));
      // compile continuation
	this->_v3->codegen (buf);

    } // ARITH::codegen

    void SETTER::codegen (code_buffer * buf)
    {
	Args_t args;
	for (auto it = this->_v1.begin(); it != this->_v1.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
	this->_v0->codegen (buf, args);
      // compile continuation
	this->_v2->codegen (buf);

    } // SETTER::codegen

    void CALLGC::codegen (code_buffer * buf)
    {
      // evaluate the roots
	Args_t roots = buf->createArgs (frag_kind::STD_FUN, this->_v0.size());
	for (auto it = this->_v0.begin(); it != this->_v0.end(); ++it) {
	  // all roots are SML values, so make sure they have the correct type
	    roots.push_back (buf->asMLValue ((*it)->codegen (buf)));
	}

	buf->callGC (roots, this->_v1);

      // compile continuation
	this->_v2->codegen (buf);

    } // CALLGC::codegen

    void RCC::codegen (code_buffer * buf)
    {
	assert (false && "RCC not yet implemented"); /* FIXME */
    } // RCC::codegen


  /***** code generation for the `frag` type *****/

    void frag::codegen (code_buffer * buf, cluster *cluster)
    {
	buf->beginFrag ();

	buf->setInsertPoint (this->_v_body->bb());

	if (cluster != nullptr) {
	    buf->setupStdEntry (cluster->get_attrs(), this);
	} else {
	    buf->setupFragEntry (this, this->_phiNodes);
	}

      // generate code for the fragment
	this->_v_body->codegen (buf);

    } // frag::codegen


  /***** code generation for the `cluster` type *****/

    void cluster::codegen (code_buffer * buf, bool isFirst)
    {
	buf->beginCluster (this, this->_fn);

      // initialize the fragments for the cluster
	for (auto frag : this->_v_frags) {
	    frag->init (buf);
	}

      // generate code for the cluster
	this->_v_frags[0]->codegen (buf, this);
	for (int i = 1;  i < this->_v_frags.size();  ++i) {
	    this->_v_frags[i]->codegen (buf, nullptr);
	}

	buf->endCluster ();

    } // cluster::codegen


  /***** code generation for the `comp_unit` type *****/

    void comp_unit::codegen (code_buffer * buf)
    {
      // initialize the buffer for the comp_unit
	buf->beginModule (this->_v_srcFile, this->_v_fns.size() + 1);

      // initialize the clusters
	this->_v_entry->init (buf, true);
	for (auto f : this->_v_fns) {
	    f->init (buf, false);
	}

      // generate code
	this->_v_entry->codegen (buf, true);
	for (auto f : this->_v_fns) {
	    f->codegen (buf, false);
	}

        buf->completeModule ();

    } // comp_unit::codegen

} // namespace CFG

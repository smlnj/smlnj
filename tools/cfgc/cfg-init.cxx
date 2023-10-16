/// \file cfg-init.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief This file holds the implementation of the `init` methods
/// for the various CFG types (defined in the `CFG` module).
///
/// \author John Reppy
///

#include "cfg.hxx"

/* control the adding of symbolic names to some values for easier debugging */
#ifndef _DEBUG
#  define NO_NAMES
#endif

namespace CFG {

  /***** initialization for the `stm` type *****/

  // helper function for setting the `_bb` field of a `stm`
  //
    inline void stm::_initBB (code_buffer * buf, bool blkEntry)
    {
	if (blkEntry) {
	    this->_bb = buf->newBB();
	} else {
	    this->_bb = nullptr;
	}
    }

    void LET::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // continue initialization
	this->_v2->init (buf, false);

    } // LET::init

    void ALLOC::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // continue initialization
	this->_v3->init (buf, false);

    } // ALLOC::init

    void APPLY::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

    } // APPLY::init

    void THROW::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

    } // THROW::init

    void GOTO::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

    } // GOTO::init

    void SWITCH::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // initialize arms of switch
	for (auto it = this->_v1.begin();  it != this->_v1.end();  ++it) {
	    (*it)->init (buf, true);
	}

    } // SWITCH::init

    void BRANCH::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // initialize arms of conditional
	this->_v3->init (buf, true);
	this->_v4->init (buf, true);

    } // BRANCH::init

    void ARITH::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // continue initialization
	this->_v3->init (buf, false);

    } // ARITH::init

    void SETTER::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // continue initialization
	this->_v2->init (buf, false);

    } // SETTER::init

    void CALLGC::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // continue initialization
	this->_v2->init (buf, false);

    } // CALLGC::init

    void RCC::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // continue initialization
	this->_v_k->init (buf, false);

    } // RCC::init


  /***** initialization for the `frag` type *****/

    // for each fragment in the cluster, we add the mapping from the fragment's
    // label to it; we also initialize the fragment's body (which creates the
    // entry block for the fragment) and we add phi nodes to the block for each
    // of the parameters.
    //
    void frag::init (code_buffer * buf)
    {
      // add the fragment to the label to fragment map
	buf->insertFrag (this->_v_lab, this);

      // initialize the fragment's body */
	this->_v_body->init (buf, true);
#ifndef NO_NAMES
	this->_v_body->bb()->setName ("L_" + std::to_string(this->_v_lab));
#endif

      // add a phi node for each parameter of the fragment
	if (this->_v_kind == frag_kind::INTERNAL) {
	  // compute the parameter types for the fragment
	    auto paramTys = buf->createParamTys (this->_v_kind, this->_v_params.size());
	    for (auto param : this->_v_params) {
		paramTys.push_back (param->get_ty()->codegen (buf));
	    }
	  // for each parameter, add a PHI node to the entry block
	    buf->setInsertPoint (this->_v_body->bb());
	    this->_phiNodes.reserve (paramTys.size());
	    for (auto ty : paramTys) {
		llvm::PHINode *phi = buf->build().CreatePHI(ty, 0);
		this->_phiNodes.push_back (phi);
	    }
	}

    }


  /***** initialization for the `cluster` type *****/

    void cluster::init (code_buffer * buf, bool isEntry)
    {
	assert (this->_v_frags.size() > 0);

	frag *entry = this->_v_frags[0];

      // add the cluster to the cluster map
	buf->insertCluster (entry->get_lab(), this);

      // set the current cluster in the code buffer (needed to allow access to the
      // cluster's attributes)
	buf->setCluster (this);

      // create and record the LLVM function for the cluster
	auto params = entry->get_params();
	std::vector<llvm::Type *> paramTys;
	paramTys.reserve (params.size());
	for (auto param : params) {
	    paramTys.push_back (param->get_ty()->codegen(buf));
	}
	llvm::FunctionType *fnTy = buf->createFnTy (entry->get_kind(), paramTys);

	std::string name = (isEntry ? "entry" : "fn") + std::to_string(entry->get_lab());
	this->_fn = buf->newFunction (fnTy, name, isEntry);

    }

} // namespace CFG

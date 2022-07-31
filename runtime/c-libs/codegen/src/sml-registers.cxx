/// \file sml-registers.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Implementation of methods for the classes defined in "sml-registers.hxx"
///
/// \author John Reppy
///

#include "sml-registers.hxx"
#include "target-info.hxx"

/***** reg_info methods *****/

// the extra arguments that are added to thread the state of the reserved
// registers through the control-flow graph.
static std::string RegNames[reg_info::NUM_REGS] = {
	"allocPtr", "limitPtr", "storePtr", "exnPtr", "varPtr"
    };

reg_info::reg_info (sml_reg_id id, int idx, int off)
  : _id(id), _idx(idx), _offset(off), _name(RegNames[static_cast<int>(id)])
{
}


/***** sml_registers methods *****/

sml_registers::sml_registers (struct target_info const *target)
{
    if (target == nullptr) {
	this->_nHWRegs = 0;
	return;
    }

  // initialize the register info for the target
    this->_usesBasePtr = ! target->hasPCRel;
    for (int i = 0;  i < reg_info::NUM_REGS;  ++i) {
	sml_reg_id id = static_cast<sml_reg_id>(i);
	if (target->stkOffset[i] != 0) {
	    this->_info[i] = reg_info::createStkReg (id, target->stkOffset[i]);
	}
	else {
	    this->_info[i] = reg_info::createReg (id, i);
	}
    }

  // initialize the info about the SML registers that are mapped to machine registers
    int i = 0;
    int nHW = 0;
    for ( ;  i < reg_info::NUM_REGS;  ++i) {
	if ((this->_info[i] != nullptr) && this->_info[i]->isMachineReg()) {
	    this->_hwRegs[nHW++] = this->_info[i];
	}
    }
    while (i < reg_info::NUM_REGS) {
	this->_hwRegs[i] = nullptr;
    }
    this->_nHWRegs = nHW;

}

/***** reg_state methods *****/

reg_state::reg_state (sml_registers const & info)
  : _basePtr(nullptr)
{
  // we initialize all of the registers to nullptr
    for (int i = 0;  i < reg_info::NUM_REGS;  i++) {
	this->_val[i] = nullptr;
    }
}

void reg_state::copyFrom (reg_state const & cache)
{
  /* NOTE: we do not copy the _basePtr, because it is invariant */
    for (int i = 0;  i < reg_info::NUM_REGS;  i++) {
	this->_val[i] = cache._val[i];
    }

}

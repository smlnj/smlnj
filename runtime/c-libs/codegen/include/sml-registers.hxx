/// \file sml-registers.hxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief This file defines the `reg_state` class, which encapsulates the
///        state of the CMachine (e.g., allocation pointer, limit pointer, ...).
///
/// \author John Reppy
///

#ifndef _SML_REGISTERS_HXX_
#define _SML_REGISTERS_HXX_

#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

#include <string>
#include <vector>

// the CMachine special "registers".  These are registers that need to be
// threaded through the environment and through function calls as extra
// parameters.  On some targets, some of these may be allocated in the stack.
//
enum class sml_reg_id {
	ALLOC_PTR = 0,		// allocation pointer
	LIMIT_PTR,		// heap-limit pointer
	STORE_PTR,		// points to list of store records
	EXN_HNDLR,		// exception handler
	VAR_PTR,		// var_ptr register
	NUM_REGS		// the number of special registers
};

class reg_info {
  public:

    static const int NUM_REGS = static_cast<int>(sml_reg_id::NUM_REGS);

  // functions for creating registers
    static reg_info *createReg (sml_reg_id id, int idx)
    {
	return new reg_info (id, idx, 0);
    }
    static reg_info *createStkReg (sml_reg_id id, int offset)
    {
	return new reg_info (id, -1, offset);
    }

    sml_reg_id id () const { return this->_id; }

  // return the index in the JWA register list for this register; will be < 0 for
  // memory-allocated registers.
    int index () const { return this->_idx; }

  // return the stack offset for this register
    int offset () const { return this->_offset; }

    std::string const &name () const { return this->_name; }

    bool isMachineReg () const { return (this->_idx >= 0); }
    bool isMemReg () const { return (this->_idx < 0); }

  private:
    sml_reg_id	_id;		// The ID of this register.
    int		_idx;		// The index of hardware register assigned to this register.
				// This value is the parameter index in the JWA calling
				// convention.  It will be -1 for stack allocated registers
    int		_offset;	// For stack allocated registers, this is the offset from
				// the stack pointer to where the register is allocated in
				// the frame
    std::string _name;		// The register's name

    reg_info (sml_reg_id id, int idx, int off);

};

// collective information about the special CMachine registers for a given target
//
class sml_registers {
  public:

  // setup the register information for the specified target architecture
  //
    sml_registers (struct target_info const *target);

  // does the target require the base address register?
    bool usesBasePtr () const { return this->_usesBasePtr; }

    reg_info const *info (sml_reg_id id) const { return this->_info[static_cast<int>(id)]; }

  // the number of special registers that are mapped to machine registers and thus
  // must be passed as extra arguments
    int numMachineRegs () const { return this->_nHWRegs; }

    reg_info const *machineReg (int idx) const { return this->_hwRegs[idx]; }

  private:
    bool	_usesBasePtr;			// true if target needs the base register to
						// compute code-address values
    int		_nHWRegs;			// the number of special registers that are
						// hardware supported.
    reg_info *	_info[reg_info::NUM_REGS];	// information about the registers.
    reg_info *	_hwRegs[reg_info::NUM_REGS];	// info about the SML registers that are
						// mapped to machine registers and, thus, are
						// passed as arguments in the JWA convention.
};

/// The reg_state tracks a mapping from CMachine registers to LLVM values.
///
class reg_state {
  public:

    reg_state () { }
    explicit reg_state (sml_registers const & info);
    ~reg_state () { }

  // get the LLVM value that represents the specified CMachine register
    llvm::Value *get (sml_reg_id r) const
    {
	return this->_val[static_cast<int>(r)];
    }
    llvm::Value *get (reg_info const *info) const { return this->get(info->id()); }

  // assign a value to a CMachine register
    void set (sml_reg_id r, llvm::Value *v)
    {
	this->_val[static_cast<int>(r)] = v;
    }

  // get the LLVM value of the base-address pointer
    llvm::Value *getBasePtr () const { return this->_basePtr; }

  // set the base-address pointer
    void setBasePtr (llvm::Value *v)
    {
	assert (v->getType()->isIntegerTy() && "base pointer should have intTy");
	this->_basePtr = v;
    }

#ifdef _DEBUG
    void clearBasePtr () { this->_basePtr = nullptr; }
#endif

    void copyFrom (reg_state const & cache);

  private:
    llvm::Value * _basePtr;			// base-address of current function; used for
						// computing position-independent labels
    llvm::Value * _val[reg_info::NUM_REGS];	// mapping from registers IDs to their current
						// representation as an LLVM value.
};

/// initialize the static register information for the target
///
sml_registers *InitRegInfo (std::string const &target);

#endif // !_SML_REGISTERS_HXX_

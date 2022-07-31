/// \file target-info.hxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Information about the target architecture and how SML is mapped
///        onto it.
///
/// \author John Reppy
///

#ifndef _TARGET_INFO_HXX_
#define _TARGET_INFO_HXX_

#include "llvm/ADT/Triple.h"

#include <string>
#include <vector>

#include "sml-registers.hxx"

struct target_info {
    std::string name;			// the target's name; this should agree with LLVM's
					// naming conventions (see lib/Support/Triple.cpp).
    std::string dataLayout;		// LLVM data layout string
    std::string spName;			// the assembly name of the stack pointer register
    llvm::Triple::ArchType arch;	// target architecture
    int wordSzB;			// size in bytes of ML word (should also be the
                                        // same as the native pointer size)
    int wordSz;				// size in bits of ML word (== 8*wordSzB)
    int numRegs;			// the number of SML registers used by the target
    int numCalleeSaves;			// the number of registers used for callee-save values
    bool hasPCRel;			// true if the target supports PC-relative addressing.
    int stkOffset[reg_info::NUM_REGS];	// byte offset from stack pointer to location where
					// the value is stored.  Will be non-zero only
					// for CMachine registers that stack allocated
    int callGCOffset;			// stack offset of call-gc entry address
    int raiseOvflwOffset;		// stack offset of raise_overflow entry address
    unsigned int allocSlopSzb;		// byte size of allocation slop

  // initialization functions
    using init_fn_t = void (*)();

    mutable bool initialized;           // set to true after initialization
    init_fn_t initTargetInfo;
    init_fn_t initTarget;
    init_fn_t initMC;
    init_fn_t initAsmParser;
    init_fn_t initAsmPrinter;

    static target_info const *infoForTarget (std::string const &name);

    static std::vector<std::string> targetNames ();

    void initialize () const
    {
        if (! this->initialized) {
            this->initTargetInfo();
            this->initTarget();
            this->initMC();
            this->initAsmParser();
            this->initAsmPrinter();
            this->initialized = true;
        }
    }

    /// the target info for the native (host) architecture
    static target_info const *native;

  // GC roots are std-link, std-clos, std-cont, callee saves, std-arg
    int numGCRoots () const { return this->numCalleeSaves + 4; }

    llvm::Triple getTriple() const;

  /// given a number of bytes, round it up to the next multiple of the
  /// target's word size
    uint64_t roundToWordSz (uint64_t nBytes) const
    {
	uint64_t mask = this->wordSzB - 1;
	return (nBytes + mask) & ~mask;
    }
};

#endif // !_TARGET_INFO_HXX_

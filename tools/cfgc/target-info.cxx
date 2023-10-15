/// \file target-info.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Implementation of target-specific information.
///
/// \author John Reppy
///

#include "target-info.hxx"

#if defined(OPSYS_DARWIN)
constexpr const char *kVendor = "apple";
constexpr const char *kOS = "macosx";
#elif defined(OPSYS_LINUX)
constexpr const char *kVendor = "unknown";
constexpr const char *kOS = "linuix";
#endif

// make sure that the "ENABLE_xxx" symbol is defined for the host architecture */
#if defined(ARCH_AMD64) && !defined(ENABLE_X86)
#  define ENABLE_X86
#endif
#if defined(ARCH_ARM64) && !defined(ENABLE_ARM64)
#  define ENABLE_ARM64
#endif

/* to support cross compiling, define the symbol "ENABLE_ALL" */
#ifdef ENABLE_ALL
#  ifndef ENABLE_ARM64
#    define ENABLE_ARM64
#  endif
#  ifndef ENABLE_X86
#    define ENABLE_X86
#  endif
#endif

#ifdef ENABLE_ARM64
extern "C" {
void LLVMInitializeAArch64TargetInfo ();
void LLVMInitializeAArch64Target ();
void LLVMInitializeAArch64TargetMC ();
void LLVMInitializeAArch64AsmParser ();
void LLVMInitializeAArch64AsmPrinter ();
}
static target_info Arm64Info = {
	"aarch64",			// official LLVM triple name
	"e-m:o-i64:64-i128:128-n32:64-S128", // LLVM data layout string
	"sp",				// stack-pointer name
	llvm::Triple::aarch64,
	8, 64,				// word size in bytes and bits
	29,				// numRegs
	3,				// numCalleeSaves
	true,				// hasPCRel
	{ 0, 0, 0, 0, 0 },		// no memory registers
	8232,				// call-gc offset
	8224,				// raise_overflow offset
	8*1024,				// allocation slop
        false,                          // initialized
	LLVMInitializeAArch64TargetInfo,// initTargetInfo
	LLVMInitializeAArch64Target,	// initTarget
	LLVMInitializeAArch64TargetMC,	// initMC
	LLVMInitializeAArch64AsmParser,	// initAsmParser
	LLVMInitializeAArch64AsmPrinter // initAsmPrinter
    };
#endif

#ifdef ENABLE_X86
extern "C" {
void LLVMInitializeX86TargetInfo ();
void LLVMInitializeX86Target ();
void LLVMInitializeX86TargetMC ();
void LLVMInitializeX86AsmParser ();
void LLVMInitializeX86AsmPrinter ();
}
static target_info X86_64Info = {
	"x86_64",			// official LLVM triple name
	"e-i64:64-n8:16:32:64-S128",	// LLVM data layout string
	"rsp",				// stack-pointer name
	llvm::Triple::x86_64,
	8, 64,				// word size in bytes and bits
	18,				// numRegs
	3,				// numCalleeSaves
	true,				// hasPCRel
	{				// offsets for memory registers
	    0, 0, 0,			// ALLOC_PTR, LIMIT_PTR, STORE_PTR
	    8224, 8232		   	// EXN_HNDLR, VAR_PTR
	},
	8240,				// call-gc offset
	8248,				// raise_overflow offset
	8*1024,				// allocation slop
        false,                          // initialized
	LLVMInitializeX86TargetInfo,	// initTargetInfo
	LLVMInitializeX86Target,	// initTarget
	LLVMInitializeX86TargetMC,	// initMC
	LLVMInitializeX86AsmParser,	// initAsmParser
	LLVMInitializeX86AsmPrinter	// initAsmPrinter
    };
#endif

static target_info const *Targets[] = {
#if defined(ENABLE_X86)
	&X86_64Info,
#endif
#if defined(ENABLE_ARM64)
	&Arm64Info,
#endif
    };

// the target info for the native (host) architecture
#if defined(ARCH_AMD64)
target_info const *target_info::native = &X86_64Info;
#elif defined(ARCH_ARM64)
target_info const *target_info::native = &Arm64Info;
#else
#  error unknown native architecture
#endif

constexpr int kNumTargets = sizeof(Targets) / sizeof(target_info *);

std::vector<std::string> target_info::targetNames ()
{
    std::vector<std::string> targetNames;
    targetNames.reserve(kNumTargets);
    for (int i = 0;  i < kNumTargets;  i++) {
        targetNames.push_back (Targets[i]->name);
    }
    return targetNames;
}

target_info const *target_info::infoForTarget (std::string const &name)
{
    for (int i = 0;  i < kNumTargets;  i++) {
	if (Targets[i]->name == name) {
	    return Targets[i];
	}
    }
    return nullptr;

}

llvm::Triple target_info::getTriple() const
{
    return llvm::Triple(this->name, kVendor, kOS);
}

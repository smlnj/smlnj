/*! \file code-object.cxx
 *
 * The CodeObject class abstracts the system-dependent object-file format.
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-sizes.h"  /* for endianess */
#include <iostream>
#include "target-info.hxx"
#include "code-object.hxx"
#include "llvm/Support/Error.h"

// SML/NJ runtime function for printing an error message and exiting
extern "C" {
extern void Die (const char *, ...);
}

/* determine the object-file format that we use on this platform */
#if defined(OPSYS_DARWIN)
/* macOS uses MachO as it object-file format */
#define OBJFF_MACHO
#elif defined(OPSYS_LINUX)
#define OBJFF_ELF
#else
#  error unknown operating system
#endif

static llvm::ExitOnError exitOnErr;

//==============================================================================

#ifdef ENABLE_ARM64

/* include the correct header file for relocation-record definitions */
#if defined(OBJFF_MACHO)
#include "llvm/BinaryFormat/MachO.h"
#elif defined(OBJFF_ELF)
#include "llvm/BinaryFormat/ELF.h"
#else
#  error unknown object-file format
#endif

//! specialized CodeObject class for AMD64 target
//
class AArch64CodeObject : public CodeObject {
  public:
    AArch64CodeObject (
	target_info const *target,
	std::unique_ptr<llvm::object::ObjectFile> objFile
    ) : CodeObject(target, std::move(objFile))
    { }

  protected:
    bool _includeDataSect (llvm::object::SectionRef &sect);
    void _resolveRelocs (llvm::object::SectionRef &sect, uint8_t *code);
};

bool AArch64CodeObject::_includeDataSect (llvm::object::SectionRef &sect)
{
    assert (sect.isData() && "expected data section");

#if defined(OBJFF_MACHO)
    auto name = sect.getName();
  // the "__const" section is used for jump tables
    return (name && name->equals("__const"));
#else
#  error only MachO supported for now
#endif
}

// To support instruction patching, we define a union type for 32-bit words
// that includes packed struct types that represent the layout of instructions
// that we must patch with relocation information.
class AArch64InsnWord {
public:
    AArch64InsnWord (uint32_t w) { this->_w.w32 = w; }

    uint32_t value () { return this->_w.w32; }

    void patchHi21 (uint32_t v)
    {
	uint32_t hi21 = (v >> 11);  		// hi 21 bits of value
	this->_w.hi21.immlo = hi21 & 3;		// low 2 bits of hi21
	this->_w.hi21.immhi = hi21 >> 2;	// high 19 bits of hi21
    }

    void patchLo12 (uint32_t v)
    {
	this->_w.lo12.imm12 = (v & 0xfff);
    }

    void patchB26 (uint32_t v)
    {
        // strip the two low bits (which should be "00"), since they will
        // be added back to get a signed 28-bit offset.
        //
        this->_w.b26.imm = (v & 0xfffffff) >> 2;
    }

private:
    //! union of the different instruction encodings that we have to patch (plus the
    //! raw 32-bit instruction word).
    //! WARNING: the order of the bitfields is dependent on the endianess of the host processor!
    //
    union {
        //! raw instruction word
        uint32_t w32;
	//! instructions with a 21-bit immediate values that represent the high
	//! 21-bits of an offset.  (these are the "PC relative" instructions)
	//
	struct {
#if defined(BYTE_ORDER_BIG)
	    uint32_t op1 : 1;		//!< opcode bit
	    uint32_t immlo : 2;		//!< low two bits of immediate value
	    uint32_t op2 : 5;		//!< more opcode bits
	    uint32_t immhi : 19;	//!< high 19 bits of immediate value
	    uint32_t rd : 5;            //!< destination register
#elif defined(BYTE_ORDER_LITTLE)
	    uint32_t rd : 5;
	    uint32_t immhi : 19;	// high 19 bits of immediate value
	    uint32_t op2 : 5;		// more opcode bits
	    uint32_t immlo : 2;		// low two bits of immediate value
	    uint32_t op1 : 1;		// opcode bit
#else
#  error must specify an endianess
#endif
	} hi21;
	// instructions with a 12-bit immediate value that is used for the
	// low bits of an offset.  (These include the add/sub immediate
	// instructions that are used to compute addresses)
	struct {
#if defined(BYTE_ORDER_BIG)
	    uint32_t op1 : 10;		//!< opcode bits
	    uint32_t imm12 : 12;	//!< 12-bit immediate value
	    uint32_t rn : 5;		//!< source register
	    uint32_t rd : 5;		//!< destination register
#elif defined(BYTE_ORDER_LITTLE)
	    uint32_t rd : 5;		// destination register
	    uint32_t rn : 5;		// source register
	    uint32_t imm12 : 12;	// 12-bit immediate value
	    uint32_t op1 : 10;		// opcode bits
#else
#  error must specify an endianess
#endif
	} lo12;
        // unconditional branch instructions with a 26-bit offset
	struct {
#if defined(BYTE_ORDER_BIG)
            uint32_t op : 6;            //!< opcode bits
            uint32_t imm : 26;          //!< 26-bit offset
#elif defined(BYTE_ORDER_LITTLE)
            uint32_t imm : 26;          // 26-bit offset
            uint32_t op : 6;            // opcode bits
#else
#  error must specify an endianess
#endif
        } b26;
    } _w;
};

// for the arm64, patching code is more challenging, because offsets are embedded
// in the instruction encoding and and the patching depends on the relocation
// type.
//
void AArch64CodeObject::_resolveRelocs (llvm::object::SectionRef &sect, uint8_t *code)
{
    for (auto reloc : sect.relocations()) {
      // the patch value; we ignore the relocation record if the symbol is not defined
	auto symb = reloc.getSymbol();
	if (sect.getObject()->symbols().end() != symb) {
          // the address to be patched (relative to the beginning of the object file)
	    auto offset = reloc.getOffset();
	  // the patch value.  PC relative addressing on the ARM is compute w.r.t. the
          // instruction (*not* the following one).
          //
#if (LLVM_VERSION_MAJOR > 10) /* getValue returns an Expected<> value as of LLVM 11.x */
	    int32_t value = (int32_t)exitOnErr(symb->getValue()) - (int32_t)offset;
#else
	    int32_t value = (int32_t)symb->getValue() - (int32_t)offset;
#endif
	  // get the instruction to be patched
	    AArch64InsnWord instr(*(uint32_t *)(code + offset));
	    switch (reloc.getType()) {
#if defined(OBJFF_MACHO)
	    case llvm::MachO::ARM64_RELOC_PAGE21:
#elif defined(OBJFF_ELF)
	    case llvm::ELF::R_AARCH64_ADR_PREL_PG_HI21:
#endif
		instr.patchHi21 (value);
	    	break;
#if defined(OBJFF_MACHO)
	    case llvm::MachO::ARM64_RELOC_PAGEOFF12:
#elif defined(OBJFF_ELF)
	    case llvm::ELF::R_AARCH64_ADD_ABS_LO12_NC:
#endif
		instr.patchLo12 (value);
	    	break;
#if defined(OBJFF_MACHO)
            case llvm::MachO::ARM64_RELOC_BRANCH26:
#elif defined(OBJFF_ELF)
            case llvm::ELF::R_AARCH64_JUMP26:
#endif
                instr.patchB26 (value);
                break;
	    default:
                Die ("Unknown relocation-record type %d at %p\n",
                    reloc.getType(), (void*)offset);
	    	break;
	    }
	  // update the instruction with the patched version
	    *(uint32_t *)(code + offset) = instr.value();
	}
    }

}
#endif // ENABLE_ARM64

//==============================================================================

#ifdef ENABLE_X86
//! specialized CodeObject class for AMD64 target
//
class AMD64CodeObject : public CodeObject {
  public:
    AMD64CodeObject (
	target_info const *target,
	std::unique_ptr<llvm::object::ObjectFile> objFile
    ) : CodeObject(target, std::move(objFile))
    { }

  protected:
    bool _includeDataSect (llvm::object::SectionRef &sect);
    void _resolveRelocs (llvm::object::SectionRef &sect, uint8_t *code);
};

bool AMD64CodeObject::_includeDataSect (llvm::object::SectionRef &sect)
{
    assert (sect.isData() && "expected data section");

    auto name = sect.getName();
#if defined(OBJFF_MACHO)
  // the "__literal16" section has literals referenced by the code for
  // floating-point negation and absolute value, and the "__const" section
  // has the literals created for the Overflow exception packet
    return (name && (name->equals("__literal16") || name->equals("__const")));
#else
  // the section ".rodata.cst16" has literals referenced by the code for
  // floating-point negation and absolute value
    return (name && name->equals(".rodata.cst16"));
#endif
}

// for the x86-64, patching the code is fairly easy, because the offset
// bytes are not embedded in the opcode part of the instruction.
//
void AMD64CodeObject::_resolveRelocs (llvm::object::SectionRef &sect, uint8_t *code)
{
    for (auto reloc : sect.relocations()) {
      // the patch value; we ignore the relocation record if the symbol is not defined
	auto symb = reloc.getSymbol();
	if (sect.getObject()->symbols().end() != symb) {
          // the address to be patched (relative to the beginning of the file)
	    auto offset = reloc.getOffset();
	  // the patch value; we compute the offset relative to the address of
	  // byte following the patched location.
#if (LLVM_VERSION_MAJOR > 10) /* getValue returns an Expected<> value as of LLVM 11.x */
	    int32_t value = (int32_t)exitOnErr(symb->getValue()) - ((int32_t)offset + 4);
#else
	    int32_t value = (int32_t)symb->getValue() - ((int32_t)offset + 4);
#endif
	  // update the offset one byte at a time (since it is not guaranteed to
	  // be 32-bit aligned)
	    for (int i = 0;  i < 4;  i++) {
		code[offset++] = value & 0xff;
		value >>= 8;
	    }
	}
    }

}

#endif // ENABLE_X86

//==============================================================================

// creation function for code objects
//
std::unique_ptr<CodeObject> CodeObject::create (
    target_info const *target,
    llvm::MemoryBufferRef objBuf)
{
  // first we create the LLVM object file from the memory buffer
    auto objFile = llvm::object::ObjectFile::createObjectFile (objBuf);
    if (objFile.takeError()) {
/* FIXME: error message */
	return std::unique_ptr<CodeObject>(nullptr);
    }

  // then wrap it in a target-specific subclass object
    std::unique_ptr<CodeObject> p;
    switch (target->arch) {
#ifdef ENABLE_ARM64
    case llvm::Triple::aarch64:
	p = std::make_unique<AArch64CodeObject>(target, std::move(*objFile));
	break;
#endif
#ifdef ENABLE_X86
    case llvm::Triple::x86_64:
	p = std::make_unique<AMD64CodeObject>(target, std::move(*objFile));
	break;
#endif
    default:
	assert (false && "unsupported architecture");
	return std::unique_ptr<CodeObject>(nullptr);
    }

    p->_computeSize();
    return p;
}

CodeObject::~CodeObject () { }

// copy the code into the specified memory
//
void CodeObject::getCode (uint8_t *code)
{
    for (auto sect : this->_sects) {
	auto contents = sect.getContents();
	if (contents.takeError()) {
	    std::cerr << "unable to get contents of section\n";
	    assert (0);
	}
	else {
	    auto szb = contents->size();
	    assert (sect.getSize() == szb && "inconsistent sizes");
	  /* copy the code into the object */
	    uint8_t *base = code + sect.getAddress();
	    memcpy (base, contents->data(), szb);
	  /* if the section is a text section, then resolve relocations */
	    if (sect.isText()) {
		this->_resolveRelocs (sect, base);
	    }
	}
    }
}

void CodeObject::dump (bool bits)
{
  // print info about the sections
    llvm::dbgs() << "=== Sections ===\n";
    bool foundTextSect = false;
    llvm::object::SectionRef textSect;
    for (auto sect : this->_obj->sections()) {
	auto name = sect.getName();
	auto addr = sect.getAddress();
	auto sz = sect.getSize();
	if (name) {
	    llvm::dbgs() << "  " << *name;
	} else {
	    llvm::dbgs() << "  <section>";
	}
	if (sect.isText()) {
	    if (! foundTextSect) {
		textSect = sect;
		foundTextSect = true;
	    }
	    llvm::dbgs() << " [TEXT] ";
	}
	else if (sect.isData()) {
	    llvm::dbgs() << " [DATA] ";
	}
	llvm::dbgs() << " " << (void *)addr << ".." << (void *)(addr+sz) << "\n";
    }

  // print the symbols
    llvm::dbgs() << "=== Symbols ===\n";
    for (auto sym : this->_obj->symbols()) {
	auto name = sym.getName();
	auto addr = sym.getAddress();
	if (name && addr) {
	    llvm::dbgs() << "  " << *name << " @ " << (void *)*addr << "\n";
	}
    }

  // dump relocation info
    for (auto sect : this->_obj->sections()) {
	this->_dumpRelocs (sect);
    }

  // conditionally dump the code bits
    if (bits && foundTextSect) {
      // first we create a scratch object to hold the relocated code
        size_t codeSzB = this->size();
        uint8_t *bytes = new uint8_t [codeSzB];
        this->getCode (bytes);
	llvm::dbgs () << "RELOCATED CODE\n";
	for (size_t i = 0;  i < codeSzB; i += 16) {
	    size_t limit = std::min(i + 16, codeSzB);
	    llvm::dbgs () << "  " << llvm::format_hex_no_prefix(i, 4) << ": ";
	    for (int j = i;  j < limit;  j++) {
		llvm::dbgs() << " " << llvm::format_hex_no_prefix(bytes[j], 2);
	    }
	    llvm::dbgs () << "\n";
	}
    }

}

void CodeObject::_dumpRelocs (llvm::object::SectionRef &sect)
{
    auto sectName = sect.getName();

    llvm::dbgs () << "RELOCATION INFO FOR "
	<< (sectName ? *sectName : "<unknown section>") << "\n";

    for (auto reloc : sect.relocations()) {
	auto offset = reloc.getOffset();
	if (reloc.getSymbol() != this->_obj->symbols().end()) {
	    auto symb = *(reloc.getSymbol());
	    auto name = symb.getName();
	    if (! name.takeError()) {
		llvm::dbgs () << "  " << *name
		    << ": addr = " << llvm::format_hex(exitOnErr(symb.getAddress()), 10)
#if (LLVM_VERSION_MAJOR > 10) /* getValue returns an Expected<> value as of LLVM 11.x */
		    << "; value = "  << llvm::format_hex(exitOnErr(symb.getValue()), 10)
#else
		    << "; value = "  << llvm::format_hex(symb.getValue(), 10)
#endif
		    << "; offset = " << llvm::format_hex(offset, 10)
// TODO: get the name associated with the type
		    << "; type = " << reloc.getType() << "\n";
	    } else {
		llvm::dbgs () << "  <unknown>: offset = "
		    << llvm::format_hex(offset, 10)
		    << "; type = " << reloc.getType() << "\n";
	    }
	}
    }

}

//! internal helper function for computing the amount of memory required
//! for the code object.
//
void CodeObject::_computeSize ()
{
  // iterate over the sections in the object file and identify which ones
  // we should include in the result.  We also compute the size of the
  // concatenation of the sections.
  //
    size_t codeSzb = 0;
    for (auto sect : this->_obj->sections()) {
	if (this->_includeSect (sect)) {
	    this->_sects.push_back (sect);
	    uint64_t addr = sect.getAddress();
	    uint64_t szb = sect.getSize();
#ifndef OBJFF_ELF
	    assert (codeSzb <= addr && "overlapping sections");
#endif
	    codeSzb = addr + szb;
	}
    }

  // check that we actual got something
    assert (codeSzb > 0 && "no useful sections in object file");

    this->_szb = codeSzb;
}

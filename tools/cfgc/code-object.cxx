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

#define BYTE_ORDER_LITTLE

#include <iostream>
#include "target-info.hxx"
#include "code-object.hxx"
#include "code-buffer.hxx"
#include "llvm/Support/Error.h"
#include "llvm/Support/MemoryBuffer.h"

/* determine the object-file format that we use on this platform and
 * include the correct header file for relocation-record definitions
 */
#if defined(OPSYS_DARWIN)
/* macOS uses MachO as it object-file format */
#define OBJFF_MACHO
#include "llvm/BinaryFormat/MachO.h"
#elif defined(OPSYS_LINUX)
#define OBJFF_ELF
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Object/ELFObjectFile.h"
#else
#  error unknown operating system
#endif

static llvm::ExitOnError exitOnErr;

//==============================================================================

/// get the name of a symbol
//
inline llvm::StringRef getName (llvm::object::SymbolRef const &sym)
{
    auto name = sym.getName ();
    if (name.takeError()) {
        return "<unknown symbol>";
    } else {
        return *name;
    }
}

/// get the value of a symbol
//
inline int64_t getValue (llvm::object::SymbolRef const &sym)
{
#if (LLVM_VERSION_MAJOR > 10) /* getValue returns an Expected<> value as of LLVM 11.x */
    return (int64_t)exitOnErr(sym.getValue());
#else
    return (int64_t)(sym.getValue());
#endif
}

// helper to get the type of a symbol ref as a string
//
static std::string _symbolTypeName (llvm::object::SymbolRef &symb)
{
    auto ty = symb.getType();
    if (! ty.takeError()) {
        switch (*ty) {
        case llvm::object::SymbolRef::ST_Unknown: return "Unknown";
        case llvm::object::SymbolRef::ST_Data: return "Data";
        case llvm::object::SymbolRef::ST_Debug: return "Debug";
        case llvm::object::SymbolRef::ST_File: return "File";
        case llvm::object::SymbolRef::ST_Function: return "Function";
        case llvm::object::SymbolRef::ST_Other: return "Other";
        default: return "<unknown type>";
        }
    } else {
        return "<unknown type>";
    }
}

//==============================================================================

#if defined(OBJFF_MACHO)

inline
Relocation::Relocation (Section const &sect, llvm::object::RelocationRef const &rr)
: type(rr.getType()), addr(rr.getOffset() + sect.getAddress())
{
    // for MACHO, the value of the relocation is the value of the symbol referenced
    // by the relocation record
    auto symbIt = rr.getSymbol();
    if (symbIt != rr.getObject()->symbols().end()) {
        this->value = getValue(*symbIt) - (int64_t)this->addr;
#if defined(ENABLE_X86)
/* FIXME: if the X86 is the only enabled architecture, then we do not need this
 * conditional test.
 */
        if (sect.codeObject()->target()->arch == llvm::Triple::x86_64) {
            // for the AMD64, the value should be relative to the address of the
            // instruction following the patch location.
            this->value -= 4;
        }
#endif
    }
}

#elif defined(OBJFF_ELF)

inline
Relocation::Relocation (Section const &sect, llvm::object::RelocationRef const &rr)
: type(rr.getType()), addr(rr.getOffset() + sect.offset())
{
    // for ELF, the relocation records are stored in a separate relocation section.
    // For a given relocation record, the symbol refers to a section (possibly
    // different from the section being patched).  For PC-relative addressing
    // the ELF relocation value is
    //
    //          S + A - P
    //
    // where
    //
    //          S       == the value of the symbol (i.e., the address of the section
    //                     named by the symbol)
    //          A       == the addend (an ELF-specific relocation-record value)
    //          P       == the patch address
    //
    auto symbIt = rr.getSymbol();
    if (symbIt != rr.getObject()->symbols().end()) {
        // get the "addend"
        int64_t addend = exitOnErr(llvm::object::ELFRelocationRef(rr).getAddend());
        // compute the relocation value
        llvm::object::ELFSymbolRef symb(*symbIt);
        if (symb.getELFType() == llvm::object::ELF::STT_SECTION) {
            // find the section named by the relocation symbol
            Section *namedSect = sect.codeObject()->findSection(getName(*symbIt));
            assert (namedSect != nullptr && "bogus relocation symbol");
            this->value = (int64_t)namedSect->offset() + addend - (int64_t)addr;
        } else {
        }
    }
}

#endif

//==============================================================================

#ifdef ENABLE_ARM64

// definition of AArch64CodeObject subclass
#include "arm64-code-object.inc"

#endif // ENABLE_ARM64

//==============================================================================

#ifdef ENABLE_X86

// definition of AMD64CodeObject subclass
#include "amd64-code-object.inc"

#endif // ENABLE_X86

//==============================================================================

// creation function for code objects; we assume that the code has already
// been generated into the code buffer's backing store (see mc_gen::compile).
//
std::unique_ptr<CodeObject> CodeObject::create (code_buffer *codeBuf)
{
    // create the LLVM object file
    auto memBuf =
        llvm::MemoryBufferRef(codeBuf->objectFileOS().str(), "<in-memory object>");
    auto objFile = llvm::object::ObjectFile::createObjectFile (memBuf);
    if (objFile.takeError()) {
/* FIXME: error message */
        return std::unique_ptr<CodeObject>(nullptr);
    }

  // then wrap it in a target-specific subclass object
    std::unique_ptr<CodeObject> p;
    const TargetInfo *target = codeBuf->targetInfo();
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
    // iterate over the included sections (see _computeSize below)
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
            uint8_t *base = code + sect.offset();
            memcpy (base, contents->data(), szb);
            /* resolve relocations */
            this->_resolveRelocsForSection (sect, code);
        }
    }

}

/// internal helper function for computing the amount of memory required
/// for the code object.  This function also initializes the vector of
/// sections that we are going to include in the code object
//
void CodeObject::_computeSize ()
{
    // iterate over the sections in the object file and identify which ones
    // we should include in the result.  We also compute the size of the
    // concatenation of the sections.
    //
    uint64_t codeSzB = 0;
    for (auto sect : this->_obj->sections()) {
        if (this->_includeSect (sect)) {
            uint64_t align = sect.getAlignment();
            uint64_t szb = sect.getSize();
#ifdef OBJFF_ELF
            // ELF object files always have zero as the section address
            assert (sect.getAddress() == 0 && "section address on zero");
            // align the codeSzB (we assume `align` is a power of 2)
            codeSzB = (codeSzB + align - 1) & ~(align - 1);
#else
            // align the section address (we assume `align` is a power of 2)
            uint64_t addr = (sect.getAddress() + align-1) & ~(align-1);
            assert (codeSzB <= addr && "overlapping sections");
            codeSzB = addr;
#endif
            this->_sects.push_back (Section(this, sect, codeSzB));
            codeSzB += szb;
        }
        else {
            // check to see if the section is a relocation section
            auto it = this->_relocationSect(sect);
            if (it != this->_obj->section_end()) {
                // `sect` contains relocation info for some other section
                auto targetSect = *it;
                // find the index of the target section; we search backwards, since
                // the target is usually (always?) the immediately preceding section.
		for (int i = this->_sects.size()-1;  i >= 0;  --i) {
		    if (this->_sects[i].isSection(targetSect)) {
			this->_sects[i].setRelocationSection(sect);
			break;
		    }
		}
            }
        }
    }

  // check that we actual got something
    assert (codeSzB > 0 && "no useful sections in object file");

    this->_szb = codeSzB;
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
            llvm::dbgs() << sect.getIndex() << ":  <" << *name << ">";
        } else {
            llvm::dbgs() << sect.getIndex() << ":  <section>";
        }
        llvm::dbgs() << " @" << (void *)(sect.getObject())
            << "[" << sect.getRawDataRefImpl().p << "]";
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
        auto it = this->_relocationSect(sect);
        if (it != this->_obj->section_end()) {
            // `sect` contains relocation info for some other section
            auto targetSect = *it;
            llvm::dbgs() << "      * relocated section = ";
            auto relocName = targetSect.getName();
            if (relocName) {
                llvm::dbgs() << *relocName << "\n";
            } else {
                llvm::dbgs() << "<unknown>\n";
            }
        }
    }

  // print the symbols
    llvm::dbgs() << "=== Symbols ===\n";
    for (auto sym : this->_obj->symbols()) {
        auto name = sym.getName();
        auto addr = sym.getAddress();
        if (name && addr) {
            llvm::dbgs() << "  " << _symbolTypeName(sym) << " "
                << *name << " @ " << (void *)*addr << "\n";
        }
    }

  // dump relocation info
    for (auto sect : this->_obj->sections()) {
        this->_dumpRelocs (sect);
    }

  // conditionally dump the code bits
    if (bits && foundTextSect && (this->_sects.size() > 0)) {
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

void CodeObject::_dumpRelocs (llvm::object::SectionRef const &sect)
{
    auto sectName = sect.getName();

    llvm::dbgs () << "RELOCATION INFO FOR "
        << (this->_includeSect(sect) ? "INCLUDED " : "")
        << (sectName ? *sectName : "<unknown section>") << "\n";

    for (auto r : sect.relocations()) {
        auto symbIt = r.getSymbol();
        if (symbIt != this->_obj->symbols().end()) {
            auto symb = *symbIt;
            auto name = symb.getName();
            std::string symbName = (name.takeError() ? "<unknown symbol>" : *name);
            auto exSymbSect = symb.getSection();
            std::string symbSectName;
            if (exSymbSect.takeError()) {
                symbSectName = "<error>";
            } else {
                auto symbSectIt = *exSymbSect;
                if (symbSectIt == this->_obj->sections().end()) {
                    symbSectName = "<no section>";
                } else {
                    auto name = symbSectIt->getName();
                    symbSectName = (name.takeError() ? "<unknown section>" : *name);
                }
            }
            auto addr = symb.getAddress();
            uint64_t symbAddr = (addr.takeError() ? 0xdeadbeef : *addr);
#if defined(OBJFF_ELF)
            llvm::object::ELFSymbolRef elfSymb(symb);
#endif
            llvm::dbgs() << "  " << this->_relocTypeToString(r.getType())
                    << ": offset = " << llvm::format_hex(r.getOffset(), 10)
                    << "; symb = [name = " << symbName
                    << "; sect = " << symbSectName
                    << "; addr = " << llvm::format_hex(symbAddr, 10)
                    << "; value = " << llvm::format_hex(symb.getValue(), 10)
#if defined(OBJFF_ELF)
                    << "; type = \"" << elfSymb.getELFTypeName()
                    << "]; addend = "
                    << exitOnErr(llvm::object::ELFRelocationRef(r).getAddend())
                    << "\n";
#else
                    << "]\n";
#endif
        }
    }

}

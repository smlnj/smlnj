/// \file amd64-code-object.inc
///
/// \copyright 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Code objects for the AMD64 (aka X86-64) target.  This file is included
///        in the \file code-object.cxx file when the AMD64 target is enabled.
///
/// \author John Reppy
///

//! specialized CodeObject class for AMD64 (aka X86-64) target
//
class AMD64CodeObject : public CodeObject {
  public:
    AMD64CodeObject (
        target_info const *target,
        std::unique_ptr<llvm::object::ObjectFile> objFile
    ) : CodeObject(target, std::move(objFile))
    { }

    ~AMD64CodeObject () { }

  protected:
    bool _includeDataSect (llvm::object::SectionRef const &sect) override;
    void _resolveRelocs (Section const &sect, uint8_t *code) override;
    std::string _relocTypeToString (uint64_t ty) override;
};

bool AMD64CodeObject::_includeDataSect (llvm::object::SectionRef const &sect)
{
    assert (sect.isData() && "expected data section");

    auto name = sect.getName();
    if (! name) {
        return false;
    }
#if defined(OBJFF_MACHO)
  // the "__literal16" section has literals referenced by the code for
  // floating-point negation and absolute value, and the "__const" section
  // has the literals created for the Overflow exception packet
    return name->equals("__literal16")
        || name->equals("__const");
#else
  // the section ".rodata.cst16" has literals referenced by the code for
  // floating-point negation and absolute value
    return name->equals(".rodata")
        || name->equals(".rodata.cst16");
#endif
}

// for the x86-64, patching the code is fairly easy, because the offset
// bytes are not embedded in the opcode part of the instruction.
//
void AMD64CodeObject::_resolveRelocs (Section const &sect, uint8_t *code)
{
    for (auto rr : sect.relocations()) {
        Relocation reloc(sect.sect, rr);
        // the patch value; we ignore the relocation record if the symbol is not defined
        if (sect.getObject()->symbols().end() != rr.getSymbol()) {
            // the patch value; we compute the offset relative to the address of
            // byte following the patched location.
            int32_t value = (int32_t)(reloc.value - reloc.addr + 4);
            switch (reloc.type) {
#if defined(OBJFF_MACHO)
	    case llvm::MachO::X86_64_RELOC_SIGNED:
	    case llvm::MachO::X86_64_RELOC_BRANCH:
#elif defined(OBJFF_ELF)
            case llvm::ELF::R_X86_64_PC32:
#endif
                // update the offset one byte at a time (since it is not
                // guaranteed to be 32-bit aligned)
                for (int i = 0;  i < 4;  i++) {
                    code[reloc.addr+i] = value & 0xff;
                    value >>= 8;
                }
                break;
            default:
                Die ("Unsupported relocation-record type %s at %p\n",
                    this->_relocTypeToString(reloc.type).c_str(),
                    (void*)reloc.addr);
                break;
            }
        }
    }

}

std::string AMD64CodeObject::_relocTypeToString (uint64_t ty)
{
    switch (ty) {
#if defined(OBJFF_ELF)
    case llvm::ELF::R_X86_64_NONE: return "R_NONE (0)";
    case llvm::ELF::R_X86_64_64: return "R_64 (1)";
    case llvm::ELF::R_X86_64_PC32: return "R_PC32 (2)";
    case llvm::ELF::R_X86_64_GOT32: return "R_GOT32 (3)";
    case llvm::ELF::R_X86_64_PLT32: return "R_PLT32 (4)";
    case llvm::ELF::R_X86_64_COPY: return "R_COPY (5)";
    case llvm::ELF::R_X86_64_GLOB_DAT: return "R_GLOB_DAT (6)";
    case llvm::ELF::R_X86_64_JUMP_SLOT: return "R_JUMP_SLOT (7)";
    case llvm::ELF::R_X86_64_RELATIVE: return "R_RELATIVE (8)";
    case llvm::ELF::R_X86_64_GOTPCREL: return "R_GOTPCREL (9)";
    case llvm::ELF::R_X86_64_32: return "R_32 (10)";
    case llvm::ELF::R_X86_64_32S: return "R_32S (11)";
    case llvm::ELF::R_X86_64_16: return "R_16 (12)";
    case llvm::ELF::R_X86_64_PC16: return "R_PC16 (13)";
    case llvm::ELF::R_X86_64_8: return "R_8 (14)";
    case llvm::ELF::R_X86_64_PC8: return "R_PC8 (15)";
    case llvm::ELF::R_X86_64_DTPMOD64: return "R_DTPMOD64 (16)";
    case llvm::ELF::R_X86_64_DTPOFF64: return "R_DTPOFF64 (17)";
    case llvm::ELF::R_X86_64_TPOFF64: return "R_TPOFF64 (18)";
    case llvm::ELF::R_X86_64_TLSGD: return "R_TLSGD (19)";
    case llvm::ELF::R_X86_64_TLSLD: return "R_TLSLD (20)";
    case llvm::ELF::R_X86_64_DTPOFF32: return "R_DTPOFF32 (21)";
    case llvm::ELF::R_X86_64_GOTTPOFF: return "R_GOTTPOFF (22)";
    case llvm::ELF::R_X86_64_TPOFF32: return "R_TPOFF32 (23)";
    case llvm::ELF::R_X86_64_PC64: return "R_PC64 (24)";
    case llvm::ELF::R_X86_64_GOTOFF64: return "R_GOTOFF64 (25)";
    case llvm::ELF::R_X86_64_GOTPC32: return "R_GOTPC32 (26)";
    case llvm::ELF::R_X86_64_GOT64: return "R_GOT64 (27)";
    case llvm::ELF::R_X86_64_GOTPCREL64: return "R_GOTPCREL64 (28)";
    case llvm::ELF::R_X86_64_GOTPC64: return "R_GOTPC64 (29)";
    case llvm::ELF::R_X86_64_GOTPLT64: return "R_GOTPLT64 (30)";
    case llvm::ELF::R_X86_64_PLTOFF64: return "R_PLTOFF64 (31)";
    case llvm::ELF::R_X86_64_SIZE32: return "R_SIZE32 (32)";
    case llvm::ELF::R_X86_64_SIZE64: return "R_SIZE64 (33)";
    case llvm::ELF::R_X86_64_GOTPC32_TLSDESC: return "R_GOTPC32_TLSDESC (34)";
    case llvm::ELF::R_X86_64_TLSDESC_CALL: return "R_TLSDESC_CALL (35)";
    case llvm::ELF::R_X86_64_TLSDESC: return "R_TLSDESC (36)";
    case llvm::ELF::R_X86_64_IRELATIVE: return "R_IRELATIVE (37)";
    case llvm::ELF::R_X86_64_GOTPCRELX: return "R_GOTPCRELX (41)";
    case llvm::ELF::R_X86_64_REX_GOTPCRELX: return "R_REX_GOTPCRELX (42)";
#elif defined(OBJFF_MACHO)
    case llvm::MachO::X86_64_RELOC_UNSIGNED: return "RELOC_UNSIGNED (0)";
    case llvm::MachO::X86_64_RELOC_SIGNED: return "RELOC_SIGNED (1)";
    case llvm::MachO::X86_64_RELOC_BRANCH: return "RELOC_BRANCH (2)";
    case llvm::MachO::X86_64_RELOC_GOT_LOAD: return "RELOC_GOT_LOAD (3)";
    case llvm::MachO::X86_64_RELOC_GOT: return "RELOC_GOT (4)";
    case llvm::MachO::X86_64_RELOC_SUBTRACTOR: return "RELOC_SUBTRACTOR (5)";
    case llvm::MachO::X86_64_RELOC_SIGNED_1: return "RELOC_SIGNED_1 (6)";
    case llvm::MachO::X86_64_RELOC_SIGNED_2: return "RELOC_SIGNED_2 (7)";
    case llvm::MachO::X86_64_RELOC_SIGNED_4: return "RELOC_SIGNED_4 (8)";
    case llvm::MachO::X86_64_RELOC_TLV: return "RELOC_TLV (9)";
#endif
    default: return std::to_string(ty);
    }
}

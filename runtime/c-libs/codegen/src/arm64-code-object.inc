/// \file arm64-code-object.inc
///
/// \copyright 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Code objects for ARM64 target  This file is included
///        in the \file code-object.cxx file when the ARM64 target is enabled.
///
/// \author John Reppy
///

//! specialized CodeObject class for ARM64 target
//
class AArch64CodeObject : public CodeObject {
  public:
    AArch64CodeObject (
        target_info const *target,
        std::unique_ptr<llvm::object::ObjectFile> objFile
    ) : CodeObject(target, std::move(objFile))
    { }

    ~AArch64CodeObject () { }

  protected:
    bool _includeDataSect (llvm::object::SectionRef const &sect) override;
    void _resolveRelocsForSection (Section const &sect, uint8_t *code) override;
    std::string _relocTypeToString (uint64_t ty) override;
};

bool AArch64CodeObject::_includeDataSect (llvm::object::SectionRef const &sect)
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
        uint32_t hi21 = (v >> 11);              // hi 21 bits of value
        this->_w.hi21.immlo = hi21 & 3;         // low 2 bits of hi21
        this->_w.hi21.immhi = hi21 >> 2;        // high 19 bits of hi21
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
            uint32_t op1 : 1;           //!< opcode bit
            uint32_t immlo : 2;         //!< low two bits of immediate value
            uint32_t op2 : 5;           //!< more opcode bits
            uint32_t immhi : 19;        //!< high 19 bits of immediate value
            uint32_t rd : 5;            //!< destination register
#elif defined(BYTE_ORDER_LITTLE)
            uint32_t rd : 5;
            uint32_t immhi : 19;        // high 19 bits of immediate value
            uint32_t op2 : 5;           // more opcode bits
            uint32_t immlo : 2;         // low two bits of immediate value
            uint32_t op1 : 1;           // opcode bit
#else
#  error must specify an endianess
#endif
        } hi21;
        // instructions with a 12-bit immediate value that is used for the
        // low bits of an offset.  (These include the add/sub immediate
        // instructions that are used to compute addresses)
        struct {
#if defined(BYTE_ORDER_BIG)
            uint32_t op1 : 10;          //!< opcode bits
            uint32_t imm12 : 12;        //!< 12-bit immediate value
            uint32_t rn : 5;            //!< source register
            uint32_t rd : 5;            //!< destination register
#elif defined(BYTE_ORDER_LITTLE)
            uint32_t rd : 5;            // destination register
            uint32_t rn : 5;            // source register
            uint32_t imm12 : 12;        // 12-bit immediate value
            uint32_t op1 : 10;          // opcode bits
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
void AArch64CodeObject::_resolveRelocsForSection (Section const &sect, uint8_t *code)
{
    for (auto rr : sect.relocations()) {
        Relocation reloc(sect, rr);
        // the patch value; we ignore the relocation record if the symbol is not defined
        if (sect.codeObject()->symbols().end() != rr.getSymbol()) {
            // the patch value.  PC relative addressing on the ARM is computed w.r.t.
            // the address of the instruction (*not* the following one).
            //
            int32_t value = (int32_t)(reloc.value - reloc.addr);
            // get the instruction to be patched
            AArch64InsnWord instr(*(uint32_t *)(code + reloc.addr));
            switch (reloc.type) {
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
                Die ("Unsupported relocation-record type %s at %p\n",
                    this->_relocTypeToString(reloc.type).c_str(),
                    (void*)reloc.addr);
                break;
            }
          // update the instruction with the patched version
            *(uint32_t *)(code + reloc.addr) = instr.value();
        }
    }

}

std::string AArch64CodeObject::_relocTypeToString (uint64_t ty)
{
    switch (ty) {
#if defined(OBJFF_ELF)
    case llvm::ELF::R_ARM_NONE: return "R_NONE (0x00)";
    case llvm::ELF::R_ARM_PC24: return "R_PC24 (0x01)";
    case llvm::ELF::R_ARM_ABS32: return "R_ABS32 (0x02)";
    case llvm::ELF::R_ARM_REL32: return "R_REL32 (0x03)";
    case llvm::ELF::R_ARM_LDR_PC_G0: return "R_LDR_PC_G0 (0x04)";
    case llvm::ELF::R_ARM_ABS16: return "R_ABS16 (0x05)";
    case llvm::ELF::R_ARM_ABS12: return "R_ABS12 (0x06)";
    case llvm::ELF::R_ARM_THM_ABS5: return "R_THM_ABS5 (0x07)";
    case llvm::ELF::R_ARM_ABS8: return "R_ABS8 (0x08)";
    case llvm::ELF::R_ARM_SBREL32: return "R_SBREL32 (0x09)";
    case llvm::ELF::R_ARM_THM_CALL: return "R_THM_CALL (0x0a)";
    case llvm::ELF::R_ARM_THM_PC8: return "R_THM_PC8 (0x0b)";
    case llvm::ELF::R_ARM_BREL_ADJ: return "R_BREL_ADJ (0x0c)";
    case llvm::ELF::R_ARM_TLS_DESC: return "R_TLS_DESC (0x0d)";
    case llvm::ELF::R_ARM_THM_SWI8: return "R_THM_SWI8 (0x0e)";
    case llvm::ELF::R_ARM_XPC25: return "R_XPC25 (0x0f)";
    case llvm::ELF::R_ARM_THM_XPC22: return "R_THM_XPC22 (0x10)";
    case llvm::ELF::R_ARM_TLS_DTPMOD32: return "R_TLS_DTPMOD32 (0x11)";
    case llvm::ELF::R_ARM_TLS_DTPOFF32: return "R_TLS_DTPOFF32 (0x12)";
    case llvm::ELF::R_ARM_TLS_TPOFF32: return "R_TLS_TPOFF32 (0x13)";
    case llvm::ELF::R_ARM_COPY: return "R_COPY (0x14)";
    case llvm::ELF::R_ARM_GLOB_DAT: return "R_GLOB_DAT (0x15)";
    case llvm::ELF::R_ARM_JUMP_SLOT: return "R_JUMP_SLOT (0x16)";
    case llvm::ELF::R_ARM_RELATIVE: return "R_RELATIVE (0x17)";
    case llvm::ELF::R_ARM_GOTOFF32: return "R_GOTOFF32 (0x18)";
    case llvm::ELF::R_ARM_BASE_PREL: return "R_BASE_PREL (0x19)";
    case llvm::ELF::R_ARM_GOT_BREL: return "R_GOT_BREL (0x1a)";
    case llvm::ELF::R_ARM_PLT32: return "R_PLT32 (0x1b)";
    case llvm::ELF::R_ARM_CALL: return "R_CALL (0x1c)";
    case llvm::ELF::R_ARM_JUMP24: return "R_JUMP24 (0x1d)";
    case llvm::ELF::R_ARM_THM_JUMP24: return "R_THM_JUMP24 (0x1e)";
    case llvm::ELF::R_ARM_BASE_ABS: return "R_BASE_ABS (0x1f)";
    case llvm::ELF::R_ARM_ALU_PCREL_7_0: return "R_ALU_PCREL_7_0 (0x20)";
    case llvm::ELF::R_ARM_ALU_PCREL_15_8: return "R_ALU_PCREL_15_8 (0x21)";
    case llvm::ELF::R_ARM_ALU_PCREL_23_15: return "R_ALU_PCREL_23_15 (0x22)";
    case llvm::ELF::R_ARM_LDR_SBREL_11_0_NC: return "R_LDR_SBREL_11_0_NC (0x23)";
    case llvm::ELF::R_ARM_ALU_SBREL_19_12_NC: return "R_ALU_SBREL_19_12_NC (0x24)";
    case llvm::ELF::R_ARM_ALU_SBREL_27_20_CK: return "R_ALU_SBREL_27_20_CK (0x25)";
    case llvm::ELF::R_ARM_TARGET1: return "R_TARGET1 (0x26)";
    case llvm::ELF::R_ARM_SBREL31: return "R_SBREL31 (0x27)";
    case llvm::ELF::R_ARM_V4BX: return "R_V4BX (0x28)";
    case llvm::ELF::R_ARM_TARGET2: return "R_TARGET2 (0x29)";
    case llvm::ELF::R_ARM_PREL31: return "R_PREL31 (0x2a)";
    case llvm::ELF::R_ARM_MOVW_ABS_NC: return "R_MOVW_ABS_NC (0x2b)";
    case llvm::ELF::R_ARM_MOVT_ABS: return "R_MOVT_ABS (0x2c)";
    case llvm::ELF::R_ARM_MOVW_PREL_NC: return "R_MOVW_PREL_NC (0x2d)";
    case llvm::ELF::R_ARM_MOVT_PREL: return "R_MOVT_PREL (0x2e)";
    case llvm::ELF::R_ARM_THM_MOVW_ABS_NC: return "R_THM_MOVW_ABS_NC (0x2f)";
    case llvm::ELF::R_ARM_THM_MOVT_ABS: return "R_THM_MOVT_ABS (0x30)";
    case llvm::ELF::R_ARM_THM_MOVW_PREL_NC: return "R_THM_MOVW_PREL_NC (0x31)";
    case llvm::ELF::R_ARM_THM_MOVT_PREL: return "R_THM_MOVT_PREL (0x32)";
    case llvm::ELF::R_ARM_THM_JUMP19: return "R_THM_JUMP19 (0x33)";
    case llvm::ELF::R_ARM_THM_JUMP6: return "R_THM_JUMP6 (0x34)";
    case llvm::ELF::R_ARM_THM_ALU_PREL_11_0: return "R_THM_ALU_PREL_11_0 (0x35)";
    case llvm::ELF::R_ARM_THM_PC12: return "R_THM_PC12 (0x36)";
    case llvm::ELF::R_ARM_ABS32_NOI: return "R_ABS32_NOI (0x37)";
    case llvm::ELF::R_ARM_REL32_NOI: return "R_REL32_NOI (0x38)";
    case llvm::ELF::R_ARM_ALU_PC_G0_NC: return "R_ALU_PC_G0_NC (0x39)";
    case llvm::ELF::R_ARM_ALU_PC_G0: return "R_ALU_PC_G0 (0x3a)";
    case llvm::ELF::R_ARM_ALU_PC_G1_NC: return "R_ALU_PC_G1_NC (0x3b)";
    case llvm::ELF::R_ARM_ALU_PC_G1: return "R_ALU_PC_G1 (0x3c)";
    case llvm::ELF::R_ARM_ALU_PC_G2: return "R_ALU_PC_G2 (0x3d)";
    case llvm::ELF::R_ARM_LDR_PC_G1: return "R_LDR_PC_G1 (0x3e)";
    case llvm::ELF::R_ARM_LDR_PC_G2: return "R_LDR_PC_G2 (0x3f)";
    case llvm::ELF::R_ARM_LDRS_PC_G0: return "R_LDRS_PC_G0 (0x40)";
    case llvm::ELF::R_ARM_LDRS_PC_G1: return "R_LDRS_PC_G1 (0x41)";
    case llvm::ELF::R_ARM_LDRS_PC_G2: return "R_LDRS_PC_G2 (0x42)";
    case llvm::ELF::R_ARM_LDC_PC_G0: return "R_LDC_PC_G0 (0x43)";
    case llvm::ELF::R_ARM_LDC_PC_G1: return "R_LDC_PC_G1 (0x44)";
    case llvm::ELF::R_ARM_LDC_PC_G2: return "R_LDC_PC_G2 (0x45)";
    case llvm::ELF::R_ARM_ALU_SB_G0_NC: return "R_ALU_SB_G0_NC (0x46)";
    case llvm::ELF::R_ARM_ALU_SB_G0: return "R_ALU_SB_G0 (0x47)";
    case llvm::ELF::R_ARM_ALU_SB_G1_NC: return "R_ALU_SB_G1_NC (0x48)";
    case llvm::ELF::R_ARM_ALU_SB_G1: return "R_ALU_SB_G1 (0x49)";
    case llvm::ELF::R_ARM_ALU_SB_G2: return "R_ALU_SB_G2 (0x4a)";
    case llvm::ELF::R_ARM_LDR_SB_G0: return "R_LDR_SB_G0 (0x4b)";
    case llvm::ELF::R_ARM_LDR_SB_G1: return "R_LDR_SB_G1 (0x4c)";
    case llvm::ELF::R_ARM_LDR_SB_G2: return "R_LDR_SB_G2 (0x4d)";
    case llvm::ELF::R_ARM_LDRS_SB_G0: return "R_LDRS_SB_G0 (0x4e)";
    case llvm::ELF::R_ARM_LDRS_SB_G1: return "R_LDRS_SB_G1 (0x4f)";
    case llvm::ELF::R_ARM_LDRS_SB_G2: return "R_LDRS_SB_G2 (0x50)";
    case llvm::ELF::R_ARM_LDC_SB_G0: return "R_LDC_SB_G0 (0x51)";
    case llvm::ELF::R_ARM_LDC_SB_G1: return "R_LDC_SB_G1 (0x52)";
    case llvm::ELF::R_ARM_LDC_SB_G2: return "R_LDC_SB_G2 (0x53)";
    case llvm::ELF::R_ARM_MOVW_BREL_NC: return "R_MOVW_BREL_NC (0x54)";
    case llvm::ELF::R_ARM_MOVT_BREL: return "R_MOVT_BREL (0x55)";
    case llvm::ELF::R_ARM_MOVW_BREL: return "R_MOVW_BREL (0x56)";
    case llvm::ELF::R_ARM_THM_MOVW_BREL_NC: return "R_THM_MOVW_BREL_NC (0x57)";
    case llvm::ELF::R_ARM_THM_MOVT_BREL: return "R_THM_MOVT_BREL (0x58)";
    case llvm::ELF::R_ARM_THM_MOVW_BREL: return "R_THM_MOVW_BREL (0x59)";
    case llvm::ELF::R_ARM_TLS_GOTDESC: return "R_TLS_GOTDESC (0x5a)";
    case llvm::ELF::R_ARM_TLS_CALL: return "R_TLS_CALL (0x5b)";
    case llvm::ELF::R_ARM_TLS_DESCSEQ: return "R_TLS_DESCSEQ (0x5c)";
    case llvm::ELF::R_ARM_THM_TLS_CALL: return "R_THM_TLS_CALL (0x5d)";
    case llvm::ELF::R_ARM_PLT32_ABS: return "R_PLT32_ABS (0x5e)";
    case llvm::ELF::R_ARM_GOT_ABS: return "R_GOT_ABS (0x5f)";
    case llvm::ELF::R_ARM_GOT_PREL: return "R_GOT_PREL (0x60)";
    case llvm::ELF::R_ARM_GOT_BREL12: return "R_GOT_BREL12 (0x61)";
    case llvm::ELF::R_ARM_GOTOFF12: return "R_GOTOFF12 (0x62)";
    case llvm::ELF::R_ARM_GOTRELAX: return "R_GOTRELAX (0x63)";
    case llvm::ELF::R_ARM_GNU_VTENTRY: return "R_GNU_VTENTRY (0x64)";
    case llvm::ELF::R_ARM_GNU_VTINHERIT: return "R_GNU_VTINHERIT (0x65)";
    case llvm::ELF::R_ARM_THM_JUMP11: return "R_THM_JUMP11 (0x66)";
    case llvm::ELF::R_ARM_THM_JUMP8: return "R_THM_JUMP8 (0x67)";
    case llvm::ELF::R_ARM_TLS_GD32: return "R_TLS_GD32 (0x68)";
    case llvm::ELF::R_ARM_TLS_LDM32: return "R_TLS_LDM32 (0x69)";
    case llvm::ELF::R_ARM_TLS_LDO32: return "R_TLS_LDO32 (0x6a)";
    case llvm::ELF::R_ARM_TLS_IE32: return "R_TLS_IE32 (0x6b)";
    case llvm::ELF::R_ARM_TLS_LE32: return "R_TLS_LE32 (0x6c)";
    case llvm::ELF::R_ARM_TLS_LDO12: return "R_TLS_LDO12 (0x6d)";
    case llvm::ELF::R_ARM_TLS_LE12: return "R_TLS_LE12 (0x6e)";
    case llvm::ELF::R_ARM_TLS_IE12GP: return "R_TLS_IE12GP (0x6f)";
    case llvm::ELF::R_ARM_PRIVATE_0: return "R_PRIVATE_0 (0x70)";
    case llvm::ELF::R_ARM_PRIVATE_1: return "R_PRIVATE_1 (0x71)";
    case llvm::ELF::R_ARM_PRIVATE_2: return "R_PRIVATE_2 (0x72)";
    case llvm::ELF::R_ARM_PRIVATE_3: return "R_PRIVATE_3 (0x73)";
    case llvm::ELF::R_ARM_PRIVATE_4: return "R_PRIVATE_4 (0x74)";
    case llvm::ELF::R_ARM_PRIVATE_5: return "R_PRIVATE_5 (0x75)";
    case llvm::ELF::R_ARM_PRIVATE_6: return "R_PRIVATE_6 (0x76)";
    case llvm::ELF::R_ARM_PRIVATE_7: return "R_PRIVATE_7 (0x77)";
    case llvm::ELF::R_ARM_PRIVATE_8: return "R_PRIVATE_8 (0x78)";
    case llvm::ELF::R_ARM_PRIVATE_9: return "R_PRIVATE_9 (0x79)";
    case llvm::ELF::R_ARM_PRIVATE_10: return "R_PRIVATE_10 (0x7a)";
    case llvm::ELF::R_ARM_PRIVATE_11: return "R_PRIVATE_11 (0x7b)";
    case llvm::ELF::R_ARM_PRIVATE_12: return "R_PRIVATE_12 (0x7c)";
    case llvm::ELF::R_ARM_PRIVATE_13: return "R_PRIVATE_13 (0x7d)";
    case llvm::ELF::R_ARM_PRIVATE_14: return "R_PRIVATE_14 (0x7e)";
    case llvm::ELF::R_ARM_PRIVATE_15: return "R_PRIVATE_15 (0x7f)";
    case llvm::ELF::R_ARM_ME_TOO: return "R_ME_TOO (0x80)";
    case llvm::ELF::R_ARM_THM_TLS_DESCSEQ16: return "R_THM_TLS_DESCSEQ16 (0x81)";
    case llvm::ELF::R_ARM_THM_TLS_DESCSEQ32: return "R_THM_TLS_DESCSEQ32 (0x82)";
    case llvm::ELF::R_ARM_THM_BF16: return "R_THM_BF16 (0x88)";
    case llvm::ELF::R_ARM_THM_BF12: return "R_THM_BF12 (0x89)";
    case llvm::ELF::R_ARM_THM_BF18: return "R_THM_BF18 (0x8a)";
    case llvm::ELF::R_ARM_IRELATIVE: return "R_IRELATIVE (0xa0)";
#elif defined(OBJFF_MACHO)
    case llvm::MachO::ARM64_RELOC_UNSIGNED: return "RELOC_UNSIGNED (0)";
    case llvm::MachO::ARM64_RELOC_SUBTRACTOR: return "RELOC_SUBTRACTOR (1)";
    case llvm::MachO::ARM64_RELOC_BRANCH26: return "RELOC_BRANCH26 (2)";
    case llvm::MachO::ARM64_RELOC_PAGE21: return "RELOC_PAGE21 (3)";
    case llvm::MachO::ARM64_RELOC_PAGEOFF12: return "RELOC_PAGEOFF12 (4)";
    case llvm::MachO::ARM64_RELOC_GOT_LOAD_PAGE21: return "RELOC_GOT_LOAD_PAGE21 (5)";
    case llvm::MachO::ARM64_RELOC_GOT_LOAD_PAGEOFF12: return "RELOC_GOT_LOAD_PAGEOFF12 (6)";
    case llvm::MachO::ARM64_RELOC_POINTER_TO_GOT: return "RELOC_POINTER_TO_GOT (7)";
    case llvm::MachO::ARM64_RELOC_TLVP_LOAD_PAGE21: return "RELOC_TLVP_LOAD_PAGE21 (8)";
    case llvm::MachO::ARM64_RELOC_TLVP_LOAD_PAGEOFF12: return "RELOC_TLVP_LOAD_PAGEOFF12 (9)";
    case llvm::MachO::ARM64_RELOC_ADDEND: return "RELOC_ADDEND (10)";
#endif
    default: return std::to_string(ty);
    }
}

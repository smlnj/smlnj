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

/// relocation info
struct Relocation {
/* TODO: define factory
    std::option<Relocation> create (CodeObject::Section &sect, llvm::object::RelocationRef &rr);
 * that returns NONE when the symbol is not defined.
 */

    Relocation (llvm::object::SectionRef const &sect, llvm::object::RelocationRef const &rr)
    : type(rr.getType()), addr(rr.getOffset())
    {
#if defined(OBJFF_ELF)
        // for ELF files, the relocation value is stored as an "addend"
        auto elfReloc = llvm::object::ELFRelocationRef(rr);
        this->value = exitOnErr(elfReloc.getAddend());
        // adjust the offset to be object-file relative
        this->addr += sect.getAddress();
#else
        auto symbIt = rr.getSymbol();
        if (symbIt != rr.getObject()->symbols().end()) {
#if (LLVM_VERSION_MAJOR > 10) /* getValue returns an Expected<> value as of LLVM 11.x */
            this->value = (int64_t)exitOnErr(symbIt->getValue());
#else
            this->value = (int64_t)(symbIt->getValue());
#endif
        }
#endif
    }

    uint64_t type;      //!< the type of relocation record
    uint64_t addr;      //!< the address of the relocation relative to the start of the
                        //!  object file
    int64_t value;      //!< the value of the relocation

}; // struct Relocation


//==============================================================================

#ifdef ENABLE_ARM64

//! specialized CodeObject class for AMD64 target
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
    void _resolveRelocs (CodeObject::Section &sect, uint8_t *code) override;
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
void AArch64CodeObject::_resolveRelocs (CodeObject::Section &sect, uint8_t *code)
{
llvm::dbgs() << "## RELOCATIONS\n";
    for (auto rr : sect.relocations()) {
        Relocation reloc(sect.sect, rr);
        // the patch value; we ignore the relocation record if the symbol is not defined
        if (sect.getObject()->symbols().end() != rr.getSymbol()) {
            // the patch value.  PC relative addressing on the ARM is compute w.r.t. the
            // address of the instruction (*not* the following one).
            //
            int32_t value = (int32_t)reloc.value - (int32_t)reloc.addr;
            // get the instruction to be patched
            AArch64InsnWord instr(*(uint32_t *)(code + reloc.addr));
llvm::dbgs() << "### " << llvm::format_hex_no_prefix(reloc.addr, 4) << ": "
<< "value = " << value << "; " << llvm::format_hex(instr.value(), 10) << " ==> ";
            switch (reloc.type) {
#if defined(OBJFF_MACHO)
            case llvm::MachO::ARM64_RELOC_PAGE21:
#elif defined(OBJFF_ELF)
            case llvm::ELF::R_AARCH64_ADR_PREL_PG_HI21:
#endif
                instr.patchHi21 (value);
llvm::dbgs() << llvm::format_hex(instr.value(), 10) << " [PAGE21]\n";
                break;
#if defined(OBJFF_MACHO)
            case llvm::MachO::ARM64_RELOC_PAGEOFF12:
#elif defined(OBJFF_ELF)
            case llvm::ELF::R_AARCH64_ADD_ABS_LO12_NC:
#endif
                instr.patchLo12 (value);
llvm::dbgs() << llvm::format_hex(instr.value(), 10) << " [PAGEOFF12]\n";
                break;
#if defined(OBJFF_MACHO)
            case llvm::MachO::ARM64_RELOC_BRANCH26:
#elif defined(OBJFF_ELF)
            case llvm::ELF::R_AARCH64_JUMP26:
#endif
                instr.patchB26 (value);
llvm::dbgs() << llvm::format_hex(instr.value(), 10) << " [BRANCH26]\n";
                break;
            default:
                llvm::dbgs() << "!!! Unsupported relocation-record type "
                    << this->_relocTypeToString(reloc.type)
                    << "at " << (void*)reloc.addr << "\n";
                break;
            }
          // update the instruction with the patched version
            *(uint32_t *)(code + reloc.addr) = instr.value();
        }
    }
llvm::dbgs() << "## END RELOCATIONS\n";

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

    ~AMD64CodeObject () { }

  protected:
    bool _includeDataSect (llvm::object::SectionRef const &sect) override;
    void _resolveRelocs (CodeObject::Section &sect, uint8_t *code) override;
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
void AMD64CodeObject::_resolveRelocs (CodeObject::Section &sect, uint8_t *code)
{
    for (auto rr : sect.relocations()) {
        Relocation reloc(sect.sect, rr);
        // the patch value; we ignore the relocation record if the symbol is not defined
        if (sect.getObject()->symbols().end() != rr.getSymbol()) {
            // the patch value; we compute the offset relative to the address of
            // byte following the patched location.
            int32_t value = (int32_t)reloc.value - (int32_t)reloc.addr + 4;
            switch (reloc.type) {
#if defined(OBJFF_MACHO)
	    case llvm::MachO::X86_64_RELOC_SIGNED:
	    case llvm::MachO::X86_64_RELOC_BRANCH:
#elif defined(OBJFF_ELF)
            case llvm::ELF::R_X86_64_PC32:
#endif
                // update the offset one byte at a time (since it is not
                // guaranteed to be 32-bit aligned)
                auto offset = reloc.addr;
                for (int i = 0;  i < 4;  i++) {
                    code[offset++] = value & 0xff;
                    value >>= 8;
                }
                break;
            default:
                llvm::dbgs() << "!!! Unsupported relocation-record type "
                    << this->_relocTypeToString(reloc.type)
                    << "at " << (void*)reloc.addr << "\n";
                break;
            }
        }
    }

}

std::string AMD64CodeObject::_relocTypeToString (uint64_t ty)
{
#if defined(OBJFF_ELF)
    switch (ty) {
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
    target_info const *target = codeBuf->targetInfo();
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
            uint8_t *base = code + sect.offset;
            memcpy (base, contents->data(), szb);
            /* resolve relocations */
            this->_resolveRelocs (sect, base);
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
            this->_sects.push_back (Section(sect, codeSzB));
            codeSzB += szb;
        }
        else {
            // check to see if the section is a relocation section
            auto it = this->_relocationSect(sect);
            if (it != this->_obj->section_end()) {
                // `sect` contains relocation info for some other section
                auto targetSect = *it;
		for (int i = 0;  i < this->_sects.size();  ++i) {
		    if (this->_sects[i].sect == targetSect) {
			this->_sects[i] =
                            Section(targetSect, this->_sects[i].offset, sect);
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
        Relocation reloc(sect, r);
        auto symbIt = r.getSymbol();
        if (symbIt != this->_obj->symbols().end()) {
            auto symb = *symbIt;
            llvm::dbgs() << "  " << _symbolTypeName(symb) << " ";
            auto name = symb.getName();
            if (! name.takeError()) {
                llvm::dbgs() << *name
                    << ": value = "  << llvm::format_hex(reloc.value, 10)
                    << "; addr = " << llvm::format_hex(reloc.addr, 10)
                    << "; type = " << this->_relocTypeToString(reloc.type) << "\n";
            } else {
                llvm::dbgs() << "<unknown symbol>: addr = "
                    << llvm::format_hex(reloc.addr, 10)
                    << "; type = " << this->_relocTypeToString(reloc.type) << "\n";
            }
        }
    }

}

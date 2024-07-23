/// \file codegen.cpp
///
/// \copyright 2024 The Fellowship of SML/NJ (https://smlnj.org)
/// All rights reserved.
///
/// SML callable wrapper for the LLVM code generator.  This code is C++, but the
/// exported functions are marked as "C" functions to avoid name mangling.
///
/// \author John Reppy
///

#include "context.hpp"
#include "cfg.hpp"
#include "codegen.h"
#include "cache-flush.h"
#include "target-info.hpp"
#include <iostream>

#include "llvm/Support/TargetSelect.h"

using Context_t = smlnj::cfgcg::Context;
using TargetInfo_t = smlnj::cfgcg::TargetInfo;

/* TODO: we should put the code buffer into the CMachine or the
 *  Context structure.
 */
//! points to a dynamically allocated code buffer; this pointer gets
//! reset if we change the target architecture.
//
static Context_t *gContext = nullptr;

// helper function for setting the target architecture and initializing
// the gContext global
//
static bool _initTarget (TargetInfo_t const *target)
{
    if (gContext != nullptr) {
	if (gContext->targetInfo() == target) {
            // the requested target is the same as the current target
            return false;
	}
        // remove the old code buffer object
	delete gContext;
    } else {
        // initialize LLVM
        target->initialize ();
    }

    gContext = Context_t::create (target);

    return (gContext == nullptr);

}

ml_val_t llvm_codegen (ml_state_t *msp, const char *src, const char *pkl, size_t pklSzb)
{
    if (gContext == nullptr) {
	if (_initTarget(TargetInfo_t::native)) {
/* FIXME: raise a SML exception instead of the fatal error */
	    llvm::report_fatal_error ("initialization failure", true);
	}
    }

    // unpickle the CFG
/* FIXME: using a std::string here probably results in extra data copying */
    asdl::memory_instream inS (std::string (pkl, pklSzb));
    CFG::comp_unit *cu = CFG::comp_unit::read (inS);
    if (cu == nullptr) {
/* FIXME: raise a SML exception instead of the fatal error */
	llvm::report_fatal_error ("unable to unpickle code", true);
    }

    // generate LLVM
    cu->codegen (gContext);

#ifdef VERIFY_LLVM
    if (gContext->verify ()) {
/* FIXME: raise a SML exception instead of the fatal error */
	llvm::report_fatal_error ("LLVM verification error", true);
    }
#endif

    // optimize the LLVM code
    gContext->optimize ();

#ifdef VERIFY_LLVM
    if (gContext->verify ()) {
/* FIXME: raise a SML exception instead of the fatal error */
	llvm::report_fatal_error ("LLVM verification error after optimization", true);
    }
#endif

    // generate the in-memory object file
    auto obj = gContext->compile ();

/* TODO: use arena allocation for the unpickler */
    // deallocate the unpickled CFG IR
    delete cu;

    if (obj) {
#ifdef DUMP_LLVM_INFO
        obj->dump (false);
#endif // DUMP_LLVM_INFO
      // copy the sections to a heap-allocated code object.  At the very end, we add the
      // name of ths source file.  The name string is word-aligned, nul-terminated,
      // and padded to a multiple of the word size.  It is followed by a byte
      // specifying its length in words (if the name is longer than 255*WORD_SZB,
      // then we omit the source file name).  This layout must be consistent with
      // the function BO_GetCodeObjTag in runtime/gc/big-objects.c.
// FIXME: this code is more complicated than necessary, and we could use larger
//   length field specified in bytes, but it is consistent with the MLRisc code in
//   compiler/CodeGen/cpscompile/smlnj-pseudoOps.sml and with the BO_GetCodeObjTag
//   runtime function.

	size_t codeSzb = obj->size();

        // round the code size up to a multiple of the word size
        size_t alignedCodeSzb = gContext->roundToWordSzInBytes (codeSzb);

        // compute the padded size of the source-file name; the computed
	// length includes the nul terminator and the length byte
        size_t srcFileLen = strlen(src) + 2;
        size_t paddedSrcFileLen = gContext->roundToWordSzInBytes (srcFileLen);
        if (paddedSrcFileLen > 255 * gContext->wordSzInBytes()) {
            // if the file name is too long, which is unexpected, omit it
            paddedSrcFileLen = 0;
        }

        // size of code-object with extras
        size_t codeObjSzb = alignedCodeSzb      // code + alignment padding
            + paddedSrcFileLen;                 // src name (including nul and length byte)

        // allocate a heap object for the code
	auto codeObj = ML_AllocCode (msp, codeObjSzb);
	ENABLE_CODE_WRITE
        // copy the code to the heap
	obj->getCode (PTR_MLtoC(unsigned char, codeObj));
        // now add the source-file name to the end of the code object
        char *srcNameLoc = PTR_MLtoC(char, codeObj) + alignedCodeSzb;
        // copy the source-file name; note that `strncpy` pads with zeros
        strncpy (srcNameLoc, src, paddedSrcFileLen);
        // add the length in words at the end
        *reinterpret_cast<unsigned char *>(srcNameLoc + paddedSrcFileLen - 1) =
            (paddedSrcFileLen / gContext->wordSzInBytes());
	DISABLE_CODE_WRITE

        // create a pair of the code object and entry-point offset
	ml_val_t arr, res;
/* FIXME: it appears (from experimentation) that the entry-point offset is always
 * zero, but we should probably be a bit more careful in the final version.
 */
	SEQHDR_ALLOC(msp, arr, DESC_word8arr, codeObj, codeSzb);
	REC_ALLOC2(msp, res, arr, 0);
	return res;
    }
    else {
/* FIXME: raise a SML exception instead of the fatal error */
	llvm::report_fatal_error ("unable to get code object", true);
    }

} /* llvm_setTarget */

/* return a SML `string list` of the names of the supported target architectures
 */
ml_val_t llvm_listTargets (ml_state_t *msp)
{
    auto targets = TargetInfo_t::targetNames();

  // construct a list of the target names
    ml_val_t lst = LIST_nil;
    for (int i = targets.size() - 1;  0 <= i;  --i) {
        ml_val_t name = ML_CString(msp, targets[i].c_str());
        LIST_cons(msp, lst, name, lst);
    }

    return lst;

} /* llvm_setTarget */

/* set the target architecture.  This call returns `true` when there
 * is an error and `false` otherwise.
 */
ml_val_t llvm_setTarget (const char *targetName)
{
    TargetInfo_t const *target;

    if (targetName == nullptr) {
        target = TargetInfo_t::native;
    }
    else {
        target = TargetInfo_t::infoForTarget (targetName);
    }

    if (target == nullptr) {
        return ML_true;
    }

    if (_initTarget(target)) {
        return ML_true;
    } else {
        return ML_false;
    }

} /* llvm_setTarget */

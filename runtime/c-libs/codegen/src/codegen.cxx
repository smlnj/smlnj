/// \file codegen.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Main code generator code.
///
/// \author John Reppy
///

#include "code-buffer.hxx"
#include "cfg.hxx"
#include "codegen.h"
#include "cache-flush.h"
#include "target-info.hxx"
#include <iostream>

#include "llvm/Support/TargetSelect.h"

static code_buffer *CodeBuf = nullptr;

// Some global flags for controlling the code generator.
// These are just for testing purposes
bool disableGC = false;

#if defined(TIME_CODEGEN)
// timer support
#include <time.h>

class Timer {
  public:
    static Timer start ()
    {
	struct timespec ts;
	clock_gettime (CLOCK_REALTIME, &ts);
	return Timer (_cvtTimeSpec(ts));
    }
    void restart ()
    {
	struct timespec ts;
	clock_gettime (CLOCK_REALTIME, &ts);
	this->_ns100 = _cvtTimeSpec(ts);
    }
    double msec () const
    {
	struct timespec ts;
	clock_gettime (CLOCK_REALTIME, &ts);
	double t = double(_cvtTimeSpec(ts) - this->_ns100);
	return t / 10000.0;
    }
  private:
    uint64_t _ns100;	// track time in 100's of nanoseconds
    static uint64_t _cvtTimeSpec (struct timespec &ts)
    {
	return (
	    10000000 * static_cast<uint64_t>(ts.tv_sec)
	    + static_cast<uint64_t>(ts.tv_nsec) / 100);
    }
    Timer (uint64_t t) : _ns100(t) { }
};
#endif // defined(TIME_CODEGEN)


ml_val_t llvm_codegen (ml_state_t *msp, const char *src, const char *pkl, size_t pklSzb)
{
#if defined(TIME_CODEGEN)
    Timer totalTimer = Timer::start();
#endif // defined(TIME_CODEGEN)

#if defined(TIME_CODEGEN)
    Timer initTimer = Timer::start();
#endif // defined(TIME_CODEGEN)
    if (CodeBuf == nullptr) {
	if (llvm_setTarget(nullptr) == ML_true) {
	    llvm::report_fatal_error ("initialization failure", true);
	}
    }
#if defined(TIME_CODEGEN)
    double initT = initTimer.msec();
#endif // defined(TIME_CODEGEN)

  // unpickle the CFG
#if defined(TIME_CODEGEN)
    Timer unpklTimer = Timer::start();
#endif // defined(TIME_CODEGEN)
/* FIXME: using a std::string here probably results in extra data copying */
    asdl::memory_instream inS (std::string (pkl, pklSzb));
    CFG::comp_unit *cu = CFG::comp_unit::read (inS);
    if (cu == nullptr) {
	llvm::report_fatal_error ("unable to unpickle code", true);
    }
#if defined(TIME_CODEGEN)
    double unpklT = unpklTimer.msec();
#endif // defined(TIME_CODEGEN)

  // generate LLVM
#if defined(TIME_CODEGEN)
    Timer genTimer = Timer::start();
#endif // defined(TIME_CODEGEN)
    cu->codegen (CodeBuf);
#if defined(TIME_CODEGEN)
    double genT = genTimer.msec();
#endif // defined(TIME_CODEGEN)

#ifdef VERIFY_LLVM
#if defined(TIME_CODEGEN)
    Timer verifyTimer = Timer::start();
#endif // defined(TIME_CODEGEN)
    if (CodeBuf->verify ()) {
	llvm::report_fatal_error ("LLVM verification error", true);
    }
#if defined(TIME_CODEGEN)
    double verifyT = verifyTimer.msec();
#endif // defined(TIME_CODEGEN)
#else
#if defined(TIME_CODEGEN)
    double verifyT = 0.0;
#endif // defined(TIME_CODEGEN)
#endif

  // optimize the LLVM code
#if defined(TIME_CODEGEN)
    Timer optTimer = Timer::start();
#endif // defined(TIME_CODEGEN)
    CodeBuf->optimize ();
#if defined(TIME_CODEGEN)
    double optT = optTimer.msec();
#endif // defined(TIME_CODEGEN)

#ifdef VERIFY_LLVM
#if defined(TIME_CODEGEN)
    verifyTimer.restart();
#endif // defined(TIME_CODEGEN)
    if (CodeBuf->verify ()) {
	llvm::report_fatal_error ("LLVM verification error after optimization", true);
    }
#if defined(TIME_CODEGEN)
    verifyT += optTimer.msec();
#endif // defined(TIME_CODEGEN)
#endif

  // generate the in-memory object file
#if defined(TIME_CODEGEN)
    Timer objGenTimer = Timer::start();
#endif // defined(TIME_CODEGEN)
    auto obj = CodeBuf->compile ();
#if defined(TIME_CODEGEN)
    double objGenT = objGenTimer.msec();
#endif // defined(TIME_CODEGEN)

/* TODO: use arena allocation for the unpickler */
  // deallocate the unpickled CFG IR
    delete cu;

    if (obj != nullptr) {
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

#if defined(TIME_CODEGEN)
	Timer relocTimer = Timer::start();
#endif // defined(TIME_CODEGEN)

	size_t codeSzb = obj->size();
        // first we round the code size up to a multiple of the word size
        size_t alignedCodeSzb = CodeBuf->roundToWordSzInBytes (codeSzb);
        // compute the padded size of the source-file name; the computed length includes
	// the nul terminator and the length byte
        size_t srcFileLen = strlen(src) + 2;
        size_t paddedSrcFileLen = CodeBuf->roundToWordSzInBytes (srcFileLen);
        if (paddedSrcFileLen > 255 * CodeBuf->wordSzInBytes()) {
            // if the file name is too long, which is unexpected, omit it
            paddedSrcFileLen = 0;
        }
        // size of code-object with extras
        size_t codeObjSzb = alignedCodeSzb      // code + alignment padding
            + paddedSrcFileLen;                 // src name (including nul and length byte)
	auto codeObj = ML_AllocCode (msp, codeObjSzb);
	ENABLE_CODE_WRITE
	obj->getCode (PTR_MLtoC(unsigned char, codeObj));
        // now add the source-file name to the end of the code object
        char *srcNameLoc = PTR_MLtoC(char, codeObj) + alignedCodeSzb;
        // copy the source-file name; note that `strncpy` pads with zeros
        strncpy (srcNameLoc, src, paddedSrcFileLen);
        // add the length in words at the end
        *reinterpret_cast<unsigned char *>(srcNameLoc + paddedSrcFileLen - 1) =
            (paddedSrcFileLen / CodeBuf->wordSzInBytes());
	DISABLE_CODE_WRITE

#if defined(TIME_CODEGEN)
	double relocT = relocTimer.msec();

  /* report stats */
llvm::dbgs() << "\"" << src << "\"," << pklSzb << "," << codeSzb << ","
    << initT << "," << unpklT << "," << genT << "," << optT << "," << verifyT << "," << relocT
    << "," << totalTimer.msec() << "\n";
#endif // defined(TIME_CODEGEN)

      /* create a pair of the code object and entry-point offset */
	ml_val_t arr, res;
    /* FIXME: it appears (from experimentation) that the entry-point offset is always
     * zero, but we should probably be a bit more careful in the final version.
     */
	SEQHDR_ALLOC(msp, arr, DESC_word8arr, codeObj, codeSzb);
	REC_ALLOC2(msp, res, arr, 0);
	return res;
    }
    else {
	llvm::report_fatal_error ("unable to get code object", true);
    }

} /* llvm_setTarget */

/* return a SML `string list` of the names of the supported target architectures
 */
ml_val_t llvm_listTargets (ml_state_t *msp)
{
    auto targets = target_info::targetNames();

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
    target_info const *target;

    if (targetName == nullptr) {
        target = target_info::native;
    }
    else {
        target = target_info::infoForTarget (targetName);
    }

    if (target == nullptr) {
        return ML_true;
    }

    if (CodeBuf != nullptr) {
	if (CodeBuf->targetInfo() == target) {
            // the requested target is the same as the current target
	    return ML_false;
	}
        // remove the old code buffer object
	delete CodeBuf;
    } else {
      // initialize LLVM
        target->initialize ();
    }

    CodeBuf = code_buffer::create (target);

    return ((CodeBuf == nullptr) ? ML_true : ML_false);

} /* llvm_setTarget */

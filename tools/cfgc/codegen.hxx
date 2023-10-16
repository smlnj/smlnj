/// \file codegen.hxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Interface to the code generator library (for testing purposes).
///
/// \author John Reppy
///

#ifndef _CODEGEN_HXX_
#define _CODEGEN_HXX_

#include <string>

enum class output { PrintAsm, AsmFile, ObjFile, Memory };

// set the target architecture.  This call returns `true` when there
// is an error and `false` otherwise.
//
bool setTarget (std::string const &target);

void codegen (std::string const & src, bool emitLLVM, bool dumpBits, output out);

#endif // !_CODEGEN_HXX_

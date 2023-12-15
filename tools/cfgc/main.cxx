/// \file main.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Main test driver for the code generator
///
/// \author John Reppy
///

#include <string>
#include <vector>
#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <cstdarg>

#include "llvm/Support/TargetSelect.h"
#include "llvm/IR/Value.h"

#include "codegen.hxx"

extern "C" {
void Die (const char *fmt, ...)
{
    va_list	ap;

    va_start (ap, fmt);
    fprintf (stderr, "cfgc: Fatal error -- ");
    vfprintf (stderr, fmt, ap);
    fprintf (stderr, "\n");
    va_end(ap);

    ::exit (1);
}
} // extern "C"

[[noreturn]] void usage ()
{
    std::cerr << "usage: cfgc [ -o | -S | -c ] [ --emit-llvm ] [ --bits ] [ --target <target> ] <pkl-file>\n";
    exit (1);
}

int main (int argc, char **argv)
{
    output out = output::PrintAsm;
    bool emitLLVM = false;
    bool dumpBits = false;
    std::string src = "";
    std::string targetArch = HOST_ARCH;

    std::vector<std::string> args(argv+1, argv+argc);

    if (args.empty()) {
	usage();
    }

    for (int i = 0;  i < args.size();  i++) {
	if (args[i][0] == '-') {
	    if (args[i] == "-o") {
		out = output::ObjFile;
	    } else if (args[i] == "-S") {
		out = output::AsmFile;
	    } else if (args[i] == "-c") {
		out = output::Memory;
	    } else if (args[i] == "--emit-llvm") {
		emitLLVM = true;
	    } else if (args[i] == "--bits") {
		dumpBits = true;
	    } else if (args[i] == "--target") {
		i++;
		if (i < args.size()) {
		    targetArch = args[i];
		} else {
		    usage();
		}
	    } else {
		usage();
	    }
	}
	else if (i < args.size()-1) {
            usage();
	}
	else { // last argument
	    src = args[i];
	}
    }
    if (src.empty()) {
        usage();
    }

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    if (setTarget (targetArch)) {
	std::cerr << "codegen: unable to set target to \"" << targetArch << "\"\n";
	return 1;
    }

    codegen (src, emitLLVM, dumpBits, out);

    return 0;

}

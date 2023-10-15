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
#include <iostream>
#include <cstdlib>

#include "llvm/Support/TargetSelect.h"
#include "llvm/IR/Value.h"

#include "codegen.hxx"

extern "C" {
void Die (const char *, ...) { }
} // extern "C"

void usage ()
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

    if (argc < 2) {
	usage();
    }

    for (int i = 1;  i < argc;  i++) {
	if (argv[i][0] == '-') {
	    std::string flag(argv[i]);
	    if (flag == "-o") {
		out = output::ObjFile;
	    } else if (flag == "-S") {
		out = output::AsmFile;
	    } else if (flag == "-c") {
		out = output::Memory;
	    } else if (flag == "--emit-llvm") {
		emitLLVM = true;
	    } else if (flag == "--bits") {
		dumpBits = true;
	    } else if (flag == "--target") {
		i++;
		if (i < argc) {
		    targetArch = argv[i];
		} else {
		    usage();
		}
	    } else {
		usage();
	    }
	}
	else if ((i < argc-1) || (src != "")) {
	    std::cerr << "usage: codegen [ -o | -S | -c ] [ --emit-llvm ] [ --bits ] <pkl-file>\n";
	    exit (1);
	}
	else {
	    src = argv[i];
	}
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

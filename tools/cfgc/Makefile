# Makefile
#
# COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
# All rights reserved.
#
# Makefile for compiling the `cfgc` test driver
#

SHELL =		/bin/sh

SMLNJ_ROOT =	../..

TARGET =	cfgc

# determine the host architecture; we use the LLVM triple
# names, which are different from the names that llvm-config
# reports.
#
UNAME_ARCH :=	$(shell uname -m)
ifeq ($(UNAME_ARCH),x86_64)
HOST_ARCH="x86_64"
else ifeq ($(UNAME_ARCH),arm64)
HOST_ARCH="aarch64"
else
HOST_ARCH=unknown
endif

LLVM_DIR =	$(SMLNJ_ROOT)/llvm
LLVM_CONFIG =	$(LLVM_DIR)/build/bin/llvm-config

LLVM_TARGETS = $(shell $(LLVM_CONFIG) --targets-built)
LLVM_CXX_FLAGS := $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_LD_FLAGS := $(shell $(LLVM_CONFIG) --ldflags)
LLVM_LIBS :=	$(shell $(LLVM_CONFIG) --libs $(LLVM_TARGETS) passes)
#		$(shell $(LLVM_CONFIG) --system-libs)
LLVM_BUILD_MODE := $(shell $(LLVM_CONFIG) --build-mode)

# determine the operating system
#
HOST_OS := $(shell uname -s)
ifeq ($(HOST_OS),Darwin)
TARGET_FLAGS += -DOPSYS_DARWIN
SYS_LIBS = -lz -lcurses
else ifeq ($(HOST_OS),Linux)
TARGET_FLAGS += -DOPSYS_LINUX
SYS_LIBS = -lz -lrt -ldl -ltinfo -lpthread -lm
endif

# determine the supported targets
#
ifneq (,$(findstring AArch64,$(LLVM_TARGETS)))
TARGET_FLAGS +=	-DENABLE_ARM64 -DARCH_ARM64
endif
ifneq (,$(findstring X86,$(LLVM_TARGETS)))
TARGET_FLAGS +=	-DENABLE_X86 -DARCH_AMD64
endif

CODEGEN_INCL =	$(SMLNJ_ROOT)/runtime/c-libs/codegen/include
CXX =		clang++
CPP_FLAGS =	-I$(LLVM_DIR)/src/include -I$(CODEGEN_INCL)
CXX_FLAGS =	-g $(LLVM_CXX_FLAGS)
LD_FLAGS =	$(LLVM_LD_FLAGS) $(SYS_LIBS)

INCLUDES =	$(wildcard $(CODEGEN_INCL)/*hxx) $(wildcard $(CODEGEN_INCL)/asdl/*hxx)
SRCS =		$(wildcard *.cxx)
OBJS =		$(SRCS:%.cxx=%.o)

$(TARGET):	$(OBJS)
	clang++ -o $(TARGET) $(LD_FLAGS) $(OBJS) $(LLVM_LIBS)

%.o: %.cxx $(INCLUDES)
	$(CXX) -c -DHOST_ARCH="\"$(HOST_ARCH)\"" $(CPP_FLAGS) $(CXX_FLAGS) $(TARGET_FLAGS) $<

code-object.o:  code-object.cxx \
                amd64-code-object.inc arm64-code-object.inc \
                $(INCLUDES)
	$(CXX) -c -DHOST_ARCH="\"$(HOST_ARCH)\"" $(CPP_FLAGS) $(CXX_FLAGS) $(TARGET_FLAGS) $<

.PHONY:		install
install:	$(TARGET)
	cp -p $(TARGET) $(SMLNJ_ROOT)/bin/

.PHONY:		clean
clean:
		rm -rf $(OBJS) $(TARGET)

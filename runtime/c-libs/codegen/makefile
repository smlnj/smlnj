#
# the makefile for the LLVM code generator library
#
# COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
# All rights reserved.
#

SHELL =		/bin/sh

RUNTIME_ROOT =	../..
CLIB_DIR =	$(RUNTIME_ROOT)/c-libs
INC_DIR =	$(RUNTIME_ROOT)/include
OBJS_DIR =	$(RUNTIME_ROOT)/objs
CFGCG_DIR =	$(RUNTIME_ROOT)/llvm/smlnj

INCLUDES =	-I$(INC_DIR) -I$(CLIB_DIR) -I$(OBJS_DIR)
CXX_INCLUDES =	-I$(CFGCG_DIR)/include

MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

#
# LLVM configuration
#
LLVM_CONFIG =	$(RUNTIME_ROOT)/bin/llvm-config

TARGETS_BUILT =	$(shell $(LLVM_CONFIG) --targets-built)
ifeq ($(findstring AArch64,$(TARGETS_BUILT)),AArch64)
  ARCH_DEFS +=	-DENABLE_ARM64
endif
ifeq ($(findstring X86,$(TARGETS_BUILT)),X86)
  ARCH_DEFS +=	-DENABLE_X86
endif

LLVM_CXX_FLAGS = $(shell $(LLVM_CONFIG) --cxxflags)
#LLVM_CPP_FLAGS = -Iinclude $(INCLUDES)
LLVM_CPP_FLAGS = -DNDEBUG -Iinclude $(INCLUDES) $(ARCH_DEFS)

LIBRARY =	libcodegen.a

VERSION =	v-dummy

C_SRCS =	codegen-lib.c \
		generate.c \
		list-targets.c \
		set-target.c

CXX_SRCS =	codegen.cpp

OBJS =		$(C_SRCS:%.c=%.o) $(CXX_SRCS:%.cpp=%.o)

$(LIBRARY)	: $(VERSION) $(OBJS)
	rm -rf $(LIBRARY)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS)
	$(RANLIB) $(LIBRARY)

$(VERSION) :
	($(MAKE) MAKE="$(MAKE)" clean)
	echo "$(VERSION)" > $(VERSION)

.c.o:	$(INC_DIR)/ml-unixdep.h $(INC_DIR)/ml-base.h $(INC_DIR)/ml-values.h \
	$(INC_DIR)/ml-objects.h \
	$(CLIB_DIR)/ml-c.h cfun-proto-list.h cfun-list.h
	$(CC) $(CFLAGS) $(DEFS) $(INCLUDES) -Iinclude -c $<

codegen.o:	codegen.cpp codegen.h
	$(CXX) -c $(CXX_FLAGS) $(LLVM_CXX_FLAGS) $(LLVM_CPP_FLAGS) $(DEFS) codegen.cpp

clean :
	rm -f v-* *.o $(LIBRARY)

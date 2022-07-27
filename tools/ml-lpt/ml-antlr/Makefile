# Makefile
#
# COPYRIGHT (c) 2009 The Fellowship of SML/NJ (http://www.smlnj.org)
# All rights reserved.
#

SHELL =			/bin/sh
SML =			sml
ML_BUILD =		ml-build
ML_MAKEDEPEND =		ml-makedepend

# NOTE: the @SMLsuffix option is new with SML/NJ 110.72
#HEAP_SUFFIX = $(shell sml @SMLsuffix)
HEAP_SUFFIX = $(shell echo 'TextIO.output (TextIO.stdErr, SMLofNJ.SysInfo.getHeapSuffix ());' | sml 2>&1 1> /dev/null)

PROGRAM =	ml-antlr
HEAP_IMAGE =	$(PROGRAM).$(HEAP_SUFFIX)

FE = 		FrontEnds
BE = 		BackEnds

SML_SRC = 	$(wildcard $(BE)/SML/*.sml)
LATEX_SRC =     $(wildcard $(BE)/LaTeX/*.sml)

FE_SRC = 	$(wildcard $(FE)/*.sml)  $(FE)/ml-antlr-based/spec.grm $(FE)/ml-antlr-based/spec.lex
BE_SRC = 	$(wildcard $(BE)/*.sml) $(SML_SRC) $(LATEX_SRC)

TEMPLATES =     $(BE)/SML/template.sml $(BE)/LaTeX/template.tex

CORE_SRC =	$(wildcard *.sml)

CM_FILES =	sources.cm ../common/lpt-common.cm

SOURCES =	$(CORE_SRC) $(FE_SRC) $(BE_SRC) $(TEMPLATES) $(CM_FILES)

build:		$(HEAP_IMAGE)

$(HEAP_IMAGE):	$(SOURCES)
	$(ML_BUILD) sources.cm Main.main $(PROGRAM)

.depend:	$(CM_FILES)
	touch .depend
	$(ML_MAKEDEPEND) -n -f .depend sources.cm $(HEAP_IMAGE)

sinclude .depend

clean:
	rm -rf .depend .cm $(HEAP_IMAGE)

devclean:
	rm -rf $(HEAP_IMAGE)
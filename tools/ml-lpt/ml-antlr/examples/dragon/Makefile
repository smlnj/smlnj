#
# COPYRIGHT (c) 20056
# John Reppy (http://www.cs.uchicago.edu/~jhr)
# Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
# All rights reserved.
#

OS =		$(shell uname -s)

ifeq ($(shell uname -s),Darwin)
HEAP_SUFFIX =		ppc-darwin
else
HEAP_SUFFIX =		x86-unix
endif

SHELL =			/bin/sh
SML =			sml
ML_BUILD =		ml-build
ML_MAKEDEPEND =		ml-makedepend

PROGRAM =	dragon
HEAP_IMAGE =	$(PROGRAM).$(HEAP_SUFFIX)

SRC = 	
SOURCES =	$(wildcard *.sml) dragon.l dragon.g.sml sources.cm

build:		dragon.g.sml $(HEAP_IMAGE)

$(HEAP_IMAGE):	$(SOURCES)
	$(ML_BUILD) sources.cm Main.main $(PROGRAM)

.depend:	$(CM_FILES)
	touch .depend
	$(ML_MAKEDEPEND) -n -f .depend sources.cm $(HEAP_IMAGE)

dragon.g.sml:	dragon.g
	ml-antlr dragon.g

sinclude .depend

clean:
	rm -rf .depend .cm dragon.l.sml dragon.g.sml $(HEAP_IMAGE)

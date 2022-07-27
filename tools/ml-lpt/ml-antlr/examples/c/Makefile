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

PROGRAM =	ct
HEAP_IMAGE =	$(PROGRAM).$(HEAP_SUFFIX)

SRC = 	
SOURCES =	$(wildcard *.sml) cc.lex c.grm.sml sources.cm

build:		c.grm.sml $(HEAP_IMAGE)

$(HEAP_IMAGE):	$(SOURCES)
	$(ML_BUILD) sources.cm Main.main $(PROGRAM)

.depend:	$(CM_FILES)
	touch .depend
	$(ML_MAKEDEPEND) -n -f .depend sources.cm $(HEAP_IMAGE)

c.grm.sml:	c.grm
	ml-antlr c.grm

sinclude .depend

clean:
	rm -rf .depend .cm cc.lex.sml c.grm.sml $(HEAP_IMAGE)

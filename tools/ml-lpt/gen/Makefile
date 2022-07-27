# Makefile
#
# COPYRIGHT (c) 2009 The Fellowship of SML/NJ (http://www.smlnj.org)
# All rights reserved.
#

SHELL =		/bin/sh

MLTON =		mlton
MLTON_FLAGS =

TARGET =	gen-template-struct

SOURCES =	gen-template-struct.mlb \
		gen-template-struct.sml \
		mlton-main.sml

$(TARGET):	$(SOURCES)
	$(MLTON) -output $(TARGET) $(MLTON_FLAGS) $(TARGET).mlb

.PHONEY:	clean
clean:
	rm -f $(TARGET)


FILES = various.c
H = LibH.libh
D = FFI
HF = ../libh.sml
CF = various.cm

$(D)/$(CF): $(FILES)
	$(SMLNJ_BINDIR)/ml-nlffigen $(CPPO) -include $(HF) -libhandle $(H) -dir $(D) -cmfile $(CF) $^

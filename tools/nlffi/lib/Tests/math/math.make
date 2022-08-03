FILES = math.h
H = LibH.libh
D = FFI
HF = ../libh.sml
CF = math.cm

$(D)/$(CF): $(FILES)
	$(SMLNJ_BINDIR)/ml-nlffigen -include $(HF) -libhandle $(H) -dir $(D) -cmfile $(CF) $^

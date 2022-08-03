FILES = ll.h
H = LLHandle.ll
D = FFI
HF = ../llhandle.sml
CF = ll.cm
CPPO =

$(D)/$(CF): $(FILES)
	$(SMLNJ_BINDIR)/ml-nlffigen $(CPPO) -heavy -include $(HF) -libhandle $(H) -dir $(D) -cmfile $(CF) $^

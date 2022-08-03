FILES = pdb.c forward.c
H = PDBHandle.pdb
D = FFI
HF = ../pdbhandle.sml
CF = pdb.cm
CPPO = -D__builtin_va_list=int

$(D)/$(CF): $(FILES)
	$(SMLNJ_BINDIR)/ml-nlffigen $(CPPO) -heavy -include $(HF) -libhandle $(H) -dir $(D) -cmfile $(CF) $^

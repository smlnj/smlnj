FFI/printf.cm: printf.h
	$(SMLNJ_BINDIR)/ml-nlffigen -include ../libh.sml -libhandle LibH.libh -dir FFI -cmfile printf.cm printf.h

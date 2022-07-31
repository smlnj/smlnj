arch_n_opsys := $(shell ../../bin/.arch-n-opsys)
ARCH := $(shell eval "$(arch_n_opsys)"; echo -n $$ARCH)
HEAP_SUFFIX := $(shell eval "$(arch_n_opsys)"; echo -n $$HEAP_SUFFIX)
BINDIR_SUFFIX=$(ARCH)-unix

# don't ask me why bindir_suffix <> heap_suffix
SML=sml.$(HEAP_SUFFIX)
BINDIR=bin.$(BINDIR_SUFFIX)
MLCMD=CMB.make()

default: sml
sml: $(SML)
sml-new: sml-new.$(HEAP_SUFFIX)

$(BINDIR).new:
	if [ -d $(BINDIR) ]; then \
	    mv $(BINDIR) $(BINDIR).new && mv $(BINDIR).old $(BINDIR) ;\
	else \
	    mkdir $(BINDIR).new ;\
	fi

$(SML): $(BINDIR)/COMPLIST
	-rm -f $@
	./xmakeml
	[ -r $(SML) ]

$(BINDIR)/COMPLIST: $(BINDIR).new

cleannew: $(BINDIR).new
	-rm -r $(BINDIR).new/*

recompile: $(SML)
	mv $(BINDIR) $(BINDIR).old && mv $(BINDIR).new $(BINDIR)
	-echo '$(MLCMD); Compiler.Stats.summary();' | ./xrun sml
	mv $(BINDIR) $(BINDIR).new && mv $(BINDIR).old $(BINDIR)

sml-new.$(HEAP_SUFFIX): recompile
	mv $(BINDIR) $(BINDIR).old && mv $(BINDIR).new $(BINDIR)
	if [ -f $(BINDIR)/COMPLIST ]; then \
	    mv $(SML) sml-old.$(HEAP_SUFFIX) &&\
	    ./xmakeml &&\
	    mv $(SML) sml-new.$(HEAP_SUFFIX) &&\
	    mv sml-old.$(HEAP_SUFFIX) $(SML) ;\
	fi
	mv $(BINDIR) $(BINDIR).new && mv $(BINDIR).old $(BINDIR)

run: $(SML)
	mv $(BINDIR) $(BINDIR).old && mv $(BINDIR).new $(BINDIR)
	./xrun $(SML)
	mv $(BINDIR) $(BINDIR).new && mv $(BINDIR).old $(BINDIR)

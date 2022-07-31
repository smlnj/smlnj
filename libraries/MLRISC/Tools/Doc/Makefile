MAINTEX=	rewrite-gen	nowhere
TEX=	$(MAINTEX:=.tex)
DVI=	$(TEX:.tex=.dvi)
PS=	$(TEX:.tex=.ps)
all:	$(DVI) $(PS)

bib:	
	for f in $(MAINTEX) ; do bibtex $$f; done

again:  bib
	rm -f $(MAINTEX:=.dvi)
	make

%.dvi:	%.tex
	latex $(@:.dvi=.tex)
%.ps:	%.dvi
	dvips -D 600 -t letter $(@:.ps=.dvi) -o
%4.ps:	%.ps
	./4up -p 2:4:1:3 <$< >$@


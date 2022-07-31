#
# This file compiles the documentation 
#

TEX= sml.tex mlrisc.tex mltexdoc.tex
PDF=	$(TEX:.tex=.pdf)

all:	pictures $(PDF)

pictures:
	(cd ../pictures; make)

pdf:	$(PDF)

%.pdf:
	latexmk -bibtex -ps- -pdf $(@:.pdf=.tex)

mlrisc.pdf: annotations.tex graphics.tex instructions.tex mltree.tex \
	delayslots.tex cells.tex cluster.tex constants.tex \
	pseudo-ops.tex streams.tex labelexp.tex labels.tex regions.tex \
	regmap.tex graphs.tex mlrisc-ir.tex compiler-graphs.tex \
	SSA.tex VLIW.tex ra.tex ILP.tex mlrisc-arch.tex \
	future-work.tex asm.tex mc.tex instrsel.tex \
	availability.tex gc.tex mlrisc-gen.tex contributions.tex \
	systems.tex mlrisc-graphics.tex contributors.tex \
	requirements.tex INTRO.tex problem.tex mlrisc-compiler.tex \
	mlrisc-ir-rep.tex backend-opt.tex sys-integration.tex \
	optimizations.tex span-dep.tex mlrisc-md.tex line-counts.tex \
	sparc.tex alpha.tex x86.tex ppc.tex mips.tex hppa.tex C6.tex \
	mltree-ext.tex mltree-util.tex

mltexdoc.pdf:	mltex.tex

clean:
	rm -f $(PDF) *.aux *.log *.bbl *.blg

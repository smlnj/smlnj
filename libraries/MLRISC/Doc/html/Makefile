#
# This file compiles the documentation 
#

HTML=	mlrisc.html mltexdoc.html


TEX=    ../latex/annotations.tex \
	../latex/graphics.tex \
	../latex/instructions.tex \
	../latex/mltree.tex \
	../latex/delayslots.tex \
	../latex/cells.tex \
	../latex/cluster.tex \
	../latex/constants.tex \
	../latex/pseudo-ops.tex \
	../latex/streams.tex \
	../latex/labelexp.tex \
	../latex/labels.tex \
	../latex/regions.tex \
	../latex/regmap.tex \
	../latex/graphs.tex \
	../latex/mlrisc-ir.tex \
	../latex/compiler-graphs.tex \
	../latex/SSA.tex \
	../latex/VLIW.tex \
	../latex/ra.tex \
	../latex/ILP.tex \
	../latex/mlrisc-arch.tex \
	../latex/future-work.tex \
	../latex/asm.tex \
	../latex/mc.tex \
	../latex/span-dep.tex \
	../latex/instrsel.tex \
	../latex/availability.tex \
	../latex/gc.tex \
	../latex/mlrisc-gen.tex \
	../latex/contributions.tex \
	../latex/systems.tex \
	../latex/mlrisc-graphics.tex \
	../latex/contributors.tex \
	../latex/requirements.tex \
	../latex/INTRO.tex \
	../latex/problem.tex \
	../latex/mlrisc-compiler.tex \
	../latex/mlrisc-ir-rep.tex \
	../latex/backend-opt.tex \
	../latex/sys-integration.tex \
	../latex/optimizations.tex \
	../latex/span-dep.tex \
	../latex/mlrisc-md.tex \
	../latex/mlrisc-ra.tex \
	../latex/line-counts.tex \
	../latex/sparc.tex \
	../latex/alpha.tex \
	../latex/x86.tex \
	../latex/ppc.tex \
	../latex/mips.tex \
	../latex/hppa.tex \
	../latex/C6.tex \
	../latex/mltree-ext.tex \
	../latex/mltree-util.tex 

all:	pictures $(HTML) makelinks

pictures:
	(cd ../pictures; make)

cvsrm:
	cvsrm $(HTML)

clean:
	/bin/rm -f *.html

makelinks:
	@if [ ! -d graphics ]; then ln -f -s ../graphics .; fi
	@if [ ! -d pictures ]; then ln -f -s ../pictures .; fi
	@if [ ! -f index.html ]; then ln -f -s INTRO.html index.html ; fi



%.html:	../latex/%.tex mltex2html mltex.thm
	perl mltex2html ../latex/$(@:.html=.tex)

mlrisc.html:	$(TEX) 
mltexdoc.html:	../latex/mltex.tex

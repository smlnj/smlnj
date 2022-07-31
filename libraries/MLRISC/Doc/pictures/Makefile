EPS=	eps/big-pict.eps \
	eps/big-pict2.eps \
	eps/cfg-rev.eps \
	eps/cfg.eps \
	eps/compiler-2.eps \
	eps/compiler.eps \
	eps/compiler2.eps \
	eps/hyperblock-formation.eps \
	eps/if-conversion.eps \
	eps/layers.eps \
	eps/mlrisc-ir-archive.eps \
	eps/mlrisc-IR.eps \
	eps/phg.eps \
	eps/region-builder.eps \
	eps/subgraph.eps \
	eps/tail-dupl.eps \
	eps/tail-duplication.eps \
	eps/trace.eps \
	eps/uncol.eps \
	eps/viewer.eps

PDF=	pdf/big-pict.pdf \
	pdf/big-pict2.pdf \
	pdf/cfg-rev.pdf \
	pdf/cfg.pdf \
	pdf/compiler-2.pdf \
	pdf/compiler.pdf \
	pdf/compiler2.pdf \
	pdf/hyperblock-formation.pdf \
	pdf/if-conversion.pdf \
	pdf/layers.pdf \
	pdf/mlrisc-ir-archive.pdf \
	pdf/mlrisc-IR.pdf \
	pdf/phg.pdf \
	pdf/region-builder.pdf \
	pdf/subgraph.pdf \
	pdf/tail-dupl.pdf \
	pdf/tail-duplication.pdf \
	pdf/trace.pdf \
	pdf/uncol.pdf \
	pdf/viewer.pdf

PNG=	png/compiler-1.png \
	png/compiler-2.png \
	png/hof-1.png \
	png/hof-2.png \
	png/mlrisc-ir-archive.png \
	png/optimization.png \
	png/sharing1.png \
	png/sharing2.png \
	png/sharing3.png \
	png/uncol.png \
	png/uncol1.png \
	png/uncol2.png


all:	mkdir $(PNG) $(PDF)

clean:	
	rm -f $(EPS) $(PNG) $(PDF)

mkdir:
	# @if [ ! -d eps ]; then mkdir eps; fi
	@if [ ! -d pdf ]; then mkdir pdf; fi
	@if [ ! -d png ]; then mkdir png; fi

eps/%.eps:	fig/%.fig
	fig2dev -L ps $< $@
pdf/%.pdf:	fig/%.fig
	fig2dev -L pdf $< $@
jpeg/%.jpg:	fig/%.fig
	fig2dev -L jpeg $< $@
png/%.png:	fig/%.fig
	fig2dev -L png $< $@

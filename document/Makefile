TEX=doc.tex
BIB=refs.bib
PDF=$(TEX:.tex=.pdf)

###################################################

all:  ${PDF}

${PDF}: ${TEX} ${BIB}
	pdflatex  ${TEX}
	bibtex `basename -s .tex ${TEX}`
	pdflatex  ${TEX}
	pdflatex  ${TEX}

clean:
	-rm -f ${PDF} *.bak *.blg *.bbl *.out *.aux *~ *.log

distclean:
	-rm -rf *.pdf *.ps *~ *.log *.bak *.out *.aux *.tar *.tar.gz *.dvi 

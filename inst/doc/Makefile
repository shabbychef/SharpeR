# stolen from yihui/knitr:
# https://github.com/yihui/knitr/blob/master/inst/doc/Makefile
#
# see also: https://github.com/lgatto/rols/tree/master/vignettes 
#
# Makefile to use knitr for package vignettes

BIBTEX      := $(shell which bibtex)

# put all PDF targets here, separated by spaces
# PDFS = knitr-intro.pdf knitr-refcard.pdf knitr-markdown.html
TARGETS = SharpeR.pdf 

all: $(TARGETS)

R_HOME    ?= /usr

clean:
	echo "clean is called" >> /tmp/vig.log
	rm -rf *.tex *.bbl *.blg *.aux *.out *.log *.spl *tikzDictionary *.md figure/

%.pdf: %.Rnw
	echo "pdf build called" >> /tmp/vig.log
	$(R_HOME)/bin/Rscript -e "if (getRversion() < '3.0.0') knitr::knit2pdf('$*.Rnw') else tools::texi2pdf('$*.tex')"
	if grep Citation $*.log > /dev/null; then $(BIBTEX) $*; \
		"$(R_HOME)/bin/R" CMD pdflatex $*.tex; fi
	if grep Rerun $*.log > /dev/null; then "$(R_HOME)/bin/R" CMD pdflatex $*.tex; fi

%.html: %.Rmd
	echo "html build called" >> /tmp/vig.log
	$(R_HOME)/bin/Rscript -e "if (getRversion() < '3.0.0') knitr::knit2html('$*.Rmd')"



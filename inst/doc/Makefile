# stolen from yihui/knitr:
# https://github.com/yihui/knitr/blob/master/inst/doc/Makefile
#
# see also: https://github.com/lgatto/rols/tree/master/vignettes 
#
# Makefile to use knitr for package vignettes

# put all PDF targets here, separated by spaces
# PDFS = knitr-intro.pdf knitr-refcard.pdf knitr-markdown.html
TARGETS = SharpeR.pdf 

all: $(TARGETS)

R_HOME    ?= /usr

clean:
	rm -rf *.tex *.bbl *.blg *.aux *.out *.log *.spl *tikzDictionary *.md figure/

%.pdf: %.Rnw
	$(R_HOME)/bin/Rscript -e "if (getRversion() < '3.0.0') knitr::knit2pdf('$*.Rnw') else tools::texi2pdf('$*.tex')"

%.html: %.Rmd
	$(R_HOME)/bin/Rscript -e "if (getRversion() < '3.0.0') knitr::knit2html('$*.Rmd')"



# 
# * Fri Dec 28 2012 04:15:55 PM Steven E. Pav <steven@cerebellumcapital.com>
#
# Makefile 'remixed' from RTikZDevice and optmatch packages. HT to Sharpie and
# markmfredrickson.
#
# you may have too
# sudo apt-get install -y texinfo
#
#
# Created: 2012.12.28
#

R_DEV_FILES 		?= $(wildcard ./R/*.r)
R_FILES 				?= $(R_DEV_FILES)
R_FILES 				+= $(wildcard ./inst/tests/*.r)
R_FILES 				+= $(wildcard ./man-roxygen/*.R)
R_FILES 				+= $(wildcard ./tests/*.R)

M4_FILES				?= $(wildcard *.m4)

VERSION 				 = 0.1305
TODAY 					:= $(shell date +%Y-%m-%d)

PKG_NAME 				:= SharpeR
PKG_VERSION			:= $(VERSION)
PKG_SRC 				:= $(shell basename $(PWD))

PKG_TGZ 				 = $(PKG_NAME)_$(PKG_VERSION).tar.gz

LOCAL 					:= .local
STAGING 				:= .staging
STAGED_PKG 			 = $(STAGING)/$(PKG_NAME)
RCHECK 					 = $(PKG_NAME).Rcheck
RCHECK_SENTINEL  = $(RCHECK)/$(PKG_NAME)/DESCRIPTION

# Specify the directory holding R binaries. To use an alternate R build (say a
# pre-prelease version) use `make RBIN=/path/to/other/R/` or `export RBIN=...`
# If no alternate bin folder is specified, the default is to use the folder
# containing the first instance of R on the PATH.
RBIN 						?= $(shell dirname "`which R`")
R         			 = $(RBIN)/R
RSCRIPT   			 = $(RBIN)/Rscript

# packages I need to test this one
TEST_DEPS  			 = testthat roxygen2 knitr TTR quantmod MASS
INSTALLED_DEPS 	 = $(patsubst %,$(LOCAL)/%/DESCRIPTION,$(TEST_DEPS)) 
PKG_TESTR 			 = tests/run-all.R

#INSTALLED_DEPS 	 = $(patsubst %,$(LOCAL)/%,$(TEST_DEPS)) 

# do not distribute these!
NODIST_FILES		 = ./Makefile $(M4_FILES) rebuildTags.sh .gitignore .gitattributes .tags .R_tags
NODIST_DIRS			 = .git man-roxygen

RD_DUMMY 				 = man/$(PKG_NAME).Rd

SUPPORT_FILES 	 = ./DESCRIPTION ./NAMESPACE $(RD_DUMMY) ./inst/doc/$(PKG_NAME).Rnw

# for R CMD build
#BUILD_FLAGS 		?= --no-vignettes
BUILD_FLAGS 		?= 

# latex bother. bleah.
#TEXINPADD    = .:$(HOME)/sys/etc/tex:$(HOME)/sys/etc/tex/SEPtex:$(HOME)/work/math/TEX
TEXINPADD    = .:./inst/doc
PRETEX       = TEXINPUTS=$(TEXINPADD):$$TEXINPUTS
PREBIB       = BSTINPUTS=$(TEXINPADD):$$BSTINPUTS \
               BIBINPUTS=$(TEXINPADD):$$BIBINPUTS 
BIBTEX      := $(shell which bibtex)

#FAST_

#########################################################################
# MACROS
#########################################################################

# install locally
INSTALLPKG = $(RLOCAL) -e "install.packages('$(1)', repos = 'http://cran.cnr.Berkeley.edu')" 
	
# make a directory
MKDIR = mkdir -p $(1)

# warn new deps
#WARN_DEPS = $(warning newer deps are $(?))
WARN_DEPS = $(warning will build $@ ; newer deps are $(?))

#########################################################################
# TARGETS
#########################################################################

# these are phony targets
.PHONY: help tags all \
	gitpull gitpush \
	news docs build install testthat tests \
	staging_d local_d \
	clean realclean \
	vignette \
	R

help:
	@echo "\nTasks for $(PKG_NAME)\n"
	@echo "Usage: \`make <task>\` where <task> is one of:"
	@echo ""
	@echo "Development Tasks"
	@echo "-----------------"
	@echo "  tags       Build the ctags, for dev purposes"
	@echo "  deps       Install dependencies for package development"
	@echo "  docs       Invoke roxygen to generate Rd files in man/"
	@echo "  testthat   Run unit tests."
	@echo '  tests       "   "     "   '
	@echo "  parallel   Create a staging version of this package."
	@echo "  build      Invoke docs and then create a package."
	@echo "  check      Invoke build and then check the package."
	@echo "  install    Invoke build and then install the result."
	@echo "  R          Invoke R in a local context with the package."
	@echo "  vignette   Build the vignette in the local context."
	@echo "  clean      Do some cleanup."
	@echo "  realclean  Do lots of cleanup."
	@echo ""
	@echo "Packaging Tasks"
	@echo "---------------"
	@echo "  gitpush    Yes, I am lazy"
	@echo ""
	@echo "Using R in: $(RBIN)"
	@echo "Set the RBIN environment variable to change this."
	@echo ""

# dev stuff
~/.ctags :
	@-echo -E '--langdef=R' >> $@
	@-echo -E '--langmap=R:.s.S.R.r.q' >> $@
	@-echo -E '--regex-R=/^[ \t]+"?([.A-Za-z][.A-Za-z0-9_]*)"?[\t]*<-[\t]*function/\1/' >> $@
	@-echo -E '--regex-R=/^"?([.A-Za-z][.A-Za-z0-9_]*)"?[ \t]*<-/\1/' >> $@

.R_tags: $(R_FILES)
	./rebuildTags.sh

tags: .R_tags

# if you use emacs (shudder)
TAGS: 
	$(R) --slave CMD rtags

DESCRIPTION : DESCRIPTION.m4 Makefile
	m4 -DVERSION=$(VERSION) -DDATE=$(TODAY) -DPKG_NAME=$(PKG_NAME) $< > $@

# macro for local R
RLOCAL = R_LIBS=$(LOCAL) $(R) --vanilla 

# make directories
local_d :
	$(call MKDIR,$(LOCAL))

staging_d :
	$(call MKDIR,$(STAGING))

# debugging
echo :
	@-echo $(R_FILES)

# install an R package in the 'LOCAL' directory.
$(LOCAL)/%/DESCRIPTION : 
	$(call MKDIR,$(LOCAL))
	$(RLOCAL) -e "install.packages('$*', repos = 'http://cran.cnr.Berkeley.edu')" 

deps: $(INSTALLED_DEPS)

# roxygen it.
man/$(PKG_NAME).Rd NAMESPACE: $(R_FILES)
	$(call WARN_DEPS)
	$(RLOCAL) --slave -e "require(roxygen2); roxygenize('.', '.', overwrite=TRUE, unlink.target=TRUE)"
	touch $@

docs: DESCRIPTION man/$(PKG_NAME).Rd 

#RSYNC_FLAGS     = -av
#RSYNC_FLAGS     = -vrlpgoD --delete
RSYNC_FLAGS     = -av --delete 

# a parallel version of this package, but without the support structure
$(STAGED_PKG)/DESCRIPTION : $(R_FILES) $(SUPPORT_FILES) 
	$(call WARN_DEPS)
	$(call MKDIR,$(STAGED_PKG))
	rsync $(RSYNC_FLAGS) \
		--include=man/ --include=man/* \
		--include=NAMESPACE --include=DESCRIPTION \
		--exclude-from=.gitignore \
		$(patsubst %,--exclude=%,$(NODIST_FILES)) \
		$(patsubst %,--exclude=%,$(NODIST_DIRS)) \
		--exclude=$(LOCAL) --exclude=$(STAGING) --exclude=$(RCHECK) \
		. $(@D)
	touch $@

parallel : $(STAGED_PKG)/DESCRIPTION

# make the 'package', which is a tar.gz
$(PKG_TGZ) : $(STAGED_PKG)/DESCRIPTION $(INSTALLED_DEPS)
	$(RLOCAL) CMD build $(BUILD_FLAGS) $(<D)

#package : $(PKG_TGZ)

build : $(PKG_TGZ)

# an 'install'
$(LOCAL)/$(PKG_NAME)/INDEX : $(PKG_TGZ) 
	$(call WARN_DEPS)
	$(call MKDIR,$(LOCAL))
	$(RLOCAL) CMD INSTALL --preclean --no-multiarch --library=$(LOCAL) $<
	touch $@

install: $(LOCAL)/$(PKG_NAME)/INDEX

# check and install
$(RCHECK_SENTINEL) : $(PKG_TGZ)
	$(call WARN_DEPS)
	$(RLOCAL) CMD check --as-cran --outdir=$@ $^ 
	
check: $(RCHECK_SENTINEL)

checksee : $(RCHECK_SENTINEL)
	okular $(RCHECK)/$(PKG_NAME)-manual.pdf

$(RCHECK)/$(PKG_NAME)/doc/$(PKG_NAME).pdf : inst/doc/$(PKG_NAME).Rnw $(RCHECK_SENTINEL)

slow_vignette : $(RCHECK)/$(PKG_NAME)/doc/$(PKG_NAME).pdf

################################
# UNIT TESTING
################################

#$(RLOCAL) --slave -e "if (require(testthat) && require($(PKG_NAME))) testthat::test_dir('./inst/tests')" | tee $@

# 2FIX:
unit_test.log : $(LOCAL)/$(PKG_NAME)/INDEX $(LOCAL)/testthat/DESCRIPTION $(PKG_TESTR)
	$(call WARN_DEPS)
	R_LIBS=$(LOCAL) R_PROFILE=load.R \
				 R_DEFAULT_PACKAGES="utils,graphics,grDevices,methods,stats,$(PKG_NAME)" R -q --no-save \
				 --slave < $(PKG_TESTR) | tee $@

testthat : unit_test.log

tests    : unit_test.log

# drop into R shell in the 'local context'
R : deps $(LOCAL)/$(PKG_NAME)/INDEX
	R_LIBS=$(LOCAL) R_PROFILE=load.R \
				 R_DEFAULT_PACKAGES="utils,graphics,grDevices,methods,stats,$(PKG_NAME)" R -q --no-save

$(PKG_NAME).pdf: inst/doc/$(PKG_NAME).Rnw deps $(LOCAL)/$(PKG_NAME)/INDEX 
	$(PRETEX) R_LIBS=$(LOCAL) R_PROFILE=load.R \
				 R_DEFAULT_PACKAGES="utils,graphics,grDevices,methods,stats,knitr,TTR,$(PKG_NAME)" \
				 R -q --no-save --slave -e "knitr::knit2pdf('$<');"
	if grep Citation $(PKG_NAME).log > /dev/null; then $(PREBIB) $(BIBTEX) $(PKG_NAME); \
		$(PRETEX) "$(R)" CMD pdflatex $(PKG_NAME).tex; fi
	if grep Rerun $(PKG_NAME).log > /dev/null; then $(PRETEX) "$(R)" CMD pdflatex $(PKG_NAME).tex; fi

vignette: $(PKG_NAME).pdf

################################
# CLEAN UP 
################################

clean : 
	-rm DESCRIPTION
	-rm -rf man/*.Rd
	-rm -rf $(STAGED_PKG)
	-rm -rf $(RCHECK)
	-rm -rf $(PKG_NAME).tex
	-rm -rf $(PKG_NAME).log
	-rm -rf $(PKG_NAME).aux
	-rm -rf $(PKG_NAME).pdf
	-rm -rf $(PKG_NAME).bbl
	-rm -rf $(PKG_NAME).blg

realclean : clean
	-rm -rf $(LOCAL)
	-rm -rf ./cache

################################
# git FOO 
################################

gitpush :
	git push origin master

gitpull :
	git pull origin master

tag :
	echo "git tag -a r$(VERSION) -m 'release $(VERSION)'"
	echo "git push --tags"

################################
# CRAN SUBMISSION
################################

# FTP junk
~/.netrc :
	echo -e "machine cran.r-project.org login anonymous password anonymous macdef init\ncd incoming\n\n" > $@

.cran_upload : $(PKG_TGZ)
	@-read -p 'really upload? [y/n] ' -n 1 yorn ; \
	[[ "$$yorn" == "y" ]] && echo -e "user anonymous anonymous\nbinary\ncd incoming\nput $(PKG_TGZ)\nls\nbye\n" | ftp -n -v cran.r-project.org

.send_email : 
	@-read -p 'really send email? [y/n] ' -n 1 yorn ; \
	[[ "$$yorn" == "y" ]] && echo "automatic message" | mail -s "CRAN submission $(PKG_NAME) $(VERSION)" CRAN@R-project.org

submit : .cran_upload .send_email

subadvice :
	@-echo -e "upload $(PKG_TGZ) to cran.r-project.org/incoming via anonymous ftp"
	@-echo -e "then email CRAN@R-project.org w/ subject 'CRAN submission $(PKG_NAME) $(VERSION)'"

#vignette:
#cd inst/doc;\
#$(R) CMD Sweave $(PKG_NAME).Rnw;\
#texi2dvi --pdf $(PKG_NAME).tex;\
#$(R) --vanilla --slave -e "tools:::compactPDF(getwd(), gs_quality='printer')"

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:tags=tags;:syntax=make:filetype=make:ai:si:cin:nu:fo=croqt:cino=p0t0c5(0:

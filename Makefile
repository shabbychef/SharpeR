# 
# * Fri Dec 28 2012 04:15:55 PM Steven E. Pav <steven@cerebellumcapital.com>
#
# stolen shamelessly from RTikZDevice/
#
# 2FIX: steal shamelessly from optmatch:
# https://github.com/markmfredrickson/optmatch/blob/master/Makefile
#
# 2FIX: add actual dependencies.
#
# Created: 2012.12.28
#

R_FILES 				?= $(wildcard ./R/*.r)
M4_FILES				?= $(wildcard *.m4)
RD_FILES				?= $(wildcard man/*.Rd)

VERSION 				 = 0.1303
TODAY 					:= $(shell date +%Y-%m-%d)


PKG_NAME 				:= SharpeR
PKG_VERSION			:= $(VERSION)
PKG_SRC 				:= $(shell basename $(PWD))

PKG_TGZ 				 = $(PKG_NAME)_$(PKG_VERSION).tar.gz

LOCAL 					:= ../.local
STAGING 				:= ../.staging
STAGED_PKG 			 = $(STAGING)/$(PKG_NAME)

# Specify the directory holding R binaries. To use an alternate R build (say a
# pre-prelease version) use `make RBIN=/path/to/other/R/` or `export RBIN=...`
# If no alternate bin folder is specified, the default is to use the folder
# containing the first instance of R on the PATH.
RBIN 						?= $(shell dirname "`which R`")
R         			 = $(RBIN)/R
RSCRIPT   			 = $(RBIN)/Rscript

# packages I need to test this one
TEST_DEPS  			 = testthat roxygen2

INSTALLED_DEPS 	 = $(patsubst %,$(LOCAL)/%,$(TEST_DEPS)) 

# do not distribute these!
NODIST_FILES		 = Makefile $(M4_FILES) rebuildTags.sh .gitignore .gitattributes .tags .R_tags
NODIST_DIRS			 = .git

SUPPORT_FILES 	 = ./DESCRIPTION ./NAMESPACE

#
.PHONY: help news tags doc build install gitpush gitpull testthat staging_d local_d

# dev stuff
.R_tags: $(R_FILES)
	./rebuildTags.sh

tags: .R_tags

DESCRIPTION : DESCRIPTION.m4
	m4 -DVERSION=$(VERSION) -DDATE=$(TODAY) $< > $@

# macro for local R
RLOCAL = R_LIBS=$(LOCAL) $(R) --vanilla 

local_d :
	mkdir -p $(LOCAL)

staging_d :
	mkdir -p $(STAGING)

echo :
	@-echo $(R_FILES)

RSYNC_FLAGS     = -av
RSYNC_FLAGS     = -vrlpgoD

$(STAGED_PKG)/DESCRIPTION : $(R_FILES) $(SUPPORT_FILES) 
	$(warning newer deps are $(?F) ?)
	rsync $(RSYNC_FLAGS) --exclude-from=.gitignore $(patsubst %,--exclude=%,$(NODIST_FILES)) \
	$(patsubst %,--exclude=%,$(NODIST_DIRS)) \
	. $(@D)

parallel : $(STAGED_PKG)/DESCRIPTION

# install locally
INSTALLPKG = $(RLOCAL) -e "install.packages('$(1)', repos = 'http://cran.cnr.Berkeley.edu')" 

$(LOCAL)/$(PKG_NAME) : $(PKG_TGZ) local_d
	$(RLOCAL) CMD INSTALL --no-multiarch --library=$(LOCAL) $<

$(LOCAL)/%/DESCRIPTION : local_d
	$(call INSTALLPKG,$(shell basename `dirname $@`))

#$(call INSTALLPKG,$(@F))

deps: $(INSTALLED_DEPS)
	
package : $(PKG_TGZ)

$(PKG_TGZ) : $(STAGED_PKG)/DESCRIPTION
	$(RLOCAL) CMD build $(<D)

#$(RD_FILES) : $(R_FILES) 

man/$(PKG_NAME).Rd : $(R_FILES)
	cd .. ; \
		$(RLOCAL) --slave -e "require(roxygen2); roxygenize('$(PKG_SRC)', '$(PKG_SRC)', overwrite=TRUE, unlink.target=TRUE)"

NAMESPACE : $(R_FILES)
	cd .. ; \
		$(RLOCAL) --slave -e "require(roxygen2); roxygenize('$(PKG_SRC)', '$(PKG_SRC)', overwrite=TRUE, unlink.target=TRUE)"

#docs: $(RD_FILES)

docs: man/$(PKG_NAME).Rd

#build: docs tags
#cd ..;\
#$(R) CMD build --no-vignettes $(PKG_SRC).build

install: $(PKG_TGZ)
	cd ..;\
		$(R) CMD INSTALL $(PKG_TGZ)

check: $(PKG_TGZ)
	cd ..;\
		$(R) CMD check --as-cran $(PKG_TGZ)

# I am lazy.
gitpush :
	git push origin master

gitpull :
	git pull origin master

# drop into R shell in the 'local context'
R : deps $(LOCAL)/$(PKG_NAME)
	R_LIBS=$(LOCAL) R_PROFILE=load.R R -q --no-save








help:
	@echo "\nExecute development tasks for $(PKG_NAME)\n"
	@echo "Usage: \`make <task>\` where <task> is one of:"
	@echo ""
	@echo "Development Tasks"
	@echo "-----------------"
	@echo "  tags       Build the ctags, for dev purposes"
	@echo "  deps       Install dependencies for package development"
	@echo "  docs       Invoke roxygen to generate Rd files in a seperate"
	@echo "             directory"
	@echo "  news       Create NEWS.Rd and NEWS.pdf from NEWS.md. 2FIX!"
	@echo "  vignette   Build a copy of the package vignette"
	@echo "  testthat   Run unit tests."
	@echo "  build      Invoke docs and then create a package"
	@echo "  check      Invoke build and then check the package"
	@echo "  install    Invoke build and then install the result"
	@echo "  test       Install a new copy of the package and run it "
	@echo "             through the testsuite"
	@echo ""
	@echo "Packaging Tasks"
	@echo "---------------"
	@echo "  gitpush    Yes, I am lazy"
	@echo "  release    Populate a release branch"
	@echo ""
	@echo "Using R in: $(RBIN)"
	@echo "Set the RBIN environment variable to change this."
	@echo ""


#vignette:
	#cd inst/doc;\
		#$(R) CMD Sweave $(PKG_NAME).Rnw;\
		#texi2dvi --pdf $(PKG_NAME).tex;\
		#$(R) --vanilla --slave -e "tools:::compactPDF(getwd(), gs_quality='printer')"

testthat : unit_test.log

unit_test.log :
	$(Rscript) -e "if (require(testthat)) testthat::test_dir('./inst/tests')" | tee $@

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:tags=tags;:syntax=make:filetype=make:ai:si:cin:nu:fo=croqt:cino=p0t0c5(0:

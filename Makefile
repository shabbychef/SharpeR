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

R_FILES           ?= $(wildcard R/*.r)

VERSION  	 = 0.1303
TODAY  		:= $(shell date +%Y-%m-%d)

# all stolen

PKGNAME 	:= $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS 	:= $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  	:= $(shell basename $(PWD))

# Specify the directory holding R binaries. To use an alternate R build (say a
# pre-prelease version) use `make RBIN=/path/to/other/R/` or `export RBIN=...`
# If no alternate bin folder is specified, the default is to use the folder
# containing the first instance of R on the PATH.
RBIN 			?= $(shell dirname "`which R`")
R          = $(RBIN)/R
RSCRIPT    = $(RBIN)/Rscript

# Controls which tests to run. Use thusly:
#
#     make test TESTS=list,of,comma,seperated,tags
#
# Tags must be single words
ifneq ($(TESTS),)
test_tags:=--run-tests=$(TESTS)
else
test_tags:=
endif

# do not distribute these!
EXTRA_FILES = Makefile rebuildTags.sh .gitignore .tags .R_tags
EXTRA_DIRS = .git

.PHONY: help news gitpush gitpull testthat

help:
	@echo "\nExecute development tasks for $(PKGNAME)\n"
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


#------------------------------------------------------------------------------
# Development Tasks
#------------------------------------------------------------------------------

.R_tags: $(R_FILES)
	./rebuildTags.sh

tags: .R_tags

DESCRIPTION : DESCRIPTION.m4
	m4 -DVERSION=$(VERSION) -DDATE=$(TODAY) $< > $@

# Set a default CRAN mirror because otherwise R refuses to install anything.
deps:
	$(R) --slave -e "options(repos = c(CRAN = 'http://cran.cnr.Berkeley.edu'));install.packages(c('roxygen2','testthat'))"

docs:
	cd .. ; \
		$(R) --vanilla --slave -e "library(roxygen2); roxygenize('$(PKGSRC)', '$(PKGSRC).build', overwrite=TRUE, unlink.target=TRUE)"; \
		cd -;
	# Cripple the new folder so you don't get confused and start doing
	# development in there.
	cd ../$(PKGSRC).build ; \
		rm $(EXTRA_FILES); \
		rm -rf $(EXTRA_DIRS); \
		rm -f `find . -name '.*.swp'`; \
		cd -;

news: NEWS.pdf

# ACK!
#NEWS.pdf : NEWS.md
	#./md2news.hs NEWS.md
	## Use this instead of Rd2txt. Rd2txt *does not* produce plaintext. The output
	## has a bunch of formatting junk for underlines and such.
	#"$(R)" --vanilla --slave -e "require(tools);Rd2txt('NEWS.Rd', 'NEWS', options=list(underline_titles=FALSE))"
	#R CMD Rd2pdf --no-preview NEWS.Rd
	## Move news into the inst directory so that it will be available to users
	## after installing the package.
	#mv NEWS.Rd inst

vignette:
	cd inst/doc;\
		$(R) CMD Sweave $(PKGNAME).Rnw;\
		texi2dvi --pdf $(PKGNAME).tex;\
		$(R) --vanilla --slave -e "tools:::compactPDF(getwd(), gs_quality='printer')"


build: docs tags
	cd ..;\
		$(R) CMD build --no-vignettes $(PKGSRC).build


install: build
	cd ..;\
		$(R) CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	cd ..;\
		$(R) CMD check --as-cran $(PKGNAME)_$(PKGVERS).tar.gz
# $(R) CMD check --as-cran --no-tests $(PKGNAME)_$(PKGVERS).tar.gz

test: install
	cd tests;\
		$(Rscript) unit_tests.R $(gc_torture) $(test_tags)

testthat : unit_test.log

unit_test.log :
	$(Rscript) -e "if (require(testthat)) testthat::test_dir('./inst/tests')" | tee $@


#------------------------------------------------------------------------------
# Packaging Tasks
#------------------------------------------------------------------------------

#release:
#@git checkout r-forge
#@git clean -fdx
#@git merge --no-edit master -s recursive -Xtheirs
#@cd ..;\
	#$(R) --vanilla --slave -e "library(roxygen2); roxygenize('$(PKGSRC)','$(PKGSRC)', copy.package=FALSE, overwrite=TRUE)"
#@echo "\nCreating Vignette..."
#@make vignette >> build.log 2>&1
#@echo "Creating NEWS...\n"
#@make news >> build.log 2>&1
#@./updateVersion.sh
#@git commit --amend -m "Build `cat inst/GIT_VERSION`"
#@echo "\nMaster branch merged. Documentation rebuilt. Version number updated."
#@echo 'Perform final touchups and commit with `git commit --amend`.'
#@echo 'Remember to run `git svn dcommit` before `git push` as synching with'
#@echo 'R-Forge SVN will alter the SHA.'

gitpush :
	git push origin master

gitpull :
	git pull origin master


#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:tags=tags;:syntax=make:filetype=make:ai:si:cin:nu:fo=croqt:cino=p0t0c5(0:

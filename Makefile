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

VERSION 				 = 0.1303
TODAY 					:= $(shell date +%Y-%m-%d)

PKG_NAME 				:= SharpeR
PKG_VERSION			:= $(VERSION)
PKG_SRC 				:= $(shell basename $(PWD))

PKG_TGZ 				 = $(PKG_NAME)_$(PKG_VERSION).tar.gz

LOCAL 					:= .local
STAGING 				:= .staging
STAGED_PKG 			 = $(STAGING)/$(PKG_NAME)
RCHECK 					 = $(PKG_NAME).Rcheck

# Specify the directory holding R binaries. To use an alternate R build (say a
# pre-prelease version) use `make RBIN=/path/to/other/R/` or `export RBIN=...`
# If no alternate bin folder is specified, the default is to use the folder
# containing the first instance of R on the PATH.
RBIN 						?= $(shell dirname "`which R`")
R         			 = $(RBIN)/R
RSCRIPT   			 = $(RBIN)/Rscript

# packages I need to test this one
TEST_DEPS  			 = testthat roxygen2

#INSTALLED_DEPS 	 = $(patsubst %,$(LOCAL)/%,$(TEST_DEPS)) 

# do not distribute these!
NODIST_FILES		 = Makefile $(M4_FILES) rebuildTags.sh .gitignore .gitattributes .tags .R_tags
NODIST_DIRS			 = .git

RD_DUMMY 				 = man/$(PKG_NAME).Rd

SUPPORT_FILES 	 = ./DESCRIPTION ./NAMESPACE $(RD_DUMMY)

#########################################################################
# MACROS
#########################################################################

# install locally
INSTALLPKG = $(RLOCAL) -e "install.packages('$(1)', repos = 'http://cran.cnr.Berkeley.edu')" 
	
# make a directory
MKDIR = mkdir -p $(1)

# warn new deps
WARN_DEPS = $(warning newer deps are $(?))

#
.PHONY: help tags \
	gitpull gitpush \
	news doc build install testthat \
	staging_d local_d \
	clean realclean

# dev stuff
.R_tags: $(R_FILES)
	./rebuildTags.sh

tags: .R_tags

DESCRIPTION : DESCRIPTION.m4
	m4 -DVERSION=$(VERSION) -DDATE=$(TODAY) $< > $@

# macro for local R
RLOCAL = R_LIBS=$(LOCAL) $(R) --vanilla 

# make directories
local_d :
	$(call MKDIR,$(LOCAL))

staging_d :
	$(call MKDIR,$(STAGING))


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
	@echo "  parallel   Create a staging version of this package."
	@echo "  build      Invoke docs and then create a package."
	@echo "  check      Invoke build and then check the package."
	@echo "  install    Invoke build and then install the result."
	@echo "  R          Invoke R in a local context with the package."
	@echo ""
	@echo "Packaging Tasks"
	@echo "---------------"
	@echo "  gitpush    Yes, I am lazy"
	@echo ""
	@echo "Using R in: $(RBIN)"
	@echo "Set the RBIN environment variable to change this."
	@echo ""


# debugging
echo :
	@-echo $(R_FILES)

# install an R package in the 'LOCAL' directory.
$(LOCAL)/%/DESCRIPTION : 
	$(call MKDIR,$(LOCAL))
	$(RLOCAL) -e "install.packages('$*', repos = 'http://cran.cnr.Berkeley.edu')" 

deps: $(patsubst %,$(LOCAL)/%/DESCRIPTION,$(TEST_DEPS)) 

# roxygen it.
man/$(PKG_NAME).Rd NAMESPACE: $(R_FILES)
	$(RLOCAL) --slave -e "require(roxygen2); roxygenize('.', '.', overwrite=TRUE, unlink.target=TRUE)"

docs: man/$(PKG_NAME).Rd

#RSYNC_FLAGS     = -av
#RSYNC_FLAGS     = -vrlpgoD --delete
RSYNC_FLAGS     = -av --delete 

# a parallel version of this package, but without the support structure
$(STAGED_PKG)/DESCRIPTION : $(R_FILES) $(SUPPORT_FILES) 
	$(call WARN_DEPS)
	$(call MKDIR,$(STAGED_PKG))
	rsync $(RSYNC_FLAGS) \
		--include=man/ --include=man/* \
		--exclude-from=.gitignore \
		$(patsubst %,--exclude=%,$(NODIST_FILES)) \
		$(patsubst %,--exclude=%,$(NODIST_DIRS)) \
		--exclude=$(LOCAL) --exclude=$(STAGING) --exclude=$(RCHECK) \
		. $(@D)

parallel : $(STAGED_PKG)/DESCRIPTION

# make the 'package', which is a tar.gz
$(PKG_TGZ) : $(STAGED_PKG)/DESCRIPTION
	$(RLOCAL) CMD build --no-vignettes $(<D)

package : $(PKG_TGZ)

# an 'install'
$(LOCAL)/$(PKG_NAME) : $(PKG_TGZ) 
	$(call WARN_DEPS)
	$(call MKDIR,$(LOCAL))
	$(RLOCAL) CMD INSTALL --no-multiarch --library=$(LOCAL) $<

install: $(LOCAL)/$(PKG_NAME)

# check an install
$(RCHECK) : $(PKG_TGZ)
	$(RLOCAL) CMD check --as-cran --outdir=$@ $^ 
	
check: $(RCHECK)

# 2FIX:
unit_test.log : $(LOCAL)/$(PKG_NAME) $(LOCAL)/testthat/DESCRIPTION
	$(RLOCAL) --slave -e "if (require(testthat) && require($(PKG_NAME))) testthat::test_dir('./inst/tests')" | tee $@

testthat : unit_test.log

# clean up.
clean : 
	rm -rf man/*.Rd
	rm -rf $(STAGED_PKG)
	rm -rf $(RCHECK)

realclean : clean
	rm -rf $(LOCAL)

# git foo
gitpush :
	git push origin master

gitpull :
	git pull origin master

# drop into R shell in the 'local context'
R : deps $(LOCAL)/$(PKG_NAME)
	R_LIBS=$(LOCAL) R_PROFILE=load.R R -q --no-save

#vignette:
	#cd inst/doc;\
		#$(R) CMD Sweave $(PKG_NAME).Rnw;\
		#texi2dvi --pdf $(PKG_NAME).tex;\
		#$(R) --vanilla --slave -e "tools:::compactPDF(getwd(), gs_quality='printer')"

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:tags=tags;:syntax=make:filetype=make:ai:si:cin:nu:fo=croqt:cino=p0t0c5(0:

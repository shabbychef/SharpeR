#
# dockerfile to CRAN-check with r-dev
#
# docker build --rm -t shabbychef/sharper-crancheck .
#
# docker run -it --rm --volume $(pwd):/srv:rw sharper-crancheck
#
# Created: 2016.01.10
# Copyright: Steven E. Pav, 2016
# Author: Steven E. Pav
# Comments: Steven E. Pav

#####################################################
# preamble# FOLDUP
FROM shabbychef/crancheck
MAINTAINER Steven E. Pav, shabbychef@gmail.com
# UNFOLD

ENV NONCE 38b1eb8c-7af2-41a4-938e-aa1fe5f79f9a

# rinstall somethings...
RUN /usr/local/bin/install2.r matrixcalc sharper xtable xts timeSeries quantmod MASS TTR testthat sandwich knitr 

#####################################################
# entry and cmd# FOLDUP
# these are the default, but remind you that you might want to use /usr/bin/R instead?
# always use array syntax:
ENTRYPOINT ["/usr/local/bin/Rdevel","CMD","check","--as-cran","--output=/tmp"]

# ENTRYPOINT and CMD are better together:
CMD ["/srv/*.tar.gz"]
# UNFOLD

#for vim modeline: (do not edit)
# vim:nu:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=Dockerfile:ft=Dockerfile:fo=croql

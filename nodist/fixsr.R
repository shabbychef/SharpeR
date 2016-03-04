# /usr/bin/r
#
# Created: 2016.01.31
# Copyright: Steven E. Pav, 2016
# Author: Steven E. Pav <steven@corecast.io>
# Comments: Steven E. Pav

library(SharpeR)
set.seed(1234)
# this is annualized
fsr <- as.sr(rnorm(1000),ope=52,epoch='yr')
# this is on a per-week basis:
wsr <- reannualize(fsr,new.ope=1,new.epoch='wk')
# these are basically the same now:
predint(fsr,100)
sqrt(52) * unlist(predint(wsr,100))
# yields:
# x -1.69 1.297 
# [1] -1.690  1.297

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r

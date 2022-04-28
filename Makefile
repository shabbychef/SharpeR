######################
# 
# Created: 2017.03.19
# Copyright: Steven E. Pav, 2017
# Author: Steven E. Pav
######################

############### FLAGS ###############

PKG_NAME 					:= SharpeR
VMAJOR 						 = 1
VMINOR 						 = 3
VPATCH  					 = 0
VDEV 							 = .003
#VDEV 							 = 

undefine RPKG_USES_RCPP

include ./rpkg_make/Makefile

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=129:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:tags=.tags;:syn=make:ft=make:ai:si:cin:nu:fo=croqt:cino=p0t0c5(0:

#
# utilities
#
# see also:
# changelog:
#
# Created: 2012.05.19
# Copyright: Steven E. Pav, 2012-2012
# Author: Steven E. Pav
# Comments: Steven E. Pav
# SVN: $Id: blankheader.txt 25454 2012-02-14 23:35:25Z steven $

# annualize and deannualize a Sharpe Ratio#FOLDUP
.annualize <- function(sr, epy) {
  return(sr * sqrt(epy))  
}
.deannualize <- function(sr.pa, epy) {
  return(sr.pa / sqrt(epy))
}
#UNFOLD

# Sharpe Ratio
# converting t <-> sr#FOLDUP

# conversion routines
# Sharpe Ratio to t stat
.sr_to_t <- function(sr, df) {
  return(sr * sqrt(df))
}
# derivative of same
.d_sr_to_t <- function(sr, df) {
  return(sqrt(df))
}
# t stat to Sharpe Ratio
.t_to_sr <- function(t, df) {
  return(t / sqrt(df))
}
#UNFOLD

# SR^*, the SR of a Markowitz portfolio
# convert SR^* <-> T2#FOLDUP
.srs_to_T2 <- function(sample.sr,n) {
	return(n * sample.sr^2)
}
.T2_to_srs <- function(T2,n) {
	return(sqrt(T2 / n))
}
#UNFOLD

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:

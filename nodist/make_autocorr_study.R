# Copyright 2012-2013 Steven E. Pav. All Rights Reserved.
# Author: Steven E. Pav

# This file is part of SharpeR.
#
# SharpeR is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# SharpeR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with SharpeR.  If not, see <http://www.gnu.org/licenses/>.

# * Thu Sep 26 2013 10:18:30 AM Steven E. Pav <steven@cerebellumcapital.com>
# pre-run the skew study b/c it takes too long.

mc.resolution <- 1000

########################################################################
# AR time series #FOLDUP

#generate an AR(1) series:
AR1.filter <- function(phi,epsilons) {
	n <- length(epsilons)
	series <- numeric(n)
	series[1] <- epsilons[1] / (1 - phi)
	for(iii in 2:n) {
		series[iii] <- phi * series[iii-1] + epsilons[iii]
	}
	return(series)
}

AR1.series <- function(n,phi,mu,sd,burn.in=100) {
	series <- mu + AR1.filter(phi,rnorm(n + burn.in,mean=0,sd = sd))
	return(series[(burn.in + 1):(burn.in + n)])
}
#UNFOLD
# vanilla Sharpe ratio in terms of whatever input units
f_vsharpe <- function(rets) {
	return(mean(rets) / sd(rets))
}
# annualized Sharpe ratio
f_asharpe <- function(rets, dpy = 253) {
	return(sqrt(dpy) * f_vsharpe(rets))
}
# the t statistic
f_tstat <- function(rets) { 
	return(sqrt(length(rets)) * f_vsharpe(rets))
}
# for phi = -0.15 to 0.15 or so, generate 2048 returns series of length
# around 3 years and look at the standard deviation of the t-statistic.

# phi is the AR parameter;
# ntrial is the number of times we run this;
# nyr is the number of years of data to look at
# dpy is days per year.
# vsr is the 'vanilla' Sharpe ratio, daily;
# sg is the daily volatility
look_spread <- function(phi,ntrial,nyr,vsr=0,sg=0.01,dpy=253) {
	n <- ceiling(nyr * dpy)
	mu <- vsr * sg
	ssrs <- replicate(ntrial,f_tstat(AR1.series(n,phi,mu,sg)))
	return(sd(ssrs))
}

# the value estimated by Van Belle's trick:
vb_spread <- function(phi,ntrial,nyr,vsr=0,sg=0.01,dpy=253) {
	sqrt((1 + phi) / (1 - phi))
}

top_phi <- 0.2
phivals <- seq(-top_phi,top_phi,length.out=15)
ntrials <- ceiling(8 * mc.resolution)
nyr <- 3

empiricals <- sapply(phivals,look_spread,ntrial = ntrials,nyr = nyr)
predicteds <- sapply(phivals,vb_spread,ntrial = ntrials,nyr = nyr)

save(phivals,empiricals,predicteds,ntrials,nyr,
		 file='autocorr_study.rda',compress='bzip2')

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r

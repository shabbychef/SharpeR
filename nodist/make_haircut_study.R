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

# * Sat Nov 02 2013 09:03:27 AM Steven E. Pav <steven@cerebellumcapital.com>
# pre-run the haircut study b/c it takes too long.


require(MASS)

# simple markowitz.
simple.marko <- function(rets) {
	mu.hat <- as.vector(apply(rets,MARGIN=2,mean,na.rm=TRUE))
	Sig.hat <- cov(rets)
	w.opt <- solve(Sig.hat,mu.hat)
	retval <- list('mu'=mu.hat,'sig'=Sig.hat,'w'=w.opt)
	return(retval)
}
# make multivariate pop. & sample w/ given zeta.star
gen.pop <- function(n,p,zeta.s=0) {
	true.mu <- matrix(rnorm(p),ncol=p)
	#generate an SPD population covariance. a hack.
	xser <- matrix(rnorm(p*(p + 100)),ncol=p)
	true.Sig <- t(xser) %*% xser
	pre.sr <- sqrt(true.mu %*% solve(true.Sig,t(true.mu)))
	#scale down the sample mean to match the zeta.s
	true.mu <- (zeta.s/pre.sr[1]) * true.mu 
  X <- mvrnorm(n=n,mu=true.mu,Sigma=true.Sig)
	retval = list('X'=X,'mu'=true.mu,'sig'=true.Sig,'SNR'=zeta.s)
	return(retval)
}
# a single simulation
sample.haircut <- function(n,p,...) {
	popX <- gen.pop(n,p,...)
	smeas <- simple.marko(popX$X)
	# I have got to figure out how to deal with vectors...
	ssnr <- (t(smeas$w) %*% t(popX$mu)) / sqrt(t(smeas$w) %*% popX$sig %*% smeas$w)
	hcut <- 1 - (ssnr / popX$SNR)
	zeta.hat.s <- 1 # noop
	return(c(hcut,zeta.hat.s))
}

# set everything up
set.seed(as.integer(charToRaw("496509a9-dd90-4347-aee2-1de6d3635724")))
ope <- 253
n.sim <- 2048 
n.stok <- 6
n.yr <- 4
n.obs <- ceiling(ope * n.yr)
zeta.s <- 1.20 / sqrt(ope)   # optimal SNR, in daily units

# run some experiments
experiments <- replicate(n.sim,sample.haircut(n.obs,n.stok,zeta.s))
hcuts <- experiments[1,]

save(n.sim,n.stok,n.yr,n.obs,zeta.s,ope,hcuts,
		 file='haircut_study.rda',compress='bzip2')

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r

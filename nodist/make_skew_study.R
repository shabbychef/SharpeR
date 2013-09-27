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

# * Mon Sep 23 2013 04:42:31 PM Steven E. Pav <steven@cerebellumcapital.com>
# pre-run the skew study b/c it takes too long.

require(LambertW)
require(MASS)
require(quantmod)
options("getSymbols.warning4.0"=FALSE)

#make this like cci matlab?
rel.returns <- function(x,k=1) {
	diff(log(x),lag=k)
}

get.ret <- function(sym,warnings=FALSE,max.try=10,...) {
	# getSymbols.yahoo will barf sometimes; do a trycatch
  trynum <- 0
	while (!exists("OHCLV") && (trynum < max.try)) {
		trynum <- trynum + 1
		try(OHLCV <- getSymbols(sym,auto.assign=FALSE,
														warnings=warnings,...),silent=TRUE)
  }
	if (substr(sym,1,1) == '^')
		sym <- substring(sym,2) 
	adj.names <- paste(c(sym,"Adjusted"),collapse=".",sep="")
	if (adj.names %in% colnames(OHLCV)) {
		adj.close <- OHLCV[,adj.names]
	} else {
		# for DJIA from FRED, say. 
		adj.close <- OHLCV[,sym]
	}
	rm(OHLCV)
	adj.close <- adj.close[!is.na(adj.close)]
	# rename it
	colnames(adj.close) <- c(sym)
	lrets <- diff(log(adj.close))
	#chop first
	lrets[-1,]
}

SPX.rets <- get.ret("^GSPC",from="1970-01-01",to="2013-01-01")

v.SPX.rets <- as.vector(SPX.rets)
colnames(v.SPX.rets) <- NULL
rownames(v.SPX.rets) <- NULL


# vanilla Sharpe ratio in terms of whatever input units
f_vsharpe <- function(rets) {
	return(mean(rets) / sd(rets))
}

mertens_se <- function(sr,n,skew=0,ex.kurt=0) {
	se <- sqrt((1 + ((sr^2) * (2 + ex.kurt)/4) - skew * sr) / n)
	return(se)
}
# test if SR == SR0
mertens_pval <- function(sr0,sr,n,...) {
	se <- mertens_se(sr,n,...)
	pv <- pnorm((sr - sr0) / se,lower.tail=FALSE)
	return(pv)
}
test_mertens <- function(x,snr.ann = 1,dpy = 253) {
	n <- length(x)
	sr <- f_vsharpe(x)
	skew <- skewness(x)
	ex.kurt <- kurtosis(x) - 3
	sr0 <- snr.ann / sqrt(dpy)
	return(mertens_pval(sr0,sr,n,skew,ex.kurt))
}

#see if the nominal 0.05 type I rate is maintained for skewed, kurtotic distributions
#check for SNR=1, annualized
ttest_snr <- function(x,snr.ann = 1,dpy = 253) {
	n <- length(x)
	myt <- sqrt(n) * f_vsharpe(x)
	return(pt(myt,df = n-1,ncp = sqrt(n/dpy) * snr.ann,lower.tail = FALSE))
}
ttest_both <- function(...) {
	lo <- ttest_snr(...)
	mr <- test_mertens(...)
	return(c(lo,mr))
}
multi_test <- function(gen,n,trials=1024) {
	pvals <- replicate(trials,ttest_both(gen(n)))
	rowMeans(pvals < 0.05)
}

# a bunch of rv generators of fixed mean and sd;#FOLDUP

#gaussian
#gen_norm <- rnorm
gen_norm <- function(n,mu = 0, sg = 1) {
	return(rnorm(n,mean=mu,sd=sg))
}

#t(10)
gen_t <- function(n,df = 10,mu = 0,sg = 1) {
	return(mu + sqrt((df-2)/df) * sg * rt(n,df = df))
}

#Tukey h
gen_tukey_h <- function(n,h = 0.1,mu = 0,sg = 1) {
	Gauss_input = create_LambertW_input("normal", beta=c(0,1))
	params = list(delta = c(h))
	LW.Gauss = create_LambertW_output(input = Gauss_input, theta = params)
	#get the moments of this distribution
	moms <- mLambertW(beta=c(0,1),distname=c("normal"),delta = h,gamma = 0, alpha = 1)
	samp <- LW.Gauss$rY(params)(n=n)
	samp <- mu  + (sg/moms$sd) * (samp - moms$mean)
}

#Lambert x Gaussian
gen_lambert_w <- function(n,dl = 0.1,mu = 0,sg = 1) {
	Gauss_input = create_LambertW_input("normal", beta=c(0,1))
	params = list(delta = c(0), gamma=c(dl), alpha = 1)
	LW.Gauss = create_LambertW_output(input = Gauss_input, theta = params)
	#get the moments of this distribution
	moms <- mLambertW(beta=c(0,1),distname=c("normal"),delta = 0,gamma = dl, alpha = 1)
	samp <- LW.Gauss$rY(params)(n=n)
	samp <- mu  + (sg/moms$sd) * (samp - moms$mean)
}

#sample from SP500
gen_SP500 <- function(n,mu = 0,sg = 1) {
	Z <- v.SPX.rets
	Z <- mu + (sg/sd(Z)) * (Z - mean(Z))
	samp <- sample(Z,n,replace=TRUE)
}

#sample from symmetric SP500
gen_sym_SP500 <- function(n,mu = 0,sg = 1) {
	Z <- c(v.SPX.rets,-v.SPX.rets)
	Z <- mu + (sg/sd(Z)) * (Z - mean(Z))
	samp <- sample(Z,n,replace=TRUE)
}

#now the moments of these things
moms_norm <- function(mu = 0,sg = 1) {
	moms <- list(skew=0,kurtosis=0,mean=mu,sd=sg)
	return(moms)
}
moms_t <- function(df = 10,mu = 0,sg = 1) {
	moms <- list(skew=0,kurtosis=6 / (df-4),mean=mu,sd=sg)
	return(moms)
}
moms_tukey_h <- function(h = 0.1,mu = 0,sg = 1) {
	Gauss_input = create_LambertW_input("normal", beta=c(0,1))
	params = list(delta = c(h))
	LW.Gauss = create_LambertW_output(input = Gauss_input, theta = params)
	#get the moments of this distribution
	moms <- mLambertW(beta=c(0,1),distname=c("normal"),delta = h,gamma = 0, alpha = 1)
	moms$mean <- mu
	moms$sd <- sg
	return(moms)
}
moms_lambert_w <- function(dl = 0.1,mu = 0,sg = 1) {
	Gauss_input = create_LambertW_input("normal", beta=c(0,1))
	params = list(delta = c(0), gamma=c(dl), alpha = 1)
	LW.Gauss = create_LambertW_output(input = Gauss_input, theta = params)
	#get the moments of this distribution
	moms <- mLambertW(beta=c(0,1),distname=c("normal"),delta = 0,gamma = dl, alpha = 1)
	moms$mean <- mu
	moms$sd <- sg
	return(moms)
}
moms_SP500 <- function(mu = 0,sg = 1) {
	Z <- v.SPX.rets
	moms <- list(skew=skewness(Z),kurtosis=kurtosis(Z) - 3,mean=mu,sd=sg)
	return(moms)
}
moms_sym_SP500 <- function(mu = 0,sg = 1) {
	Z <- c(v.SPX.rets,-v.SPX.rets)
	moms <- NULL
	moms <- list(skew=0,kurtosis=kurtosis(Z) - 3,mean=mu,sd=sg)
	return(moms)
}

#UNFOLD

#to replicate randomness
dpy <- 253
nobs <- round(3 * dpy)
daily.mean <- 1/sqrt(dpy)
ntrials <- 4096

set.seed(1984)

new.res <- function(name,param=" ",moms,gen,nobs,ntrials=100) {
	errs <- multi_test(gen,nobs,ntrials)
	res <- data.frame(distribution = name,
										param = param,
										skew = moms$skew,
										kurtosis=moms$kurtosis,
										typeI = errs[1],
										cor.typeI = errs[2])
	return(res)
}


# put them together#FOLDUP

moms <- moms_norm(mu=daily.mean)
res <- new.res(name = "Gaussian",param = " ",moms = moms,
							 gen = function(n){ gen_norm(n,mu=daily.mean) },
							 nobs=nobs,ntrials=ntrials)

moms <- moms_t(df=10,mu=daily.mean)
res.nxt <- new.res(name = "Student's t",param = "df = 10",moms = moms,
							 gen = function(n){ gen_t(n,mu=daily.mean) },
							 nobs=nobs,ntrials=ntrials)
res <- merge(res,res.nxt,all=TRUE)

moms <- moms_SP500(mu=daily.mean)
res.nxt <- new.res(name = "SP500",param = "",moms = moms,
							 gen = function(n){ gen_SP500(n,mu=daily.mean) },
							 nobs=nobs,ntrials=ntrials)
res <- merge(res,res.nxt,all=TRUE)

moms <- moms_sym_SP500(mu=daily.mean)
res.nxt <- new.res(name = "symmetric SP500",param = "",moms = moms,
							 gen = function(n){ gen_sym_SP500(n,mu=daily.mean) },
							 nobs=nobs,ntrials=ntrials)
res <- merge(res,res.nxt,all=TRUE)

for (my.h in c(0.1,0.24,0.4)) {
	moms <- moms_tukey_h(h=my.h,mu=daily.mean)
	res.nxt <- new.res(name = "Tukey h",param = sprintf("h = %.1f",my.h),
										 moms = moms,
										 gen = function(n){ gen_tukey_h(n,h=my.h,mu=daily.mean) },
										 nobs=nobs,ntrials=ntrials)
	res <- merge(res,res.nxt,all=TRUE)
}

for (my.dl in c(0.4,0.2,-0.2,-0.4,-0.8,-1.2)) {
	moms <- moms_lambert_w(dl=my.dl,mu=daily.mean)
	res.nxt <- new.res(name = "Lambert x Gaussian",
										 param = sprintf("delta = %.1f",my.dl),
										 moms = moms,
										 gen = function(n){ 
											gen_lambert_w(n,dl=my.dl,mu=daily.mean) },
										 nobs=nobs,ntrials=ntrials)
	res <- merge(res,res.nxt,all=TRUE)
}
#UNFOLD

# fix the sigfigs issue?
res[,3] = signif(res[,3],digits=2)
res[,4] = signif(res[,4],digits=2)
res[,5] = signif(res[,5],digits=2)

save(res,ntrials,SPX.rets,file='skew_study.rda',compress='bzip2')

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r

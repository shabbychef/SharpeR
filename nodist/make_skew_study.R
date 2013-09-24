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
#see if the nominal 0.05 type I rate is maintained for skewed, kurtotic distributions
#check for SNR=1, annualized
ttest_snr <- function(x,snr.ann = 1,dpy = 253) {
	n <- length(x)
	myt <- sqrt(n) * f_vsharpe(x)
	return(pt(myt,df = n-1,ncp = sqrt(n/dpy) * snr.ann,lower.tail = FALSE))
}
multi_test <- function(gen,n,trials=1024) {
	pvals <- replicate(trials,ttest_snr(gen(n)))
	mean(pvals < 0.05)
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
mc.resolution <- 1000
ntrials <- ceiling(1.0235 * mc.resolution)

set.seed(1977)

# put them together#FOLDUP

moms <- moms_norm(mu=daily.mean)
res <- data.frame(distribution = "Gaussian",param = " ",skew = moms$skew,ex.kurtosis=moms$kurtosis,typeI = multi_test(function(n)(gen_norm(n,mu=daily.mean)),nobs,ntrials))

moms <- moms_t(df=10,mu=daily.mean)
res.t <- data.frame(distribution = "Student's t",param = "df = 10",skew = moms$skew,ex.kurtosis=moms$kurtosis,typeI = multi_test(function(n)(gen_t(n,mu=daily.mean)),nobs,ntrials))
res <- merge(res,res.t,all=TRUE)

moms <- moms_SP500(mu=daily.mean)
res.h1 <- data.frame(distribution = "SP500",param = "",skew = moms$skew,ex.kurtosis=moms$kurtosis,typeI = multi_test(function(n)(gen_SP500(n,mu=daily.mean)),nobs,ntrials))
res <- merge(res,res.h1,all=TRUE)

moms <- moms_sym_SP500(mu=daily.mean)
res.h1 <- data.frame(distribution = "symmetric SP500",param = "",skew = moms$skew,ex.kurtosis=moms$kurtosis,typeI = multi_test(function(n)(gen_sym_SP500(n,mu=daily.mean)),nobs,ntrials))
res <- merge(res,res.h1,all=TRUE)

moms <- moms_tukey_h(h=0.1,mu=daily.mean)
res.h1 <- data.frame(distribution = "Tukey h",param = "h = 0.1",skew = moms$skew,ex.kurtosis=moms$kurtosis,typeI = multi_test(function(n)(gen_tukey_h(n,h=0.1,mu=daily.mean)),nobs,ntrials))
res <- merge(res,res.h1,all=TRUE)

moms <- moms_tukey_h(h=0.24,mu=daily.mean)
res.h2 <- data.frame(distribution = "Tukey h",param = "h = 0.24",skew = moms$skew,ex.kurtosis=moms$kurtosis,typeI = multi_test(function(n)(gen_tukey_h(n,h=0.24,mu=daily.mean)),nobs,ntrials))
res <- merge(res,res.h2,all=TRUE)

moms <- moms_tukey_h(h=0.4,mu=daily.mean)
res.h2 <- data.frame(distribution = "Tukey h",param = "h = 0.4",skew = moms$skew,ex.kurtosis=moms$kurtosis,typeI = multi_test(function(n)(gen_tukey_h(n,h=0.4,mu=daily.mean)),nobs,ntrials))
res <- merge(res,res.h2,all=TRUE)

dl <- -0.2
moms <- moms_lambert_w(dl=dl,mu=daily.mean)
res.h3 <- data.frame(distribution = "Lambert W x Gaussian",param = sprintf("delta = %.1f",dl),skew = moms$skew,ex.kurtosis=moms$kurtosis,typeI = multi_test(function(n)(gen_lambert_w(n,dl=dl,mu=daily.mean)),nobs,ntrials))
res <- merge(res,res.h3,all=TRUE)

dl <- -0.4
moms <- moms_lambert_w(dl=dl,mu=daily.mean)
res.h3 <- data.frame(distribution = "Lambert W x Gaussian",param = sprintf("delta = %.1f",dl),skew = moms$skew,ex.kurtosis=moms$kurtosis,typeI = multi_test(function(n)(gen_lambert_w(n,dl=dl,mu=daily.mean)),nobs,ntrials))
res <- merge(res,res.h3,all=TRUE)

dl <- -0.8
moms <- moms_lambert_w(dl=dl,mu=daily.mean)
res.h3 <- data.frame(distribution = "Lambert W x Gaussian",param = sprintf("delta = %.1f",dl),skew = moms$skew,ex.kurtosis=moms$kurtosis,typeI = multi_test(function(n)(gen_lambert_w(n,dl=dl,mu=daily.mean)),nobs,ntrials))
res <- merge(res,res.h3,all=TRUE)

dl <- -1.6
moms <- moms_lambert_w(dl=dl,mu=daily.mean)
res.h3 <- data.frame(distribution = "Lambert W x Gaussian",param = sprintf("delta = %.1f",dl),skew = moms$skew,ex.kurtosis=moms$kurtosis,typeI = multi_test(function(n)(gen_lambert_w(n,dl=dl,mu=daily.mean)),nobs,ntrials))
res <- merge(res,res.h3,all=TRUE)
#UNFOLD

# fix the sigfigs issue?
res[,3] = signif(res[,3],digits=2)
res[,4] = signif(res[,4],digits=2)
res[,5] = signif(res[,5],digits=2)

save(res,ntrials,SPX.rets,file='skew_study.rda',compress='bzip2')

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r

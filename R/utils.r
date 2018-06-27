# Copyright 2012-2014 Steven E. Pav. All Rights Reserved.
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

# env var:
# nb: 
# see also:
# todo:
# changelog: 
#
# Created: 2012.05.19
# Copyright: Steven E. Pav, 2012-2013
# Author: Steven E. Pav
# Comments: Steven E. Pav

# matrix utils#FOLDUP
# asymetric toeplitz, like Matlab's; 
# allows you to give the first row vector and the first column vector.
.atoeplitz <- function(rv,cv=rv) {
	if (!is.vector(rv)) 
		stop("'rv' is not a vector")
	if (!is.vector(cv)) 
		stop("'cv' is not a vector")
	nc <- length(rv)
	nr <- length(cv)
	if (rv[1] != cv[1])
		stop("'rv' and 'cv' must match at first element")

	A <- matrix(raw(), nr, nc)
	retval <- matrix(ifelse((col(A) > row(A)),rv[abs(col(A) - row(A)) + 1L],cv[abs(col(A) - row(A)) + 1L]), nr, nc)
	return(retval)
}

# the mean gram 
#.mean.gram <- function(X) { (t(X) %*% X) / dim(X)[1] }

# quadratic forms
.qform <- function(bread,meat) { # nocov start
	return(t(bread) %*% (meat %*% bread))
} # nocov end
# and 'outer' 
.qoform <- function(bread,meat) {
	return(bread %*% (meat %*% t(bread)))
}
#UNFOLD

# stats utils#FOLDUP
# convert p-value for a 1-sided test into one for a two-sided test.
.oneside2two <- function(pv) {
	retval <- 1 - 2 * abs(0.5 - pv)
	return(retval)
}
#UNFOLD

# class utils#FOLDUP
# infer the total time from an xts object,
# as a difftime
.infer_delt_xts <- function(anxts) {
	TEO <- time(anxts)
	delt <- difftime(TEO[length(TEO)],TEO[1],units='days')
	return(delt)
}
# infer the observations per epoch from an xts object
.infer_ope_xts <- function(anxts) {
	delt <- .infer_delt_xts(anxts)
	n.row <- dim(anxts)[1]
	days.per.row <- as.numeric(delt) / (n.row - 1)
	ope <- 365.25 / days.per.row
	return(ope)
}
#UNFOLD

# annualize and deannualize a Sharpe Ratio#FOLDUP
# ' @param sr the Sharpe Ratio, in per sqrt(epoch) units.
# ' @param ope the number of observations per epoch. no default here.
# ' @return the annualized Sharpe ratio, in per sqrt(epoch) units.
# ' @aliases .deannualize
# ' @export
.annualize <- function(sr, ope) {
  return(sr * sqrt(ope))  
}
# ' @export
.deannualize <- function(sr.pa, ope) {
  return(sr.pa / sqrt(ope))
}

# for T^2: 
.annualize2 <- function(T2, ope) { # nocov start
  return(T2 * ope)  
} # nocov end
.deannualize2 <- function(T2.pa, ope) { # nocov start
  return(T2.pa / ope)
} # nocov end
#UNFOLD

# conversions
# converting T2 <-> F#FOLDUP
# convert hotelling T2 to F statistic
.T2_to_F <- function(T2, p, n) {
	return(T2 * (n - p) / (p * (n - 1)))
}
# derivative of same
.d_T2_to_F <- function(T2, p, n) {
	return((n - p) / (p * (n - 1)))
}
# don't need this?
.logT2_to_logF <- function(logT2, p, n) {
	return(logT2 + log(.T2_to_F(1, p, n)))  # correct but mildly inefficient
}
# convert F statistic to hotelling T2
.F_to_T2 <- function(F, p, n) {
	return(F * (p * (n - 1)) / (n - p))
}
#.logF_to_logT2 <- function(logF, p, n) {
	#return(logF + .F_to_T2(1, p, n))  # correct but mildly inefficient
#}
#UNFOLD

# converting T2 <-> sropt#FOLDUP
# convert hotelling T2 to maximal SR statistic, SR^*
.T2_to_sropt <- function(T2, n) {
	return(sqrt(T2 / n))
}
# derivative of same
.d_T2_to_sropt <- function(T2, n) {
	return(0.5 / sqrt(T2 * n))
}
.logT2_to_logsropt <- function(logT2, n) {
	return(0.5 * (logT2 - log(n)))
}
# convert sropt to T2; 
# 2FIX: this is a total cluster fuck b/c negative sropt
# are 'out of range'; silently mark them up to zero here
# and pretend nothing happened? crap.
.sropt_to_T2 <- function(sropt, n) {
	return(n * pmax(0,sropt)^2)
}
#UNFOLD

# moments computations, translated from Yong Bao's Gauss code:# FOLDUP

.bao_func <- function(y,rhat=NULL,na.rm=TRUE) {
	if (na.rm) {
		y <- na.omit(y)
	}
	Shat <- mean(y) / sd(y)
	if (is.null(rhat)) {
		rhat <- .smplgamma(y)
	}
	TT <- length(y)
  biasf1 <- .bias1(TT,Shat,rhat);
	biasf2 <- .bias2(TT,Shat,rhat);
	vf <- .variance(TT,Shat,rhat);   
	list(shat=Shat,rhat=rhat,TT=TT,
			 bias1=biasf1,bias2=biasf2,var=vf)
}
.smplgamma <- function(y) {
	x <- y - mean(y)
	k <- .kstat(x)
	retv <- k[3:6] / (k[2] ^ ((3:6)/2))
	retv
}

# /* k-statistic, see Kendall's Advanced Theory of Statistics */
.kstat <- function(x) {
	n <- length(x)
	s <- unlist(lapply(1:6,function(iii) { sum(x^iii) }))
	nn <- n^(1:6)
	nd <- exp(lfactorial(n) - lfactorial(n-(1:6)))
	k <- rep(0,6)
	k[1] <- s[1]/n;
	k[2] <- (n*s[2]-s[1]^2)/nd[2];
	k[3] <- (nn[2]*s[3]-3*n*s[2]*s[1]+2*s[1]^3)/nd[3]
	k[4] <- ((nn[3]+nn[2])*s[4]-4*(nn[2]+n)*s[3]*s[1]-3*(nn[2]-n)*s[2]^2 +12*n*s[2]*s[1]^2-6*s[1]^4)/nd[4];
	k[5] <- ((nn[4]+5*nn[3])*s[5]-5*(nn[3]+5*nn[2])*s[4]*s[1]-10*(nn[3]-nn[2])*s[3]*s[2] 
					 +20*(nn[2]+2*n)*s[3]*s[1]^2+30*(nn[2]-n)*s[2]^2*s[1]-60*n*s[2]*s[1]^3 +24*s[1]^5)/nd[5];
	k[6] <- ((nn[5]+16*nn[4]+11*nn[3]-4*nn[2])*s[6]
					-6*(nn[4]+16*nn[3]+11*nn[2]-4*n)*s[5]*s[1]
					-15*n*(n-1)^2*(n+4)*s[4]*s[2]-10*(nn[4]-2*nn[3]+5*nn[2]-4*n)*s[3]^2
					+30*(nn[3]+9*nn[2]+2*n)*s[4]*s[1]^2+120*(nn[3]-n)*s[3]*s[2]*s[1]
					+30*(nn[3]-3*nn[2]+2*n)*s[2]^3-120*(nn[2]+3*n)*s[3]*s[1]^3
					-270*(nn[2]-n)*s[2]^2*s[1]^2+360*n*s[2]*s[1]^4-120*s[1]^6)/nd[6];

	k
}

.bias2 <- function(TT,S,r) {
	retv <- 3*S/4/TT+49*S/32/TT^2-r[1]*(1/2/TT+3/8/TT^2)+S*r[2]*(3/8/TT-15/32/TT^2) +3*r[3]/8/TT^2-5*S*r[4]/16/TT^2-5*S*r[1]^2/4/TT^2+105*S*r[2]^2/128/TT^2-15*r[1]*r[2]/16/TT^2;
}

.bias1 <- function(TT,S,r) {
	3*S/4/TT-r[1]*(1/2/TT)+S*r[2]*(3/8/TT);
}

.variance <- function(TT,S,r) {
	(1+S^2/2)/TT+(19*S^2/8+2)/TT^2-r[1]*S*(1/TT+5/2/TT^2)
    +r[2]*S^2*(1/4/TT+3/8/TT^2)+5*r[3]*S/4/TT^2-3*r[4]*S^2/8/TT^2
    +r[1]^2*(7/4/TT^2-3*S^2/2/TT^2)
    -15*r[1]*r[2]*S/4/TT^2+39*r[2]^2*S^2/32/TT^2;
}

#set.seed(2134)
#y <- rnorm(1000)
#bl <- .bao_func(y)
# UNFOLD

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r

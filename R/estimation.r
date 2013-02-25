# Copyright 2012 Steven E. Pav. All Rights Reserved.
# Author: Steven E. Pav

# This file is part of Ratarb.
#
# Ratarb is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Ratarb is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with Ratarb.  If not, see <http://www.gnu.org/licenses/>.

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
# SVN: $Id: blankheader.txt 25454 2012-02-14 23:35:25Z steven $

#' @include utils.r
#' @include distributions.r

# note: on citations, use the Chicago style from google scholar. tks.

########################################################################
# Estimation 
########################################################################

# confidence intervals on the Sharpe ratio#FOLDUP

# standard errors
#the sample.sr should *not* be annualized
.sr_se_weirdo <- function(sample.sr,n) {
	cn <- .srbias(n)
	dn <- (n-1) / ((n-3) * cn * cn)
	W  <- (sample.sr / cn) ** 2
	se <- sqrt((dn/n) + W * (dn - 1))
	return(se)
}

.sr_se_walck <- function(sample.sr,n) {
	se <- sqrt((1/n) + 0.5 * sample.sr ** 2 / (n - 1))
	return(se)
}

.sr_se_lo <- function(sample.sr,n,ss.adjust=FALSE) {
	# small sample adjustment; works better in practice.
	df <- ifelse(ss.adjust,n-1,n)
	se <- sqrt((1 + 0.5 * sample.sr ** 2) / df)
	return(se)
}

#' @title Standard error of Sharpe ratio
#'
#' @description 
#'
#' Estimates the standard error of the Sharpe ratio statistic. 
#'
#' @details 
#'
#' There are three methods:
#'
#' \itemize{
#' \item The default, \code{t}, based on Johnson & Welch, with a correction
#' for small sample size. 
#' \item An asymptotically equivalent method, \code{Lo}, based on Lo,
#' which is Johnson & Welch's method but without correcting for d.f.
#' \item An approximation based on normality, \code{Z}.
#' \item An approximation based on an F statistic, \code{F}.
#' }
#'
#' @usage
#'
#' sr.se(sr,df,opy,type=c("t","Lo","Z","F")) 
#'
#' @param sr an observed Sharpe ratio statistic, annualized.
#' @param df the number of observations the statistic is based on. This 
#'        is one more than the number of degrees of freedom in the
#'        corresponding t-statistic, although the effect will be small
#'        when \code{df} is large.
#' @param opy the number of observations per 'year'. \code{x}, \code{q}, and 
#'        \code{snr} are quoted in 'annualized' units, that is, per square root 
#'        'year', but returns are observed possibly at a rate of \code{opy} per 
#'        'year.' default value is 1, meaning no deannualization is performed.
#' @param type the estimator type. one of \code{"t", "Lo", "Z", "F"}
#' @keywords htest
#' @return an estimate of standard error.
#' @seealso sr-distribution functions, \code{\link{dsr}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @references 
#'
#' Walck, C. "Hand-book on STATISTICAL DISTRIBUTIONS for experimentalists."
#' 1996. \url{http://www.stat.rice.edu/~dobelman/textfiles/DistributionsHandbook.pdf}
#'
#' Johnson, N. L., and Welch, B. L. "Applications of the non-central t-distribution."
#' Biometrika 31, no. 3-4 (1940): 362-389. \url{http://dx.doi.org/10.1093/biomet/31.3-4.362}
#'
#' Lo, Andrew W. "The statistics of Sharpe ratios." Financial Analysts Journal (2002): 36-52.
#' \url{http://ssrn.com/paper=377260}
#'
#' @examples 
#' opy <- 253
#' df <- opy * 6
#' rvs <- rsr(1, df, 1.0, opy)
#' anse <- sr.se(rvs,df,opy,type="t")
#' anse2 <- sr.se(rvs,df,opy,type="Z")
#'
#'@export
sr.se <- function(sr,df,opy,type=c("t","Lo","Z","F")) { 
	if (!missing(opy)) {
		sr <- .deannualize(sr,opy)
	}
	type <- match.arg(type)
	se <- switch(type,
							 t = .sr_se_lo(sr,df,ss.adjust=TRUE),
							 Lo = .sr_se_lo(sr,df,ss.adjust=FALSE),
							 Z = .sr_se_walck(sr,df),
							 F = .sr_se_weirdo(sr,n))
	if (!missing(opy)) {
		se <- .annualize(se,opy)
	}
	return(se)
}

#' @title Confidence Interval on Signal-Noise Ratio
#'
#' @description 
#'
#' Computes approximate confidence intervals on the Signal-Noise ratio given the Sharpe ratio.
#'
#' @details 
#'
#' none yet...
#'
#' @usage
#'
#' sr.confint(sr,df,level=0.95,type=c("exact","t","Z","F"),opy=1,level.lo=(1-level)/2,level.hi=1-level.lo)
#'
#' @param sr an observed Sharpe ratio statistic, annualized.
#' @param df the number of observations the statistic is based on. This 
#'        is one more than the number of degrees of freedom in the
#'        corresponding t-statistic, although the effect will be small
#'        when \code{df} is large.
#' @param level the confidence level required.
#' @param type the estimator type. one of \code{"t", "Lo", "Z", "F"}
#' @param opy the number of observations per 'year'. \code{x}, \code{q}, and 
#'        \code{snr} are quoted in 'annualized' units, that is, per square root 
#'        'year', but returns are observed possibly at a rate of \code{opy} per 
#'        'year.' default value is 1, meaning no deannualization is performed.
#' @param level.lo the lower bound for the confidence interval.
#' @param level.hi the upper bound for the confidence interval.
#' @keywords htest
#' @return A matrix (or vector) with columns giving lower and upper
#' confidence limits for the SNR. These will be labelled as
#' level.lo and level.hi in %
#' @seealso \code{\link{confint}}, \code{\link{sr.se}}, \code{\link{qlambdap}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#'
#' @examples 
#' opy <- 253
#' df <- opy * 6
#' rvs <- rsr(1, df, 1.0, opy)
#' aci <- sr.confint(rvs,df,type="t",opy=opy)
#' aci2 <- sr.confint(rvs,df,type="Z",opy=opy)
#'
#'@export
sr.confint <- function(sr,df,level=0.95,type=c("exact","t","Z","F"),
											 opy=1,level.lo=(1-level)/2,level.hi=1-level.lo) {
	#2FIX: the order of arguments is really wonky. where does opy go?
	if (!missing(opy)) {
		sr <- .deannualize(sr,opy)
	}
	type <- match.arg(type)
	if  (type == "exact") {
		tstat <- .sr_to_t(sr, df)
		if (level.lo > 0) {
			ci.lo <- qlambdap(level.lo,df-1,tstat,lower.tail=TRUE)
		} else ci.lo <- -Inf
		if (level.hi < 1) {
			ci.hi <- qlambdap(level.hi,df-1,tstat,lower.tail=TRUE)
		} else ci.hi <- Inf
	} else if (type == "t") {
		# already annualized;
		se <- sr.se(sr,df,type=type)
		midp <- sr
		zalp <- qnorm(c(level.lo,level.hi))
		ci <- midp + zalp * se
	} else if (type == "Z") {
		# already annualized;
		se <- sr.se(sr,df,type=type)
		midp <- sr * (1 - 1 / (4 * (df - 1)))
		zalp <- qnorm(c(level.lo,level.hi))
		ci <- midp + zalp * se
	} else if (type == "F") {
		# already annualized;
		se <- sr.se(sr,df,type=type)
		cn <- .srbias(df)
		midp <- sr / cn
		zalp <- qnorm(c(level.lo,level.hi))
		ci <- midp + zalp * se
	} else stop("internal error")

	retval <- matrix(ci,nrow=1)
	colnames(retval) <- sapply(c(level.lo,level.hi),function(x) { sprintf("%g %%",100*x) })
	return(retval)
}
											 

# 2FIX: start here:

#UNFOLD

# point inference on srstar/ncp of F#FOLDUP

# compute an unbiased estimator of the non-centrality parameter
.F_ncp_unbiased <- function(Fs,df1,df2) {
	ncp.unb <- (Fs * (df2 - 2) * df1 / df2) - df1
	return(ncp.unb)
}
#MLE of the ncp based on a single F-stat
.F_ncp_MLE <- function(Fs,df1,df2,ub=NULL,lb=0) {
	if (Fs <= 1) { return(0.0) }  # Spruill's Thm 3.1, eqn 8
	max.func <- function(z) { df(Fs,df1,df2,ncp=z,log=TRUE) }

	if (is.null(ub)) {
		prevdpf <- -Inf
		ub <- 1
		dpf <- max.func(ub)
		while (prevdpf < dpf) {
			prevdpf <- dpf
			ub <- 2 * ub
			dpf <- max.func(ub)
		}
		lb <- ifelse(ub > 2,ub/4,lb)
	}
	ncp.MLE <- optimize(max_func,c(lb,ub),maximum=TRUE)$maximum;
	return(ncp.MLE)
}
# KRS estimator of the ncp based on a single F-stat
.F_ncp_KRS <- function(Fs,df1,df2) {
	xbs <- Fs * (df1/df2)
	delta0 <- (df2 - 2) * xbs - df1
	phi2 <- 2 * xbs * (df2 - 2) / (df1 + 2)
	delta2 <- max(delta0,phi2)
	return(delta2)
}

#' @title Inference on noncentrality parameter of observed F statistic
#'
#' @description 
#'
#' Estimates the non-centrality parameter associated with an observed
#' statistic following a (non-central) F distribution.
#'
#' @details 
#'
#' Let \eqn{F}{F} be an observed statistic distributed as a non-central F with 
#' \eqn{\nu_1}{df1}, \eqn{\nu_2}{df2} degrees of freedom and non-centrality 
#' parameter \eqn{\delta^2}{delta^2}. Three methods are presented to
#' estimate the non-centrality parameter from the statistic:
#'
#' \itemize{
#' \item an unbiased estimator, which, unfortunately, may be negative.
#' \item the Maximum Likelihood Estimator, which may be zero, but not
#' negative.
#' \item the estimator of Kubokawa, Roberts, and Shaleh (KRS), which
#' is a shrinkage estimator.
#' }
#'
#' Since a Hotelling distribution is equivalent to the F-distribution
#' up to scaling, the same estimators can be used to estimate the 
#' non-centrality parameter of a non-central Hotelling T-squared statistic.
#'
#' @usage
#'
#' F_ncp_est(Fs, df1, df2, type=c("KRS","MLE","unbiased"))
#'
#' T2_ncp_est(T2,df1,df2,...) 
#'
#' @param Fs a (non-central) F statistic.
#' @param T2 a (non-central) Hotelling T-squared statistic.
#' @param df1 the numerator degrees of freedom.
#' @param df2 the denominator degrees of freedom.
#' @param type the estimator type. one of \code{"KRS", "MLE", "unbiased"}
#' @keywords htest
#' @return an estimate of the non-centrality parameter.
#' @aliases T2_ncp_est
#' @seealso F-distribution functions, \code{\link{df}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @references 
#'
#' Kubokawa, T., C. P. Robert, and A. K. Saleh. "Estimation of noncentrality parameters." 
#' Canadian Journal of Statistics 21, no. 1 (1993): 45-57. \url{http://www.jstor.org/stable/3315657}
#'
#' Spruill, M. C. "Computation of the maximum likelihood estimate of a noncentrality parameter." 
#' Journal of multivariate analysis 18, no. 2 (1986): 216-224.
#' \url{http://www.sciencedirect.com/science/article/pii/0047259X86900709}
#'
#' @examples 
#' rvs <- rf(1024, 4, 1000, 5)
#' unbs <- F_ncp_est(rvs, 4, 1000, type="unbiased")
#'
F_ncp_est <- function(Fs,df1,df2,type=c("KRS","MLE","unbiased")) {
	# type defaults to "KRS":
	type <- match.arg(type)
	Fncp <- switch(type,
								 MLE = .F_ncp_MLE(Fs,df1,df2),
								 KRS = .F_ncp_KRS(Fs,df1,df2),
								 unbiased = .F_ncp_unbiased(Fs,df1,df2))
	return(Fncp)
}
#' @export 
T2_ncp_est <- function(T2,df1,df2,...) {
	Fs <- .T2_to_F(T2, df1, df2)
	Fdf1 <- df1
	Fdf2 <- df2 - df1
	retv <- F_ncp_est(Fs,Fdf1,Fdf2,...)
	# the NCP is legit
	retv <- Fncp
	return(retv)
}
#UNFOLD


# extract statistics (t-stat) from lm object:
# https://stat.ethz.ch/pipermail/r-help/2009-February/190021.html
#
# also: 
# see the code in summary.lm to see how sigma is calculated
# or
# sigma <- sqrt(deviance(fit) / df.residual(fit))
# then base on confint? coef/vcov
#
# junkyard


#t_power_rule <- function(n,alpha = 0.05,beta = 0.20,



########################################################################
#inversions#FOLDUP

#
#
#find the non-centrality parameter; that is, find
#ncp such that pt(t,df,ncp) = alpha
f_nct_cdf_ncp <- function(t,df,alpha) {
	#find approximate endpoints;
	flo <- min(-1,t - qnorm((1 + alpha)/2))
	fhi <- max(1,t - qnorm((alpha)/2))
	#expand them until pt(t,df,flo) > alpha and pt(t,df,fhi) < alpha
	while ((pt(t,df,flo) < alpha) && (flo > -100000)) { flo <- 2 * flo }
	while ((pt(t,df,fhi) > alpha) && (fhi < 100000)) { fhi <- 2 * fhi }
	ncp <- uniroot(function(ncp,t,df,alpha)(pt(t,df,ncp) - alpha),
								 c(flo,fhi),t = t,df = df,alpha = alpha)
	return(ncp$root)
}

#find some t such that
# phi(t-ncp) = f(t;df,ncp) 
#where phi is the density function of the normal
#and f(x;df,ncp) is the density function of a noncentral
#t-distribution with n d.o.f. and noncentrality parameter ncp

f_eq_t_pdf_disc <- function(df,ncp = 0) {
	lims <- c(-abs(ncp) - 20,ncp)
	foo <- uniroot(function(t,ncp,df)(dnorm(t,mean = ncp,sd=1) -
																		dt(t,df=df,ncp=ncp)),
								 lims,df = df,ncp = ncp)
	return(foo$root)
}

#find 
# max_t | Phi(t-ncp) - F(t;df,ncp) |
#where Phi is the distribution function of the normal
#and F(x;df,ncp) is the distribution function of a noncentral
#t-distribution with n d.o.f. and noncentrality parameter ncp

f_max_t_cdf_disc <- function(df,ncp = 0) {
	opt_t <- f_eq_t_pdf_disc(df=df,ncp=ncp)
	disc <- abs(pnorm(opt_t,mean = ncp,sd=1) -
							pt(opt_t,df=df,ncp=ncp))
	return(disc)
}
#UNFOLD

#compute the asymptotic mean and variance of the sqrt of a
#non-central F distribution

f_sqrt_ncf_asym_mu <- function(df1,df2,ncp = 0) {
	return(sqrt((df2 / (df2 - 2)) * (df1 + ncp) / df1))
}
f_sqrt_ncf_asym_var <- function(df1,df2,ncp = 0) {
	return((1 / (2 * df1)) * 
				 (((df1 + ncp) / (df2 - 2)) + (2 * ncp + df1) / (df1 + ncp)))
}
f_sqrt_ncf_apx_pow <- function(df1,df2,ncp,alpha = 0.05) {
	zalp <- qnorm(1 - alpha)
	numr <- 1 - f_sqrt_ncf_asym_mu(df1,df2,ncp = ncp) + zalp / sqrt(2 * df1)
	deno <- sqrt(f_sqrt_ncf_asym_var(df1,df2,ncp = ncp))
	return(1 - pnorm(numr / deno))
}


# to get a hotelling statistic from n x k matrix x:
# myt <- summary(manova(lm(x ~ 1)),test="Hotelling-Lawley",intercept=TRUE)
#              Df Hotelling-Lawley approx F num Df den Df Pr(>F)
#(Intercept)   1          0.00606     1.21      5    995    0.3
#
# HLT <- myt$stats[1,"Hotelling-Lawley"]
#
# myt <- summary(manova(lm(x ~ 1)),intercept=TRUE)
# HLT <- sum(myt$Eigenvalues) #?
# ...
# 


	


########################################################################
# confidence intervals

# inference on F's ncp#FOLDUP

# confidence distribution, gives CIs
qcofncp <- function(p,Fs,df1,df2,ub=NULL) {
	# return max{0 <= ncp <= ub | pf(Fs,df1,df2,ncp) >= 1 - p}
	# or 0 if none exist
	f.zer <- pf(Fs,df1,df2,0)
	if (f.zer < (1-p)) {
		return(0)
	} else {
		if (is.null(ub)) {
			ub <- 1.0
			fpf <- pf(Fs,df1,df2,ub)
			while (fpf >= (1-p)) {
				ub <- 2.0 * ub
				fpf <- pf(Fs,df1,df2,ub)
			}
			lb <- 0.5 * ub
		} else {
			lb <- 0
			fpf <- pf(Fs,df1,df2,ub)
		}
		# now call uniroot
		zerf <- function(z,n,xv,limv) { pf(xv,p,n-p,n*z^2) - limv }
		ncp <- uniroot(function(ncp,Fs,df1,df2,tgt){pf(Fs,df1,df2,ncp) - tgt},
									 c(lb,ub),Fs=Fs,df1=df1,df2=df2,tgt=1-p)$root
		return(ncp)
	}
}

# use same to construct confidence intervals
fncp.ci <- function(F,df1,df2,alpha.lo=0.025,alpha.hi=1-alpha.lo) {
	if (alpha.hi >= 1) {
		cihi <- Inf
	} else {
		cihi <- qcofncp(alpha.hi,F,df1,df2)
	}

	if (alpha.lo <= 0) {
		cilo <- 0
	} else {
		if (is.finite(cihi)) {
			cilo <- qcofncp(alpha.lo,F,df1,df2,ub=cihi)
		} else {
			cilo <- qcofncp(alpha.lo,F,df1,df2)
		}
	}
	return(list('lo' = cilo,'hi' = cihi))
}

#UNFOLD

# inference on Hotelling's ncp, by extension#FOLDUP

# confidence distribution, gives CIs
qcoT2ncp <- function(plev,T2,p,n,ub=NULL) {
	# convert to F
	Fs <- f_hot2F(T2=T2,p=p,n=n)
	if (!is.null(ub)) {
		ub <- f_hot2F(T2=ub,p=p,n=n)
	}
	# delegate
	F.ncp <- qcofncp(plev,Fs,df1=p,df2=n-p,ub=ub)
	# they have the same ncp; no back conversion
	return(F.ncp)
}

# use same to construct confidence intervals
T2ncp.ci <- function(T2,p,n,alpha.lo=0.025,alpha.hi=1-alpha.lo) {
	# convert to F
	Fs <- f_hot2F(T2=T2,p=p,n=n)
	# delegate
	F.ci <- fncp.ci(Fs,df1=p,df2=n-p,alpha.lo=alpha.lo,alpha.hi=alpha.hi)
	# they have the same ncp; no back conversion
	return(F.ci)
}

#MLE of the ncp
T2ncp.mle <- function(T2,p,n,ub=NULL) {
	# convert to F
	Fs <- f_hot2F(T2=T2,p=p,n=n)
	if (!is.null(ub)) {
		ub <- f_hot2F(T2=ub,p=p,n=n)
	}
	# delegate
	F.mle <- fncp.mle(Fs,df1=p,df2=n-p,ub=ub)
	# they have the same ncp; no back conversion
	return(F.mle)
}
#UNFOLD

# SR^* is a Hotelling, basically. 

# convert SR^* <-> T2
f_srstar2hot <- function(sample.sr,n) {
	return(n * sample.sr^2)
}
f_hot2srstar <- function(T2,n) {
	return(sqrt(T2 / n))
}

# inference on SR^*'s ncp, by extension#FOLDUP

# confidence distribution, gives CIs
qcosrstarncp <- function(plev,srs,p,n,ub=NULL) {
	# convert to T2
	T2 <- f_srstar2hot(sample.sr=srs,n=n)
	if (!is.null(ub)) {
		ub <- f_srstar2hot(sample.sr=ub,n=n)
	}
	# delegate
	T2.ncp <- qcoT2ncp(plev,T2,p=p,n=n,ub=ub)
	# convert back
	return(f_hot2srstar(T2.ncp,n=n))
}

# use same to construct confidence intervals
srstarncp.ci <- function(srs,p,n,alpha.lo=0.025,alpha.hi=1-alpha.lo) {
	# convert to T2
	T2 <- f_srstar2hot(sample.sr=srs,n=n)
	# delegate
	T2.ci <- T2ncp.ci(T2,p=p,n=n,alpha.lo=alpha.lo,alpha.hi=alpha.hi)
	# convert back
	return(list('lo' = f_hot2srstar(T2.ci$lo,n=n),'hi' = f_hot2srstar(T2.ci$hi,n=n)))
}

#MLE of the ncp
srstarncp.mle <- function(srs,p,n,ub=NULL) {
	# convert to T2
	T2 <- f_srstar2hot(sample.sr=srs,n=n)
	if (!is.null(ub)) {
		ub <- f_srstar2hot(sample.sr=ub,n=n)
	}
	# delegate
	T2.mle <- T2ncp.mle(T2,p=p,n=n,ub=ub)
	# convert back
	return(f_hot2srstar(T2.mle,n=n))
}
#UNFOLD

## confidence intervals on optimal Sharpe#FOLDUP
#2FIX: export the guts of this...
#(1 - alpha) confidence interval on optimal SNR under assumption that portfolio
#optimizes in-sample Sharpe ratio, with n observations on p assets.
f_srstar_ci <- function(sample.sr,n,p,alpha = 0.05) {
	xval <- (n * (n-p) / (p * (n-1))) * sample.sr^2
	zerf <- function(z,n,xv,limv) { pf(xv,p,n-p,n*z^2) - limv }

	pfzero <- pf(xval, p, n-p, 0)
	if (pfzero < alpha / 2) {
		cihi <- 0
		cilo <- 0
	} else {
		#approximate upper bound 
		up <- 2 * (sample.sr + pnorm(1 - alpha / 4) / sqrt(2 * n))
		fup <- zerf(up,n,xv = xval,limv = 1 - alpha/2)
		while (fup < 0) {
			up <- 2 * up;
			fup <- zerf(up,n,xv = xval,limv = 1 - alpha/2)
		}

		if (pfzero < 1 - alpha / 2) {
			cilo <- 0
		} else {
			cilo <- uniroot(zerf,
											c(0,up),n = n,xv = xval,limv = 1 - alpha/2)$root
		}
		cihi <- uniroot(zerf,
										c(cilo,up),n = n,xv = xval,limv = alpha/2)$root
	}
	return(list('lo' = cilo,'hi' = cihi))
}
#UNFOLD

# convert SR^* <-> T2
f_srstar2hot <- function(sample.sr,n) {
	return(n * sample.sr^2)
}
f_hot2srstar <- function(T2,n) {
	return(sqrt(T2 / n))
}


# standard errors and confidence intervals
#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:

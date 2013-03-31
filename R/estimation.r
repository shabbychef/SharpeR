# Copyright 2012 Steven E. Pav. All Rights Reserved.
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

#' @include utils.r
#' @include distributions.r

# note: on citations, use the Chicago style from google scholar. tks.

########################################################################
# Estimation 
########################################################################
# Sharpe Ratio#FOLDUP
#' @title Compute the Sharpe ratio.
#'
#' @description 
#'
#' Computes the Sharpe ratio of some observed returns.
#'
#' @details
#'
#' Suppose \eqn{x_i}{xi} are \eqn{n}{n} independent returns of some
#' asset.
#' Let \eqn{\bar{x}}{xbar} be the sample mean, and \eqn{s}{s} be
#' the sample standard deviation (using Bessel's correction). Let \eqn{c_0}{c0}
#' be the 'risk free rate'.  Then
#' \deqn{z = \frac{\bar{x} - c_0}{s}}{z = (xbar - c0)/s} 
#' is the (sample) Sharpe ratio.
#' 
#' The units of \eqn{z}{z} are \eqn{\mbox{time}^{-1/2}}{per root time}.
#' Typically the Sharpe ratio is \emph{annualized} by multiplying by
#' \eqn{\sqrt{\mbox{opy}}}{sqrt(opy)}, where \eqn{\mbox{opy}}{opy} 
#' is the number of observations
#' per year (or whatever the target annualization epoch.)
#'
#' @usage
#'
#' full.sr(x,c0=0,opy=1,na.rm=FALSE)
#'
#' sr(x,...)
#'
#' @param x vector of returns.
#' @param c0 the 'risk-free' or 'disastrous' rate of return. this is
#'        assumed to be given in the same units as x, \emph{not}
#'        in 'annualized' terms.
#' @param opy the number of observations per 'year'. This is used to
#'        'annualize' the answer.
#' @param na.rm logical.  Should missing values be removed?
#' @param ... the above extra parameters,  passed on to \code{full.sr}
#' @keywords univar 
#' @return \code{full.sr} returns a list with containing the following components:
#' \item{sr}{the annualized Sharpe ratio.}
#' \item{df}{the number of observations.}
#' \code{sr} just returns the numeric annualized Sharpe.
#' @aliases sr
#' @seealso sr-distribution functions, \code{\link{dsr}, \link{psr}, \link{qsr}, \link{rsr}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @family sr
#' @references 
#'
#' Sharpe, William F. "Mutual fund performance." Journal of business (1966): 119-138.
#' \url{http://ideas.repec.org/a/ucp/jnlbus/v39y1965p119.html}
#' 
#' Lo, Andrew W. "The statistics of Sharpe ratios." Financial Analysts Journal (2002): 36-52.
#' \url{http://ssrn.com/paper=377260}
#'
#' @examples 
#' rvs <- full.sr(rnorm(253*8),opy=253)
#' rvs <- sr(rnorm(253*8),opy=253)
#'
full.sr <- function(x,c0=0,opy=1,na.rm=FALSE) {
	sr <- (mean(x,na.rm=na.rm) - c0) / sd(x,na.rm=na.rm)
	if (!missing(opy))
		sr <- .annualize(sr,opy)
	df <- ifelse(na.rm,sum(!is.na(x)),length(x))
	#units(sr) <- "yr^-0.5"
	retval <- list(sr = sr,df = df,c0 = c0,opy = opy)
	return(retval)
}
#' @export 
sr <- function(x,...) {
	# delegate
	subret <- full.sr(x,...)
	return(subret$sr)
}

# compute the markowitz portfolio
.markowitz <- function(X,mu=NULL,Sigma=NULL) {
	na.omit(X)
	if (is.null(mu)) 
		mu <- colMeans(X)
	if (is.null(Sigma)) 
		Sigma <- cov(X)
	w <- solve(Sigma,mu)
	n <- dim(X)[1]
	retval <- list(w = w, mu = mu, Sigma = Sigma, df1 = length(w), df2 = n)
	return(retval)
}

# compute Hotelling's statistic.
.hotelling <- function(X) {
	retval <- .markowitz(X)
	retval$T2 <- retval$df2 * (retval$mu %*% retval$w)
	return(retval)
}

#' @title Compute the Sharpe ratio of the Markowitz portfolio.
#'
#' @description 
#'
#' Computes the Sharpe ratio of the Markowitz portfolio of some observed returns.
#'
#' @details
#' 
#' Suppose \eqn{x_i}{xi} are \eqn{n}{n} independent draws of a \eqn{q}{q}-variate
#' normal random variable with mean \eqn{\mu}{mu} and covariance matrix
#' \eqn{\Sigma}{Sigma}. Let \eqn{\bar{x}}{xbar} be the (vector) sample mean, and 
#' \eqn{S}{S} be the sample covariance matrix (using Bessel's correction). Let
#' \deqn{\zeta(w) = \frac{w^{\top}\bar{x} - c_0}{\sqrt{w^{\top}S w}}}{zeta(w) = (w'xbar - c0)/sqrt(w'Sw)}
#' be the (sample) Sharpe ratio of the portfolio \eqn{w}{w}, subject to 
#' risk free rate \eqn{c_0}{c0}.
#'
#' Let \eqn{w_*}{w*} be the solution to the portfolio optimization problem:
#' \deqn{\max_{w: 0 < w^{\top}S w \le R^2} \zeta(w),}{max {zeta(w) | 0 < w'Sw <= R^2},}
#' with maximum value \eqn{z_* = \zeta\left(w_*\right)}{z* = zeta(w*)}.
#' Then 
#' \deqn{w_* = R \frac{S^{-1}\bar{x}}{\sqrt{\bar{x}^{\top}S^{-1}\bar{x}}}}{%
#' w* = R S^-1 xbar / sqrt(xbar' S^-1 xbar)}
#' and
#' \deqn{z_* = \sqrt{\bar{x}^{\top} S^{-1} \bar{x}} - \frac{c_0}{R}}{%
#' z* = sqrt(xbar' S^-1 xbar) - c0/R}
#'
#' The units of \eqn{z_*}{z*} are \eqn{\mbox{time}^{-1/2}}{per root time}.
#' Typically the Sharpe ratio is \emph{annualized} by multiplying by
#' \eqn{\sqrt{\mbox{opy}}}{sqrt(opy)}, where \eqn{\mbox{opy}}{opy} 
#' is the number of observations
#' per year (or whatever the target annualization epoch.)
#'
#' @usage
#'
#' full.sropt(X,drag=0,opy=1)
#'
#' sropt(X,...)
#'
#' @param X matrix of returns.
#' @param drag the 'drag' term, \eqn{c_0/R}{c0/R}. defaults to 0. It is assumed
#'        that \code{drag} has been annualized, \emph{i.e.} has been multiplied
#'        by \eqn{\sqrt{opy}}{sqrt(opy)}. This is in contrast to the \code{c0}
#'        term given to \code{\link{sr}}.
#' @param opy the number of observations per 'year'. The returns are observed
#'        at a rate of \code{opy} per 'year.' default value is 1, meaning no 
#'        annualization is performed.
#' @param ... the above extra parameters,  passed on to \code{full.sropt}
#' @keywords univar 
#' @return A list with containing the following components:
#' \item{w}{the optimal portfolio.}
#' \item{mu}{the estimated mean return vector.}
#' \item{Sigma}{the estimated covariance matrix.}
#' \item{df1}{the number of assets.}
#' \item{df2}{the number of observed vectors.}
#' \item{T2}{the Hotelling \eqn{T^2} statistic.}
#' \item{sropt}{the maximal Sharpe statistic.}
#' \item{drag}{the input \code{drag} term.}
#' \item{opy}{the input \code{opy} term.}
#' @aliases sropt
#' @seealso \code{\link{full.sr}}, sropt-distribution functions, 
#' \code{\link{dsropt}, \link{psropt}, \link{qsropt}, \link{rsropt}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @family sropt
#' @examples 
#' rvs <- full.sropt(matrix(rnorm(253*8*4),ncol=4),drag=0,opy=253)
#' rvs <- sropt(matrix(rnorm(253*8*4),ncol=4),drag=0,opy=253)
#'
full.sropt <- function(X,drag=0,opy=1) {
	retval <- .hotelling(X)
	zeta.star <- sqrt(retval$T2 / retval$df2)
	if (!missing(opy))
		zeta.star <- .annualize(zeta.star,opy)
	retval$sropt <- zeta.star - drag

	#units(retval$sropt) <- "yr^-0.5"
	retval$drag <- drag
	retval$opy <- opy
	return(retval)
}
#' @export 
sropt <- function(X,...) {
	subret <- full.sropt(X,...)
	return(subret$sropt)
}


#UNFOLD

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
#' 2FIX; document
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
#' sr.se(z,df,opy,type=c("t","Lo","Z","F")) 
#'
#' @param z an observed Sharpe ratio statistic, annualized.
#' @param df the number of observations the statistic is based on. This 
#'        is one more than the number of degrees of freedom in the
#'        corresponding t-statistic, although the effect will be small
#'        when \code{df} is large.
#' @param opy the number of observations per 'year'. \code{z}, and \code{zeta}
#'        are quoted in 'annualized' units, that is, per square root 
#'        'year', but returns are observed possibly at a rate of \code{opy} per 
#'        'year.' default value is 1, meaning no deannualization is performed.
#' @param type the estimator type. one of \code{"t", "Lo", "Z", "F"}
#' @keywords htest
#' @return an estimate of standard error.
#' @seealso sr-distribution functions, \code{\link{dsr}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @family sr
#' @note
#' Eventually this should include corrections for autocorrelation, skew,
#' kurtosis.
#' @references 
#'
#' Walck, C. "Hand-book on STATISTICAL DISTRIBUTIONS for experimentalists."
#' 1996. \url{http://www.stat.rice.edu/~dobelman/textfiles/DistributionsHandbook.pdf}
#'
#' Johnson, N. L., and Welch, B. L. "Applications of the non-central t-distribution."
#' Biometrika 31, no. 3-4 (1940): 362-389. \url{http://dx.doi.org/10.1093/biomet/31.3-4.362}
#'
#' Lo, Andrew W. "The statistics of Sharpe ratios." Financial Analysts Journal 58, no. 4 
#' (2002): 36-52. \url{http://ssrn.com/paper=377260}
#'
#' Opdyke, J. D. "Comparing Sharpe Ratios: So Where are the p-values?" Journal of Asset
#' Management 8, no. 5 (2006): 308-336. \url{http://ssrn.com/paper=886728}
#'
#' @examples 
#' opy <- 253
#' df <- opy * 6
#' rvs <- rsr(1, df, 1.0, opy)
#' anse <- sr.se(rvs,df,opy,type="t")
#' anse2 <- sr.se(rvs,df,opy,type="Z")
#'
#'@export
sr.se <- function(z,df,opy,type=c("t","Lo","Z","F")) { 
	# 2FIX: add opdyke corrections for skew and kurtosis?
	# 2FIX: add autocorrelation correction?
	if (!missing(opy)) {
		z <- .deannualize(z,opy)
	}
	type <- match.arg(type)
	se <- switch(type,
							 t = .sr_se_lo(z,df,ss.adjust=TRUE),
							 Lo = .sr_se_lo(z,df,ss.adjust=FALSE),
							 Z = .sr_se_walck(z,df),
							 F = .sr_se_weirdo(z,df))
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
#' Constructs confidence intervals on the Signal-Noise ratio given observed
#' Sharpe ratio statistic. The available methods are:
#'
#' \itemize{
#' \item The default, \code{exact}, which is only exact when returns are
#' normal, based on inverting the non-central t
#' distribution.
#' \item A method based on the standard error of a non-central t distribution.
#' \item A method based on a normal approximation.
#' \item A method based on an F statistic.
#' }
#'
#' @usage
#'
#' sr.confint(z,df,level=0.95,type=c("exact","t","Z","F"),opy=1,
#'            level.lo=(1-level)/2,level.hi=1-level.lo)
#'
#' @param z an observed Sharpe ratio statistic, annualized.
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
#' level.lo and level.hi in \%, \emph{e.g.} \code{"2.5 \%"}
#' @seealso \code{\link{confint}}, \code{\link{sr.se}}, \code{\link{qlambdap}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @family sr
#' @examples 
#' opy <- 253
#' df <- opy * 6
#' rvs <- rsr(1, df, 1.0, opy)
#' aci <- sr.confint(rvs,df,type="t",opy=opy)
#' aci2 <- sr.confint(rvs,df,type="Z",opy=opy)
#'
#'@export
sr.confint <- function(z,df,level=0.95,type=c("exact","t","Z","F"),
											 opy=1,level.lo=(1-level)/2,level.hi=1-level.lo) {
	#2FIX: the order of arguments is really wonky. where does opy go?
	if (!missing(opy)) {
		z <- .deannualize(z,opy)
	}
	type <- match.arg(type)
	if  (type == "exact") {
		tstat <- .sr_to_t(z, df)
		ci.lo <- qlambdap(level.lo,df-1,tstat,lower.tail=TRUE)
		ci.hi <- qlambdap(level.hi,df-1,tstat,lower.tail=TRUE)
		ci <- c(ci.lo,ci.hi)
	} else if (type == "t") {
		# already annualized;
		se <- sr.se(z,df,type=type)
		midp <- z
		zalp <- qnorm(c(level.lo,level.hi))
		ci <- midp + zalp * se
	} else if (type == "Z") {
		# already annualized;
		se <- sr.se(z,df,type=type)
		midp <- z * (1 - 1 / (4 * (df - 1)))
		zalp <- qnorm(c(level.lo,level.hi))
		ci <- midp + zalp * se
	} else if (type == "F") {
		# already annualized;
		se <- sr.se(z,df,type=type)
		cn <- .srbias(df)
		midp <- z / cn
		zalp <- qnorm(c(level.lo,level.hi))
		ci <- midp + zalp * se
	} else stop("internal error")

	retval <- matrix(ci,nrow=1)
	colnames(retval) <- sapply(c(level.lo,level.hi),function(x) { sprintf("%g %%",100*x) })
	return(retval)
}
											 
#' @title Confidence Interval on Maximal Signal-Noise Ratio
#'
#' @description 
#'
#' Computes approximate confidence intervals on the Signal-Noise ratio given the Sharpe ratio.
#'
#' @details 
#'
#' Suppose \eqn{x_i}{xi} are \eqn{n}{n} independent draws of a \eqn{q}{q}-variate
#' normal random variable with mean \eqn{\mu}{mu} and covariance matrix
#' \eqn{\Sigma}{Sigma}. Let \eqn{\bar{x}}{xbar} be the (vector) sample mean, and 
#' \eqn{S}{S} be the sample covariance matrix (using Bessel's correction). 
#' Let 
#' \deqn{z_* = \sqrt{\bar{x}^{\top} S^{-1} \bar{x}}}{z* = sqrt(xbar' S^-1 xbar)}
#' Given observations of \eqn{z_*}{z*}, compute confidence intervals on the
#' population analogue, defined as
#' \deqn{\zeta_* = \sqrt{\mu^{\top} \Sigma^{-1} \mu}}{zeta* = sqrt(mu' Sigma^-1 mu)}
#'
#' @usage
#'
#' sropt.confint(z.s,df1,df2,level=0.95,
#'                opy=1,level.lo=(1-level)/2,level.hi=1-level.lo)
#'
#' @param z.s an observed Sharpe ratio statistic, annualized.
#' @inheritParams qco_sropt
#' @inheritParams dsropt
#' @inheritParams qsropt
#' @inheritParams psropt
#' @param level the confidence level required.
#' @param opy the number of observations per 'year'. \code{x}, \code{q}, and 
#'        \code{snr} are quoted in 'annualized' units, that is, per square root 
#'        'year', but returns are observed possibly at a rate of \code{opy} per 
#'        'year.' default value is 1, meaning no deannualization is performed.
#' @param level.lo the lower bound for the confidence interval.
#' @param level.hi the upper bound for the confidence interval.
#' @keywords htest
#' @return A matrix (or vector) with columns giving lower and upper
#' confidence limits for the SNR. These will be labelled as
#' level.lo and level.hi in \%, \emph{e.g.} \code{"2.5 \%"}
#' @seealso \code{\link{confint}}, \code{\link{sr.confint}}, \code{\link{qco_sropt}}, \code{\link{sropt.test}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @family sropt
#' @examples 
#' opy <- 253
#' df <- opy * 6
#' rvs <- rsr(1, df, 1.0, opy)
#' aci <- sr.confint(rvs,df,opy=opy)
#'
#'@export
sropt.confint <- function(z.s,df1,df2,level=0.95,
											     opy=1,level.lo=(1-level)/2,level.hi=1-level.lo) {
	#2FIX: the order of arguments is really wonky. where does opy go?
	#if (!missing(opy)) {
		#z.s <- .deannualize(z.s,opy)
	#}

	ci.hi <- qco_sropt(level.hi,df1=df1,df2=df2,z.s=z.s,opy=opy,lower.tail=TRUE)
	ci.lo <- qco_sropt(level.lo,df1=df1,df2=df2,z.s=z.s,opy=opy,lower.tail=TRUE,ub=ci.hi)
	ci <- c(ci.lo,ci.hi)

	retval <- matrix(ci,nrow=1)
	colnames(retval) <- sapply(c(level.lo,level.hi),function(x) { sprintf("%g %%",100*x) })
	return(retval)
}

#UNFOLD

# point inference on sropt/ncp of F#FOLDUP

# compute an unbiased estimator of the non-centrality parameter
.F_ncp_unbiased <- function(Fs,df1,df2) {
	ncp.unb <- (Fs * (df2 - 2) * df1 / df2) - df1
	return(ncp.unb)
}

#MLE of the ncp based on a single F-stat
.F_ncp_MLE_single <- function(Fs,df1,df2,ub=NULL,lb=0) {
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
	ncp.MLE <- optimize(max.func,c(lb,ub),maximum=TRUE)$maximum;
	return(ncp.MLE)
}
.F_ncp_MLE <- Vectorize(.F_ncp_MLE_single,
											vectorize.args = c("Fs","df1","df2"),
											SIMPLIFY = TRUE)

# KRS estimator of the ncp based on a single F-stat
.F_ncp_KRS <- function(Fs,df1,df2) {
	xbs <- Fs * (df1/df2)
	delta0 <- (df2 - 2) * xbs - df1
	phi2 <- 2 * xbs * (df2 - 2) / (df1 + 2)
	delta2 <- pmax(delta0,phi2)
	return(delta2)
}

#' @export 
F.inference <- function(Fs,df1,df2,type=c("KRS","MLE","unbiased")) {
	# type defaults to "KRS":
	type <- match.arg(type)
	Fncp <- switch(type,
								 MLE = .F_ncp_MLE(Fs,df1,df2),
								 KRS = .F_ncp_KRS(Fs,df1,df2),
								 unbiased = .F_ncp_unbiased(Fs,df1,df2))
	return(Fncp)
}
#' @export 
T2.inference <- function(T2,df1,df2,...) {
	Fs <- .T2_to_F(T2, df1, df2)
	Fdf1 <- df1
	Fdf2 <- df2 - df1
	retv <- F.inference(Fs,Fdf1,Fdf2,...)
	# the NCP is legit
	retv <- retv
	return(retv)
}
#' @title Inference on noncentrality parameter of F or F-like statistic 
#'
#' @description 
#'
#' Estimates the non-centrality parameter associated with an observed
#' statistic following a (non-central) F, \eqn{T^2}, or sropt distribution. 
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
#' The sropt distribution is equivalent to a Hotelling up to a 
#' square root and some rescalings. 
#' 
#' The non-centrality parameter of the sropt distribution is 
#' the square root of that of the Hotelling, \emph{i.e.} has
#' units 'per square root time'. As such, the \code{'unbiased'}
#' type can be problematic!
#'
#' @usage
#'
#' F.inference(Fs, df1, df2, type=c("KRS","MLE","unbiased"))
#'
#' T2.inference(T2,df1,df2,...) 
#'
#' sropt.inference(z.s,df1,df2,opy=1,drag=0,...)
#'
#' @param Fs a (non-central) F statistic.
#' @param T2 a (non-central) Hotelling \eqn{T^2} statistic.
#' @param z.s an observed Sharpe ratio statistic, annualized.
#' @inheritParams qco_sropt
#' @inheritParams dsropt
#' @inheritParams qsropt
#' @inheritParams psropt
#' @param type the estimator type. one of \code{c("KRS", "MLE", "unbiased")}
#' @param opy the number of observations per 'year'. \code{z.s} is  
#'        assumed given in 'annualized' units, that is, per 'year',
#'        but returns are observed possibly at a rate of \code{opy} per 
#'        'year.' default value is 1, meaning no deannualization is performed.
#' @param drag the 'drag' term, \eqn{c_0/R}{c0/R}. defaults to 0. It is assumed
#'        that \code{drag} has been annualized, \emph{i.e.} is given in the
#'        same units as \code{z.s}.
#' @param ... the \code{type} which is passed on to \code{F.inference}.
#' @keywords htest
#' @return an estimate of the non-centrality parameter.
#' @aliases F.inference T2.inference sropt.inference
#' @seealso F-distribution functions, \code{\link{df}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @family sropt Hotelling
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
#' unbs <- F.inference(rvs, 4, 1000, type="unbiased")
#' # generate some sropts
#' true.snrstar <- 1.25
#' df1 <- 6
#' df2 <- 2000
#' opy <- 253
#' rvs <- rsropt(500, df1, df2, true.snrstar, opy)
#' est1 <- sropt.inference(rvs,df1,df2,opy,type='unbiased')  
#' est2 <- sropt.inference(rvs,df1,df2,opy,type='KRS')  
#' est3 <- sropt.inference(rvs,df1,df2,opy,type='MLE')
#'
sropt.inference <- function(z.s,df1,df2,opy=1,drag=0,...) {
	if (!missing(drag) && (drag != 0)) 
		z.s <- z.s + drag
	if (!missing(opy)) 
		z.s <- .deannualize(z.s, opy)
	T2 <- .sropt_to_T2(z.s, df2)
	retval <- T2.inference(T2,df1,df2,...)
	# convert back
	retval <- .T2_to_sropt(retval, df2)
	if (!missing(opy)) 
		retval <- .annualize(retval, opy)
	if (!missing(drag) && (drag != 0)) 
		retval <- retval - drag
	return(retval)
}
#UNFOLD

# notes:
# extract statistics (t-stat) from lm object:
# https://stat.ethz.ch/pipermail/r-help/2009-February/190021.html
#
# also: 
# see the code in summary.lm to see how sigma is calculated
# or
# sigma <- sqrt(deviance(fit) / df.residual(fit))
# then base on confint? coef/vcov

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

## junkyard#FOLDUP

##compute the asymptotic mean and variance of the sqrt of a
##non-central F distribution

#f_sqrt_ncf_asym_mu <- function(df1,df2,ncp = 0) {
	#return(sqrt((df2 / (df2 - 2)) * (df1 + ncp) / df1))
#}
#f_sqrt_ncf_asym_var <- function(df1,df2,ncp = 0) {
	#return((1 / (2 * df1)) * 
				 #(((df1 + ncp) / (df2 - 2)) + (2 * ncp + df1) / (df1 + ncp)))
#}
#f_sqrt_ncf_apx_pow <- function(df1,df2,ncp,alpha = 0.05) {
	#zalp <- qnorm(1 - alpha)
	#numr <- 1 - f_sqrt_ncf_asym_mu(df1,df2,ncp = ncp) + zalp / sqrt(2 * df1)
	#deno <- sqrt(f_sqrt_ncf_asym_var(df1,df2,ncp = ncp))
	#return(1 - pnorm(numr / deno))
#}
##UNFOLD

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:

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
# SVN: $Id: blankheader.txt 25454 2012-02-14 23:35:25Z steven $

#' @include utils.r

# 2FIX: is df the # of observations or the d.f. of the t-stat? ack!

# note: on citations, use the Chicago style from google scholar. tks.

# the following are useful for grokking R
# showMethods("print") 
# getAnywhere("t.test.default")

########################################################################
# Distributions
########################################################################

# geometric bias in the expectation of the non-central t-stat with a
# given number of d.f.
# this is 1 over 'c4', essentially:
# http://mathworld.wolfram.com/StandardDeviationDistribution.html
# http://finzi.psych.upenn.edu/R/library/IQCC/html/c4.html
# http://math.stackexchange.com/questions/71573/the-limit-of-the-ratio-of-two-gammax-functions
.tbias <- function(df) { 
	retval <- sqrt(df / 2) * exp(lgamma((df-1)/2) - lgamma(df/2))
	return(retval)
}
# same, but for sr:
.srbias <- function(n) { 
	return(.tbias(n-1))
}

# Sharpe ratio as a distribution
# dsr, psr, qsr, rsr#FOLDUP
#' @title The (non-central) Sharpe ratio.
#'
#' @description 
#'
#' Density, distribution function, quantile function and random
#' generation for the Sharpe ratio distribution with \code{df} degrees of freedom
#' (and optional signal-noise-ratio \code{snr}).
#'
#' @details
#'
#' Suppose \eqn{x_i}{xi} are \eqn{n}{n} independent draws of a normal random
#' variable with mean \eqn{\mu}{mu} and variance \eqn{\sigma^2}{sigma^2}.
#' Let \eqn{\bar{x}}{xbar} be the sample mean, and \eqn{s}{s} be
#' the sample standard deviation (using Bessel's correction). Let \eqn{c_0}{c0}
#' be the 'risk free rate'.  Then
#' \deqn{z = \frac{\bar{x} - c_0}{s}}{z = (xbar - c0)/s} 
#' is the (sample) Sharpe ratio.
#' 
#' The units of \eqn{z}{z} is \eqn{\mbox{time}^{-1/2}}{per root time}.
#' Typically the Sharpe ratio is \emph{annualized} by multiplying by
#' \eqn{\sqrt{p}}{sqrt(p)}, where \eqn{p}{p} is the number of observations
#' per year (or whatever the target annualization epoch.)
#'
#' Letting \eqn{z = \sqrt{p}\frac{\bar{x}-c_0}{s}}{z = sqrt(p)(xbar - c0)/s},
#' where the sample estimates are based on \eqn{n}{n} observations, 
#' then \eqn{z}{z} takes a (non-central) Sharpe ratio distribution
#' with \eqn{n}{n} 'degrees of freedom', non-centrality parameter
#' \eqn{\delta = \frac{\mu - c_0}{\sigma}}{delta = (mu - c0)/sigma}, and 
#' annualization parameter \eqn{p}{p}.
#'
#' @usage
#'
#' dsr(x, df, snr, opy, log = FALSE)
#'
#' psr(q, df, snr, opy, ...)
#'
#' qsr(p, df, snr, opy, lower.tail = TRUE, log.p = FALSE) 
#'
#' rsr(n, df, snr, opy)
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. 
#' @param df the number of observations the statistic is based on. This 
#'        is one more than the number of degrees of freedom in the
#'        corresponding t-statistic, although the effect will be small
#'        when \code{df} is large.
#' @param snr the 'signal-to-noise' parameter, defined as the population
#'        mean divided by the population standard deviation, 'annualized'.
#' @param opy the number of observations per 'year'. \code{x}, \code{q}, and 
#'        \code{snr} are quoted in 'annualized' units, that is, per square root 
#'        'year', but returns are observed possibly at a rate of \code{opy} per 
#'        'year.' default value is 1, meaning no deannualization is performed.
#' @param log,log.p logical; if TRUE, probabilities p are given as \eqn{\mbox{log}(p)}{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are
#'        \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.
#' @param ... arguments passed on to the respective F distribution functions, namely
#' \code{lower.tail} with default \code{TRUE}, and \code{log.p}, with default \code{FALSE}.
#' @keywords distribution 
#' @return \code{dsr} gives the density, \code{psr} gives the distribution function,
#' \code{qsr} gives the quantile function, and \code{rsr} generates random deviates.
#'
#' Invalid arguments will result in return value \code{NaN} with a warning.
#' @aliases psr qsr rsr
#' @seealso t-distribution functions, \code{\link{dt}, \link{pt}, \link{qt}, \link{rt}}
#' @note
#' This is a thin wrapper on the t distribution. 
#' The functions \code{\link{dt}, \link{pt}, \link{qt}} can accept ncp from
#' limited range (\eqn{|\delta|\le 37.62}{delta <= 37.62}). Some corrections
#' may have to be made here for large \code{snr}.
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @family sr
#' @references 
#'
#' Sharpe, William F. "Mutual fund performance." Journal of business (1966): 119-138.
#' \url{http://ideas.repec.org/a/ucp/jnlbus/v39y1965p119.html}
#' 
#' Lo, Andrew W. "The statistics of Sharpe ratios." Financial Analysts Journal 58, no. 4 
#' (2002): 36-52. \url{http://ssrn.com/paper=377260}
#'
#' @examples 
#' rvs <- rsr(2048, 253*6, 0, 253)
#' dvs <- dsr(rvs, 253*6, 0, 253)
#' pvs <- psr(rvs, 253*6, 0, 253)
#' plot(ecdf(pvs))
#' pvs <- psr(rvs, 253*6, 1, 253)
#' plot(ecdf(pvs))
#'
dsr <- function(x, df, snr, opy, log = FALSE) {
	if (!missing(opy)) {
		x <- .deannualize(x,opy)
		if (!missing(snr)) { snr <- .deannualize(x,opy) }
	}

	tx <- .sr_to_t(x, df)
	if (missing(snr)) {
		xd <- dt(tx, df = df - 1, log = log)
	} else {
		ncp <- .sr_to_t(snr, df)
		xd <- dt(tx, df = df - 1, ncp = ncp, log = log)
	}
	if (log) {
		retv <- xd + log(.d_sr_to_t(snr, df))		
	} else {
		retv <- xd * .d_sr_to_t(snr, df)
	}
	return(retv)	
}
#' @export 
psr <- function(q, df, snr, opy, ...) {
	if (!missing(opy)) {
		q <- .deannualize(q, opy)
		if (!missing(snr)) {
			snr <- .deannualize(snr, opy)
		}
	}
	tq <- .sr_to_t(q, df)
	if (missing(snr)) {
		retv <- pt(q = tq, df = df - 1, ...)		
	} else {
		ncp <- .sr_to_t(snr, df)
		retv <- pt(q = tq, df = df - 1, ncp = ncp, ...)		
	}
	return(retv)	
}
#' @export 
qsr <- function(p, df, snr, opy, lower.tail = TRUE, log.p = FALSE) {
	if (!missing(opy) && !missing(snr)) {
		snr <- .deannualize(snr, opy)
	}
	if (missing(snr)) {
		tq <- qt(p, df = df - 1, lower.tail = lower.tail, log.p = log.p)
	} else {
		ncp <- .sr_to_t(snr, df)
		tq <- qt(p, df = df - 1, ncp = ncp, 
						 lower.tail = lower.tail, log.p = log.p)
	}
	retv <- .t_to_sr(tq, df)
	# annualize
	if (!missing(opy)) {
		retv <- .annualize(retv, opy)
	}
	return(retv)
}
#' @export 
rsr <- function(n, df, snr, opy) {
	if (!missing(opy) && !missing(snr)) {
		snr <- .deannualize(snr, opy)
	}
	if (missing(snr)) {
		tr <- rt(n, df = df - 1)
	} else {
		ncp <- .sr_to_t(snr, df)
		tr <- rt(n, df = df - 1, ncp = ncp) 
	}
	retv <- .t_to_sr(tr, df)
	# annualize
	if (!missing(opy)) {
		retv <- .annualize(retv, opy)
	}
	return(retv)
}
#UNFOLD

# Hotelling
# dT2, pT2, qT2, rT2#FOLDUP
#' @title The (non-central) Hotelling distribution.
#'
#' @description 
#'
#' Density, distribution function, quantile function and random
#' generation for the Hotelling distribution distribution with 
#' \code{df1} and \code{df2} degrees of freedom
#' (and optional non-centrality parameter \code{delta2}).
#'
#' @details
#'
#' Suppose \eqn{x_i}{xi} are \eqn{n}{n} independent draws of a \eqn{q}{q}-variate
#' normal random variable with mean \eqn{\mu}{mu} and covariance matrix
#' \eqn{\Sigma}{Sigma}. Let \eqn{\bar{x}}{xbar} be the (vector) sample mean, and 
#' \eqn{S}{S} be the sample covariance matrix (using Bessel's correction). Then
#' \deqn{T^2 = n \bar{x}^{\top}S^{-1}\bar{x}}{T^2 = n xbar' S^-1 xbar} 
#' follows a (non-central)
#' Hotelling T-squared distribution with \eqn{q}{q} and \eqn{n-1}{n-1}
#' degrees of freedom, and non-centrality parameter
#' \deqn{\delta^2 = n \mu^{\top}\Sigma^{-1}\mu}{delta^2 = n mu' Sigma^-1 mu}
#'
#' The (non-central) T-squared distribution is a (non-central) F distribution
#' up to scaling which depends on \eqn{q}{q} and \eqn{n}{n}.
#'
#' @usage
#'
#' dT2(x, df1, df2, delta2, log = FALSE)
#'
#' pT2(q, df1, df2, delta2, ...)
#'
#' qT2(p, df1, df2, delta2, ...)
#'
#' rT2(n, df1, df2, delta2)
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. 
#' @param df1 the dimension of the vector space from which multivariate
#'        observations had been drawn, \eqn{q}{q}.
#' @param df2 the number of observations that the sample mean and covariance
#'        are based on, \eqn{n}{n}.
#' @param delta2 the population non-centrality parameter, defined as 
#'        \eqn{\delta^2 = n \mu^{\top}\Sigma^{-1}\mu}{delta^2 = n (mu' Sigma^-1 mu)}.
#'        defaults to 0, i.e. a central \eqn{T^2}{T2} distribution.
#' @param log logical; if TRUE, probabilities p are given as \eqn{\mbox{log}(p)}{log(p)}.
#' @param ... arguments passed on to the respective F distribution functions, namely
#' \code{lower.tail} with default \code{TRUE}, and \code{log.p}, with default \code{FALSE}.
#' @keywords distribution 
#' @return \code{dT2} gives the density, \code{pT2} gives the distribution function,
#' \code{qT2} gives the quantile function, and \code{rT2} generates random deviates.
#'
#' Invalid arguments will result in return value \code{NaN} with a warning.
#' @aliases pT2 
#' @aliases qT2 
#' @aliases rT2 
#' @seealso F-distribution functions, \code{\link{df}, \link{pf}, \link{qf}, \link{rf}},
#' Sharpe ratio distribution, \code{\link{dsr}, \link{psr}, \link{qsr}, \link{rsr}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @family Hotelling
#' @note
#' This is a thin wrapper on the F distribution, provided for convenience.
#' @references 
#'
#' Wikipedia contributors, 'Hotelling's T-squared distribution', Wikipedia, The Free Encyclopedia, 
#' 11 December 2012, 13:38 UTC, \url{http://en.wikipedia.org/w/index.php?title=Hotelling\%27s_T-squared_distribution\&oldid=527530524}
#' [accessed 9 January 2013]
#'
#' Bilodeau, Martin, and David Brenner. Theory of multivariate statistics. Springer, 1999.
#' \url{http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.172.3290}
#'
#' Timm, Neil H. Applied multivariate analysis: methods and case studies. Springer, 2002.
#' \url{http://books.google.com/books?id=vtiyg6fnnskC}
#' 
#' Hotelling, Harold. "The Generalization of Student's Ratio." Annals of Mathematical 
#' Statistics 2, no. 3 (1931): 360--378. \url{http://projecteuclid.org/euclid.aoms/1177732979}
#'
#' @examples 
#' rvs <- rT2(2048, 4, 253*6, 0)
#' dvs <- dT2(rvs, 4, 253*6, 0)
#' pvs <- pT2(rvs, 4, 253*6, 0)
#' plot(ecdf(pvs))
#' pvs <- pT2(rvs, 4, 253*6, 1)
#' plot(ecdf(pvs))
#' 
dT2 <- function(x, df1, df2, delta2, log = FALSE) {
	Fs <- .T2_to_F(x, df1, df2)
	if (missing(delta2)) {
		dv <- df(Fs, df1 = df1, df2 = df2 - df1, log = log)
	} else {
		dv <- df(Fs, df1 = df1, df2 = df2 - df1, ncp = delta2, log = log)
	}
	if (log) {
		retv <- (dv + log(.d_T2_to_F(x, df1, df2)))
	} else {
		retv <- (dv * .d_T2_to_F(x, df1, df2))
	}
	return(retv)
}
#' @export 
pT2 <- function(q, df1, df2, delta2, ...) {
	Fs <- .T2_to_F(q, df1, df2)
	if (missing(delta2)) {
		#retv <- pf(Fs, df1 = df1, df2 = df2 - df1, ncp = 0, ...)
		retv <- pf(Fs, df1 = df1, df2 = df2 - df1, ...)
	} else {
		retv <- pf(Fs, df1 = df1, df2 = df2 - df1, ncp = delta2, ...)
	}
	return(retv)
}
#' @export 
qT2 <- function(p, df1, df2, delta2, ... ) {
	if (missing(delta2)) {
		#Fq <- qf(Fp, df1 = df1, df2 = df2 - df1, ncp = 0, ... )
		Fq <- qf(p, df1 = df1, df2 = df2 - df1, ... )
	} else {
		Fq <- qf(p, df1 = df1, df2 = df2 - df1, ncp = delta2, ... )
	}
	retv <- .F_to_T2(Fq, df1, df2)
	return(retv)
}
#' @export 
rT2 <- function(n, df1, df2, delta2) {
	if (missing(delta2)) {
		Fr <- rf(n, df1 = df1, df2 = df2 - df1)
	} else {
		Fr <- rf(n, df1 = df1, df2 = df2 - df1, ncp = delta2)
	}
	retv <- .F_to_T2(Fr, df1, df2)
	return(retv)
}
#UNFOLD

# SR^*
# dsrstar, psrstar, qsrstar, rsrstar#FOLDUP
#' @title The (non-central) maximal Sharpe ratio distribution.
#'
#' @description 
#'
#' Density, distribution function, quantile function and random
#' generation for the maximal Sharpe ratio distribution with 
#' \code{df1} and \code{df2} degrees of freedom
#' (and optional maximal signal-noise-ratio \code{snrstar}).
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
#' The variable \eqn{z_*}{z*} follows a \emph{Maximal Sharpe ratio}
#' distribution. For convenience, we may assume that the sample statistic
#' has been annualized by 
#' in the same manner as the Sharpe ratio (see \code{\link{dsr}}), that
#' is, by multiplying by \eqn{p}{p}, the number of observations per
#' epoch.
#' 
#' The distribution is parametrized by the number of independent observations,
#' \eqn{n}, the number of assets, \eqn{q}, the noncentrality parameter,
#' \eqn{\delta^2 = \mu^{\top}\Sigma^{-1}\mu}{delta^2 = mu' Sigma^-1 mu},
#' the 'drag' term, \eqn{c_0/R}{c0/R}, and the annualization factor, \eqn{p}.
#' The drag term makes this a location family of distributions, and 
#' by default we assume it is zero.
#'
#' @usage
#'
#' dsrstar(x, df1, df2, snrstar, opy, drag = 0, log = FALSE)
#'
#' psrstar(q, df1, df2, snrstar, opy, drag = 0, ...)
#'
#' qsrstar(p, df1, df2, snrstar, opy, drag = 0, ...)
#'
#' rsrstar(n, df1, df2, snrstar, opy, drag = 0, ...)
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. 
#' @param df1 the number of assets in the portfolio.
#' @param df2 the number of observations.
#' @param snrstar the non-centrality parameter, defined as 
#'        \eqn{\delta = \sqrt{\mu' \Sigma^{-1} \mu}}{delta = sqrt(mu' Sigma^-1 mu)}
#'        for population parameters.
#'        defaults to 0, \emph{i.e.} a central maximal Sharpe ratio distribution.
#' @param opy the number of observations per 'year'. \code{x}, \code{q}, and 
#'        \code{snrstar} are quoted in 'annualized' units, that is, per 'year',
#'        but returns are observed possibly at a rate of \code{opy} per 
#'        'year.' default value is 1, meaning no deannualization is performed.
#' @param drag the 'drag' term, \eqn{c_0/R}{c0/R}. defaults to 0. It is assumed
#'        that \code{drag} has been annualized, \emph{i.e.} is given in the
#'        same units as \code{x} and \code{q}.
#' @param log logical; if TRUE, probabilities p are given as \eqn{\mbox{log}(p)}{log(p)}.
#' @param ... arguments passed on to the respective Hotelling \eqn{T^2} functions.
#' @keywords distribution 
#' @return \code{dsrstar} gives the density, \code{psrstar} gives the distribution function,
#' \code{qsrstar} gives the quantile function, and \code{rsrstar} generates random deviates.
#'
#' Invalid arguments will result in return value \code{NaN} with a warning.
#' @aliases psrstar
#' @aliases qsrstar 
#' @aliases rsrstar
#' @seealso Hotelling \eqn{T^2}-distribution functions, \code{\link{dT2},\link{pT2},\link{qT2},\link{rT2}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @family srstar
#' @note
#' This is a thin wrapper on the Hotelling T-squared distribution, provided for
#' convenience.
#' @references 
#'
#' Kan, Raymond and Smith, Daniel R. "The Distribution of the Sample Minimum-Variance Frontier."
#' Journal of Management Science 54, no. 7 (2008): 1364--1380.
#' \url{http://mansci.journal.informs.org/cgi/doi/10.1287/mnsc.1070.0852}
#'
#' @examples 
#' # generate some variates 
#' rvs <- rsrstar(2048, 8, 253*6, 0, 253)
#' hist(rvs)
#' # these should be uniform:
#' isp <- psrstar(rvs, 8, 253*6, 0, 253)
#' plot(ecdf(isp))
#'
dsrstar <- function(x, df1, df2, snrstar, opy, drag = 0, log = FALSE) {
	if (!missing(drag) && (drag != 0)) {
		x <- x + drag
	}
	if (!missing(opy)) {
		x <- .deannualize(x, opy)
		if (!missing(snrstar)) {
			snrstar <- .deannualize(snrstar, opy)
		}
	}
	x.T2 <- .srstar_to_T2(x, df2)
	if (missing(snrstar)) {
		delta2 <- 0
	} else {
		delta2 <- .srstar_to_T2(snrstar, df2)
	}
	d.T2 <- dT2(x.T2, df1, df2, delta2, log=log)
	if (log) {
		retv <- (d.T2 - log(.d_T2_to_srstar(x, df2)))
	} else {
		retv <- (d.T2 / .d_T2_to_srstar(x, df2))
	}
	return(retv)
}
#' @export 
psrstar <- function(q, df1, df2, snrstar, opy, drag = 0, ...) {
	if (!missing(drag) && (drag != 0)) {
		q <- q + drag
	}
	if (!missing(opy)) {
		q <- .deannualize(q, opy)
		if (!missing(snrstar)) {
			snrstar <- .deannualize(snrstar, opy)
		}
	}
	q.T2 <- .srstar_to_T2(q, df2)
	if (missing(snrstar)) {
		delta2 = 0.0
	} else {
		delta2 <- .srstar_to_T2(snrstar, df2)
	}
	retv <- pT2(q.T2, df1, df2, delta2, ...)
	return(retv)
}
#' @export 
qsrstar <- function(p, df1, df2, snrstar, opy, drag = 0, ...) {
	if (missing(snrstar)) {
		delta2 = 0.0
	} else {
		if (!missing(opy)) {
			snrstar <- .deannualize(snrstar, opy)
		}
		delta2 <- .srstar_to_T2(snrstar, df2)
	}
	q.T2 <- qT2(p, df1, df2, delta2, ...)
	retv <- .T2_to_srstar(q.T2, df2)
	if (!missing(opy)) {
		retv <- .annualize(retv,opy)
	}
	if (!missing(drag) && (drag != 0)) {
		retv <- retv - drag
	}
	return(retv)
}
#' @export 
rsrstar <- function(n, df1, df2, snrstar, opy, drag = 0, ...) {
	if (missing(snrstar)) {
		delta2 = 0.0
	} else {
		if (!missing(opy)) {
			snrstar <- .deannualize(snrstar, opy)
		}
		delta2 <- .srstar_to_T2(snrstar, df2)
	}
	r.T2 <- rT2(n, df1, df2, delta2, ...) 
	retv <- .T2_to_srstar(r.T2, df2)
	if (!missing(opy)) {
		retv <- .annualize(retv,opy)
	}
	if (!missing(drag) && (drag != 0)) {
		retv <- retv - drag
	}
	return(retv)
}
#UNFOLD

# lambda prime
# plambdap, qlambdap#FOLDUP
#' @title The lambda-prime distribution.
#'
#' @description 
#'
#' Distribution function and quantile function for LeCoutre's
#' lambda-prime distribution with \code{df} degrees of freedom
#' and the observed t-statistic, \code{tstat}.
#'
#' @details
#'
#' Let \eqn{t}{t} be distributed
#' as a non-central t with \eqn{\nu}{v} degrees of freedom and non-centrality
#' parameter \eqn{\delta}{ncp}. We can view this as
#' \deqn{t = \frac{Z + \delta}{\sqrt{V/\nu}}.}{t = (Z + ncp)/sqrt(V/v)}
#' where \eqn{Z}{Z} is a standard normal, \eqn{\delta}{ncp} is the
#' non-centrality parameter, \eqn{V}{V} is a chi-square RV with \eqn{\nu}{v}
#' degrees of freedom, independent of \eqn{Z}{Z}.  We can rewrite this as
#' \deqn{\delta = t\sqrt{V/\nu} + Z.}{ncp = t sqrt(V/v) + Z}
#' 
#' Thus a 'lambda-prime' random variable with parameters \eqn{t}{t} and
#' \eqn{\nu}{v} is one expressable as a sum
#' \deqn{t\sqrt{V/\nu} + Z}{t sqrt(V/v) + Z}
#' for Chi-square \eqn{V}{V} with \eqn{\nu}{v} d.f., independent from
#' standard normal \eqn{Z}{Z}
#'
#' @usage
#'
#' plambdap(q, df, tstat, lower.tail = TRUE, log.p = FALSE)
#'
#' qlambdap(p, df, tstat, lower.tail = TRUE, log.p = FALSE)
#'
#' @param q vector of quantiles.
#' @param p vector of probabilities.
#' @param df the degrees of freedom of the t-statistic.
#' @param tstat the observed (non-central) t-statistic.
#' @param log.p logical; if TRUE, probabilities p are given as \eqn{\mbox{log}(p)}{log(p)}.
#' @inheritParams qsr
#' @inheritParams psr
#' @inheritParams dsr
#' @keywords distribution 
#' @return \code{dlambdap} gives the density, \code{plambdap} gives the distribution function,
#' \code{qlambdap} gives the quantile function, and \code{rlambdap} generates random deviates.
#'
#' Invalid arguments will result in return value \code{NaN} with a warning.
#' @aliases plambdap
#' @aliases qlambdap 
#' @aliases rlambdap
#' @seealso t-distribution functions, \code{\link{dt},\link{pt},\link{qt},\link{rt}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @family sr
#' @references 
#'
#' Lecoutre, Bruno. "Another look at confidence intervals for the noncentral t distribution." 
#' Journal of Modern Applied Statistical Methods 6, no. 1 (2007): 107--116.
#' \url{http://www.univ-rouen.fr/LMRS/Persopage/Lecoutre/telechargements/Lecoutre_Another_look-JMSAM2007_6(1).pdf}
#'
#' Lecoutre, Bruno. "Two useful distributions for Bayesian predictive procedures under normal models."
#' Journal of Statistical Planning and Inference 79  (1999): 93--105. 
#'
#' @note
#' \code{plambdap} should be an increasing function of the argument \code{q},
#' and decreasing in \code{tstat}. \code{qlambdap} should be increasing
#' in \code{p}
#' @examples 
#' rvs <- rnorm(2048)
#' pvs <- plambdap(rvs, 253*6, 0.5)
#' plot(ecdf(pvs))
#' pvs <- plambdap(rvs, 253*6, 1)
#' plot(ecdf(pvs))
#' pvs <- plambdap(rvs, 253*6, -0.5)
#' plot(ecdf(pvs))
#' # test vectorization:
#' qv <- qlambdap(0.1,128,2)
#' qv <- qlambdap(c(0.1),128,2)
#' qv <- qlambdap(c(0.2),128,2)
#' qv <- qlambdap(c(0.2),253,2)
#' qv <- qlambdap(c(0.1,0.2),128,2)
#' qv <- qlambdap(c(0.1,0.2),c(128,253),2)
#' qv <- qlambdap(c(0.1,0.2),c(128,253),c(2,4))
#' qv <- qlambdap(c(0.1,0.2),c(128,253),c(2,4,8,16))
#'
plambdap <- function(q,df,tstat,lower.tail=TRUE,log.p=FALSE) {
	# this is just a silly wrapper on pt
	retv <- pt(q=tstat,df=df,ncp=q,lower.tail=!lower.tail,log.p=log.p)
	return(retv)
}

# create a scalar function that we later vectorize. 
.qlambdap <- function(p,df,tstat,lower.tail=TRUE,log.p=FALSE) {
	if (!log.p) {
		if (p == 1)
			return(ifelse(lower.tail,Inf,-Inf))
		if (p == 0)
			return(ifelse(lower.tail,-Inf,Inf))
		if ((p < 0) || (p > 1))
			return (NaN)
	} else {
		if (p == 0)
			return(ifelse(lower.tail,Inf,-Inf))
		if (p > 0)
			return (NaN)
		if (is.infinite(p))
			return(ifelse(lower.tail,-Inf,Inf))
	}

	# create a function increasing in its argument that
	# we wish to zero
	if (lower.tail) {
		zerf <- function(q) {
			return(plambdap(q,df=df,tstat=tstat,lower.tail=lower.tail,log.p=log.p) - p)
		}
	} else {
		zerf <- function(q) {
			return(p - plambdap(q,df=df,tstat=tstat,lower.tail=lower.tail,log.p=log.p))
		}
	}
	zmax = 2 * max(qnorm(p,lower.tail=TRUE,log.p=log.p),qnorm(p,lower.tail=FALSE,log.p=log.p))
	flo <- min(-1,tstat - zmax)
	fhi <- max( 1,tstat + zmax)

	#find approximate endpoints;
	#expand them until pt(tstat,df,flo) > p and pt(tstat,df,fhi) < p
	FLIM <- 1e8
	while ((zerf(flo) > 0) && (flo > -FLIM)) { flo <- 2 * flo }
	while ((zerf(fhi) < 0) && (fhi < FLIM))  { fhi <- 2 * fhi }

	ncp <- uniroot(zerf,c(flo,fhi))
	return(ncp$root)
}
#' @export 
qlambdap <- Vectorize(.qlambdap, 
											vectorize.args = c("p","df","tstat"),
											SIMPLIFY = TRUE)
#UNFOLD

# co-SR^*
# pcosrstar, qcosrstar#FOLDUP
#' @title The 'confidence distribution' for maximal Sharpe ratio.
#'
#' @description 
#'
#' Distribution function and quantile function for the 'confidence
#' distribution' of the maximal Sharpe ratio. This is just an inversion
#' to perform inference on snrstar given observed statistic srstar.
#'
#' @details
#' 
#' Let \eqn{z_*}{z*} follows a \emph{Maximal Sharpe ratio} distribution
#' (see \code{\link{psrstar}}) for known degrees of freedom, and 
#' unknown non-centrality parameter \eqn{\delta^2}{delta^2}. 
#' 
#'
#' 2FIX
#' 
#'
#' @usage
#'
#' pcosrstar(q,df1,df2,srstar,opy,lower.tail=TRUE,log.p=FALSE) 
#'
#' qcosrstar(p,df1,df2,srstar,opy,lower.tail=TRUE,log.p=FALSE,lb=0,ub=Inf) 
#'
#' @param q vector of quantiles.
#' @param p vector of probabilities.
#' @param srstar an observed Sharpe ratio statistic, annualized.
#' @param opy the number of observations per 'year'. \code{x}, \code{q}, and 
#'        \code{srstar} are quoted in 'annualized' units, that is, per 'year',
#'        but returns are observed possibly at a rate of \code{opy} per 
#'        'year.' default value is 1, meaning no deannualization is performed.
#' @param log.p logical; if TRUE, probabilities p are given as \eqn{\mbox{log}(p)}{log(p)}.
#' @param lb the lower bound for the output of \code{qcosrstar}.
#' @param ub the upper bound for the output of \code{qcosrstar}.
#' @inheritParams dsrstar
#' @inheritParams qsrstar
#' @inheritParams psrstar
#' @inheritParams qsr
#' @inheritParams dsr
#' @inheritParams psr
#' @keywords distribution 
#' @return \code{pcosrstar} gives the distribution function, and
#' \code{qcosrstar} gives the quantile function.
#'
#' Invalid arguments will result in return value \code{NaN} with a warning.
#' @aliases qcosrstar 
#' @seealso \code{\link{dsrstar},\link{psrstar},\link{qsrstar},\link{rsrstar}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @family srstar
#' @note
#' When \code{lower.tail} is true, \code{pcosrstar} is monotonic increasing 
#' with respect to \code{q}, and decreasing in \code{srstar}; these are reversed
#' when \code{lower.tail} is false. Similarly, \code{qcosrstar} is increasing
#' in \code{sign(as.double(lower.tail) - 0.5) * p} and
#' \code{- sign(as.double(lower.tail) - 0.5) * srstar}.
#'
#' @examples 
#'
#' snrstar <- 2.0
#' opy <- 253
#' ntest <- 2000
#' df1 <- 4
#' df2 <- 6 * opy
#' rvs <- rsrstar(ntest,df1=df1,df2=df2,snrstar=snrstar)
#' qvs <- seq(0,10,length.out=101)
#' pps <- pcosrstar(qvs,df1,df2,rvs[1],opy)
#' if (require(txtplot))
#'  txtplot(qvs,pps)
#' pps <- pcosrstar(qvs,df1,df2,rvs[1],opy,lower.tail=FALSE)
#' if (require(txtplot))
#'  txtplot(qvs,pps)
#' 
#' # 2FIX: shove these into the unit tests for monotonicity?
#' svs <- seq(0,4,length.out=101)
#' pps <- pcosrstar(2,df1,df2,svs,opy)
#' if (require(txtplot))
#'  txtplot(svs,pps)
#' pps <- pcosrstar(2,df1,df2,svs,opy,lower.tail=FALSE)
#' if (require(txtplot))
#'  txtplot(svs,pps)
#' 
#' if (require(txtplot))
#'  txtplot(qvs,pps)
#' pps <- pcosrstar(qvs,df1,df2,rvs[1],opy,lower.tail=FALSE)
#' if (require(txtplot))
#'  txtplot(qvs,pps)
#' pcosrstar(-1,df1,df2,rvs[1],opy)
#'
#' qvs <- qcosrstar(0.05,df1=df1,df2=df2,srstar=rvs)
#' mean(qvs > snrstar)
#' qvs <- qcosrstar(0.5,df1=df1,df2=df2,srstar=rvs)
#' mean(qvs > snrstar)
#' qvs <- qcosrstar(0.95,df1=df1,df2=df2,srstar=rvs)
#' mean(qvs > snrstar)
#' # test vectorization:
#' qv <- qcosrstar(0.1,df1,df2,rvs)
#' qv <- qcosrstar(c(0.1,0.2),df1,df2,rvs)
#' qv <- qcosrstar(c(0.1,0.2),c(df1,2*df1),df2,rvs)
#' qv <- qcosrstar(c(0.1,0.2),c(df1,2*df1),c(df2,2*df2),rvs)
#'
# 2FIX: add opy?
pcosrstar <- function(q,df1,df2,srstar,opy=1,lower.tail=TRUE,log.p=FALSE) {
	# 2FIX: do the annualization just once for efficiency?
	# this is just a silly wrapper on psrstar
	# delegate
	retv <- psrstar(q=srstar,df1=df1,df2=df2,snrstar=q,opy=opy,
									lower.tail=!lower.tail,log.p=log.p)  # sic the tail reversal
	return(retv)
}
# create a scalar function that we later vectorize. 
# 
# this inverts pcosrstar; note that when lower.tail=TRUE,
# pcosrstar is increasing in q, but decreasing in srstar.
# pcosrstar only accepts non-negative q 
#
# here we try to find lb <= q < ub such that
# pcosrstar(q,df1,df2,srstar,opy,lower.tail,log.p) = p
# however, there may be no such q, since we are limited to
# the range [lp,up) where
# lp = pcosrstar(lb,df1,df2,srstar,opy,lower.tail,log.p)
# up = pcosrstar(ub,df1,df2,srstar,opy,lower.tail,log.p)
# if p < lp we return lb;
# if q >= up, we return ub;
.qcosrstar <- function(p,df1,df2,srstar,opy,lower.tail=TRUE,log.p=FALSE,
												lb=0,ub=Inf) {
	if ((lb > ub) || (is.infinite(lb)) || (min(lb,ub) < 0))
		stop("nonsensical lb and/or ub")

	if (!missing(opy)) 
		srstar <- .deannualize(srstar,opy)

	# create a function increasing in its argument that
	# we wish to zero
	# do *not* pass on opy b/c this function is a tight loop
	if (lower.tail) {
		zerf <- function(q) {
			pcosrstar(q,df1=df1,df2=df2,srstar=srstar,lower.tail=lower.tail,log.p=log.p) - p
		}
	} else {
		zerf <- function(q) {
			p - pcosrstar(q,df1=df1,df2=df2,srstar=srstar,lower.tail=lower.tail,log.p=log.p)
		}
	}
	flb <- zerf(lb)
	if (flb > 0)
		return(lb)
	if (is.infinite(ub)) {
		ub <- 1 + lb
		fub <- zerf(ub)
		while ((fub < 0) && (!is.infinite(ub))) {
			ub <- 2 * ub
			fub <- zerf(ub)
		}
		if (is.infinite(ub) && (fub < 0)) 
			return(ub)
	} else {
		fub <- zerf(ub)
		if (fub < 0)
			return(ub)
	}

	ncp <- uniroot(zerf,interval=c(lb,ub),
								 f.lower=flb,f.upper=fub)
	retval <- ifelse(missing(opy),ncp$root,.annualize(ncp$root,opy))
	return(retval)
}
#' @export 
qcosrstar <- Vectorize(.qcosrstar,
											vectorize.args = c("p","df1","df2","srstar"),
											SIMPLIFY = TRUE)
#UNFOLD

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:

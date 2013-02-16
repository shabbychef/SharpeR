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

#         @include utils.R
#source("utils.r")

# 2FIX: is df the # of observations or the d.f. of the t-stat? ack!

# note: on citations, use the Chicago style from google scholar. tks.

# the following are useful for grokking R
# showMethods("print") 
# getAnywhere("t.test.default")
#

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


# Sharpe Ratio as a distribution
# dsr, psr, qsr, rsr#FOLDUP
#' @title The (non-central) Sharpe Ratio.
#'
#' @description 
#'
#' Density, distribution function, quantile function and random
#' generation for the Sharpe Ratio distribution with \code{df} degrees of freedom
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
#' is the (sample) Sharpe Ratio.
#' 
#' The units of \eqn{z}{z} is \eqn{\mbox{time}^{-1/2}}{per root time}.
#' Typically the Sharpe Ratio is \emph{annualized} by multiplying by
#' \eqn{\sqrt{p}}{sqrt(p)}, where \eqn{p}{p} is the number of observations
#' per year (or whatever the target annualization epoch.)
#'
#' Letting \eqn{z = \sqrt{p}\frac{\bar{x}-c_0}{s}}{z = sqrt(p)(xbar - c0)/s},
#' where the sample estimates are based on \eqn{n}{n} observations, 
#' then \eqn{z}{z} takes a (non-central) Sharpe Ratio distribution
#' with \eqn{n}{n} 'degrees of freedom', non-centrality parameter
#' \eqn{\delta = \frac{\mu - c_0}{\sigma}}{delta = (mu - c0)/sigma}, and 
#' annualization parameter \eqn{p}{p}.
#'
#' @usage
#'
#' dsr(x, df, snr, opy, log = FALSE)
#'
#' psr(q, df, snr, opy, lower.tail = TRUE, log.p = FALSE) 
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
#' @references 
#'
#' Sharpe, William F. "Mutual fund performance." Journal of business (1966): 119-138.
#' \url{http://ideas.repec.org/a/ucp/jnlbus/v39y1965p119.html}
#' 
#' Lo, Andrew W. "The statistics of Sharpe ratios." Financial Analysts Journal (2002): 36-52.
#' \url{http://ssrn.com/paper=377260}
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
#' pT2(q, df1, df2, delta2, lower.tail = TRUE, log.p = FALSE) 
#'
#' qT2(p, df1, df2, delta2, lower.tail = TRUE, log.p = FALSE) 
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
#' @param log,log.p logical; if TRUE, probabilities p are given as \eqn{\mbox{log}(p)}{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are
#'        \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.
#' @keywords distribution 
#' @return \code{dT2} gives the density, \code{pT2} gives the distribution function,
#' \code{qT2} gives the quantile function, and \code{rT2} generates random deviates.
#'
#' Invalid arguments will result in return value \code{NaN} with a warning.
#' @aliases pT2 qT2 rT2 
#' @seealso F-distribution functions, \code{\link{df}, \link{pf}, \link{qf}, \link{rf}},
#' Sharpe Ratio distribution, \code{\link{dsr}, \link{psr}, \link{qsr}, \link{rsr}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
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
#' @title The (non-central) maximal Sharpe Ratio distribution.
#'
#' @description 
#'
#' Density, distribution function, quantile function and random
#' generation for the maximal Sharpe Ratio distribution with 
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
#' be the (sample) Sharpe Ratio of the portfolio \eqn{w}{w}, subject to 
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
#' The variable \eqn{z_*}{z*} follows a \emph{Maximal Sharpe Ratio}
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
#' psrstar(q, df1, df2, snrstar, opy, drag = 0, lower.tail = TRUE, log.p = FALSE) 
#'
#' qsrstar(p, df1, df2, snrstar, opy, drag = 0, lower.tail = TRUE, log.p = FALSE) 
#'
#' rsrstar(n, df1, df2, snrstar, opy, drag = 0)
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. 
#' @param df1 the number of assets in the portfolio.
#' @param df2 the number of observations.
#' @param snrstar the non-centrality parameter, defined as 
#'        \eqn{\delta = \sqrt{\mu' \Sigma^{-1} \mu}}{delta = sqrt(mu' Sigma^-1 mu)}
#'        for population parameters.
#'        defaults to 0, \emph{i.e.} a central maximal Sharpe Ratio distribution.
#' @param opy the number of observations per 'year'. \code{x}, \code{q}, and 
#'        \code{snrstar} are quoted in 'annualized' units, that is, per 'year',
#'        but returns are observed possibly at a rate of \code{opy} per 
#'        'year.' default value is 1, meaning no deannualization is performed.
#' @param drag the 'drag' term, \eqn{c_0/R}{c0/R}. defaults to 0. It is assumed
#'        that \code{drag} has been annualized, \emph{i.e.} is given in the
#'        same units as \code{x} and \code{q}.
#' @param log,log.p logical; if TRUE, probabilities p are given as \eqn{\mbox{log}(p)}{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are
#'        \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.
#' @keywords distribution 
#' @return \code{dsrstar} gives the density, \code{psrstar} gives the distribution function,
#' \code{qsrstar} gives the quantile function, and \code{rsrstar} generates random deviates.
#'
#' Invalid arguments will result in return value \code{NaN} with a warning.
#' @aliases psrstar qsrstar rsrstar
#' @seealso Hotelling T2-distribution functions, \code{\link{dT2},\link{pT2},\link{qT2},\link{rT2}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @note
#' This is a thin wrapper on the Hotelling T-squared distribution, provided for
#' convenience.
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

# confidence distributions?

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
#' @param lower.tail logical; if TRUE (default), probabilities are
#'        \eqn{P[X \le x]}{P\[X <= x\]}, otherwise, \eqn{P[X > x]}{P\[X > x\]}.
#' @keywords distribution 
#' @return \code{dlambdap} gives the density, \code{plambdap} gives the distribution function,
#' \code{qlambdap} gives the quantile function, and \code{rlambdap} generates random deviates.
#'
#' Invalid arguments will result in return value \code{NaN} with a warning.
#' @aliases plambdap qlambdap rlambdap
#' @seealso t-distribution functions, \code{\link{dt},\link{pt},\link{qt},\link{rt}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @references 
#'
#' Lecoutre, Bruno. "Another look at confidence intervals for the noncentral t distribution." 
#' Journal of Modern Applied Statistical Methods 6, no. 1 (2007): 107-116.
#' \url{http://www.univ-rouen.fr/LMRS/Persopage/Lecoutre/telechargements/Lecoutre_Another_look-JMSAM2007_6(1).pdf}
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
#'
plambdap <- function(q,df,tstat,lower.tail=TRUE,log.p=FALSE) {
	# this is just a silly wrapper on pt
	retv <- pt(q=tstat,df=df,ncp=q,lower.tail=!lower.tail,log.p=log.p)
	return(retv)
}
# create a scalar function that we later vectorize. 
.qlambdap <- function(p,df,tstat,lower.tail=TRUE,log.p=FALSE) {
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
#qlambdap(0.1,128,2)
#qlambdap(c(0.1),128,2)
#qlambdap(c(0.2),128,2)
#qlambdap(c(0.2),253,2)
#qlambdap(c(0.1,0.2),128,2)
#qlambdap(c(0.1,0.2),c(128,253),2)
#qlambdap(c(0.1,0.2),c(128,253),c(2,4))
#qlambdap(c(0.1,0.2),c(128,253),c(2,4,8,16))

########################################################################
# Inference
########################################################################

# hypothesis tests.

# equality of SR#FOLDUP

#' @title Paired test for equality of Sharpe Ratio
#'
#' @description 
#'
#' Performs a hypothesis test of equality of Sharpe ratios of p assets
#' given paired observations.
#'
#' @details 
#'
#' Given \eqn{n} \emph{i.i.d.} observations of the excess returns of
#' \eqn{p} strategies, we test
#' \deqn{H_0: \frac{\mu_i}{\sigma_i} = \frac{\mu_j}{\sigma_j}, 1 \le i < j \le p}{H0: sr1 = sr2 = ...}
#' using the method of Wright, et. al. 
#' 
#' More generally, a matrix of constrasts, \eqn{E}{E} can be given, and we can
#' test
#' \deqn{H_0: E s = 0,}{H0: E s = 0,}
#' where \eqn{s}{s} is the vector of Sharpe ratios of the \eqn{p} strategies.
#' 
#' Both chi-squared and F- approximations are supported.
#' 
#' @usage
#'
#' sr.equality.test(X,contrasts=NULL,type=c("chisq","F"))
#'
#' @param X an \eqn{n \times p}{n x p} matrix of paired observations.
#' @param contrasts an \eqn{k \times p}{k x p} matrix of the contrasts
#         to test. This defaults to a matrix which tests sequential equality.
#' @param type which approximation to use. 'chisq' is preferred when
#'        the returns are non-normal, but the approximation is asymptotic.
#' @keywords htest
#' @return Object of class \code{htest}, a list of the test statistic,
#' the size of \code{X}, and the \code{method} noted.
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @references 
#'
#' Wright, J. A., Yam, S. C. P., and Yung, S. P. "A note on the test for the
#' equality of multiple Sharpe ratios and its application on the evaluation
#' of iShares." J. Risk. to appear. 
#' \url{http://www.sta.cuhk.edu.hk/scpy/Preprints/John\%20Wright/A\%20test\%20for\%20the\%20equality\%20of\%20multiple\%20Sharpe\%20ratios.pdf}
#'
#' Leung, P.-L., and Wong, W.-K. "On testing the equality of multiple Sharpe ratios, with 
#' application on the evaluation of iShares." J. Risk 10, no. 3 (2008): 15-30.
#' \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=907270}
#'
#' Memmel, C. "Performance hypothesis testing with the Sharpe Ratio." Finance
#' Letters 1 (2003): 21-23.
#'
#' @examples 
#' rv <- sr.equality.test(matrix(rnorm(500*5),500,5))
#' # test for uniformity
#' pvs <- replicate(500,{ x <- sr.equality.test(matrix(rnorm(400*5),400,5),type="chisq")
#'                        x$p.value })
#' plot(ecdf(pvs))
#' abline(0,1,col='red') 
#'
#'@export
sr.equality.test <- function(X,contrasts=NULL,type=c("chisq","F")) {
	dname <- deparse(substitute(X))
	type <- match.arg(type)
	n <- dim(X)[1]
	p <- dim(X)[2]
	if (is.null(contrasts))
		contrasts <- .atoeplitz(c(1,-1,array(0,p-2)),c(1,array(0,p-2)))
	k <- dim(contrasts)[1]
	if (dim(contrasts)[2] != p)
		stop("size mismatch in 'X', 'contrasts'")

	# compute moments
	m1 <- colMeans(X)
	m2 <- colMeans(X^2)
	# construct Sigma hat
	Shat <- cov(cbind(X,X^2))
	#Shat <- .agram(cbind(X,X^2))
	# the SR
	SR <- m1 / sqrt(diag(Shat[1:p,1:p]))

	# construct D matrix
	deno <- (m2 - m1^2)^(3/2)
	D1 <- diag(m2 / deno)
	D2 <- diag(-m1 / (2*deno))
	Dt <- rbind(D1,D2)

	# Omegahat
	Ohat <- t(Dt) %*% Shat %*% Dt

	# the test statistic:
	ESR <- contrasts %*% SR
	T2 <- n * t(ESR) %*% solve(contrasts %*% Ohat %*% t(contrasts),ESR)

	pval <- switch(type,
								 chisq = pchisq(T2,df=k,ncp=0,lower.tail=FALSE),
								 F = pf((n-k) * T2/((n-1) * k),df1=k,df2=n-k,lower.tail=FALSE))

	# attach names
	names(T2) <- "T2"
	names(k) <- "contrasts"
	method <- paste(c("test for equality of Sharpe Ratio, via",type,"test"),collapse=" ")

	retval <- list(statistic = T2, parameter = k,
							 df1 = p, df2 = n, p.value = pval, 
							 method = method, data.name = dname)
	class(retval) <- "htest"
	return(retval)
}
#UNFOLD


# power of tests:#FOLDUP

#' @title Power calculations for Sharpe Ratio tests
#'
#' @description 
#'
#' Compute power of test, or determine parameters to obtain target power.
#'
#' @details 
#'
#' Suppose you perform a single-sample test for significance of the
#' Sharpe Ratio based on the corresponding single-sample t-test. 
#' Given any three of: the effect size (the population SNR), the
#' number of observations, and the type I and type II rates,
#' this computes the fourth.
#'
#' This is a thin wrapper on \code{\link{power.t.test}}.
#'
#' Exactly one of the parameters \code{n}, \code{snr}, \code{power}, and 
#' \code{sig.level} must be passed as NULL, and that parameter is determined 
#' from the others.  Notice that \code{sig.level} has non-NULL default, so NULL 
#' must be explicitly passed if you want to compute it.
#' 
#' @usage
#'
#' power.sr.test(n=NULL,snr=NULL,sig.level=0.05,power=NULL,
#'                           alternative=c("one.sided","two.sided"),opy=NULL) 
#'
#' @param n Number of observations
#' @param snr the 'signal-to-noise' parameter, defined as the population
#'        mean divided by the population standard deviation, 'annualized'.
#' @param sig.level Significance level (Type I error probability).
#' @param power Power of test (1 minus Type II error probability).
#' @param alternative One- or two-sided test.
#' @param opy the number of observations per 'year'. \code{x}, \code{q}, and 
#'        \code{snr} are quoted in 'annualized' units, that is, per square root 
#'        'year', but returns are observed possibly at a rate of \code{opy} per 
#'        'year.' default value is 1, meaning no deannualization is performed.
#' @keywords htest
#' @return Object of class \code{power.htest}, a list of the arguments
#' (including the computed one) augmented with \code{method}, \code{note}
#' and \code{n.yr} elements, the latter is the number of years under the
#' given annualization (\code{opy}), \code{NA} if none given.
#' @seealso \code{\link{power.t.test}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @examples 
#' anex <- power.sr.test(253,1,0.05,NULL,opy=253) 
#' anex <- power.sr.test(n=253,snr=NULL,sig.level=0.05,power=0.5,opy=253) 
#' anex <- power.sr.test(n=NULL,snr=0.6,sig.level=0.05,power=0.5,opy=253) 
#'
#'@export
power.sr.test <- function(n=NULL,snr=NULL,sig.level=0.05,power=NULL,
													alternative=c("one.sided","two.sided"),
													opy=NULL) {
	# stolen from power.t.test
	if (sum(sapply(list(n, snr, power, sig.level), is.null)) != 1) 
			stop("exactly one of 'n', 'snr', 'power', and 'sig.level' must be NULL")
	if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > 
			sig.level | sig.level > 1)) 
			stop("'sig.level' must be numeric in [0, 1]")
	type <- "one.sample"
	alternative <- match.arg(alternative)
	if (!missing(opy) && !is.null(opy) && !is.null(snr)) {
		snr <- .deannualize(snr,opy)
	}
	# delegate
	subval <- power.t.test(n=n,delta=snr,sd=1,sig.level=sig.level,
												 power=power,type=type,alternative=alternative,
												 strict=FALSE)
	# interpret
	subval$snr <- subval$delta
	if (!missing(opy) && !is.null(opy)) {
		subval$snr <- .annualize(subval$snr,opy)
		subval$n.yr <- subval$n / opy
	} else {
		subval$n.yr <- NA
	}
	
	retval <- subval[c("n","n.yr","snr","sig.level","power","alternative","note","method")]
	retval <- structure(retval,class=class(subval))
	return(retval)
}

# 2FIX: should this be expanded in its own right?
power.T2.test <- function(df1=NULL,df2=NULL,ncp=NULL,sig.level=0.05,power=NULL) {
	# stolen from power.anova.test
	if (sum(sapply(list(df1, df2, ncp, power, sig.level), is.null)) != 1) 
		stop("exactly one of 'df1', 'df2', 'ncp', 'power', and 'sig.level' must be NULL")
	if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > 
		sig.level | sig.level > 1)) 
		stop("'sig.level' must be numeric in [0, 1]")
	p.body <- quote({
		delta2 <- df2 * ncp
		pT2(qT2(sig.level, df1, df2, lower.tail = FALSE), df1, df2, delta2, lower.tail = FALSE)
	})
	if (is.null(power)) 
		power <- eval(p.body)
	else if (is.null(df1)) 
		df1 <- uniroot(function(df1) eval(p.body) - power, c(1, 3e+03))$root
	else if (is.null(df2)) 
		df2 <- uniroot(function(df2) eval(p.body) - power, c(3, 1e+06))$root
	else if (is.null(ncp))
		ncp <- uniroot(function(ncp) eval(p.body) - power, c(0, 3e+01))$root
	else if (is.null(sig.level)) 
		sig.level <- uniroot(function(sig.level) eval(p.body) - power, c(1e-10, 1 - 1e-10))$root
	else stop("internal error")
	NOTE <- "one sided test"
	METHOD <- "Hotelling test"
	retval <- structure(list(df1 = df1, df2 = df2, ncp = ncp, delta2 = df2 * ncp,
								 sig.level = sig.level, power = power, 
								 note = NOTE, method = METHOD), class = "power.htest")
	return(retval)
}

#UNFOLD

# confidence intervals on the Sharpe Ratio#FOLDUP

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

#' @title Standard error of Sharpe Ratio
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
#' sr_se(sr,df,opy,type=c("t","Lo","Z","F")) 
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
#' anse <- sr_se(rvs,df,opy,type="t")
#' anse2 <- sr_se(rvs,df,opy,type="Z")
#'
#'@export
sr_se <- function(sr,df,opy,type=c("t","Lo","Z","F")) { 
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


# 2FIX: start here:

# compute confidence intervals on the ncp of a nct 
nct.confint <- function(ts,df,level=0.95,type=c("exact","t","Z","F"),
												level.lo=(1-level)/2,level.hi=1-level.lo) {

	# 2FIX: start here
	invisible(NULL)
}



f_sr_ci_shab <- function(sample.sr,n,alpha = 0.05) {
	cn <- .srbias(n)
	medv <- sample.sr / cn
	se <- f_sr_se_shab(sample.sr,n)
	zalp <- qnorm(1 - alpha / 2)
	cilo <- medv - zalp * se
	cihi <- medv + zalp * se
	return(list('lo' = cilo,'hi' = cihi))
}

f_sr_ci_lo <- function(sample.sr,n,alpha = 0.05) {
	se <- f_sr_se_lo(sample.sr,n)
	zalp <- qnorm(1 - alpha / 2)
	cilo <- sample.sr - zalp * se
	cihi <- sample.sr + zalp * se
	return(list('lo' = cilo,'hi' = cihi))
}

# Walck gives this normal approximation
f_sr_ci_walck <- function(sample.sr,n,alpha = 0.05) {
	se <- f_sr_se_walck(sample.sr,n)
	zalp <- qnorm(1 - alpha / 2)
	midp <- sample.sr * (1 - 1 / (4 * (n - 1)))
	cilo <- midp - zalp * se
	cihi <- midp + zalp * se
	return(list('lo' = cilo,'hi' = cihi))
}

# these are the 'exact' symmetric CI, which I first saw in 
# Scholz' paper. I thought they were novel at that time.:w
f_sr_ci_scholz <- function(sample.sr,n,alpha = 0.05) {
	sn <- sqrt(n)
	t <- sample.sr * sn
	cilo <- (1 / sn) * f_nct_cdf_ncp(t,df = n-1,alpha = 1 - alpha/2)
	cihi <- (1 / sn) * f_nct_cdf_ncp(t,df = n-1,alpha = alpha/2)
	return(list('lo' = cilo,'hi' = cihi))
}
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
# power #FOLDUP

# the power of an f-test 
f_fpower <- function(df1,df2,ncp,alpha = 0.05) {
	pf(qf(alpha,df1=df1,df2=df2,ncp=0,lower.tail=FALSE),df1 = df1,df2 = df2,ncp = ncp,lower.tail = FALSE)
}

# the power of the Hotelling test
f_hpower <- function(n,p,rhosq,alpha = 0.05) {
	f_fpower(df1 = p,df2 = n - p,ncp = n * rhosq,alpha = alpha)
}
#UNFOLD
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

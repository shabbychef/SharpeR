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
# Copyright: Steven E. Pav, 2012-2012
# Author: Steven E. Pav
# Comments: Steven E. Pav
# SVN: $Id: blankheader.txt 25454 2012-02-14 23:35:25Z steven $

#         @include utils.R
#source("utils.R")

# 2FIX: is df the # of observations or the d.f. of the t-stat? ack!

# Sharpe Ratio as a distribution
# dsr, psr, qsr, rsr#FOLDUP
#' @title The (non-central) Sharpe Ratio.
#'
#' @description 
#'
#' Suppose \eqn{x_i}{xi} are \eqn{n}{n} independent draws of a normal random
#' variable with mean \eqn{\mu}{mu} and variance \eqn{\sigma^2}{sigma^2}.
#' Let \eqn{\bar{x}}{xbar} be the sample mean, and \eqn{s}{s} be
#' the sample standard deviation (using Bessel's correction). Then
#' \eqn{z = \frac{\bar{x}}{s}}{z = xbar/s} is the (sample) Sharpe Ratio.
#' 
#' The units of \eqn{z}{z} is \eqn{\mbox{time}^{-1/2}}{per root time}.
#' Typically the Sharpe Ratio is \emph{annualized} by multiplying by
#' \eqn{\sqrt{p}}{sqrt(p)}, where \eqn{p}{p} is the number of observations
#' per year (or whatever the target annualization epoch.)
#'
#' Letting \eqn{z = \sqrt{p}\frac{\bar{x}}{s}}{z = sqrt(p) xbar/s},
#' where the sample estimates are based on \eqn{n}{n} observations, 
#' then \eqn{z}{z} takes a (non-central) Sharpe Ratio distribution
#' with \eqn{n}{n} 'degrees of freedom', non-centrality parameter
#' \eqn{\delta = \frac{\mu}{\sigma}}{delta = mu/sigma}, and 
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
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @references 
#'
#' William F. Sharpe, 'Mutual Fund Performance,' Journal of Business, vol. 39, 
#' p 119, 1965. \url{http://ideas.repec.org/a/ucp/jnlbus/v39y1965p119.html}
#' 
#' Andrew W. Lo, 'The Statistics of Sharpe Ratios,' Financial Analysts Journal,
#' vol. 58, no. 4, 2002. \url{http://ssrn.com/paper=377260}
#'
#' @examples \dontrun{
#' rvs <- rsr(2048, 253*6, 0, 253)
#' dvs <- dsr(rvs, 253*6, 0, 253)
#' pvs <- psr(rvs, 253*6, 0, 253)
#' plot(ecdf(pvs))
#' pvs <- psr(rvs, 253*6, 1, 253)
#' plot(ecdf(pvs))
#'}
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
#' Here we assume that the sample statistic has been \emph{annualized}
#' in the same manner as the Sharpe ratio (see \code{\link{dsr}}), that
#' is, by multiplying by \eqn{p}{p}, the number of observations per
#' epoch.
#'
#' @usage
#'
#' dT2(x, df1, df2, delta2, opy, log = FALSE)
#'
#' pT2(q, df1, df2, delta2, opy, lower.tail = TRUE, log.p = FALSE) 
#'
#' qT2(p, df1, df2, delta2, opy, lower.tail = TRUE, log.p = FALSE) 
#'
#' rT2(n, df1, df2, delta2, opy)
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
#' @param opy the number of observations per 'year'. \code{x}, \code{q}, and 
#'        \code{delta2} are quoted in 'annualized' units, that is, per 'year',
#'        but returns are observed possibly at a rate of \code{opy} per 
#'        'year.' default value is 1, meaning no deannualization is performed.
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
#' The annualization should just go away.
#' @references 
#' Wikipedia contributors, 'Hotelling's T-squared distribution', Wikipedia, The Free Encyclopedia, 
#' 11 December 2012, 13:38 UTC, \url{http://en.wikipedia.org/w/index.php?title=Hotelling\%27s_T-squared_distribution\&oldid=527530524}
#' [accessed 9 January 2013]
#' @examples \dontrun{
#' rvs <- rT2(2048, 4, 253*6, 0, 253)
#' dvs <- dT2(rvs, 4, 253*6, 0, 253)
#' pvs <- pT2(rvs, 4, 253*6, 0, 253)
#' plot(ecdf(pvs))
#' pvs <- pT2(rvs, 4, 253*6, 1, 253)
#' plot(ecdf(pvs))
#' }
dT2 <- function(x, df1, df2, delta2, opy, log = FALSE) {
	if (!missing(opy)) {
		x <- .deannualize2(x, opy)
		if (!missing(delta2)) {
			delta2 <- .deannualize2(delta2, opy)
		}
	}
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
pT2 <- function(q, df1, df2, delta2, opy, ...) {
	if (!missing(opy)) {
		q <- .deannualize2(q, opy)
		if (!missing(delta2)) {
			delta2 <- .deannualize2(delta2, opy)
		}
	}
	Fs <- .T2_to_F(q, df1, df2)
	if (missing(delta2)) {
		retv <- pf(Fs, df1 = df1, df2 = df2 - df1, ncp = 0, ...)
	} else {
		retv <- pf(Fs, df1 = df1, df2 = df2 - df1, ncp = delta2, ...)
	}
	return(retv)
}
#' @export 
qT2 <- function(p, df1, df2, delta2, opy, lower.tail = TRUE, log.p = FALSE) {
	if (!missing(opy) && !missing(delta2)) {
		delta2 <- .deannualize2(delta2, opy)
	}
	if (log.p) {
		Fp <- .logT2_to_logF(p, df1, df2)
	} else {
		Fp <- .T2tologF(p, df1, df2)
	}
	if (missing(delta2)) {
		Fq <- qf(Fp, df1 = df1, df2 = df2 - df2, ncp = 0,
						 lower.tail = lower.tail, log.p = log.p)
	} else {
		Fq <- qf(Fp, df1 = df1, df2 = df2 - df2, ncp = delta2,
						 lower.tail = lower.tail, log.p = log.p)
	}
	retv <- .F_to_T2(Fq, df1, df2)
	if (!missing(opy)) {
		retv <- .annualize2(retv,opy)
	}
	return(retv)
}
#' @export 
rT2 <- function(n, df1, df2, delta2, opy) {
	if (!missing(opy) && !missing(delta2)) {
		delta2 <- .deannualize2(delta2, opy)
	}
	if (missing(delta2)) {
		Fr <- rf(n, df1 = df1, df2 = df2 - df1)
	} else {
		Fr <- rf(n, df1 = df1, df2 = df2 - df1, ncp = delta2)
	}
	retv <- .F_to_T2(Fr, df1, df2)
	if (!missing(opy)) {
		retv <- .annualize2(retv,opy)
	}
	return(retv)
}
#UNFOLD

# SR^*
# dsrstar, psrstar, qsrstar, rsrstar#FOLDUP
#' @title The (non-central) maximal Sharpe Ratio distribution.
#'
#' @description 
#'
#' Suppose \eqn{x_i}{xi} are \eqn{n}{n} independent draws of a \eqn{q}{q}-variate
#' normal random variable with mean \eqn{\mu}{mu} and covariance matrix
#' \eqn{\Sigma}{Sigma}. Let \eqn{\bar{x}}{xbar} be the (vector) sample mean, and 
#' \eqn{S}{S} be the sample covariance matrix (using Bessel's correction). Then
#' \eqn{z^2 = \bar{x}^{\top}S^{-1}\bar{x}}{z^2 = xbar' S^-1 xbar} is
#' the squared (sample) Sharpe Ratio of the \emph{Markowitz portfolio}, 
#' \eqn{w \propto_{+} S^{-1}\bar{x}}{w = S^-1 xbar}. The Markowitz
#' portfolio maximizes the (sample) Sharpe Ratio over all static portfolios,
#' and thus \eqn{z}{z} is the maximal achievable Sharpe Ratio.
#'
#'
#' @usage
#'
#' dsrstar(x, df1, df2, snrstar, opy, log = FALSE)
#'
#' psrstar(q, df1, df2, snrstar, opy, lower.tail = TRUE, log.p = FALSE) 
#'
#' qsrstar(p, df1, df2, snrstar, opy, lower.tail = TRUE, log.p = FALSE) 
#'
#' rsrstar(n, df1, df2, snrstar, opy)
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. 
#' @param df1 the number of assets in the portfolio.
#' @param df2 the number of observations.
#' @param snrstar the non-centrality parameter, defined as 
#'        \eqn{\zeta^{*} = \sqrt{\mu' \Sigma^{-1} \mu}}{zeta^* = sqrt(mu' Sigma^-1 mu)}
#'        for population parameters
#'        defaults to 0, i.e. a central maximal Sharpe Ratio distribution.
#' @param opy the number of observations per 'year'. \code{x}, \code{q}, and 
#'        \code{snrstar} are quoted in 'annualized' units, that is, per 'year',
#'        but returns are observed possibly at a rate of \code{opy} per 
#'        'year.' default value is 1, meaning no deannualization is performed.
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
#' @examples \dontrun{
#' # generate some variates 
#' rvs <- rsrstar(2048, 8, 253*6, 0, 253)
#' hist(rvs)
#' # these should be uniform:
#' isp <- psrstar(rvs, 8, 253*6, 0, 253)
#' plot(ecdf(isp))
#'}
#' @export 
dsrstar <- function(x, df1, df2, snrstar, opy, log = FALSE) {
	x.T2 <- .srstar_to_T2(x, df2)
	if (missing(snrstar)) {
		delta2 <- 0
	} else {
		delta2 <- .srstar_to_T2(snrstar, df2)
	}
	d.T2 <- dT2(x.T2, df1, df2, delta2, opy=opy, log=log)
	if (log) {
		retv <- (d.T2 - log(.d_T2_to_srstar(x, df2)))
	} else {
		retv <- (d.T2 / .d_T2_to_srstar(x, df2))
	}
	return(retv)
}
#' @export 
psrstar <- function(q, df1, df2, snrstar, ...) {
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
qsrstar <- function(p, df1, df2, snrstar, ...) {
	if (missing(snrstar)) {
		delta2 = 0.0
	} else {
		delta2 <- .srstar_to_T2(snrstar, df2)
	}
	q.T2 <- qT2(p, df1, df2, delta2, ...)
	retv <- .T2_to_srstar(q.T2, df2)
	return(retv)
}
#' @export 
rsrstar <- function(n, df1, df2, snrstar, ...) {
	if (missing(snrstar)) {
		delta2 = 0.0
	} else {
		delta2 <- .srstar_to_T2(snrstar, df2)
	}
	r.T2 <- rT2(n, df1, df2, delta2, ...) 
	retv <- .T2_to_srstar(r.T2, df2)
	return(retv)
}
#UNFOLD

# confidence distributions 

#' @title The lambda-prime distribution.
#'
#' @description 
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
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. 
#' @param df the degrees of freedom of the t-statistic.
#' @param tstat the observed (non-central) t-statistic.
#' @param log,log.p logical; if TRUE, probabilities p are given as \eqn{\mbox{log}(p)}{log(p)}.
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
#' Bruno Lecoutre, 'Another Look at the Confidence Intervals for
#' the Noncentral T Distribution,' J. Mod. Appl. Stat. Meth., vol. 6, no. 1,
#' pp 107-116, 2007, \url{http://www.univ-rouen.fr/LMRS/Persopage/Lecoutre/telechargements/Lecoutre_Another_look-JMSAM2007_6(1).pdf}
#' @examples \dontrun{
#' rvs <- rnorm(2048)
#' pvs <- plambdap(rvs, 253*6, 0.5)
#' plot(ecdf(pvs))
#' pvs <- plambdap(rvs, 253*6, 1)
#' plot(ecdf(pvs))
#' pvs <- plambdap(rvs, 253*6, -0.5)
#' plot(ecdf(pvs))
#'}
plambdap <- function(q,df,tstat,lower.tail=TRUE,...) {
	# this is just a silly wrapper on pt
	retv <- pt(q=tstat,df=df,ncp=q,lower.tail=!lower.tail,...)
	return(retv)
}
qlambdap <- function(p,df,tstat,lower.tail=TRUE,log.p=FALSE) {
	if (log.p) {
		flo <- min(-1,tstat - qnorm((1 + p)/2))
		fhi <- max(1,tstat - qnorm((p)/2))
	} else {

	}
	# 2FIX: log.p not respected. bleah. lower.tail? bleah.
	#find approximate endpoints;
	#expand them until pt(tstat,df,flo) > p and pt(tstat,df,fhi) < p
	while ((pt(tstat,df,flo,lower.tail,log.p) < p) && (flo > -100000)) { flo <- 2 * flo }
	while ((pt(tstat,df,fhi,lower.tail,log.p) > p) && (fhi < 100000)) { fhi <- 2 * fhi }
	ncp <- uniroot(function(ncp)(pt(tstat,df=df,ncp=ncp,lower.tail=lower.tail,log.p=log.p) - p),
								 c(flo,fhi))
	return(ncp$root)
}




# junkyard


########################################################################
## expectation of the t:#FOLDUP
# the geometric bias of Sharpe ratio
f_tbias <- function(n) { 
	sqrt((n-1) / 2) * exp(lgamma((n-2)/2) - lgamma((n-1)/2))
}

#approximate tbias
f_apx_tbias1 <- function(n) { 
	1 + (0.75 / n)
}
f_apx_tbias2 <- function(n) { 
#1 + (0.75 / n) + (2 / n**2)
	1 + (0.75 / (n - 1)) + (32 / (25 * ((n-1) ** 2)))
}
#UNFOLD
########################################################################
# power #FOLDUP

# the power of the univariate t-test;
f_tpower <- function(n,rho = 0,alpha = 0.05) {
	f_tpower_ncp(ncp = sqrt(n) * rho,n = n,alpha = alpha)
}

# the power of the univariate t-test as function of the ncp
f_tpower_ncp <- function(ncp,n,alpha = 0.05) {
	pt(qt(1-alpha,n-1),df = n-1,ncp = ncp,lower.tail = FALSE)
}

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
# sample size computations#FOLDUP

# find the sample size for a given power of the univariate hotelling test
f_hreqsize <- function(rhosq,p,powr = 0.80,alpha = 0.05) {
	#2FIX: get a sane upper bound!
	zz <- uniroot(function(n,p,rhosq,alpha,powr)(f_hpower(n,p,rhosq,alpha) - powr),
								c(max(8,p+1),2000000 * 10 / (rhosq)),p = p,rhosq = rhosq,powr = powr,alpha = alpha)
	return(zz$root)
}

#estimate the required sample size using a fit...
#this was all done at alpha = 0.05, so that is not a variable.
f_est_hreqsize <- function(rhosq,p,powr = 0.80) {
	estv <- exp(2.259 - log(rhosq) + 0.363 * log(p) + 1.31 * log(powr) - 0.0757 * log(powr) * log(p))
}

# find the sample size for a given power of the univariate t-test
f_treqsize <- function(rho,powr = 0.80,alpha = 0.05) {
	zz <- uniroot(function(n,rho,alpha,powr)(f_tpower(n,rho,alpha) - powr),
								c(8,20 * 10 / (rho*rho)),rho = rho,powr = powr,alpha = alpha)
	return(zz$root)
}
#UNFOLD
########################################################################
#inversions#FOLDUP

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



########################################################################
# confidence intervals

## confidence intervals on the Sharpe Ratio#FOLDUP

#the sample.sr should *not* be annualized
f_sr_se_shab <- function(sample.sr,n) {
	cn <- f_tbias(n)
	dn <- (n-1) / ((n-3) * cn * cn)
	W  <- (sample.sr / cn) ** 2
	se <- sqrt((dn/n) + W * (dn - 1))
}

f_sr_se_walck <- function(sample.sr,n) {
	se <- sqrt((1/n) + 0.5 * sample.sr ** 2 / (n - 1))
}

f_sr_se_lo <- function(sample.sr,n) {
	se <- sqrt((1 + 0.5 * sample.sr ** 2) / (n - 1))
}

f_sr_ci_shab <- function(sample.sr,n,alpha = 0.05) {
	cn <- f_tbias(n)
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

#MLE of the ncp based on a single F-stat
fncp.mle <- function(Fs,df1,df2,ub=NULL,lb=0) {
	if (Fs <= 1) { return(0.0) }  # Spruill's Thm 3.1, eqn 8
	if (is.null(ub)) {
		prevdpf <- -Inf
		ub <- 1
		dpf <- df(Fs,df1,df2,ncp=ub,log=TRUE)
		while (prevdpf < dpf) {
			prevdpf <- dpf
			ub <- 2 * ub
			dpf <- df(Fs,df1,df2,ncp=ub,log=TRUE)
		}
		lb <- ifelse(ub > 2,ub/4,lb)
	}
	ncp.mle <- optimize(function(ncp){df(Fs,df1,df2,ncp=ncp,log=TRUE)},
											c(lb,ub),maximum=TRUE)$maximum;
	return(ncp.mle)
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

########################################################################
# distributions#FOLDUP

# density, distribution, quantile, and generator for (noncentral) Hotelling
# distribution; this is just a rescaling of the (noncentral) F distribution.
dhot <- function(x, p, n, ncp = 0, log = FALSE) {
	z <- df(f_hot2F(x,p = p,n = n),df1 = p,df2 = n - p,ncp = ncp,log = log)
	if (log) {
		return(log(f_F2hot(1)) + z)
	} else {
		return(f_F2hot(z))
	}
}

phot <- function(q, p, n, ncp = 0, lower.tail = TRUE, log.p = FALSE) {
	return(pf(f_hot2F(q,p = p,n = n),df1 = p,df2 = n - p,ncp = ncp, 
						lower.tail = lower.tail, log.p = log.p))
}
qhot <- function(pct, p, n, ncp = 0, lower.tail = TRUE, log.p = FALSE) {
	return(f_F2hot(qf(pct,df1 = p,df2 = n - p,ncp = ncp, 
						lower.tail = lower.tail, log.p = log.p),p = p,n = n))
}
rhot <- function(ngen, p, n, ncp = 0) {
	return(f_F2hot(rf(ngen, df1 = p,df2 = n - p,ncp = ncp),p = p,n = n))
}

#Hotelling 
gen_hot_T2 <- function(n,p = 1,df = 10,mean = 0,sd = 1) {
	#2FIX: this was just cut and paste from gen_t and is not
  #correct .
	return(mean + sqrt((df-2)/df) * sd * rt(n,df = df))
}
#UNFOLD


# standard errors and confidence intervals
#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:

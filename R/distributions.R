#
# distributions
#
# see also:
# changelog:
#
# Created: 2012.05.19
# Copyright: Steven E. Pav, 2012-2012
# Author: Steven E. Pav
# Comments: Steven E. Pav
# SVN: $Id: blankheader.txt 25454 2012-02-14 23:35:25Z steven $
#
#         @include utils.R
#source("utils.R")

# Sharpe Ratio as a distribution
# dsr, psr, qsr, rsr#FOLDUP
#' the (non-central) Sharpe Ratio
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
#' @param df the number of observations the statistic is based on.
#' @param snr the 'signal-to-noise' parameter, defined as the population
#'        mean divided by the population standard deviation, 'annualized'.
#' @param opy the number of observations per 'year'. \code{x}, \code{q}, and 
#'        \code{snr} are quoted in 'annualized' units, that is, per square root 
#'        'year', but returns are observed possibly at a rate of \code{opy} per 
#'        'year.' default value is 1, meaning no deannualization is performed.
#' @param log,log.p logical; if TRUE, probabilities p are given as \eqn{\mbox{log}(p)}{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are
#'        \eqn{P[X \le x]}{P\[X <= x\]}, otherwise, \eqn{P[X > x]}{P\[X > x\]}.
#' @keywords distribution 
#' @return \code{dsr} gives the density, \code{psr} gives the distribution function,
#' \code{qsr} gives the quantile function, and \code{rsr} generates random deviates.
#'
#' Invalid arguments will result in return value \code{NaN} with a warning.
#' @aliases psr qsr rsr
#' @seealso t-distribution functions, \code{\link{dt},\link{pt},\link{qt},\link{rt}}
#' @export 
#' @author Steven E. Pav <shabbychef@@gmail.com>
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
#' the (non-central) Hotelling \eqn{T^2}{T2} distribution on a df1-vector with
#' df2-observations.
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
#'        observations had been drawn.
#' @param df2 the number of observations.
#' @param delta2 the non-centrality parameter, defined as 
#'        \eqn{delta^2 = df_2 * (\mu' \Sigma^{-1} \mu)}{delta^2 = df2 * (mu' Sigma^-1 mu)} for population parameters
#'        defaults to 0, i.e. a central \eqn{T^2}{T2} distribution.
#' @param opy the number of observations per 'year'. \code{x}, \code{q}, and 
#'        \code{delta2} are quoted in 'annualized' units, that is, per 'year',
#'        but returns are observed possibly at a rate of \code{opy} per 
#'        'year.' default value is 1, meaning no deannualization is performed.
#' @param log,log.p logical; if TRUE, probabilities p are given as \eqn{\mbox{log}(p)}{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are
#' @keywords distribution 
#' @return \code{dT2} gives the density, \code{pT2} gives the distribution function,
#' \code{qT2} gives the quantile function, and \code{rT2} generates random deviates.
#'
#' Invalid arguments will result in return value \code{NaN} with a warning.
#' @aliases pT2 qT2 rT2 
#' @seealso F-distribution functions, \code{\link{df},\link{pf},\link{qf},\link{rf}}
#' @export 
#' @author Steven E. Pav <shabbychef@@gmail.com>
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
#' the (non-central) maximal Sharpe Ratio distribution on df1-assets with
#' df2-observations.
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
#'        \eqn{snr^{*} = \sqrt{\mu' \Sigma^{-1} \mu}}{snrstar = sqrt(mu' Sigma^-1 mu)}
#'        for population parameters
#'        defaults to 0, i.e. a central sr^* distribution.
#' @param opy the number of observations per 'year'. \code{x}, \code{q}, and 
#'        \code{snrstar} are quoted in 'annualized' units, that is, per 'year',
#'        but returns are observed possibly at a rate of \code{opy} per 
#'        'year.' default value is 1, meaning no deannualization is performed.
#' @param log,log.p logical; if TRUE, probabilities p are given as \eqn{\mbox{log}(p)}{log(p)}.
#' @param lower.tail logical; if TRUE (default), probabilities are
#' @keywords distribution 
#' @return \code{dsrstar} gives the density, \code{psrstar} gives the distribution function,
#' \code{qsrstar} gives the quantile function, and \code{rsrstar} generates random deviates.
#'
#' Invalid arguments will result in return value \code{NaN} with a warning.
#' @aliases psrstar qsrstar rsrstar
#' @seealso Hotelling T2-distribution functions, \code{\link{dT2},\link{pT2},\link{qT2},\link{rT2}}
#' @export 
#' @author Steven E. Pav <shabbychef@@gmail.com>
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

# standard errors and confidence intervals
#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:

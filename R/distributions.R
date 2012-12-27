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

source("utils.R")

# Sharpe Ratio
# Sharpe Ratio as a distribution#FOLDUP

#' PDF of the Sharpe Ratio
#'
#' @param x vector of quantiles.
#' @param q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. if \code{length(n) > 1}, the length is taken to be the number required.
#' @param df the number of observations the statistic is based on.
#' @param snr the 'signal-to-noise' parameter.
#' @keywords Sharpe t-stat quant
#' @return some value
#' @usage 
#'   does this go on and on?
#' @aliases psr qsr rsr
#' @references none
#' @seealso NA
#' @export 
#' @author Steven E. Pav <shabbychef@@gmail.com>
#' @examples none
dsr <- function(x, df, snr, log = FALSE) {
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
psr <- function(q, df, snr, ...) {
  tq <- .sr_to_t(q, df)
  if (missing(snr)) {
    retv <- pt(q = tq, df = df - 1, ...)    
  } else {
    ncp <- .sr_to_t(snr, df)
    retv <- pt(q = tq, df = df - 1, ncp = ncp, ...)    
  }
  return(retv)  
}
qsr <- function(p, df, snr, lower.tail = TRUE, log.p = FALSE) {
  if (missing(snr)) {
    tq <- qt(p, df = df - 1, lower.tail = lower.tail, log.p = log.p)
  } else {
    ncp <- .sr_to_t(snr, df)
    tq <- qt(p, df = df - 1, ncp = ncp, 
             lower.tail = lower.tail, log.p = log.p)
  }
  retv <- .t_to_sr(tq, df)
  return(retv)
}
rsr <- function(n, df, snr) {
  if (missing(snr)) {
    tr <- rt(n, df = df - 1)
  } else {
    ncp <- .sr_to_t(snr, df)
    tr <- rt(n, df = df - 1, ncp = ncp) 
  }
  retv <- .t_to_sr(tr, df)
  return(retv)
}
#UNFOLD

# Hotelling
# dT2, pT2, qT2, rT2#FOLDUP
dT2 <- function(x, df1, df2, ncp, log = FALSE) {
	Fs <- .T2_to_F(x, df1, df2)
	if (missing(ncp)) {
		dv <- df(Fs, df1 = df1, df2 = df2 - df1, log = log)
	} else {
		dv <- df(Fs, df1 = df1, df2 = df2 - df1, ncp = ncp, log = log)
	}
	if (log) {
		retv <- (dv + log(.d_T2_to_F(x, df1, df2)))
	} else {
		retv <- (dv * .d_T2_to_F(x, df1, df2))
	}
	return(retv)
}
pT2 <- function(q, df1, df2, ...) {
	Fs <- .T2_to_F(q, df1, n)
	retv <- pf(Fs, df1 = df1, df2 = df2 - df1, ...)
	return(retv)
}
qT2 <- function(p, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE) {
	if (log.p) {
		Fp <- .logT2_to_logF(p, df1, df2)
	} else {
		Fp <- .T2tologF(p, df1, df2)
	}
	if (missing(ncp)) {
		Fq <- qf(Fp, df1 = df1, df2 = df2 - df2, 
						 lower.tail = lower.tail, log.p = log.p)
	} else {
		Fq <- qf(Fp, df1 = df1, df2 = df2 - df2, ncp = ncp,
						 lower.tail = lower.tail, log.p = log.p)
	}
	retv <- .F_to_T2(Fq, df1, df2)
	return(retv)
}
rT2 <- function(n, df1, df2, ncp) {
	if (missing(ncp)) {
		Fr <- rf(n, df1 = df1, df2 = df2 - df1)
	} else {
		Fr <- rf(n, df1 = df1, df2 = df2 - df1, ncp = ncp)
	}
	retv <- .F_to_T2(Fr, df1, df2)
	return(retv)
}
#UNFOLD

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:

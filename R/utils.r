#
# utilities
#
# see also:
# changelog:
#
# Created: 2012.05.19
# Copyright: Steven E. Pav, 2012-2012
# Author: Steven E. Pav
# Comments: Steven E. Pav
# SVN: $Id: blankheader.txt 25454 2012-02-14 23:35:25Z steven $

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
#' The units of \eqn{z}{z} is \eqn{\mbox{time}^{-1/2}}{per root time}.
#' Typically the Sharpe ratio is \emph{annualized} by multiplying by
#' \eqn{\sqrt{\mbox{opy}}}{sqrt(opy)}, where \eqn{\mbox{opy}}{opy} 
#' is the number of observations
#' per year (or whatever the target annualization epoch.)
#'
#'
#' @usage
#'
#' sharpe(x,c0=0,opy=1,na.rm=FALSE)
#'
#' @param x vector of returns.
#' @param c0 the 'risk-free' or 'disastrous' rate of return.
#' @param opy the number of observations per 'year'. This is used to
#'        'annualize' the answer.
#' @param na.rm logical.  Should missing values be removed?
#' @keywords univar 
#' @return the annualized Sharpe ratio.
#' @seealso sr-distribution functions, \code{\link{dsr}, \link{psr}, \link{qsr}, \link{rsr}}
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
#' rvs <- sharpe(rnorm(253*8),opy=253)
#'
sharpe <- function(x,c0=0,opy=1,na.rm=FALSE) {
	sr <- (mean(x,na.rm=na.rm) - c0) / sd(x,na.rm=na.rm)
	if (!missing(opy))
		sr <- .annualize(sr,opy)
	return(sr)
}

# compute the markowitz portfolio
.markowitz <- function(X,mu=NULL,Sigma=NULL) {
	na.omit(X)
	if (is.null(mu)) 
		mu <- colMeans(X)
	if (is.null(Sigma)) 
		Sigma <- cov(X)
	w <- solve(Sigma,t(mu))
	n <- dim(X)[1]
	retval <- list(w = w, mu = mu, Sigma = Sigma, n = n)
	return(retval)
}


#UNFOLD
# annualize and deannualize a Sharpe Ratio#FOLDUP
#' @param sr the Sharpe Ratio, in per sqrt(epoch) units.
#' @param opy the number of observations per year. no default here.
#' @return the annualized Sharpe ratio, in per sqrt(year) units.
#' @aliases .deannualize
.annualize <- function(sr, opy) {
  return(sr * sqrt(opy))  
}
.deannualize <- function(sr.pa, opy) {
  return(sr.pa / sqrt(opy))
}

# for T^2: 
.annualize2 <- function(T2, opy) {
  return(T2 * opy)  
}
.deannualize2 <- function(T2.pa, opy) {
  return(T2.pa / opy)
}
#UNFOLD

# converting t <-> sr#FOLDUP

# conversion routines
# Sharpe Ratio to t stat
.sr_to_t <- function(sr, df) {
  return(sr * sqrt(df))
}
# derivative of same
.d_sr_to_t <- function(sr, df) {
  return(sqrt(df))
}
# t stat to Sharpe Ratio
.t_to_sr <- function(t, df) {
  return(t / sqrt(df))
}
#UNFOLD

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

# SR^*, the SR of a Markowitz portfolio
# converting T2 <-> srstar#FOLDUP
# convert hotelling T2 to maximal SR statistic, SR^*
.T2_to_srstar <- function(T2, n) {
	return(sqrt(T2 / n))
}
# derivative of same
.d_T2_to_srstar <- function(T2, n) {
	return(0.5 / sqrt(T2 * n))
}
.logT2_to_logsrstar <- function(logT2, n) {
	return(0.5 * (logT2 - log(n)))
}
# convert T2 to srstar
.srstar_to_T2 <- function(srstar, n) {
	return(n * srstar^2)
}
#UNFOLD

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:

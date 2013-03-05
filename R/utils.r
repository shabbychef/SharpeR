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
#' full.srstar(X,drag=0,opy=1)
#'
#' srstar(X,...)
#'
#' @param X matrix of returns.
#' @param drag the 'drag' term, \eqn{c_0/R}{c0/R}. defaults to 0. It is assumed
#'        that \code{drag} has been annualized, \emph{i.e.} has been multiplied
#'        by \eqn{\sqrt{opy}}{sqrt(opy)}. This is in contrast to the \code{c0}
#'        term given to \code{\link{sr}}.
#' @param opy the number of observations per 'year'. The returns are observed
#'        at a rate of \code{opy} per 'year.' default value is 1, meaning no 
#'        annualization is performed.
#' @param ... the above extra parameters,  passed on to \code{full.srstar}
#' @keywords univar 
#' @return A list with containing the following components:
#' \item{w}{the optimal portfolio.}
#' \item{mu}{the estimated mean return vector.}
#' \item{Sigma}{the estimated covariance matrix.}
#' \item{df1}{the number of assets.}
#' \item{df2}{the number of observed vectors.}
#' \item{T2}{the Hotelling \eqn{T^2} statistic.}
#' \item{srstar}{the maximal Sharpe statistic.}
#' \item{drag}{the input \code{drag} term.}
#' \item{opy}{the input \code{opy} term.}
#' @aliases srstar
#' @seealso \code{\link{full.sr}}, srstar-distribution functions, 
#' \code{\link{dsrstar}, \link{psrstar}, \link{qsrstar}, \link{rsrstar}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#'
#' @examples 
#' rvs <- full.srstar(matrix(rnorm(253*8*4),ncol=4),drag=0,opy=253)
#' rvs <- srstar(matrix(rnorm(253*8*4),ncol=4),drag=0,opy=253)
#'
full.srstar <- function(X,drag=0,opy=1) {
	retval <- .hotelling(X)
	zeta.star <- sqrt(retval$T2 / retval$df2)
	if (!missing(opy))
		zeta.star <- .annualize(zeta.star,opy)
	retval$srstar <- zeta.star - drag

	#units(retval$srstar) <- "yr^-0.5"
	retval$drag <- drag
	retval$opy <- opy
	return(retval)
}
#' @export 
srstar <- function(X,...) {
	subret <- full.srstar(X,...)
	return(subret$srstar)
}


#UNFOLD

# convert p-value for a 1-sided test into one for a two-sided test.
.oneside2two <- function(pv) {
	retval <- 1 - 2 * abs(0.5 - pv)
	return(retval)
}

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

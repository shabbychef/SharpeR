# Copyright 2012-2013 Steven E. Pav. All Rights Reserved.
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
# Created: 2013.04.16
# Copyright: Steven E. Pav, 2012-2013
# Author: Steven E. Pav
# Comments: Steven E. Pav

#' @include utils.r
#' @include distributions.r

# note: on citations, use the Chicago style from google scholar. tks.

########################################################################
# Sharpe Ratio#FOLDUP

# spawn a "SR" object.
# the Sharpe Ratio is a rescaled t-statistic.
#
# SR = R t
#
# where R is the 'rescaling', and
# t = (betahat' v - c0) / sigmahat
# is distributed as a non-central t with
# df degrees of freedom and non-centrality
# parameter
# delta = (beta' v - c0) / (sigma R)
#
# for 'convenience' we re-express SR and delta
# in 'annualized' units by multiplying them by
# sqrt(opy)
.spawn_sr <- function(sr,df,c0,opy,rescal) {
	retval <- list(sr = sr,df = df,c0 = c0,opy = opy,rescal = rescal)
	class(retval) <- "sr"
	return(retval)
}
# compute SR in only one place. I hope.
.compute_sr <- function(mu,c0,sigma,opy) {
	sr <- (mu - c0) / sigma
	if (!missing(opy))
		sr <- sr * sqrt(opy)
	return(sr)
}
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
#' sr(x,...)
#'
#' @param x vector of returns.
#' @param c0 the 'risk-free' or 'disastrous' rate of return. this is
#'        assumed to be given in the same units as x, \emph{not}
#'        in 'annualized' terms.
#' @param opy the number of observations per 'year'. This is used to
#'        'annualize' the answer.
#' @keywords univar 
#' @return a list containing the following components:
#' \item{sr}{the annualized Sharpe ratio.}
#' \item{df}{the number of observations.}
#' \item{opy}{the annualization factor.}
#' cast to class \code{sr}.
#' @seealso sr-distribution functions, \code{\link{dsr}, \link{psr}, \link{qsr}, \link{rsr}}
#' @rdname sr
#' @export sr
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
#' # Sharpe's 'model': just given a bunch of returns.
#' asr <- sr(rnorm(253*8),opy=253)
#' # given an xts object:
#' if (require(quantmod)) {
#'   getSymbols('IBM')
#'   lrets <- diff(log(IBM[,"IBM.Adjusted"]))
#'   asr <- sr(lrets,na.rm=TRUE)
#' }
#' # on a linear model, find the 'Sharpe' of the residual term
#' Factors <- matrix(rnorm(253*5*6),ncol=6)
#' Returns <- rnorm(dim(Factors)[1],0.003)
#' APT_mod <- lm(Returns ~ Factors)
#' asr <- sr(APT_mod,opy=253)
#'   
sr <- function(x,c0=0,opy=1,...) {
	UseMethod("sr", x)
}
#' @param na.rm logical.  Should missing values be removed?
#' @rdname sr
#' @method sr default
#' @S3method sr default
sr.default <- function(x,c0=0,opy=1,na.rm=FALSE) {
	mu <- mean(x,na.rm=na.rm)
	sigma <- sd(x,na.rm=na.rm)
	sr <- .compute_sr(mu,c0,sigma,opy)
	df <- ifelse(na.rm,sum(!is.na(x)),length(x))
	retval <- .spawn_sr(sr,df=df-1,c0=c0,opy=opy,rescal=1/sqrt(df))
	return(retval)
}
#' @param x a fit model of class \code{lm}.
#' @rdname sr
#' @method sr lm 
#' @S3method sr lm
sr.lm <- function(x,c0=0,opy=1,na.rm=FALSE) {
	modl <- x
	mu <- modl$coefficients["(Intercept)"]
	sigma <- sqrt(deviance(modl) / modl$df.residual)
	sr <- .compute_sr(mu,c0,sigma,opy)
	XXinv <- vcov(modl) / sigma^2
	rescal <- sqrt(XXinv["(Intercept)","(Intercept)"])
	retval <- .spawn_sr(sr,df=modl$df.residual,c0=c0,opy=opy,rescal=rescal)
	return(retval)
}
#' @param anxts an xts object.
#' @rdname sr
#' @method sr xts 
#' @S3method sr xts
sr.xts <- function(anxts,c0=0,opy=1,na.rm=FALSE) {
	if (missing(opy)) {
		TEO <- time(anxts)
		days.per.row <- as.double((TEO[length(TEO)] - TEO[1]) / (length(TEO) - 1))
		opy <- 365.25 / days.per.row
	}
	retval <- sr.default(anxts,c0=c0,opy=opy,na.rm=na.rm)
	return(retval)
}
#' @title Is this in the "sr" class?
#'
#' @description 
#'
#' Checks if an object is in the class \code{'sr'}
#'
#' @details
#'
#' To satisfy the minimum requirements of an S3 class.
#'
#' @usage
#'
#' is.sr(x)
#'
#' @param x an object of some kind.
#' @return a boolean.
#' @seealso sr
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @export
#'
#' @examples 
#' rvs <- sr(rnorm(253*8),opy=253)
#' is.sr(rvs)
is.sr <- function(x) inherits(x,"sr")

#' @S3method format sr
#' @export
format.sr <- function(x,...) {
	# oh! ugly! ugly!
	retval <- capture.output(print(x,...))
	return(retval)
}
#' @S3method print sr
#' @export
print.sr <- function(x,...) {
	tval <- .sr2t(x)
	pval <- pt(tval,x$df,lower.tail=FALSE)
	coefs <- cbind(x$sr,tval,pval)
	colnames(coefs) <- c("stat","t.stat","p.value")
	rownames(coefs) <- c("Sharpe")
	printCoefmat(coefs,P.values=TRUE,has.Pvalue=TRUE)
}
# @hadley's suggested form
# print.sr <- function(x,...) cat(format(x,...), "\n")

# SR methods#FOLDUP
# get the t-stat associated with an SR object.
.sr2t <- function(x) {
	tval <- x$sr / (x$rescal * sqrt(x$opy))
	return(tval)
}
# and the reverse
.t2sr <- function(x,tval) {
	srval <- tval * (x$rescal * sqrt(x$opy))
	return(srval)
}
.psr <- function(q,zeta,...) {
	retv <- prt(q$sr,df=q$df,K=(q$rescal * sqrt(q$opy)),rho=zeta,...)
	return(retv)
}
.dsr <- function(q,zeta,...) {
	retv <- drt(q$sr,df=q$df,K=(q$rescal * sqrt(q$opy)),rho=zeta,...)
	return(retv)
}

#' @title Change the annualization of a Sharpe ratio.
#'
#' @description 
#'
#' Changes the annualization factor of a Sharpe ratio statistic.
#'
#' @usage
#'
#' reannualize(x,opy)
#'
#' @param x an object of class \code{sr}
#' @param opy the new observations per year
#' @return an object of class \code{sr} with the annualization
#' parameter updated.
#' @seealso sr
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @export
#'
#' @examples 
#' mysr <- sr(rnorm(253*8),opy=253)
#' # turn back to 'daily' Sharpe
#' mysr2 <- reannualize(mysr,opy=1)
reannualize <- function(x,opy) {
	if (!is.sr(x)) stop("must give sr object")
	x$opy <- opy
	return(x)
}
#UNFOLD
#UNFOLD

########################################################################
# Optimal Sharpe ratio#FOLDUP

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
#' sropt(X,drag=0,opy=1)
#'
#' @param X matrix of returns.
#' @param drag the 'drag' term, \eqn{c_0/R}{c0/R}. defaults to 0. It is assumed
#'        that \code{drag} has been annualized, \emph{i.e.} has been multiplied
#'        by \eqn{\sqrt{opy}}{sqrt(opy)}. This is in contrast to the \code{c0}
#'        term given to \code{\link{sr}}.
#' @param opy the number of observations per 'year'. The returns are observed
#'        at a rate of \code{opy} per 'year.' default value is 1, meaning no 
#'        annualization is performed.
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
#' @seealso \code{\link{sr}}, sropt-distribution functions, 
#' \code{\link{dsropt}, \link{psropt}, \link{qsropt}, \link{rsropt}}
#' @export 
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @family sropt
#' @examples 
#' rvs <- sropt(matrix(rnorm(253*8*4),ncol=4),drag=0,opy=253)
#'
sropt <- function(X,drag=0,opy=1) {
	retval <- .hotelling(X)
	zeta.star <- sqrt(retval$T2 / retval$df2)
	if (!missing(opy))
		zeta.star <- .annualize(zeta.star,opy)
	retval$sropt <- zeta.star - drag

	#units(retval$sropt) <- "yr^-0.5"
	retval$drag <- drag
	retval$opy <- opy
	class(retval) <- "sropt"
	return(retval)
}
#UNFOLD

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r

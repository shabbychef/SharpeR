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
#' @include estimation.r

########################################################################
# Sharpe Ratio#FOLDUP

# spawn a "SR" object.
#' @title Create an 'sr' object.
#'
#' @description 
#'
#' Spawns an object of class \code{sr}.
#'
#' @details
#'
#' The \code{sr} class contains information about a rescaled t-statistic.
#' The following are list attributes of the object:
#' \itemize{
#' \item \code{sr} The Sharpe ratio statistic.
#' \item \code{df} The d.f. of the equivalent t-statistic.
#' \item \code{c0} The drag 'risk free rate' used.
#' \item \code{opy} The 'observations per year'.
#' \item \code{rescal} The rescaling parameter.
#' \item \code{epoch} The string name of the 'epoch'.
#' }
#'
#' The stored Sharpe statistic, \code{sr} is equal to the t-statistic 
#' times \eqn{rescal * sqrt{opy}}{rescal * sqrt(opy)}.
#'
#' For the most part, this constructor should \emph{not} be called directly,
#' rather \code{\link{as.sr}} should be called instead to compute the
#' Sharpe ratio.
#'
#' @usage
#'
#' sr(sr,df,c0=0,opy=1,rescal=sqrt(1/(df+1)),epoch="yr") 
#'
#' @param sr a Sharpe ratio statistic.
#' @param df the degrees of freedom of the equivalent t-statistic.
#' @param c0 the 'risk-free' or 'disastrous' rate of return. this is
#'        assumed to be given in the same units as x, \emph{not}
#'        in 'annualized' terms.
#' @param opy the number of observations per 'year'. This is used to
#'        'annualize' the answer.
#' @param rescal the rescaling parameter.
#' @param epoch the string representation of the 'year', defaulting
#'        to 'yr'.
#' @keywords univar 
#' @return a list cast to class \code{sr}.
#' @seealso \code{\link{as.sr}}
#' @rdname sr
#' @export sr
#' @template etc
#' @template sr
#'
#' @note
#' 2FIX: allow rownames? 
#'
#' @examples 
#' # roll your own.
#' opy <- 253
#' zeta <- 1.0
#' n <- 6 * opy
#' rvs <- rsr(1,n,zeta,opy=opy)
#' roll.own <- sr(sr=rvs,df=n-1,opy=opy,rescal=sqrt(1/n))
#' # put a bunch in. naming becomes a problem.
#' rvs <- rsr(5,n,zeta,opy=opy)
#' roll.own <- sr(sr=rvs,df=n-1,opy=opy,rescal=sqrt(1/n))
#'
sr <- function(sr,df,c0=0,opy=1,rescal=sqrt(1/(df+1)),epoch="yr") {
	retval <- list(sr = sr,df = df,c0 = c0,
								 opy = opy,rescal = rescal,epoch = epoch)
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
#' as.sr(x,...)
#'
#' @param x vector of returns.
#' @param c0 the 'risk-free' or 'disastrous' rate of return. this is
#'        assumed to be given in the same units as x, \emph{not}
#'        in 'annualized' terms.
#' @param opy the number of observations per 'year'. This is used to
#'        'annualize' the answer.
#' @param epoch the string representation of the 'year', defaulting
#'        to 'yr'.
#' @param ... further arguments to be passed to or from methods.
#' @keywords univar 
#' @return a list containing the following components:
#' \item{sr}{the annualized Sharpe ratio.}
#' \item{df}{the t-stat degrees of freedom.}
#' \item{c0}{the risk free term.}
#' \item{opy}{the annualization factor.}
#' \item{rescal}{the rescaling factor.}
#' \item{epoch}{the string epoch.}
#' cast to class \code{sr}.
#' @seealso sr-distribution functions, \code{\link{dsr}, \link{psr}, \link{qsr}, \link{rsr}}
#' @rdname as.sr
#' @export as.sr
#' @template etc
#' @template sr
#' @references 
#' 
#' Lo, Andrew W. "The statistics of Sharpe ratios." Financial Analysts Journal (2002): 36-52.
#' \url{http://ssrn.com/paper=377260}
#'
#' @examples 
#' # Sharpe's 'model': just given a bunch of returns.
#' asr <- as.sr(rnorm(253*8),opy=253)
#' # or a matrix, with a name
#' my.returns <- matrix(rnorm(253*10),ncol=1)
#' colnames(my.returns) <- c("my strategy")
#' asr <- as.sr(my.returns)
#' # given an xts object:
#' if (require(quantmod)) {
#'   IBM <- getSymbols('IBM',auto.assign=FALSE)
#'   lrets <- diff(log(IBM[,"IBM.Adjusted"]))
#'   asr <- as.sr(lrets,na.rm=TRUE)
#' }
#' # on a linear model, find the 'Sharpe' of the residual term
#' nfac <- 5
#' nyr <- 10
#' opy <- 253
#' set.seed(as.integer(charToRaw("determinstic")))
#' Factors <- matrix(rnorm(opy*nyr*nfac,mean=0,sd=0.0125),ncol=nfac)
#' Betas <- exp(0.1 * rnorm(dim(Factors)[2]))
#' Returns <- (Factors %*% Betas) + rnorm(dim(Factors)[1],mean=0.0005,sd=0.012)
#' APT_mod <- lm(Returns ~ Factors)
#' asr <- as.sr(APT_mod,opy=opy)
#' # try again, but make the Returns independent of the Factors.
#' Returns <- rnorm(dim(Factors)[1],mean=0.0005,sd=0.012)
#' APT_mod <- lm(Returns ~ Factors)
#' asr <- as.sr(APT_mod,opy=opy)
#'   
as.sr <- function(x,c0=0,opy=1,...) {
	UseMethod("as.sr", x)
}
#' @param na.rm logical.  Should missing values be removed?
#' @rdname as.sr
#' @method as.sr default
#' @S3method as.sr default
as.sr.default <- function(x,c0=0,opy=1,na.rm=FALSE,epoch="yr") {
	mu <- mean(x,na.rm=na.rm)
	sigma <- sd(x,na.rm=na.rm)
	z <- .compute_sr(mu,c0,sigma,opy)
	dim(z) <- c(1,1)
	rownames(z) <- unlist(dimnames(x))
	if (is.null(rownames(z)))
		rownames(z) <- deparse(substitute(x))
	df <- ifelse(na.rm,sum(!is.na(x)),length(x))
	retval <- sr(z,df=df-1,c0=c0,opy=opy,
							 rescal=1/sqrt(df),epoch=epoch)
	return(retval)
}
#' @param modl a fit model of class \code{lm}.
#' @rdname as.sr
#' @method as.sr lm 
#' @S3method as.sr lm
as.sr.lm <- function(modl,c0=0,opy=1,na.rm=FALSE,epoch="yr") {
	mu <- modl$coefficients["(Intercept)"]
	sigma <- sqrt(deviance(modl) / modl$df.residual)
	z <- .compute_sr(mu,c0,sigma,opy)
	dim(z) <- c(1,1)
	rownames(z) <- deparse(substitute(modl))
	XXinv <- vcov(modl) / sigma^2
	rescal <- sqrt(XXinv["(Intercept)","(Intercept)"])
	retval <- sr(z,df=modl$df.residual,c0=c0,opy=opy,
							 rescal=rescal,epoch=epoch)
	return(retval)
}
#' @param anxts an xts object.
#' @rdname as.sr
#' @method as.sr xts 
#' @S3method as.sr xts
as.sr.xts <- function(anxts,c0=0,opy=1,...) {
	if (missing(opy)) {
		opy <- .infer_opy_xts(anxts)
	}
	retval <- as.sr.default(anxts,c0=c0,opy=opy,...)
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
#' @template etc
#' @family sr
#' @export
#'
#' @examples 
#' rvs <- as.sr(rnorm(253*8),opy=253)
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
	serr <- se(x,type="t")
	coefs <- cbind(x$sr,serr,tval,pval)
	#colnames(coefs) <- c("stat","t.stat","p.value")
	colnames(coefs) <- c(paste(c("SR/sqrt(",x$epoch,")"),sep="",collapse=""),
											 "Std. Error","t value","Pr(>t)")
	rownames(coefs) <- if (is.null(rownames(x$sr))) c("Sharpe") else rownames(x$sr)
	printCoefmat(coefs,P.values=TRUE,has.Pvalue=TRUE,
							 digits=max(2, getOption("digits") - 3),
							 cs.ind=c(1,2),tst.ind=c(3),dig.tst=2)
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
#' reannualize(x,opy,epoch)
#'
#' @param x an object of class \code{sr}.
#' @param opy the new observations per year (or epoch). If none given, it is
#' not updated.
#' @param epoch a string representation of the epoch. If none given, it is not
#' updated.
#' @return an object of class \code{sr} with the annualization or epoch updated.
#' @seealso sr
#' @template etc
#' @family sr
#' @export
#'
#' @examples 
#' # compute a 'daily' Sharpe
#' mysr <- as.sr(rnorm(253*8),opy=1)
#' # turn into annual 
#' mysr2 <- reannualize(mysr,opy=253,epoch="yr")
reannualize <- function(x,opy,epoch) {
	if (!is.sr(x)) stop("must give sr object")
	if (!missing(opy)) {
		x$sr <- x$sr * sqrt(opy / x$opy)
		x$opy <- opy
	}
	if (!missing(epoch)) x$epoch <- epoch
	return(x)
}
#UNFOLD
#UNFOLD

########################################################################
# Optimal Sharpe ratio#FOLDUP






markowitz <- function(mu,Sigma,df2,w=NULL) {
	if (is.null(w))
		w <- solve(Sigma,mu)
	retv <- list(w=w,mu=mu,Sigma=Sigma,df1=length(w),df2=df2)
	class(retv) <- "markowitz"
	return(retv)
}
as.markowitz <- function(X,...) {
	UseMethod("as.markowitz", X)
}
# compute the markowitz portfolio
as.markowitz.default <- function(X,mu=NULL,Sigma=NULL) {
	na.omit(X)
	if (is.null(mu)) 
		mu <- colMeans(X)
	if (is.null(Sigma)) 
		Sigma <- cov(X)
	df2 <- dim(X)[1]
	retv <- markowitz(mu,Sigma,df2)
	return(retv)
}

# compute Hotelling's statistic.
.hotelling <- function(X) {
	retval <- as.markowitz(X)
	retval$T2 <- retval$df2 * (retval$mu %*% retval$w)
	return(retval)
}

# spawn a "SROPT" object.
#' @title Create an 'sropt' object.
#'
#' @description 
#'
#' Spawns an object of class \code{sropt}.
#'
#' @details
#'
#' The \code{sropt} class contains information about a rescaled T^2-statistic.
#' The following are list attributes of the object:
#' \itemize{
#' \item \code{sropt} The (optimal) Sharpe ratio statistic.
#' \item \code{df1} The number of assets.
#' \item \code{df2} The number of observations.
#' \item \code{drag} The drag term, which is the 'risk free rate' divided by
#' the maximum risk.
#' \item \code{opy} The 'observations per year'.
#' \item \code{epoch} The string name of the 'epoch'.
#' }
#'
#' For the most part, this constructor should \emph{not} be called directly,
#' rather \code{\link{as.sropt}} should be called instead to compute the
#' needed statistics.
#'
#' @usage
#'
#' sropt(z.s,df1,df2,drag=0,opy=1,epoch="yr")
#'
#' @param z.s an optimum Sharpe ratio statistic.
#' @inheritParams dsropt
#' @param drag the 'drag' term, \eqn{c_0/R}{c0/R}. defaults to 0. It is assumed
#'        that \code{drag} has been annualized, \emph{i.e.} has been multiplied
#'        by \eqn{\sqrt{opy}}{sqrt(opy)}. This is in contrast to the \code{c0}
#'        term given to \code{\link{sr}}.
#' @param opy the number of observations per 'year'. The returns are observed
#'        at a rate of \code{opy} per 'year.' default value is 1, meaning no 
#'        annualization is performed.
#' @param epoch the string representation of the 'year', defaulting
#'        to 'yr'.
#' @keywords univar 
#' @return a list cast to class \code{sropt}.
#' @seealso \code{\link{as.sropt}}
#' @rdname sropt
#' @export 
#' @template etc
#' @template sropt
#'
#' @note
#' 2FIX: allow rownames?
#'
#' @examples 
#' # roll your own.
#' opy <- 253
#' zeta.s <- 1.0
#' df1 <- 10
#' df2 <- 6 * opy
#' rvs <- rsropt(1,df1,df2,zeta.s,opy,drag=0)
#' roll.own <- sropt(z.s=rvs,df1,df2,drag=0,opy=opy)
#' # put a bunch in. naming becomes a problem.
#' rvs <- rsropt(5,df1,df2,zeta.s,opy,drag=0)
#' roll.own <- sropt(z.s=rvs,df1,df2,drag=0,opy=opy)
#'
sropt <- function(z.s,df1,df2,drag=0,opy=1,epoch="yr") {
	retval <- list(sropt = z.s,df1 = df1,df2 = df2,
								 drag = drag,opy = opy,epoch = epoch)
	class(retval) <- "sropt"
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
#' as.sropt(X,drag=0,opy=1,epoch="yr")
#'
#' @param X matrix of returns.
#' @inheritParams sropt 
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
#' @rdname as.sropt
#' @export 
#' @template etc
#' @template sropt
#' @examples 
#' nfac <- 5
#' nyr <- 10
#' opy <- 253
#' # simulations with no covariance structure.
#' # under the null:
#' set.seed(as.integer(charToRaw("determinstic")))
#' Returns <- matrix(rnorm(opy*nyr*nfac,mean=0,sd=0.0125),ncol=nfac)
#' asro <- as.sropt(Returns,drag=0,opy=opy)
#' # under the alternative:
#' Returns <- matrix(rnorm(opy*nyr*nfac,mean=0.0005,sd=0.0125),ncol=nfac)
#' asro <- as.sropt(Returns,drag=0,opy=opy)
as.sropt <- function(X,drag=0,opy=1,epoch="yr") {
	UseMethod("as.sropt", X)
}
#' @rdname as.sropt
#' @method as.sropt default
#' @S3method as.sropt default
as.sropt.default <- function(X,drag=0,opy=1,epoch="yr") {
	# somehow call sropt!
	hotval <- .hotelling(X)
	# what fucking bother.
	quasi.sropt <- hotval[c("df2","T2")]
	quasi.sropt$opy <- opy
	quasi.sropt$drag <- drag
	z.s <- .T2sropt(quasi.sropt)
	dim(z.s) <- c(1,1)

	# this stinks
	retv <- sropt(z.s=z.s,df1=hotval$df1,df2=hotval$df2,
								drag=drag,opy=opy,epoch=epoch)

	# 2FIX: have to store T2 in here now. bleah.
	retv$T2 <- hotval$T2
	# 2FIX: merge markowitz in?

	return(retv)
}
#' @S3method print sropt
#' @export
print.sropt <- function(x,...) {
	Tval <- x$T2
	pval <- pT2(Tval,x$df1,x$df2,lower.tail=FALSE)
	coefs <- cbind(x$sropt,Tval,pval)
	colnames(coefs) <- c(paste(c("SR/sqrt(",x$epoch,")"),sep="",collapse=""),
											 "T^2 value","Pr(>T^2)")
	rownames(coefs) <- if (is.null(rownames(x$sropt))) c("Sharpe") else rownames(x$sropt)
	printCoefmat(coefs,P.values=TRUE,has.Pvalue=TRUE,
							 digits=max(2, getOption("digits") - 3),
							 cs.ind=c(1),tst.ind=c(2),dig.tst=2)
}

# SROPT methods#FOLDUP
# get the T2-stat associated with an SROPT object.
.sropt2T <- function(x) {
	Tval <- x$T2
	if (is.null(Tval)) {
		tval <- .deannualize(x$sropt + x$drag,x$opy)
		Tval <- x$df2 * (tval^2)
	}
	return(Tval)
}
# and the reverse
.T2sropt <- function(x,Tval=x$T2) {
	z.star <- sqrt(Tval / x$df2)
	z.star <- .annualize(z.star,x$opy)
	z.star <- z.star - x$drag
	return(z.star)
}
#UNFOLD
#UNFOLD

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
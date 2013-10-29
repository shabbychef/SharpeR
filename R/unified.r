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
# Created: 2013.10.23
# Copyright: Steven E. Pav, 2012-2013
# Author: Steven E. Pav
# Comments: Steven E. Pav

# note to self on why one need not explicitly multiply the Ohat
# covariance by '1/n' :
# the vcov function, and its workalikes, computes variance-covariance
# of the estimate, and thus have the proper d.f. adjustments in
# their output, as shown by this code:
# test some code;
# set.seed(as.integer(charToRaw("c3e1e960-2926-4278-9ed3-5fc2f41e7b9e")))
# X <- matrix(rnorm(1000*5),ncol=5)
# foomod <- lm(X ~ 1)
# print(vcov(foomod))
# print(1000*vcov(foomod))
# print(cov(X))

# variance covariance#FOLDUP
#' @title Compute variance covariance of 'Unified' Second Moment 
#'
#' @description
#'
#' Computes the variance covariance matrix of sample mean and second moment.
#'
#' @details
#'
#' Given \eqn{p}-vector \eqn{x}, the 'unified' sample is the 
#' \eqn{p(p+3)/2} vector of \eqn{x} stacked on top 
#' of \eqn{\mbox{vech}(x x^{\top})}{vech(x x')}. 
#' Given \eqn{n} contemporaneous observations of \eqn{p}-vectors,
#' stacked as rows in the \eqn{n \times p}{n x p} matrix \eqn{X},
#' this function computes the mean and the variance-covariance
#' matrix of the 'unified' sample. 
#'
#' One may use the default method for computing covariance,
#' via the \code{\link{vcov}} function, or via a 'fancy' estimator,
#' like \code{sandwich:vcovHAC}, \code{sandwich:vcovHC}, \emph{etc.}
#'
#' @usage
#'
#' sm_vcov(X,vcov.func=vcov)
#'
#' @param X an \eqn{n \times p}{n x p} matrix of observed returns.
#' @param vcov.func a function which takes an object of class \code{lm},
#' and computes a variance-covariance matrix.
#' @keywords univar 
#'
#' @return a list containing the following components:
#' \item{mu}{a \eqn{q = p(p+3)/2} vector of the mean, then the vech'd second
#' moment of the sample data}
#' \item{Ohat}{the \eqn{q \times q}{q x q} estimated variance covariance matrix.}
#' \item{n}{the number of rows in \code{X}.}
#' \item{p}{the number of assets.}
#' @seealso \code{\link{ism_vcov}}, \code{\link{sr_vcov}}
#' @rdname sm_vcov
#' @export 
#' @template etc
#'
#' @examples 
#' X <- matrix(rnorm(1000*3),ncol=3)
#' Sigmas <- sm_vcov(X)
#' # make it fat tailed:
#' X <- matrix(rt(1000*3,df=5),ncol=3)
#' Sigmas <- sm_vcov(X)
#' \dontrun{
#' if (require(sandwich)) {
#'  Sigmas <- sm_vcov(X,vcov.func=vcovHC)
#' }
#' }
#' # add some autocorrelation to X
#' Xf <- filter(X,c(0.2),"recursive")
#' colnames(Xf) <- colnames(X)
#' Sigmas <- sm_vcov(Xf)
#' \dontrun{
#' if (require(sandwich)) {
#'	Sigmas <- sm_vcov(Xf,vcov.func=vcovHAC)
#' }
#' }
#'
sm_vcov <- function(X,vcov.func=vcov) {
	X <- na.omit(X)
	n <- dim(X)[1]
	p <- dim(X)[2]
	q <- p * (p+3) / 2
	# fill in Y
	Y <- cbind(X,matrix(NA,nrow=dim(X)[1],ncol=p*(p+1)/2))

	offs <- 0
	for (iii in 1:p) {
		Y[,p+offs+1+(0:(p-iii))] = X[,iii] * X[,iii:p]
		offs <- offs + (p-iii) + 1
	}

	# model Y
	mod2 <- lm(Y ~ 1)
	rm(Y)

	mu <- mod2$coefficients

	# estimate Sigma hat
	Ohat = vcov.func(mod2)
	rm(mod2)

	# get names;
	base.names <- if (is.null(colnames(X))) sapply(1:p,function(n) { sprintf("asset_%03d",n) }) else colnames(X)
	
	strnames <- base.names
	offs <- 0
	for (iii in 1:p) {
		# inefficient, yes, but whatever.
		strnames <- c(strnames,
				unlist(lapply(base.names[iii:p],function(x) { paste(base.names[iii],"x",x) })))
		offs <- offs + (p-iii) + 1
	}
	
	dim(mu) <- c(q,1)
	rownames(mu) <- strnames
	rownames(Ohat) <- strnames
	colnames(Ohat) <- strnames
	retval <- list(mu=mu,Ohat=Ohat,n=n,p=p)
	return(retval)
}

#' @title Compute variance covariance of Inverse 'Unified' Second Moment 
#'
#' @description
#'
#' Computes the variance covariance matrix of the inverse unified 
#' second moment matrix.
#'
#' @details
#'
#' Given \eqn{p}-vector \eqn{x} with mean \eqn{\mu}{mu} and
#' covariance, \eqn{\Sigma}{Sigma}, let \eqn{y} be \eqn{x}
#' with a one prepended. Then let 
#' \eqn{\Theta = E\left(y y^{\top}\right)}{Theta = E[yy']},
#' the uncentered second moment matrix. The inverse of
#' \eqn{\Theta}{Theta} contains the (negative) Markowitz portfolio 
#' and the precision matrix. 
#'
#' Given \eqn{n} contemporaneous observations of \eqn{p}-vectors,
#' stacked as rows in the \eqn{n \times p}{n x p} matrix \eqn{X},
#' this function estimates the mean and the asymptotic 
#' variance-covariance matrix of \eqn{\Theta^{-1}}{Theta^-1}.
#'
#' One may use the default method for computing covariance,
#' via the \code{\link{vcov}} function, or via a 'fancy' estimator,
#' like \code{sandwich:vcovHAC}, \code{sandwich:vcovHC}, \emph{etc.}
#'
#' @usage
#'
#' ism_vcov(X,vcov.func=vcov)
#'
#' @param X an \eqn{n \times p}{n x p} matrix of observed returns.
#' @param vcov.func a function which takes an object of class \code{lm},
#' and computes a variance-covariance matrix.
#' @keywords univar 
#'
#' @return a list containing the following components:
#' \item{mu}{a \eqn{q = p(p+3)/2} vector of the negative Markowitz 
#' portfolio, then the vech'd precision matrix of the sample data}
#' \item{Ohat}{the \eqn{q \times q}{q x q} estimated variance 
#' covariance matrix.}
#' \item{n}{the number of rows in \code{X}.}
#' \item{p}{the number of assets.}
#' @seealso \code{\link{sm_vcov}}, \code{\link{sr_vcov}}
#' @rdname ism_vcov
#' @export 
#' @template etc
#'
#' @note
#'
#' By flipping the sign of \eqn{X}, the inverse of 
#' \eqn{\Theta}{Theta} contains the \emph{positive} Markowitz
#' portfolio and the precision matrix on \eqn{X}. Performing
#' this transform before passing the data to this function
#' should be considered idiomatic.
#'
#' @examples 
#' X <- matrix(rnorm(1000*3),ncol=3)
#' # putting in -X is idiomatic:
#' ism <- ism_vcov(-X)
#' # compute the marginal Wald test statistics:
#' ism.mu <- ism$mu[1:ism$p]
#' ism.Sg <- ism$Ohat[1:ism$p,1:ism$p]
#' wald.stats <- ism.mu / sqrt(diag(ism.Sg))
#'
#' # make it fat tailed:
#' X <- matrix(rt(1000*3,df=5),ncol=3)
#' ism <- ism_vcov(X)
#' wald.stats <- ism$mu[1:ism$p] / sqrt(diag(ism$Ohat[1:ism$p,1:ism$p]))
#' \dontrun{
#' if (require(sandwich)) {
#'  ism <- ism_vcov(X,vcov.func=vcovHC)
#'  wald.stats <- ism$mu[1:ism$p] / sqrt(diag(ism$Ohat[1:ism$p,1:ism$p]))
#' }
#' }
#' # add some autocorrelation to X
#' Xf <- filter(X,c(0.2),"recursive")
#' colnames(Xf) <- colnames(X)
#' ism <- ism_vcov(Xf)
#' wald.stats <- ism$mu[1:ism$p] / sqrt(diag(ism$Ohat[1:ism$p,1:ism$p]))
#' \dontrun{
#' if (require(sandwich)) {
#'	ism <- ism_vcov(Xf,vcov.func=vcovHAC)
#'  wald.stats <- ism$mu[1:ism$p] / sqrt(diag(ism$Ohat[1:ism$p,1:ism$p]))
#' }
#' }
#'
ism_vcov <- function(X,vcov.func=vcov) {
	# delegate
	sm_est <- sm_vcov(X,vcov.func=vcov.func)
	# interpret
	p <- sm_est$p
	q <- p * (p + 3) /2
	mu <- sm_est$mu[1:p]
	dim(mu) <- c(p,1)
	Sig2 <- matrix(NA,nrow=p,ncol=p)
	Sig2[lower.tri(Sig2,diag=TRUE)] <- sm_est$mu[(p+1):length(sm_est$mu)]
	Sig2 <- t(Sig2)
	Sig2[lower.tri(Sig2,diag=TRUE)] <- sm_est$mu[(p+1):length(sm_est$mu)]
	Theta <- cbind(rbind(1,mu),rbind(t(mu),Sig2))

	iTheta <- solve(Theta)
	deriv <- iTheta %x% iTheta  # kron 
	
	# now elimination time
	elim.idx <- lower.tri(Theta,diag=TRUE)
	dim(elim.idx) <- c((p+1)*(p+1),1)
	elim.idx[1] <- FALSE

	sub.deriv <- deriv[elim.idx,elim.idx]
	Ohat <- t(sub.deriv) %*% sm_est$Ohat %*% sub.deriv

	mu <- iTheta[elim.idx]
	strnames <- rownames(sm_est$mu)
	dim(mu) <- c(q,1)
	rownames(mu) <- strnames
	rownames(Ohat) <- strnames
	colnames(Ohat) <- strnames
	retval <- list(mu=mu,Ohat=Ohat,n=sm_est$n,p=p)
	return(retval)
}
#UNFOLD

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r

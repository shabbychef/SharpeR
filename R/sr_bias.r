# /usr/bin/r
#
# Copyright 2018-2018 Steven E. Pav. All Rights Reserved.
# Author: Steven E. Pav 
#
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
#
# Created: 2018.10.06
# Copyright: Steven E. Pav, 2018
# Author: Steven E. Pav <shabbychef@gmail.com>
# Comments: Steven E. Pav

# 2FIX: to make this DRY should probably perform these on t-stats instead.
# moments computations, translated from Yong Bao's Gauss code:# FOLDUP
#.bao_func <- function(y,rhat=NULL,na.rm=TRUE) {
	#if (na.rm) { y <- na.omit(y) }
	#Shat <- mean(y) / sd(y)
	#if (is.null(rhat)) { rhat <- .smplgamma(y) }
	#TT <- length(y)
	#biasf1 <- .bias1(TT,Shat,rhat);
	#biasf2 <- .bias2(TT,Shat,rhat);
	#vf <- .variance(TT,Shat,rhat);   
#}

# we use these elsewhere
.smplgamma <- function(y,muy=mean(y,na.rm=TRUE),n=length(y),na.rm=TRUE) {
	x <- y - muy
	if (na.rm) { x <- na.omit(x) }
	k <- .kstat(x,n=n)
	retv <- k[3:6] / (k[2] ^ ((3:6)/2))
	retv
}

# /* k-statistic, see Kendall's Advanced Theory of Statistics */
.kstat <- function(x,n=length(x)) {
	s <- unlist(lapply(1:6,function(iii) { sum(x^iii) }))
	nn <- n^(1:6)
	nd <- exp(lfactorial(n) - lfactorial(n-(1:6)))
	k <- rep(0,6)
	# sample mean
	k[1] <- s[1]/n;
	# sample variance
	k[2] <- (n*s[2]-s[1]^2)/nd[2];
	# skewness?
	k[3] <- (nn[2]*s[3]-3*n*s[2]*s[1]+2*s[1]^3)/nd[3]
	# excess kurtosis.
	k[4] <- ((nn[3]+nn[2])*s[4]-4*(nn[2]+n)*s[3]*s[1]-3*(nn[2]-n)*s[2]^2 +12*n*s[2]*s[1]^2-6*s[1]^4)/nd[4];
	# whatchamacallit
	k[5] <- ((nn[4]+5*nn[3])*s[5]-5*(nn[3]+5*nn[2])*s[4]*s[1]-10*(nn[3]-nn[2])*s[3]*s[2] 
					 +20*(nn[2]+2*n)*s[3]*s[1]^2+30*(nn[2]-n)*s[2]^2*s[1]-60*n*s[2]*s[1]^3 +24*s[1]^5)/nd[5];
	# yeah, ok.
	k[6] <- ((nn[5]+16*nn[4]+11*nn[3]-4*nn[2])*s[6]
					-6*(nn[4]+16*nn[3]+11*nn[2]-4*n)*s[5]*s[1]
					-15*n*(n-1)^2*(n+4)*s[4]*s[2]-10*(nn[4]-2*nn[3]+5*nn[2]-4*n)*s[3]^2
					+30*(nn[3]+9*nn[2]+2*n)*s[4]*s[1]^2+120*(nn[3]-n)*s[3]*s[2]*s[1]
					+30*(nn[3]-3*nn[2]+2*n)*s[2]^3-120*(nn[2]+3*n)*s[3]*s[1]^3
					-270*(nn[2]-n)*s[2]^2*s[1]^2+360*n*s[2]*s[1]^4-120*s[1]^6)/nd[6];

	k
}

.bias2 <- function(TT,S,r) {
	3*S/4/TT+49*S/32/TT^2-r[1]*(1/2/TT+3/8/TT^2)+S*r[2]*(3/8/TT-15/32/TT^2) +3*r[3]/8/TT^2-5*S*r[4]/16/TT^2-5*S*r[1]^2/4/TT^2+105*S*r[2]^2/128/TT^2-15*r[1]*r[2]/16/TT^2;
}
.bias1 <- function(TT,S,r) {
	#3*S/4/TT-r[1]*(1/2/TT)+S*r[2]*(3/8/TT);
	(2 + r[2])*S*3/8/TT - r[1]/2/TT;
}

#.variance <- function(TT,S,r) {
	#(1+S^2/2)/TT+(19*S^2/8+2)/TT^2-r[1]*S*(1/TT+5/2/TT^2)
    #+r[2]*S^2*(1/4/TT+3/8/TT^2)+5*r[3]*S/4/TT^2-3*r[4]*S^2/8/TT^2
    #+r[1]^2*(7/4/TT^2-3*S^2/2/TT^2)
    #-15*r[1]*r[2]*S/4/TT^2+39*r[2]^2*S^2/32/TT^2;
#}

# UNFOLD

#' @title sr_bias .
#'
#' @description 
#'
#' Computes the asymptotic bias of the sample Sharpe ratio based on moments.
#'
#' @details
#'
#' The sample Sharpe ratio has bias of the form
#' \deqn{B = \left(\frac{3}{4n} + 3 \frac{\gamma_2}{8n}\right) \zeta - 
#' \frac{1}{2n} \gamma_1 + o\left(n^{-3/2}\right),}
#' where \eqn{\zeta} is the population Signal Noise ratio, 
#' \eqn{n} is the sample size, \eqn{\gamma_1} is the population skewness,
#' and \eqn{\gamma_2} is the population excess kurtosis.
#' This form of the bias appears as Equation (5) in Bao, which
#' claims an accuracy of only \eqn{o\left(n^{-1}\right)}. The
#' author believes this approximation is slightly more accurate.
#'
#' A more accurate form is given by Bao (Equation (3)) as 
#' \deqn{B = \frac{3\zeta}{4n}\zeta + \frac{49\zeta}{32n^2} - \gamma_1 \left(\frac{1}{2n} + \frac{3}{8n^2}\right) + \gamma_2 \zeta
#' \left(\frac{3}{8n} - \frac{15}{32n^2}\right) + \frac{3\gamma_3}{8n^2} - \frac{5\gamma_4 \zeta}{16n^2} - \frac{5\gamma_1^2\zeta}{4n^2}
#' + \frac{105\gamma_2^2 \zeta}{128 n^2} - \frac{15 \gamma_1 \gamma_2}{16n^2} + o\left(n^{-2}\right),}
#' where \eqn{\gamma_3} through \eqn{\gamma_5} are the fifth through
#' seventh cumulants of the error term.
#'
#' See \sQuote{The Sharpe Ratio: Statistics and Applications},
#' section 3.2.3.
#'
#' @param snr   the population Signal Noise ratio. Often one will use
#' the population estimate instead.
#' @param n     the sample size that the Shapre ratio is observed on.
#' @param cumulants  a vector of the third through fourth, or the third
#' through seventh population cumulants of the random variable.
#' More terms are needed for the higher accuracy approximation.
#' @param type  determines the order of accuracy of the bias approximation.
#' Takes values of
#' \describe{
#' \item{simple}{We compute the simple approximation using only the skewness
#' and excess kurtosis.}
#' \item{second_order}{We compute the more accurate approximation, given by
#' Bao, which is accurate to \eqn{o\left(n^{-2}\right)}.}
#' }
#'
#' @return the approximate bias of the Sharpe ratio. The bias is the
#' expected value of the sample Sharpe minus the Signal Noise ratio.
#' @template ref-Bao
#' @template etc
#' @template ref-tsrsa
#' @seealso \code{\link{sr_variance}}
#' @note much of the code is adapted from Gauss code provided by Yong Bao.
#'
#' @examples 
#'
#' # bias under normality:
#' sr_bias(1, 100, rep(0,2), type='simple')
#' sr_bias(1, 100, rep(0,5), type='second_order')
#'
#' # plugging in sample estimates
#' x <- rnorm(1000)
#' n <- length(x)
#' mu <- mean(x)
#' sdv <- sd(x)
#' snr <- mu / sdv
#' # these are not great estimates, but close enough:
#' sku <- mean((x-mu)^3) / sdv^3
#' kur <- (mean((x-mu)^4) / sdv^4) - 4
#' sr_bias(snr, n, c(sku,kur), type='simple')
#'
#' @export
sr_bias <- function(snr, n, cumulants, type=c('simple','second_order')) {
	type <- match.arg(type)
	retv <- switch(type,
								 simple=.bias1(n,snr,cumulants),
								 second_order=.bias2(n,snr,cumulants))
	retv
}
#' @title sr_variance .
#'
#' @description 
#' 
#' Computes the variance of the sample Sharpe ratio.
#'
#' @details
#'
#' The sample Sharpe ratio has variance of the form
#' \deqn{V = \frac{1}{n}\left(1 + \frac{\zeta^2}{2}\right)
#' +\frac{1}{n^2}\left(\frac{19\zeta^2}{8} + 2\right)
#' -\gamma_1\zeta\left(\frac{1}{n} + \frac{5}{2n^2}\right)
#' +\gamma_2\zeta^2\left(\frac{1}{4n} + \frac{3}{8n^2}\right)
#' +\frac{5\gamma_3\zeta}{4n^2} 
#' +\gamma_1^2\left(\frac{7}{4n^2} - \frac{3\zeta^2}{2n^2}\right)
#' +\frac{39\gamma_2^2\zeta^2}{32n^2} 
#' -\frac{15\gamma_1\gamma_2\zeta}{4n^2} 
#' +o\left(n^{-2}\right),}
#' where \eqn{\zeta} is the population Signal Noise ratio, 
#' \eqn{n} is the sample size, \eqn{\gamma_1} is the population skewness,
#' and \eqn{\gamma_2} is the population excess kurtosis, and
#' \eqn{\gamma_3} through \eqn{\gamma_5} are the fifth through
#' seventh cumulants of the error term.
#' This form of the variance appears as Equation (4) in Bao.
#'
#' See \sQuote{The Sharpe Ratio: Statistics and Applications},
#' section 3.2.3.
#'
#' @inheritParams sr_bias
#' @return the variance of the sample statistic.
#' @seealso \code{\link{sr_bias}}.
#' @template ref-Bao
#' @template etc
#' @template ref-tsrsa
#'
#' @examples 
#' # variance under normality:
#' sr_variance(1, 100, rep(0,5))
#' @export
sr_variance <- function(snr, n, cumulants) {
	r <- cumulants
	TT <- n
	S <- snr
	# this is from Yong Bao's code:
	retv <- (1+S^2/2)/TT+(19*S^2/8+2)/TT^2-r[1]*S*(1/TT+5/2/TT^2)
    +r[2]*S^2*(1/4/TT+3/8/TT^2)+5*r[3]*S/4/TT^2-3*r[4]*S^2/8/TT^2
    +r[1]^2*(7/4/TT^2-3*S^2/2/TT^2)
    -15*r[1]*r[2]*S/4/TT^2+39*r[2]^2*S^2/32/TT^2;
}

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r

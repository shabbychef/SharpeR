# Copyright 2012-2025 Steven E. Pav. All Rights Reserved.
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
# Created: 2022.05.02
# Copyright: Steven E. Pav, 2025
# Author: Steven E. Pav
# Comments: Steven E. Pav

# returns the chi-bar-square weights and degrees of freedom for inference when the covariance is diagonal.
.chibsq_wts <- function(numel) {
  # weights from chi-bar square distro
  cwts <- exp(lchoose(numel,0:numel) - numel * log(2))
  okwt <- (cwts > 1e-9)
  sbdf <- which(okwt) - 1
  sbwt <- cwts[okwt]
  # should check that sum(sbwt) is around 1
  stopifnot(abs(sum(sbwt) - 1) < 1e-5)
	return(list(weights=sbwt,dfs=sbdf))
}




#' @title test for multiple Sharpe ratios.
#'
#' @description 
#'
#' Performs tests for the hypothesis 
#' \deqn{\forall i \zeta_i \le \zeta_0}{all i: zeta_i <= zeta_0}
#' against the alternative
#' \deqn{\exists i zeta_i > \zeta_0}{exists i: zeta_i > zeta_0}
#'
#' Multiple methods are supported for the test, including Bonferroni
#' correction, a chi-bar-square test, and Follman's test.
#'
#' It is assumed that returns have a compound symmetric correlation structure.
#' That is, the correlation matrix has \eqn{\rho}{rho} on all off-diagonal
#' elements.
#' Returns are assumed to follow an elliptical distribution with kurtosis
#' factor \eqn{\kappa}{kappa}, which equals 1 in the case of Gaussian
#' returns. The kurtosis factor is one third the kurtosis of marginal returns.
#'
#' @details
#'
#' A few test methodologies are supported. These are described in more
#' detail in Section 4.1 of \emph{The Sharpe Ratio: Statistics and Applications}.
#' \itemize{
#' \item Performs the Bonferroni correction as described in equation (4.8).
#' \item The chi-bar-square test described in section 4.1.3.
#' \item Follman's test, given in equation (4.20). This test
#' does not yet support Hansen's asymptotic correction and may not
#' produce confidence intervals.
#' }
#' Moreover, Hansen's \sQuote{log-log} adjustment is also optionally applied.
#'
#' @param srs  A vector of Sharpe ratios, quoted in terms of a given epoch.
#' @param df  The number of \sQuote{degrees of freedom} of the Sharpe ratios,
#' which are assumed to have been measured over the same period.
#' The degrees of freedom are one less than the number of observed returns.
#' @template param-ope
#' @param kappa  The kurtosis factor of returns. The value 1 corresponds to
#' Gaussian returns, while larger values are more kurtotic.
#' @param rho  The assumed common correlation among returns.
#' @param loglog  Whether to apply Hansen's \sQuote{log-log} adjustment to the
#' number of effective strategies tested. Not yet applied for Follman's test.
#' @param zeta_0  The cutoff for the test. We test whether all Signal-noise
#' ratios are equal to zeta_0.  This value is quoted in terms of the same epoch
#' as \code{srs}.
#' @param conf.level confidence level of the test. We perform a one-sided test.
#' @param type which method to apply.
#' @return A list with class \code{"htest"} containing the following components:
#' \item{statistic}{the value of the statistic.}
#' \item{parameter}{the degrees of freedom for the statistic.}
#' \item{p.value}{the p-value for the test.}
#' \item{conf.int}{a one-sided confidence interval appropriate to the specified alternative hypothesis.}
#' \item{alternative}{a character string describing the alternative hypothesis.}
#' \item{method}{a character string indicating what type of test was performed.}
#' \item{data.name}{a character string giving the name(s) of the data.}
#' @keywords htest
#' @keywords overoptimism
#' @template etc
#' @template ref-tsrsa
#' @template ref-SEP2019b
#'
#' @references
#' Follman, D. "A Simple Multivariate Test for One-Sided Alternatives." 
#' JASA, 91, no 434 (1996): 854-861. \doi{10.2307/2291680}
#'
#' Hansen, P. R. "A Test for Superior Predictive Ability."
#' J. Bus. Ec. Stats, 23, no 4 (2005). \doi{10.1198/073500105000000063}
#'
#' @examples 
#' # generate some fake data
#' ope <- 252
#' zeta0 <- 1.0
#' set.seed(1234)
#' zetas <- rsr(50, zeta=zeta0, df=ope*2, ope=ope)
#' sr_max_test(zetas,df=ope*2,ope=ope,type='Bonferroni')
#' sr_max_test(zetas,zeta_0=zeta0,df=ope*2,ope=ope,type='Bonferroni')
#' sr_max_test(zetas,zeta_0=zeta0,df=ope*2,ope=ope,type='chi-bar-square')
#' @template etc
#' @template etc
#' @importFrom stats qchisq
#' @seealso sr_conditional_test
#' @rdname sr_max_test
#' @export
sr_max_test <- function(srs, df, ope=1, kappa=1, rho=0, zeta_0=0, 
												conf.level=0.95, 
												type=c('Bonferroni','chi-bar-square','Follman'),
												loglog=TRUE) {
	type <- tolower(match.arg(type))
	method <- type
	dname <- deparse(substitute(srs))
	nday <- 1 + df
	nstrat <- length(srs)

	# adjust back to individual units
	zetas <- srs / sqrt(ope)
	z0    <- zeta_0 / sqrt(ope)
	meansr <- mean(zetas)

	a0 <- (1-rho) + (kappa/2) * z0^2 * (1-rho^2)
	a2 <- rho + ((kappa-1)/4) * z0^2 + (kappa/2) * z0^2 * rho^2
	b0 <- 1 / sqrt(a0)
	b2 <- (-1 / (nstrat*sqrt(a0))) + (1/nstrat) / sqrt(a0 + nstrat*a2)
	cval <- b0 + nstrat * b2

	# this is what equation (4.17) should be.
	# under the null we should have
	# rootn_xi ~ N(sqrt(nday)*cval*z0*1, I)
	rootn_xi <- sqrt(nday) * (b0 * (zetas - meansr) + cval * (meansr - z0))

	nabove <- nstrat
	if (loglog) { 
    # hansens procedures; 
		nabove <- sum(rootn_xi > (sqrt(nday)*cval*z0 - sqrt(2*log(log(nday)))))
		if ((type=='follman') && (!missing(loglog))) {
			warning("Cannot yet apply log-log adjustment to Follman's test.")
		}
	}
	parameter <- nabove

	alpha <- 1 - conf.level
	alternative <- 'greater'
	if (nabove < 1) {
		# no grounds to reject
		statistic <- Inf
		clow <- -Inf
		pval <- 1
	} else {
		if (type == "bonferroni") {
			z <- max(rootn_xi)
			statistic <- z
			# now compare z to 1 - alpha/k cutoff
			pval <- min(1,nabove * pnorm(statistic,lower.tail=FALSE))
			names(statistic) <- "Z statistic"
			zlow <- qnorm(alpha/nabove,lower.tail=FALSE)

			zerme <- function(az0) {
				a0 <- (1-rho) + (kappa/2) * az0^2 * (1-rho^2)
				a2 <- rho + ((kappa-1)/4) * az0^2 + (kappa/2) * az0^2 * rho^2
				b0 <- 1 / sqrt(a0)
				b2 <- (-1 / (nstrat*sqrt(a0))) + (1/nstrat) / sqrt(a0 + nstrat*a2)
				cval <- b0 + nstrat * b2
				my_rootn_xi <- sqrt(nday) * (b0 * (zetas - meansr) + cval * (meansr - az0))
				my_pval <- min(1,nabove * pnorm(max(my_rootn_xi),lower.tail=FALSE))
				my_pval - alpha
			}
			# this is close:
			clow <- meansr + (b0 * (max(zetas)-meansr) - (zlow/sqrt(nday))) / cval
			truelims <- 2.0*abs(clow)*c(-1,1)
			clow <- uniroot(zerme,
											interval=truelims,  # a hack
											maxiter=2000)$root

		} else if (type == 'chi-bar-square') {
			chibs <- sum(pmax(rootn_xi,0)^2)
			statistic <- chibs
			names(statistic) <- "chi-bar-square statistic"

			chval <- .chibsq_wts(nabove)
			pval <- sum(chval$weights * pchisq(chibs,df=chval$dfs,lower.tail=FALSE))

			zerme <- function(az0) {
				a0 <- (1-rho) + (kappa/2) * az0^2 * (1-rho^2)
				a2 <- rho + ((kappa-1)/4) * az0^2 + (kappa/2) * az0^2 * rho^2
				b0 <- 1 / sqrt(a0)
				b2 <- (-1 / (nstrat*sqrt(a0))) + (1/nstrat) / sqrt(a0 + nstrat*a2)
				cval <- b0 + nstrat * b2
				my_rootn_xi <- sqrt(nday) * (b0 * (zetas - meansr) + cval * (meansr - az0))
				my_chibs <- sum(pmax(my_rootn_xi,0)^2)
				my_pval <- sum(chval$weights * pchisq(my_chibs,df=chval$dfs,lower.tail=FALSE))
				my_pval - alpha
			}
			truelims <- meansr + c((b0/cval) * (max(zetas)-meansr),(b0/cval) * (min(zetas)-meansr) - qnorm(alpha,lower.tail=FALSE)/sqrt(nday))
			clow <- uniroot(zerme,
											interval=truelims,  # a hack
											maxiter=2000)$root
		} else {
			g0 <- nday * b0^2 * sum((zetas - meansr)^2)
			g1 <- nday * (nstrat * (cval * (meansr - z0))^2)
			# same as sum(rootn_xi^2) btw
			g2 <- g0 + g1
			follp <- pchisq(g2,df=nabove,lower.tail=FALSE)
			statistic <- g2
			names(statistic) <- "Follmans chi-square statistic"
			pval <- ifelse(meansr > z0,0.5 * follp,1 - 0.5*follp)
			clow_1 <- ifelse(alpha < 0.5,
											 qchisq(2*alpha,df=nabove,lower.tail=FALSE),
											 qchisq(2*(1-alpha),df=nabove,lower.tail=FALSE))
			clow_1 <- (clow_1 / nday) - g0
			if (clow_1 > 0) {
				clow_1 <- sqrt(clow_1 / nstrat) / cval
				clow <- meansr - clow_1
			} else {
				clow <- NA_real_
			}
		}
	}
	cint <- c(sqrt(ope)*clow,Inf)
	attr(cint, "conf.level") <- conf.level
	names(zeta_0) <- 'group signal-noise ratio'

	# may need an estimate and a null.value?

	retval <- list(statistic = statistic, parameter = parameter,
								 p.value = pval, 
								 alternative = alternative, null.value = zeta_0,
								 conf.int = cint, loglog = loglog,
								 method = method, data.name = dname)
	class(retval) <- "htest"
	return(retval)
}


#' @title conditional test for maximum Sharpe ratios.
#'
#' @description 
#'
#' Performs tests for the hypothesis 
#' \deqn{\zeta_{(k)} \le \zeta_0}{zeta_(k) <= zeta_0}
#' against the alternative
#' \deqn{\zeta_{(k)} > \zeta_0,}{zeta_(k) > zeta_0,}
#' where \eqn{\zeta_{(k)}}{zeta_(k)} is the signal-noise ratio of the
#' asset selected because it has the largest Sharpe ratio.
#' The test is conditional on having selected the maximum.
#' Testing is via the polyhedral lemma of Lee \emph{et al.}
#'
#' @details
#'
#' Performs the conditional estimation procedure as outlined in 
#' Section 4.1.5 of \emph{The Sharpe Ratio: Statistics and Applications}.
#'
#' @param srs  A vector of Sharpe ratios, quoted in terms of a given epoch.
#' @param df  The number of \sQuote{degrees of freedom} of the Sharpe ratios,
#' which are assumed to have been measured over the same period.
#' The degrees of freedom are one less than the number of observed returns.
#' @template param-ope
#' @param R      The correlation matrix of returns. Not needed if Rmax is given.
#' @param Rmax   The column of the correlation matrix corresponding to the 
#' maximal element of \code{srs}.
#' @param zeta_0  The cutoff for the test. We test whether all Signal-noise
#' ratios are equal to zeta_0.  This value is quoted in terms of the same epoch
#' as \code{srs}.
#' @param conf.level confidence level of the test. 
#' @param alternative a character string specifying the alternative hypothesis,
#'        must be one of \code{"two.sided"} (default), \code{"greater"} or
#'        \code{"less"}. You can specify just the initial letter.
#' @return A list with class \code{"htest"} containing the following components:
#' \item{statistic}{the value of the conditional normal statistic.}
#' \item{parameter}{the degrees of freedom for the statistic.}
#' \item{p.value}{the p-value for the test.}
#' \item{conf.int}{a one-sided confidence interval appropriate to the specified alternative hypothesis.}
#' \item{alternative}{a character string describing the alternative hypothesis.}
#' \item{method}{a character string indicating what type of test was performed.}
#' \item{data.name}{a character string giving the name(s) of the data.}
#' @keywords htest
#' @keywords overoptimism
#' @template etc
#' @template ref-tsrsa
#' @template ref-SEP2019b
#' @importFrom epsiwal pconnorm ci_connorm
#'
#' @seealso sr_max_test
#' @references
#' Lee, J. D., Sun, D. L., Sun, Y. and Taylor, J. E. "Exact post-selection inference, 
#' with application to the Lasso." Ann. Statist. 44, no. 3 (2016): 907-927.
#' doi:10.1214/15-AOS1371. \url{https://arxiv.org/abs/1311.6238}
#'
#' @examples 
#' # generate some fake data
#' ope <- 252
#' zeta0 <- 1.0
#' set.seed(1234)
#' zetas <- rsr(50, zeta=zeta0, df=ope*2, ope=ope)
#' sr_conditional_test(zetas,df=ope*2,ope=ope,R=diag(length(zetas)))
#' @template etc
#' @rdname sr_conditional_test
#' @export
sr_conditional_test <- function(srs, df, ope=1, R=NULL, Rmax=NULL, zeta_0=0, 
																conf.level=0.95, 
																alternative=c("two.sided","less","greater")) {
	# all this stolen from t.test.default:
	alternative <- match.arg(alternative)
	dname <- deparse(substitute(srs))

	nday <- 1 + df
	nstrat <- length(srs)
	kidx <- which.max(srs)
	if (is.null(Rmax)) { 
		Rmax = R[,kidx]
	}
	mySigma_eta <- (1/nday) * Rmax

	# adjust back to individual units
	zetas <- srs / sqrt(ope)
	z0    <- zeta_0 / sqrt(ope)

	y <- zetas
	testzeta <- z0

	# collect the maximum, so reorder the A above
	yord <- order(y,decreasing=TRUE)
	revo <- seq_len(nstrat)
	revo[yord] <- revo

  A1 <- cbind(-1,diag(nstrat-1))
	A <- A1[,revo]
	nu <- rep(0,nstrat)
	nu[yord[1]] <- 1
	b <- rep(0,nstrat-1)

	pval <- epsiwal::pconnorm(y=y,A=A,b=b,eta=nu,eta_mu=testzeta,Sigma_eta=mySigma_eta,lower.tail=TRUE)
	pval <- switch(alternative,
								 two.sided = .oneside2two(pval),
								 less = 1-pval,
								 greater = pval)

	statistic <- y[kidx]
	names(statistic) <- "Conditional Z statistic"
	parameter <- df

	alpha <- 1 - conf.level
	ps <- switch(alternative,
							 two.sided = c(1-alpha/2,alpha/2),
							 less = c(1,alpha),
							 greater = c(1-alpha,0))
	cint <- epsiwal::ci_connorm(y=y,A=A,b=b,eta=nu,Sigma_eta=mySigma_eta,p=ps)
	cint <- sort(cint)
	#idx <- 2
	#epsiwal::pconnorm(y=y,A=A,b=b,eta=nu,eta_mu=cint[idx],Sigma_eta=mySigma_eta,lower.tail=TRUE) - ps[idx]

	# convert back
	cint <- sqrt(ope) * cint
	attr(cint, "conf.level") <- conf.level
	names(zeta_0) <- 'signal-noise ratio conditional on max'

	method <- 'conditional inference'

	# may need an estimate and a null.value?

	retval <- list(statistic = statistic, parameter = parameter,
								 p.value = pval, 
								 alternative = alternative, null.value = zeta_0,
								 conf.int = cint, 
								 method = method, data.name = dname)
	class(retval) <- "htest"
	return(retval)
}

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r

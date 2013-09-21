\name{as.sropt}
\alias{as.sropt}
\alias{as.sropt.default}
\alias{as.sropt.xts}
\title{Compute the Sharpe ratio of the Markowitz portfolio.}
\usage{
  as.sropt(X,drag=0,ope=1,epoch="yr")

  \method{as.sropt}{default} (X, drag = 0, ope = 1,
    epoch = "yr")

  \method{as.sropt}{xts} (X, drag = 0, ope = 1,
    epoch = "yr")
}
\arguments{
  \item{X}{matrix of returns, or \code{xts} object.}

  \item{drag}{the 'drag' term, \eqn{c_0/R}{c0/R}. defaults
  to 0. It is assumed that \code{drag} has been annualized,
  \emph{i.e.} has been multiplied by
  \eqn{\sqrt{ope}}{sqrt(ope)}. This is in contrast to the
  \code{c0} term given to \code{\link{sr}}.}

  \item{ope}{the number of observations per 'epoch'. For
  convenience of interpretation, The Sharpe ratio is
  typically quoted in 'annualized' units for some epoch,
  that is, 'per square root epoch', though returns are
  observed at a frequency of \code{ope} per epoch.  The
  default value is 1, meaning the code will not attempt to
  guess, what the observation frequency is, and no
  annualization adjustments will be made.}

  \item{epoch}{the string representation of the 'epoch',
  defaulting to 'yr'.}
}
\value{
  An object of class \code{sropt}.
}
\description{
  Computes the Sharpe ratio of the Markowitz portfolio of
  some observed returns.
}
\details{
  Suppose \eqn{x_i}{xi} are \eqn{n}{n} independent draws of
  a \eqn{q}{q}-variate normal random variable with mean
  \eqn{\mu}{mu} and covariance matrix \eqn{\Sigma}{Sigma}.
  Let \eqn{\bar{x}}{xbar} be the (vector) sample mean, and
  \eqn{S}{S} be the sample covariance matrix (using
  Bessel's correction). Let \deqn{\zeta(w) =
  \frac{w^{\top}\bar{x} - c_0}{\sqrt{w^{\top}S w}}}{zeta(w)
  = (w'xbar - c0)/sqrt(w'Sw)} be the (sample) Sharpe ratio
  of the portfolio \eqn{w}{w}, subject to risk free rate
  \eqn{c_0}{c0}.

  Let \eqn{w_*}{w*} be the solution to the portfolio
  optimization problem: \deqn{\max_{w: 0 < w^{\top}S w \le
  R^2} \zeta(w),}{max {zeta(w) | 0 < w'Sw <= R^2},} with
  maximum value \eqn{z_* = \zeta\left(w_*\right)}{z* =
  zeta(w*)}. Then \deqn{w_* = R
  \frac{S^{-1}\bar{x}}{\sqrt{\bar{x}^{\top}S^{-1}\bar{x}}}}{%
  w* = R S^-1 xbar / sqrt(xbar' S^-1 xbar)} and \deqn{z_* =
  \sqrt{\bar{x}^{\top} S^{-1} \bar{x}} - \frac{c_0}{R}}{%
  z* = sqrt(xbar' S^-1 xbar) - c0/R}

  The units of \eqn{z_*}{z*} are
  \eqn{\mbox{time}^{-1/2}}{per root time}. Typically the
  Sharpe ratio is \emph{annualized} by multiplying by
  \eqn{\sqrt{\mbox{ope}}}{sqrt(ope)}, where
  \eqn{\mbox{ope}}{ope} is the number of observations per
  year (or whatever the target annualization epoch.)

  Note that if \code{ope} and \code{epoch} are not given,
  the converter from \code{xts} attempts to infer the
  observations per epoch, assuming yearly epoch.
}
\examples{
nfac <- 5
nyr <- 10
ope <- 253
# simulations with no covariance structure.
# under the null:
set.seed(as.integer(charToRaw("be determinstic")))
Returns <- matrix(rnorm(ope*nyr*nfac,mean=0,sd=0.0125),ncol=nfac)
asro <- as.sropt(Returns,drag=0,ope=ope)
# under the alternative:
Returns <- matrix(rnorm(ope*nyr*nfac,mean=0.0005,sd=0.0125),ncol=nfac)
asro <- as.sropt(Returns,drag=0,ope=ope)
# generating correlated multivariate normal data in a more sane way
if (require(MASS)) {
  nstok <- 10
  nfac <- 3
  nyr <- 10
  ope <- 253
  X.like <- 0.01 * matrix(rnorm(500*nfac),ncol=nfac) \%*\%
    matrix(runif(nfac*nstok),ncol=nstok)
  Sigma <- cov(X.like) + diag(0.003,nstok)
  # under the null:
  Returns <- mvrnorm(ceiling(ope*nyr),mu=matrix(0,ncol=nstok),Sigma=Sigma)
  asro <- as.sropt(Returns,ope=ope)
  # under the alternative
  Returns <- mvrnorm(ceiling(ope*nyr),mu=matrix(0.001,ncol=nstok),Sigma=Sigma)
  asro <- as.sropt(Returns,ope=ope)
}
\dontrun{
# using real data.
if (require(quantmod)) {
  get.ret <- function(sym,...) {
    OHLCV <- getSymbols(sym,auto.assign=FALSE,...)
    lrets <- diff(log(OHLCV[,paste(c(sym,"Adjusted"),collapse=".",sep="")]))
    # chomp first NA!
    lrets[-1,]
  }
  get.rets <- function(syms,...) { some.rets <- do.call("cbind",lapply(syms,get.ret,...)) }
  some.rets <- get.rets(c("IBM","AAPL","A","C","SPY","XOM"))
  asro <- as.sropt(some.rets)
}
}
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  \code{\link{sropt}}, \code{\link{sr}}, sropt-distribution
  functions, \code{\link{dsropt}, \link{psropt},
  \link{qsropt}, \link{rsropt}}

  Other sropt: \code{\link{confint.del_sropt}},
  \code{\link{confint.sr}}, \code{\link{confint.sropt}},
  \code{\link{dsropt}}, \code{\link{is.sropt}},
  \code{\link{power.sropt_test}},
  \code{\link{reannualize}}, \code{\link{reannualize.sr}},
  \code{\link{reannualize.sropt}}, \code{\link{sropt}},
  \code{\link{sropt_test}}
}
\keyword{univar}


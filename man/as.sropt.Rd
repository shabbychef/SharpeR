\name{as.sropt}
\alias{as.sropt}
\alias{as.sropt.default}
\alias{as.sropt.xts}
\alias{sropt}
\title{Compute the Sharpe ratio of the Markowitz portfolio.}
\usage{
  as.sropt(X,drag=0,opy=1,epoch="yr")

  \method{as.sropt}{default} (X, drag = 0, opy = 1,
    epoch = "yr")

  \method{as.sropt}{xts} (anxts, drag = 0, opy = 1,
    epoch = "yr")
}
\arguments{
  \item{X}{matrix of returns.}

  \item{anxts}{an xts object.}

  \item{drag}{the 'drag' term, \eqn{c_0/R}{c0/R}. defaults
  to 0. It is assumed that \code{drag} has been annualized,
  \emph{i.e.} has been multiplied by
  \eqn{\sqrt{opy}}{sqrt(opy)}. This is in contrast to the
  \code{c0} term given to \code{\link{sr}}.}

  \item{opy}{the number of observations per 'year'. The
  returns are observed at a rate of \code{opy} per 'year.'
  default value is 1, meaning no annualization is
  performed.}

  \item{epoch}{the string representation of the 'year',
  defaulting to 'yr'.}
}
\value{
  A list with containing the following components:
  \item{w}{the optimal portfolio.} \item{mu}{the estimated
  mean return vector.} \item{Sigma}{the estimated
  covariance matrix.} \item{df1}{the number of assets.}
  \item{df2}{the number of observed vectors.} \item{T2}{the
  Hotelling \eqn{T^2} statistic.} \item{sropt}{the maximal
  Sharpe statistic.} \item{drag}{the input \code{drag}
  term.} \item{opy}{the input \code{opy} term.}
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
  \eqn{\sqrt{\mbox{opy}}}{sqrt(opy)}, where
  \eqn{\mbox{opy}}{opy} is the number of observations per
  year (or whatever the target annualization epoch.)
}
\examples{
nfac <- 5
nyr <- 10
opy <- 253
# simulations with no covariance structure.
# under the null:
set.seed(as.integer(charToRaw("determinstic")))
Returns <- matrix(rnorm(opy*nyr*nfac,mean=0,sd=0.0125),ncol=nfac)
asro <- as.sropt(Returns,drag=0,opy=opy)
# under the alternative:
Returns <- matrix(rnorm(opy*nyr*nfac,mean=0.0005,sd=0.0125),ncol=nfac)
asro <- as.sropt(Returns,drag=0,opy=opy)
# using real data.
if (require(quantmod)) {
  getret <- function(sym,...) {
    OHLCV <- getSymbols(sym,auto.assign=FALSE,...)
    lrets <- diff(log(OHLCV[,paste(c(sym,"Adjusted"),collapse=".",sep="")]))
  }
  getrets <- function(syms,...) { some.rets <- do.call("cbind",lapply(syms,getret,...)) }
  some.rets <- getrets(c("IBM","AAPL","A","C","SPY","XOM"))
  asro <- as.sropt(some.rets)
}
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  \code{\link{sr}}, sropt-distribution functions,
  \code{\link{dsropt}, \link{psropt}, \link{qsropt},
  \link{rsropt}}

  Other sropt: \code{\link{confint.sropt}},
  \code{\link{dsropt}}, \code{\link{pco_sropt}},
  \code{\link{sropt}}, \code{\link{sropt_confint}},
  \code{\link{sropt.test}}
}
\keyword{univar}


\name{full.srstar}
\alias{full.srstar}
\alias{srstar}
\title{Compute the Sharpe ratio of the Markowitz portfolio.}
\usage{
  full.srstar(X,drag=0,opy=1)

  srstar(X,...)
}
\arguments{
  \item{X}{matrix of returns.}

  \item{drag}{the 'drag' term, \eqn{c_0/R}{c0/R}. defaults
  to 0. It is assumed that \code{drag} has been annualized,
  \emph{i.e.} has been multiplied by
  \eqn{\sqrt{opy}}{sqrt(opy)}. This is in contrast to the
  \code{c0} term given to \code{\link{sr}}.}

  \item{opy}{the number of observations per 'year'. The
  returns are observed at a rate of \code{opy} per 'year.'
  default value is 1, meaning no annualization is
  performed.}

  \item{...}{the above extra parameters, passed on to
  \code{full.srstar}}
}
\value{
  A list with containing the following components:
  \item{w}{the optimal portfolio.} \item{mu}{the estimated
  mean return vector.} \item{Sigma}{the estimated
  covariance matrix.} \item{df1}{the number of assets.}
  \item{df2}{the number of observed vectors.} \item{T2}{the
  Hotelling \eqn{T^2} statistic.} \item{srstar}{the maximal
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
rvs <- full.srstar(matrix(rnorm(253*8*4),ncol=4),drag=0,opy=253)
rvs <- srstar(matrix(rnorm(253*8*4),ncol=4),drag=0,opy=253)
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  \code{\link{full.sr}}, srstar-distribution functions,
  \code{\link{dsrstar}, \link{psrstar}, \link{qsrstar},
  \link{rsrstar}}

  Other srstar: \code{\link{dsrstar}},
  \code{\link{pcosrstar}}, \code{\link{srstar.confint}},
  \code{\link{srstar.test}}
}
\keyword{univar}


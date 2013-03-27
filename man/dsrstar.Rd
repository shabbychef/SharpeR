\name{dsrstar}
\alias{dsrstar}
\alias{psrstar}
\alias{qsrstar}
\alias{rsrstar}
\title{The (non-central) maximal Sharpe ratio distribution.}
\usage{
  dsrstar(x, df1, df2, zeta.s, opy, drag = 0, log = FALSE)

  psrstar(q, df1, df2, zeta.s, opy, drag = 0, ...)

  qsrstar(p, df1, df2, zeta.s, opy, drag = 0, ...)

  rsrstar(n, df1, df2, zeta.s, opy, drag = 0, ...)
}
\arguments{
  \item{x,q}{vector of quantiles.}

  \item{p}{vector of probabilities.}

  \item{n}{number of observations.}

  \item{df1}{the number of assets in the portfolio.}

  \item{df2}{the number of observations.}

  \item{zeta.s}{the non-centrality parameter, defined as
  \eqn{\zeta_* = \sqrt{\mu^{\top}\Sigma^{-1}\mu},}{zeta* =
  sqrt(mu' Sigma^-1 mu),} for population parameters.
  defaults to 0, \emph{i.e.} a central maximal Sharpe ratio
  distribution.}

  \item{opy}{the number of observations per 'year'.
  \code{x}, \code{q}, and \code{zeta.s} are quoted in
  'annualized' units, that is, per 'year', but returns are
  observed possibly at a rate of \code{opy} per 'year.'
  default value is 1, meaning no deannualization is
  performed.}

  \item{drag}{the 'drag' term, \eqn{c_0/R}{c0/R}. defaults
  to 0. It is assumed that \code{drag} has been annualized,
  \emph{i.e.} is given in the same units as \code{x} and
  \code{q}.}

  \item{log}{logical; if TRUE, probabilities p are given as
  \eqn{\mbox{log}(p)}{log(p)}.}

  \item{...}{arguments passed on to the respective
  Hotelling \eqn{T^2} functions.}
}
\value{
  \code{dsrstar} gives the density, \code{psrstar} gives
  the distribution function, \code{qsrstar} gives the
  quantile function, and \code{rsrstar} generates random
  deviates.

  Invalid arguments will result in return value \code{NaN}
  with a warning.
}
\description{
  Density, distribution function, quantile function and
  random generation for the maximal Sharpe ratio
  distribution with \code{df1} and \code{df2} degrees of
  freedom (and optional maximal signal-noise-ratio
  \code{zeta.s}).
}
\details{
  Suppose \eqn{x_i}{xi} are \eqn{n} independent draws of a
  \eqn{q}-variate normal random variable with mean
  \eqn{\mu}{mu} and covariance matrix \eqn{\Sigma}{Sigma}.
  Let \eqn{\bar{x}}{xbar} be the (vector) sample mean, and
  \eqn{S} be the sample covariance matrix (using Bessel's
  correction). Let \deqn{Z(w) = \frac{w^{\top}\bar{x} -
  c_0}{\sqrt{w^{\top}S w}}}{Z(w) = (w'xbar -
  c0)/sqrt(w'Sw)} be the (sample) Sharpe ratio of the
  portfolio \eqn{w}, subject to risk free rate
  \eqn{c_0}{c0}.

  Let \eqn{w_*}{w*} be the solution to the portfolio
  optimization problem: \deqn{\max_{w: 0 < w^{\top}S w \le
  R^2} Z(w),}{max {Z(w) | 0 < w'Sw <= R^2},} with maximum
  value \eqn{z_* = Z\left(w_*\right)}{z* = Z(w*)}. Then
  \deqn{w_* = R
  \frac{S^{-1}\bar{x}}{\sqrt{\bar{x}^{\top}S^{-1}\bar{x}}}}{%
  w* = R S^-1 xbar / sqrt(xbar' S^-1 xbar)} and \deqn{z_* =
  \sqrt{\bar{x}^{\top} S^{-1} \bar{x}} - \frac{c_0}{R}}{%
  z* = sqrt(xbar' S^-1 xbar) - c0/R}

  The variable \eqn{z_*}{z*} follows a \emph{Maximal Sharpe
  ratio} distribution. For convenience, we may assume that
  the sample statistic has been annualized in the same
  manner as the Sharpe ratio, that is by multiplying by
  \eqn{d}, the number of observations per epoch.

  The Maximal Sharpe Ratio distribution is parametrized by
  the number of assets, \eqn{q}, the number of independent
  observations, \eqn{n}, the noncentrality parameter,
  \deqn{\zeta_* = \sqrt{\mu^{\top}\Sigma^{-1}\mu},}{zeta* =
  sqrt(mu' Sigma^-1 mu),} the 'drag' term,
  \eqn{c_0/R}{c0/R}, and the annualization factor, \eqn{d}.
  The drag term makes this a location family of
  distributions, and by default we assume it is zero.

  The parameters are encoded as follows: \itemize{ \item
  \eqn{q} is denoted by \code{df1}. \item \eqn{n} is
  denoted by \code{df2}. \item \eqn{\zeta_*}{zeta*} is
  denoted by \code{zeta.s}. \item \eqn{d} is denoted by
  \code{opy}. \item \eqn{c_0/R} is denoted by \code{drag}.
  }
}
\note{
  This is a thin wrapper on the Hotelling T-squared
  distribution, provided for convenience.
}
\examples{
# generate some variates
rvs <- rsrstar(2048, 8, 253*6, 0, 253)
hist(rvs)
# these should be uniform:
isp <- psrstar(rvs, 8, 253*6, 0, 253)
plot(ecdf(isp))
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\references{
  Kan, Raymond and Smith, Daniel R. "The Distribution of
  the Sample Minimum-Variance Frontier." Journal of
  Management Science 54, no. 7 (2008): 1364--1380.
  \url{http://mansci.journal.informs.org/cgi/doi/10.1287/mnsc.1070.0852}
}
\seealso{
  Hotelling \eqn{T^2}-distribution functions,
  \code{\link{dT2},\link{pT2},\link{qT2},\link{rT2}}

  Other srstar: \code{\link{full.srstar}},
  \code{\link{pcosrstar}}, \code{\link{srstar.confint}},
  \code{\link{srstar.test}}
}
\keyword{distribution}


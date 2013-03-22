\name{dT2}
\alias{dT2}
\alias{pT2}
\alias{qT2}
\alias{rT2}
\title{The (non-central) Hotelling distribution.}
\usage{
  dT2(x, df1, df2, delta2, log = FALSE)

  pT2(q, df1, df2, delta2, ...)

  qT2(p, df1, df2, delta2, ...)

  rT2(n, df1, df2, delta2)
}
\arguments{
  \item{x,q}{vector of quantiles.}

  \item{p}{vector of probabilities.}

  \item{n}{number of observations.}

  \item{df1}{the dimension of the vector space from which
  multivariate observations had been drawn, \eqn{q}{q}.}

  \item{df2}{the number of observations that the sample
  mean and covariance are based on, \eqn{n}{n}.}

  \item{delta2}{the population non-centrality parameter,
  defined as \eqn{\delta^2 = n
  \mu^{\top}\Sigma^{-1}\mu}{delta^2 = n (mu' Sigma^-1 mu)}.
  defaults to 0, i.e. a central \eqn{T^2}{T2}
  distribution.}

  \item{log}{logical; if TRUE, probabilities p are given as
  \eqn{\mbox{log}(p)}{log(p)}.}

  \item{...}{arguments passed on to the respective F
  distribution functions, namely \code{lower.tail} with
  default \code{TRUE}, and \code{log.p}, with default
  \code{FALSE}.}
}
\value{
  \code{dT2} gives the density, \code{pT2} gives the
  distribution function, \code{qT2} gives the quantile
  function, and \code{rT2} generates random deviates.

  Invalid arguments will result in return value \code{NaN}
  with a warning.
}
\description{
  Density, distribution function, quantile function and
  random generation for the Hotelling distribution
  distribution with \code{df1} and \code{df2} degrees of
  freedom (and optional non-centrality parameter
  \code{delta2}).
}
\details{
  Suppose \eqn{x_i}{xi} are \eqn{n}{n} independent draws of
  a \eqn{q}{q}-variate normal random variable with mean
  \eqn{\mu}{mu} and covariance matrix \eqn{\Sigma}{Sigma}.
  Let \eqn{\bar{x}}{xbar} be the (vector) sample mean, and
  \eqn{S}{S} be the sample covariance matrix (using
  Bessel's correction). Then \deqn{T^2 = n
  \bar{x}^{\top}S^{-1}\bar{x}}{T^2 = n xbar' S^-1 xbar}
  follows a (non-central) Hotelling T-squared distribution
  with \eqn{q}{q} and \eqn{n-1}{n-1} degrees of freedom,
  and non-centrality parameter \deqn{\delta^2 = n
  \mu^{\top}\Sigma^{-1}\mu}{delta^2 = n mu' Sigma^-1 mu}

  The (non-central) T-squared distribution is a
  (non-central) F distribution up to scaling which depends
  on \eqn{q}{q} and \eqn{n}{n}.
}
\note{
  This is a thin wrapper on the F distribution, provided
  for convenience.
}
\examples{
rvs <- rT2(2048, 4, 253*6, 0)
dvs <- dT2(rvs, 4, 253*6, 0)
pvs <- pT2(rvs, 4, 253*6, 0)
plot(ecdf(pvs))
pvs <- pT2(rvs, 4, 253*6, 1)
plot(ecdf(pvs))
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\references{
  Wikipedia contributors, 'Hotelling's T-squared
  distribution', Wikipedia, The Free Encyclopedia, 11
  December 2012, 13:38 UTC,
  \url{http://en.wikipedia.org/w/index.php?title=Hotelling\%27s_T-squared_distribution\&oldid=527530524}
  [accessed 9 January 2013]

  Bilodeau, Martin, and David Brenner. Theory of
  multivariate statistics. Springer, 1999.
  \url{http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.172.3290}

  Timm, Neil H. Applied multivariate analysis: methods and
  case studies. Springer, 2002.
  \url{http://books.google.com/books?id=vtiyg6fnnskC}

  Hotelling, Harold. "The Generalization of Student's
  Ratio." Annals of Mathematical Statistics 2, no. 3
  (1931): 360--378.
  \url{http://projecteuclid.org/euclid.aoms/1177732979}
}
\seealso{
  F-distribution functions, \code{\link{df}, \link{pf},
  \link{qf}, \link{rf}}, Sharpe ratio distribution,
  \code{\link{dsr}, \link{psr}, \link{qsr}, \link{rsr}}
}
\keyword{distribution}


\name{dsr}
\alias{dsr}
\alias{psr}
\alias{qsr}
\alias{rsr}
\title{The (non-central) Sharpe ratio.}
\usage{
  dsr(x, df, zeta, opy, ...)

  psr(q, df, zeta, opy, ...)

  qsr(p, df, zeta, opy, ...)

  rsr(n, df, zeta, opy)
}
\arguments{
  \item{x,q}{vector of quantiles.}

  \item{p}{vector of probabilities.}

  \item{n}{number of observations.}

  \item{df}{the number of observations the statistic is
  based on. This is one more than the number of degrees of
  freedom in the corresponding t-statistic, although the
  effect will be small when \code{df} is large.}

  \item{zeta}{the 'signal-to-noise' parameter,
  \eqn{\zeta}{zeta} defined as the population mean divided
  by the population standard deviation, 'annualized'.}

  \item{opy}{the number of observations per 'year'.
  \code{x}, \code{q}, and \code{zeta} are quoted in
  'annualized' units, that is, per square root 'year', but
  returns are observed possibly at a rate of \code{opy} per
  'year.' default value is 1, meaning no deannualization is
  performed.}

  \item{...}{arguments passed on to the respective
  t-distribution functions, namely \code{lower.tail} with
  default \code{TRUE}, \code{log} with default
  \code{FALSE}, and \code{log.p} with default
  \code{FALSE}.}
}
\value{
  \code{dsr} gives the density, \code{psr} gives the
  distribution function, \code{qsr} gives the quantile
  function, and \code{rsr} generates random deviates.

  Invalid arguments will result in return value \code{NaN}
  with a warning.
}
\description{
  Density, distribution function, quantile function and
  random generation for the Sharpe ratio distribution with
  \code{df} degrees of freedom (and optional
  signal-noise-ratio \code{zeta}).
}
\details{
  Suppose \eqn{x_i}{xi} are \eqn{n} independent draws of a
  normal random variable with mean \eqn{\mu}{mu} and
  variance \eqn{\sigma^2}{sigma^2}. Let \eqn{\bar{x}}{xbar}
  be the sample mean, and \eqn{s} be the sample standard
  deviation (using Bessel's correction). Let \eqn{c_0}{c0}
  be the 'risk free rate'.  Then \deqn{z = \frac{\bar{x} -
  c_0}{s}}{z = (xbar - c0)/s} is the (sample) Sharpe ratio.

  The units of \eqn{z} is \eqn{\mbox{time}^{-1/2}}{per root
  time}. Typically the Sharpe ratio is \emph{annualized} by
  multiplying by \eqn{\sqrt{d}}{sqrt(d)}, where \eqn{d} is
  the number of observations per year (or whatever the
  target annualization epoch.)

  Letting \eqn{z = \sqrt{d}\frac{\bar{x}-c_0}{s}}{z =
  sqrt(d)(xbar - c0)/s}, where the sample estimates are
  based on \eqn{n} observations, then \eqn{z}{z} takes a
  (non-central) Sharpe ratio distribution parametrized by
  \eqn{n} 'degrees of freedom', non-centrality parameter
  \eqn{\zeta = \frac{\mu - c_0}{\sigma}}{zeta = (mu -
  c0)/sigma}, and annualization parameter \eqn{d}.

  The parameters are encoded as follows: \itemize{ \item
  \eqn{n} is denoted by \code{df}. \item \eqn{\zeta}{zeta}
  is denoted by \code{zeta}. \item \eqn{d} is denoted by
  \code{opy}. ('Observations Per Year') }

  If the returns violate the assumptions of normality,
  independence, etc (\emph{as they always should in the
  real world}), the sample Sharpe Ratio will not follow
  this distribution. It does provide, however, a reasonable
  approximation in many cases.
}
\note{
  This is a thin wrapper on the t distribution. The
  functions \code{\link{dt}, \link{pt}, \link{qt}} can
  accept ncp from limited range (\eqn{|\delta|\le
  37.62}{delta <= 37.62}). Some corrections may have to be
  made here for large \code{zeta}.
}
\examples{
rvs <- rsr(2048, 253*6, 0, 253)
dvs <- dsr(rvs, 253*6, 0, 253)
pvs <- psr(rvs, 253*6, 0, 253)
plot(ecdf(pvs))
pvs <- psr(rvs, 253*6, 1, 253)
plot(ecdf(pvs))
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\references{
  Sharpe, William F. "Mutual fund performance." Journal of
  business (1966): 119-138.
  \url{http://ideas.repec.org/a/ucp/jnlbus/v39y1965p119.html}

  Lo, Andrew W. "The statistics of Sharpe ratios."
  Financial Analysts Journal 58, no. 4 (2002): 36-52.
  \url{http://ssrn.com/paper=377260}
}
\seealso{
  t-distribution functions, \code{\link{dt}, \link{pt},
  \link{qt}, \link{rt}}

  Other sr: \code{\link{confint.sr}},
  \code{\link{plambdap}}, \code{\link{power.sr.test}},
  \code{\link{se}}, \code{\link{se.default}},
  \code{\link{se.sr}}, \code{\link{sr}},
  \code{\link{sr.default}}, \code{\link{sr.equality.test}},
  \code{\link{sr.lm}}, \code{\link{sr.test}},
  \code{\link{sr.xts}}
}
\keyword{distribution}


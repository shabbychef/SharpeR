\name{full.sr}
\alias{full.sr}
\alias{sr}
\title{Compute the Sharpe ratio.}
\usage{
  full.sr(x,c0=0,opy=1,na.rm=FALSE)

  sr(x,...)
}
\arguments{
  \item{x}{vector of returns.}

  \item{c0}{the 'risk-free' or 'disastrous' rate of return.
  this is assumed to be given in the same units as x,
  \emph{not} in 'annualized' terms.}

  \item{opy}{the number of observations per 'year'. This is
  used to 'annualize' the answer.}

  \item{na.rm}{logical.  Should missing values be removed?}

  \item{...}{the above extra parameters, passed on to
  \code{full.sr}}
}
\value{
  \code{full.sr} returns a list with containing the
  following components: \item{sr}{the annualized Sharpe
  ratio.} \item{df}{the number of observations.} \code{sr}
  just returns the numeric annualized Sharpe.
}
\description{
  Computes the Sharpe ratio of some observed returns.
}
\details{
  Suppose \eqn{x_i}{xi} are \eqn{n}{n} independent returns
  of some asset. Let \eqn{\bar{x}}{xbar} be the sample
  mean, and \eqn{s}{s} be the sample standard deviation
  (using Bessel's correction). Let \eqn{c_0}{c0} be the
  'risk free rate'.  Then \deqn{z = \frac{\bar{x} -
  c_0}{s}}{z = (xbar - c0)/s} is the (sample) Sharpe ratio.

  The units of \eqn{z}{z} are \eqn{\mbox{time}^{-1/2}}{per
  root time}. Typically the Sharpe ratio is
  \emph{annualized} by multiplying by
  \eqn{\sqrt{\mbox{opy}}}{sqrt(opy)}, where
  \eqn{\mbox{opy}}{opy} is the number of observations per
  year (or whatever the target annualization epoch.)
}
\examples{
rvs <- full.sr(rnorm(253*8),opy=253)
rvs <- sr(rnorm(253*8),opy=253)
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\references{
  Sharpe, William F. "Mutual fund performance." Journal of
  business (1966): 119-138.
  \url{http://ideas.repec.org/a/ucp/jnlbus/v39y1965p119.html}

  Lo, Andrew W. "The statistics of Sharpe ratios."
  Financial Analysts Journal (2002): 36-52.
  \url{http://ssrn.com/paper=377260}
}
\seealso{
  sr-distribution functions, \code{\link{dsr}, \link{psr},
  \link{qsr}, \link{rsr}}

  Other sr: \code{\link{dsr}}, \code{\link{plambdap}},
  \code{\link{power.sr.test}}, \code{\link{sr.confint}},
  \code{\link{sr.equality.test}}, \code{\link{sr.se}},
  \code{\link{sr.test}}
}
\keyword{univar}


\name{sr}
\alias{sr}
\alias{sr.default}
\alias{sr.lm}
\alias{sr.xts}
\title{Compute the Sharpe ratio.}
\usage{
  sr(x,...) c0=0,opy=1,na.rm=FALSE)

  \method{sr}{default} (x, c0 = 0, opy = 1, na.rm = FALSE)

  \method{sr}{lm} (x, c0 = 0, opy = 1, na.rm = FALSE)

  \method{sr}{xts} (x, c0 = 0, opy = 1, na.rm = FALSE)
}
\arguments{
  \item{x}{vector of returns.}

  \item{c0}{the 'risk-free' or 'disastrous' rate of return.
  this is assumed to be given in the same units as x,
  \emph{not} in 'annualized' terms.}

  \item{opy}{the number of observations per 'year'. This is
  used to 'annualize' the answer.}

  \item{na.rm}{logical.  Should missing values be removed?}

  \item{x}{a fit model of class \code{lm}.}

  \item{anxts}{an xts object.}
}
\value{
  a list containing the following components: \item{sr}{the
  annualized Sharpe ratio.} \item{df}{the number of
  observations.} \item{opy}{the annualization factor.} cast
  to class \code{sr}.
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
# Sharpe's 'model': just given a bunch of returns.
asr <- sr(rnorm(253*8),opy=253)
# given an xts object:
if (require(quantmod)) {
  getSymbols('IBM')
  lrets <- diff(log(IBM[,"IBM.Adjusted"]))
  asr <- sr(lrets,na.rm=TRUE)
}
# on a linear model, find the 'Sharpe' of the residual term
Factors <- matrix(rnorm(253*5*6),ncol=6)
Returns <- rnorm(dim(Factors)[1],0.003)
APT_mod <- lm(Returns ~ Factors)
asr <- sr(APT_mod,opy=253)
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

  Other sr: \code{\link{confint.sr}}, \code{\link{dsr}},
  \code{\link{plambdap}}, \code{\link{power.sr.test}},
  \code{\link{se}}, \code{\link{se.default}},
  \code{\link{se.sr}}, \code{\link{sr.equality.test}},
  \code{\link{sr.test}}
}
\keyword{univar}


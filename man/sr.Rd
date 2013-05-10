\name{sr}
\alias{sr}
\title{Create an 'sr' object.}
\usage{
  sr(sr,df,c0=0,ope=1,rescal=sqrt(1/(df+1)),epoch="yr")
}
\arguments{
  \item{sr}{a Sharpe ratio statistic.}

  \item{df}{the degrees of freedom of the equivalent
  t-statistic.}

  \item{c0}{the 'risk-free' or 'disastrous' rate of return.
  this is assumed to be given in the same units as x,
  \emph{not} in 'annualized' terms.}

  \item{ope}{the number of observations per 'epoch'. For
  convenience of interpretation, The Sharpe ratio is
  typically quoted in 'annualized' units for some epoch,
  that is, 'per square root epoch', though returns are
  observed at a frequency of \code{ope} per epoch.  The
  default value is 1, meaning the code will not attempt to
  guess, what the observation frequency is, and no
  annualization adjustments will be made.}

  \item{rescal}{the rescaling parameter.}

  \item{epoch}{the string representation of the 'epoch',
  defaulting to 'yr'.}
}
\value{
  a list cast to class \code{sr}.
}
\description{
  Spawns an object of class \code{sr}.
}
\details{
  The \code{sr} class contains information about a rescaled
  t-statistic. The following are list attributes of the
  object: \itemize{ \item \code{sr} The Sharpe ratio
  statistic. \item \code{df} The d.f. of the equivalent
  t-statistic. \item \code{c0} The drag 'risk free rate'
  used. \item \code{ope} The 'observations per epoch'.
  \item \code{rescal} The rescaling parameter. \item
  \code{epoch} The string name of the 'epoch'. }

  The stored Sharpe statistic, \code{sr} is equal to the
  t-statistic times \eqn{rescal * sqrt{ope}}{rescal *
  sqrt(ope)}.

  For the most part, this constructor should \emph{not} be
  called directly, rather \code{\link{as.sr}} should be
  called instead to compute the Sharpe ratio.
}
\note{
  2FIX: allow rownames?
}
\examples{
# roll your own.
ope <- 253
zeta <- 1.0
n <- 6 * ope
rvs <- rsr(1,n,zeta,ope=ope)
roll.own <- sr(sr=rvs,df=n-1,ope=ope,rescal=sqrt(1/n))
# put a bunch in. naming becomes a problem.
rvs <- rsr(5,n,zeta,ope=ope)
roll.own <- sr(sr=rvs,df=n-1,ope=ope,rescal=sqrt(1/n))
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\references{
  Sharpe, William F. "Mutual fund performance." Journal of
  business (1966): 119-138.
  \url{http://ideas.repec.org/a/ucp/jnlbus/v39y1965p119.html}
}
\seealso{
  \code{\link{reannualize}}

  \code{\link{as.sr}}

  Other sr: \code{\link{as.sr}},
  \code{\link{as.sr.data.frame}},
  \code{\link{as.sr.default}}, \code{\link{as.sr.lm}},
  \code{\link{as.sr.xts}}, \code{\link{confint.sr}},
  \code{\link{confint.sropt}}, \code{\link{dsr}},
  \code{\link{is.sr}}, \code{\link{power.sr_test}},
  \code{\link{print.sr}}, \code{\link{print.sropt}},
  \code{\link{reannualize}}, \code{\link{reannualize.sr}},
  \code{\link{reannualize.sropt}}, \code{\link{se}},
  \code{\link{se.sr}}, \code{\link{sr_equality_test}},
  \code{\link{sr_test}}
}
\keyword{univar}


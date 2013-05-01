\name{sropt}
\alias{sropt}
\title{Create an 'sropt' object.}
\usage{
  sropt(z.s,df1,df2,drag=0,ope=1,epoch="yr",T2=NULL)
}
\arguments{
  \item{z.s}{an optimum Sharpe ratio statistic.}

  \item{drag}{the 'drag' term, \eqn{c_0/R}{c0/R}. defaults
  to 0. It is assumed that \code{drag} has been annualized,
  \emph{i.e.} has been multiplied by
  \eqn{\sqrt{ope}}{sqrt(ope)}. This is in contrast to the
  \code{c0} term given to \code{\link{sr}}.}

  \item{T2}{the Hotelling \eqn{T^2}{T^2} statistic. If not
  given, it is computed from the given information.}

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

  \item{df1}{the number of assets in the portfolio.}

  \item{df2}{the number of observations.}
}
\value{
  a list cast to class \code{sropt}.
}
\description{
  Spawns an object of class \code{sropt}.
}
\details{
  The \code{sropt} class contains information about a
  rescaled T^2-statistic. The following are list attributes
  of the object: \itemize{ \item \code{sropt} The (optimal)
  Sharpe ratio statistic. \item \code{df1} The number of
  assets. \item \code{df2} The number of observations.
  \item \code{drag} The drag term, which is the 'risk free
  rate' divided by the maximum risk. \item \code{ope} The
  'observations per epoch'. \item \code{epoch} The string
  name of the 'epoch'. }

  For the most part, this constructor should \emph{not} be
  called directly, rather \code{\link{as.sropt}} should be
  called instead to compute the needed statistics.
}
\note{
  2FIX: allow rownames?
}
\examples{
# roll your own.
ope <- 253
zeta.s <- 1.0
df1 <- 10
df2 <- 6 * ope
rvs <- rsropt(1,df1,df2,zeta.s,ope,drag=0)
roll.own <- sropt(z.s=rvs,df1,df2,drag=0,ope=ope)
print(roll.own)
# put a bunch in. naming becomes a problem.
rvs <- rsropt(5,df1,df2,zeta.s,ope,drag=0)
roll.own <- sropt(z.s=rvs,df1,df2,drag=0,ope=ope)
print(roll.own)
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  \code{\link{reannualize}}

  \code{\link{as.sropt}}

  Other sropt: \code{\link{as.sropt}},
  \code{\link{as.sropt.default}},
  \code{\link{as.sropt.xts}}, \code{\link{confint.sr}},
  \code{\link{confint.sropt}}, \code{\link{dsropt}},
  \code{\link{is.sropt}}, \code{\link{power.sropt_test}},
  \code{\link{reannualize}}, \code{\link{reannualize.sr}},
  \code{\link{reannualize.sropt}}, \code{\link{sropt_test}}
}
\keyword{univar}


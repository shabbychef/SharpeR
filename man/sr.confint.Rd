\name{sr.confint}
\alias{sr.confint}
\title{Confidence Interval on Signal-Noise Ratio}
\usage{
  sr.confint(sr,df,level=0.95,type=c("exact","t","Z","F"),opy=1,
    level.lo=(1-level)/2,level.hi=1-level.lo)
}
\arguments{
  \item{sr}{an observed Sharpe ratio statistic,
  annualized.}

  \item{df}{the number of observations the statistic is
  based on. This is one more than the number of degrees of
  freedom in the corresponding t-statistic, although the
  effect will be small when \code{df} is large.}

  \item{level}{the confidence level required.}

  \item{type}{the estimator type. one of \code{"t", "Lo",
  "Z", "F"}}

  \item{opy}{the number of observations per 'year'.
  \code{x}, \code{q}, and \code{snr} are quoted in
  'annualized' units, that is, per square root 'year', but
  returns are observed possibly at a rate of \code{opy} per
  'year.' default value is 1, meaning no deannualization is
  performed.}

  \item{level.lo}{the lower bound for the confidence
  interval.}

  \item{level.hi}{the upper bound for the confidence
  interval.}
}
\value{
  A matrix (or vector) with columns giving lower and upper
  confidence limits for the SNR. These will be labelled as
  level.lo and level.hi in \%, \emph{e.g.} \code{"2.5 \%"}
}
\description{
  Computes approximate confidence intervals on the
  Signal-Noise ratio given the Sharpe ratio.
}
\details{
  Constructs confidence intervals on the Signal-Noise ratio
  given observed Sharpe ratio statistic. The available
  methods are:

  \itemize{ \item The default, \code{exact}, which is only
  exact when returns are normal, based on inverting the
  non-central t distribution. \item A method based on the
  standard error of a non-central t distribution. \item A
  method based on a normal approximation. \item A method
  based on an F statistic. }
}
\examples{
opy <- 253
df <- opy * 6
rvs <- rsr(1, df, 1.0, opy)
aci <- sr.confint(rvs,df,type="t",opy=opy)
aci2 <- sr.confint(rvs,df,type="Z",opy=opy)
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  \code{\link{confint}}, \code{\link{sr.se}},
  \code{\link{qlambdap}}

  Other sr: \code{\link{dsr}}, \code{\link{full.sr}},
  \code{\link{plambdap}}, \code{\link{power.sr.test}},
  \code{\link{sr.equality.test}}, \code{\link{sr.se}},
  \code{\link{sr.test}}
}
\keyword{htest}


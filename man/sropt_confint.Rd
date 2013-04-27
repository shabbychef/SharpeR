\name{sropt_confint}
\alias{confint.sropt}
\alias{sropt_confint}
\title{Confidence Interval on Maximal Signal-Noise Ratio}
\usage{
  sropt_confint(z.s,df1,df2,level=0.95,
    ope=1,level.lo=(1-level)/2,level.hi=1-level.lo)

  \method{confint}{sropt} (z.s, parm, level = 0.95, ...)
}
\arguments{
  \item{z.s}{an observed Sharpe ratio statistic,
  annualized.}

  \item{level}{the confidence level required.}

  \item{ope}{the number of observations per 'epoch'. The
  Sharpe ratio is typically quoted in 'annualized' units
  for some epoch, that is, 'per square root epoch', though
  returns are observed at a frequency of \code{ope} per
  epoch. The default value is 1, meaning the observation
  period is the epoch, and no annualization adjustments are
  made.}

  \item{level.lo}{the lower bound for the confidence
  interval.}

  \item{level.hi}{the upper bound for the confidence
  interval.}

  \item{parm}{ignored here}

  \item{df1}{the number of assets in the portfolio.}

  \item{df2}{the number of observations.}

  \item{...}{arguments passed on to the respective
  Hotelling \eqn{T^2} functions.}
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
  Suppose \eqn{x_i}{xi} are \eqn{n}{n} independent draws of
  a \eqn{q}{q}-variate normal random variable with mean
  \eqn{\mu}{mu} and covariance matrix \eqn{\Sigma}{Sigma}.
  Let \eqn{\bar{x}}{xbar} be the (vector) sample mean, and
  \eqn{S}{S} be the sample covariance matrix (using
  Bessel's correction). Let \deqn{z_* =
  \sqrt{\bar{x}^{\top} S^{-1} \bar{x}}}{z* = sqrt(xbar'
  S^-1 xbar)} Given observations of \eqn{z_*}{z*}, compute
  confidence intervals on the population analogue, defined
  as \deqn{\zeta_* = \sqrt{\mu^{\top} \Sigma^{-1}
  \mu}}{zeta* = sqrt(mu' Sigma^-1 mu)}
}
\examples{
# fix these!
ope <- 253
df1 <- 6
df2 <- ope * 6
rvs <- as.matrix(rnorm(df1*df2),ncol=df1)
sro <- as.sropt(rvs)
aci <- confint(sro)
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  \code{\link{confint}}, \code{\link{sropt.test}}

  Other sropt: \code{\link{as.sropt}},
  \code{\link{as.sropt.default}},
  \code{\link{as.sropt.xts}}, \code{\link{dsropt}},
  \code{\link{power.sropt_test}}, \code{\link{sropt}},
  \code{\link{sropt_test}}
}
\keyword{htest}


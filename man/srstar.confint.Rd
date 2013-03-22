\name{srstar.confint}
\alias{srstar.confint}
\title{Confidence Interval on Maximal Signal-Noise Ratio}
\usage{
  srstar.confint(srstar,df1,df2,level=0.95,
    opy=1,level.lo=(1-level)/2,level.hi=1-level.lo)
}
\arguments{
  \item{level}{the confidence level required.}

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

  \item{df1}{the number of assets in the portfolio.}

  \item{df2}{the number of observations.}
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
opy <- 253
df <- opy * 6
rvs <- rsr(1, df, 1.0, opy)
aci <- sr.confint(rvs,df,opy=opy)
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  \code{\link{confint}}, \code{\link{sr.confint}},
  \code{\link{qcosrstar}}, \code{\link{srstar.test}}

  Other srstar: \code{\link{dsrstar}},
  \code{\link{full.srstar}}, \code{\link{pcosrstar}},
  \code{\link{srstar.test}}
}
\keyword{htest}


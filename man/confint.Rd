\name{confint.sr}
\alias{confint.del_sropt}
\alias{confint.sr}
\alias{confint.sropt}
\title{Confidence Interval on (optimal) Signal-Noise Ratio}
\usage{
  \method{confint}{sr} (object, parm, level = 0.95,
    level.lo = (1 - level)/2, level.hi = 1 - level.lo,
    type = c("exact", "t", "Z"), ...)

  \method{confint}{sropt} (object, parm, level = 0.95,
    level.lo = (1 - level)/2, level.hi = 1 - level.lo, ...)

  \method{confint}{del_sropt} (object, parm, level = 0.95,
    level.lo = (1 - level)/2, level.hi = 1 - level.lo, ...)
}
\arguments{
  \item{object}{an observed Sharpe ratio statistic, of
  class \code{sr} or \code{sropt}.}

  \item{parm}{ignored here, but required for the general
  method.}

  \item{level}{the confidence level required.}

  \item{level.lo}{the lower confidence level required.}

  \item{level.hi}{the upper confidence level required.}

  \item{type}{which method to apply.}

  \item{...}{further arguments to be passed to or from
  methods.}
}
\value{
  A matrix (or vector) with columns giving lower and upper
  confidence limits for the parameter. These will be
  labelled as level.lo and level.hi in \%, \emph{e.g.}
  \code{"2.5 \%"}
}
\description{
  Computes approximate confidence intervals on the
  (optimal) Signal-Noise ratio given the (optimal) Sharpe
  ratio. Works on objects of class \code{sr} and
  \code{sropt}.
}
\details{
  Constructs confidence intervals on the Signal-Noise ratio
  given observed Sharpe ratio statistic. The available
  methods are:

  \itemize{ \item The default, \code{exact}, which is only
  exact when returns are normal, based on inverting the
  non-central t distribution. \item A method based on the
  standard error of a non-central t distribution. \item A
  method based on a normal approximation. }

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
# using "sr" class:
ope <- 253
df <- ope * 6
xv <- rnorm(df, 1 / sqrt(ope))
mysr <- as.sr(xv,ope=ope)
confint(mysr,level=0.90)
# using "lm" class
yv <- xv + rnorm(length(xv))
amod <- lm(yv ~ xv)
mysr <- as.sr(amod,ope=ope)
confint(mysr,level.lo=0.05,level.hi=1.0)
# rolling your own.
ope <- 253
df <- ope * 6
zeta <- 1.0
rvs <- rsr(128, df, zeta, ope)
roll.own <- sr(sr=rvs,df=df,c0=0,ope=ope)
aci <- confint(roll.own,level=0.95)
coverage <- 1 - mean((zeta < aci[,1]) | (aci[,2] < zeta))
# using "sropt" class
ope <- 253
df1 <- 4
df2 <- ope * 3
rvs <- as.matrix(rnorm(df1*df2),ncol=df1)
sro <- as.sropt(rvs,ope=ope)
aci <- confint(sro)
# on sropt, rolling your own.
zeta.s <- 1.0
rvs <- rsropt(128, df1, df2, zeta.s, ope)
roll.own <- sropt(z.s=rvs,df1,df2,drag=0,ope=ope)
aci <- confint(roll.own,level=0.95)
coverage <- 1 - mean((zeta.s < aci[,1]) | (aci[,2] < zeta.s))
# using "del_sropt" class
nfac <- 5
nyr <- 10
ope <- 253
set.seed(as.integer(charToRaw("be determinstic")))
Returns <- matrix(rnorm(ope*nyr*nfac,mean=0,sd=0.0125),ncol=nfac)
# hedge out the first one:
G <- matrix(diag(nfac)[1,],nrow=1)
asro <- as.del_sropt(Returns,G,drag=0,ope=ope)
aci <- confint(asro,level=0.95)
# under the alternative
Returns <- matrix(rnorm(ope*nyr*nfac,mean=0.001,sd=0.0125),ncol=nfac)
asro <- as.del_sropt(Returns,G,drag=0,ope=ope)
aci <- confint(asro,level=0.95)
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
  \code{\link{confint}}, \code{\link{se}}

  Other sr: \code{\link{as.sr}},
  \code{\link{as.sr.data.frame}},
  \code{\link{as.sr.default}}, \code{\link{as.sr.lm}},
  \code{\link{as.sr.matrix}},
  \code{\link{as.sr.timeSeries}}, \code{\link{as.sr.xts}},
  \code{\link{dsr}}, \code{\link{is.sr}},
  \code{\link{power.sr_test}},
  \code{\link{print.del_sropt}}, \code{\link{print.sr}},
  \code{\link{print.sropt}}, \code{\link{reannualize}},
  \code{\link{reannualize.sr}},
  \code{\link{reannualize.sropt}}, \code{\link{se}},
  \code{\link{se.sr}}, \code{\link{sr}},
  \code{\link{sr_equality_test}}, \code{\link{sr_test}},
  \code{\link{sr_vcov}}

  Other sropt: \code{\link{as.sropt}},
  \code{\link{as.sropt.default}},
  \code{\link{as.sropt.xts}}, \code{\link{dsropt}},
  \code{\link{is.sropt}}, \code{\link{power.sropt_test}},
  \code{\link{reannualize}}, \code{\link{reannualize.sr}},
  \code{\link{reannualize.sropt}}, \code{\link{sropt}},
  \code{\link{sropt_test}}
}
\keyword{htest}


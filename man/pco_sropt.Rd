\name{pco_sropt}
\alias{pco_sropt}
\alias{qco_sropt}
\title{The 'confidence distribution' for maximal Sharpe ratio.}
\usage{
  pco_sropt(q,df1,df2,z.s,opy,lower.tail=TRUE,log.p=FALSE)

  qco_sropt(p,df1,df2,z.s,opy,lower.tail=TRUE,log.p=FALSE,lb=0,ub=Inf)
}
\arguments{
  \item{q}{vector of quantiles.}

  \item{p}{vector of probabilities.}

  \item{z.s}{an observed Sharpe ratio statistic,
  annualized.}

  \item{opy}{the number of observations per 'year'.
  \code{z.s}, \code{q}, and \code{zeta.s} are quoted in
  'annualized' units, that is, per 'year', but returns are
  observed possibly at a rate of \code{opy} per 'year.'
  default value is 1, meaning no deannualization is
  performed.}

  \item{log.p}{logical; if TRUE, probabilities p are given
  as \eqn{\mbox{log}(p)}{log(p)}.}

  \item{lower.tail}{logical; if TRUE (default),
  probabilities are \eqn{P[X \le x]}{P[X <= x]}, otherwise,
  \eqn{P[X > x]}{P[X > x]}.}

  \item{lb}{the lower bound for the output of
  \code{qco_sropt}.}

  \item{ub}{the upper bound for the output of
  \code{qco_sropt}.}

  \item{df1}{the number of assets in the portfolio.}

  \item{df2}{the number of observations.}
}
\value{
  \code{pco_sropt} gives the distribution function, and
  \code{qco_sropt} gives the quantile function.

  Invalid arguments will result in return value \code{NaN}
  with a warning.
}
\description{
  Distribution function and quantile function for the
  'confidence distribution' of the maximal Sharpe ratio.
  This is just an inversion to perform inference on
  \eqn{\zeta_*}{zeta*} given observed statistic
  \eqn{z_*}{z*}.
}
\details{
  Suppose \eqn{z_*}{z*} follows a \emph{Maximal Sharpe
  ratio} distribution (see \code{\link{package-SharpeR}})
  for known degrees of freedom, and unknown non-centrality
  parameter \eqn{\zeta_*}{zeta*}. The 'confidence
  distribution' views \eqn{\zeta_*}{zeta*} as a random
  quantity once \eqn{z_*}{z*} is observed. As such, the CDF
  of the confidence distribution is the same as that of the
  Maximal Sharpe ratio (up to a flip of \code{lower.tail});
  while the quantile function is used to compute confidence
  intervals on \eqn{\zeta_*}{zeta*} given \eqn{z_*}{z*}.
}
\note{
  When \code{lower.tail} is true, \code{pco_sropt} is
  monotonic increasing with respect to \code{q}, and
  decreasing in \code{sropt}; these are reversed when
  \code{lower.tail} is false. Similarly, \code{qco_sropt}
  is increasing in \code{sign(as.double(lower.tail) - 0.5)
  * p} and \code{- sign(as.double(lower.tail) - 0.5) *
  sropt}.
}
\examples{
zeta.s <- 2.0
opy <- 253
ntest <- 2000
df1 <- 4
df2 <- 6 * opy
rvs <- rsropt(ntest,df1=df1,df2=df2,zeta.s=zeta.s)
qvs <- seq(0,10,length.out=101)
pps <- pco_sropt(qvs,df1,df2,rvs[1],opy)
if (require(txtplot))
 txtplot(qvs,pps)
pps <- pco_sropt(qvs,df1,df2,rvs[1],opy,lower.tail=FALSE)
if (require(txtplot))
 txtplot(qvs,pps)

# 2FIX: shove these into the unit tests for monotonicity?
svs <- seq(0,4,length.out=101)
pps <- pco_sropt(2,df1,df2,svs,opy)
if (require(txtplot))
 txtplot(svs,pps)
pps <- pco_sropt(2,df1,df2,svs,opy,lower.tail=FALSE)
if (require(txtplot))
 txtplot(svs,pps)

if (require(txtplot))
 txtplot(qvs,pps)
pps <- pco_sropt(qvs,df1,df2,rvs[1],opy,lower.tail=FALSE)
if (require(txtplot))
 txtplot(qvs,pps)
pco_sropt(-1,df1,df2,rvs[1],opy)

qvs <- qco_sropt(0.05,df1=df1,df2=df2,z.s=rvs)
mean(qvs > zeta.s)
qvs <- qco_sropt(0.5,df1=df1,df2=df2,z.s=rvs)
mean(qvs > zeta.s)
qvs <- qco_sropt(0.95,df1=df1,df2=df2,z.s=rvs)
mean(qvs > zeta.s)
# test vectorization:
qv <- qco_sropt(0.1,df1,df2,rvs)
qv <- qco_sropt(c(0.1,0.2),df1,df2,rvs)
qv <- qco_sropt(c(0.1,0.2),c(df1,2*df1),df2,rvs)
qv <- qco_sropt(c(0.1,0.2),c(df1,2*df1),c(df2,2*df2),rvs)
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  \code{\link{dsropt},\link{psropt},\link{qsropt},\link{rsropt}}

  Other sropt: \code{\link{as.sropt}},
  \code{\link{as.sropt.default}},
  \code{\link{confint.sropt}}, \code{\link{dsropt}},
  \code{\link{sropt}}, \code{\link{sropt_confint}},
  \code{\link{sropt.test}}
}
\keyword{distribution}


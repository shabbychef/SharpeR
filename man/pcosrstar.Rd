\name{pcosrstar}
\alias{pcosrstar}
\alias{qcosrstar}
\title{The 'confidence distribution' for maximal Sharpe ratio.}
\usage{
  pcosrstar(q,df1,df2,srstar,opy,lower.tail=TRUE,log.p=FALSE)

  qcosrstar(p,df1,df2,srstar,opy,lower.tail=TRUE,log.p=FALSE,lb=0,ub=Inf)
}
\arguments{
  \item{q}{vector of quantiles.}

  \item{p}{vector of probabilities.}

  \item{srstar}{an observed Sharpe ratio statistic,
  annualized.}

  \item{opy}{the number of observations per 'year'.
  \code{x}, \code{q}, and \code{srstar} are quoted in
  'annualized' units, that is, per 'year', but returns are
  observed possibly at a rate of \code{opy} per 'year.'
  default value is 1, meaning no deannualization is
  performed.}

  \item{log.p}{logical; if TRUE, probabilities p are given
  as \eqn{\mbox{log}(p)}{log(p)}.}

  \item{lb}{the lower bound for the output of
  \code{qcosrstar}.}

  \item{ub}{the upper bound for the output of
  \code{qcosrstar}.}

  \item{df1}{the number of assets in the portfolio.}

  \item{df2}{the number of observations.}

  \item{lower.tail}{logical; if TRUE (default),
  probabilities are \eqn{P[X \le x]}{P[X <= x]}, otherwise,
  \eqn{P[X > x]}{P[X > x]}.}
}
\value{
  \code{pcosrstar} gives the distribution function, and
  \code{qcosrstar} gives the quantile function.

  Invalid arguments will result in return value \code{NaN}
  with a warning.
}
\description{
  Distribution function and quantile function for the
  'confidence distribution' of the maximal Sharpe ratio.
  This is just an inversion to perform inference on snrstar
  given observed statistic srstar.
}
\details{
  Let \eqn{z_*}{z*} follows a \emph{Maximal Sharpe ratio}
  distribution (see \code{\link{SharpeR}}) for known
  degrees of freedom, and unknown non-centrality parameter
  \eqn{\delta}{delta}. The 'confidence distribution' views
  \eqn{\delta}{delta} as a random quantity once
  \eqn{z_*}{z*} is observed. As such, the CDF of the
  confidence distribution is the same as that of the
  Maximal Sharpe ratio (up to a flip of \code{lower.tail});
  while the quantile function is used to compute confidence
  intervals on \eqn{\delta}{delta} given \eqn{z_*}{z*}.
}
\note{
  When \code{lower.tail} is true, \code{pcosrstar} is
  monotonic increasing with respect to \code{q}, and
  decreasing in \code{srstar}; these are reversed when
  \code{lower.tail} is false. Similarly, \code{qcosrstar}
  is increasing in \code{sign(as.double(lower.tail) - 0.5)
  * p} and \code{- sign(as.double(lower.tail) - 0.5) *
  srstar}.
}
\examples{
snrstar <- 2.0
opy <- 253
ntest <- 2000
df1 <- 4
df2 <- 6 * opy
rvs <- rsrstar(ntest,df1=df1,df2=df2,snrstar=snrstar)
qvs <- seq(0,10,length.out=101)
pps <- pcosrstar(qvs,df1,df2,rvs[1],opy)
if (require(txtplot))
 txtplot(qvs,pps)
pps <- pcosrstar(qvs,df1,df2,rvs[1],opy,lower.tail=FALSE)
if (require(txtplot))
 txtplot(qvs,pps)

# 2FIX: shove these into the unit tests for monotonicity?
svs <- seq(0,4,length.out=101)
pps <- pcosrstar(2,df1,df2,svs,opy)
if (require(txtplot))
 txtplot(svs,pps)
pps <- pcosrstar(2,df1,df2,svs,opy,lower.tail=FALSE)
if (require(txtplot))
 txtplot(svs,pps)

if (require(txtplot))
 txtplot(qvs,pps)
pps <- pcosrstar(qvs,df1,df2,rvs[1],opy,lower.tail=FALSE)
if (require(txtplot))
 txtplot(qvs,pps)
pcosrstar(-1,df1,df2,rvs[1],opy)

qvs <- qcosrstar(0.05,df1=df1,df2=df2,srstar=rvs)
mean(qvs > snrstar)
qvs <- qcosrstar(0.5,df1=df1,df2=df2,srstar=rvs)
mean(qvs > snrstar)
qvs <- qcosrstar(0.95,df1=df1,df2=df2,srstar=rvs)
mean(qvs > snrstar)
# test vectorization:
qv <- qcosrstar(0.1,df1,df2,rvs)
qv <- qcosrstar(c(0.1,0.2),df1,df2,rvs)
qv <- qcosrstar(c(0.1,0.2),c(df1,2*df1),df2,rvs)
qv <- qcosrstar(c(0.1,0.2),c(df1,2*df1),c(df2,2*df2),rvs)
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  \code{\link{dsrstar},\link{psrstar},\link{qsrstar},\link{rsrstar}}

  Other srstar: \code{\link{dsrstar}},
  \code{\link{full.srstar}}, \code{\link{srstar.confint}},
  \code{\link{srstar.test}}
}
\keyword{distribution}


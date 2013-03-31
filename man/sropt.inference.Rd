\name{sropt.inference}
\alias{F.inference}
\alias{sropt.inference}
\alias{T2.inference}
\title{Inference on noncentrality parameter of F or F-like statistic}
\usage{
  F.inference(Fs, df1, df2, type=c("KRS","MLE","unbiased"))

  T2.inference(T2,df1,df2,...)

  sropt.inference(z.s,df1,df2,opy=1,drag=0,...)
}
\arguments{
  \item{Fs}{a (non-central) F statistic.}

  \item{T2}{a (non-central) Hotelling \eqn{T^2} statistic.}

  \item{z.s}{an observed Sharpe ratio statistic,
  annualized.}

  \item{type}{the estimator type. one of \code{c("KRS",
  "MLE", "unbiased")}}

  \item{opy}{the number of observations per 'year'.
  \code{z.s} is assumed given in 'annualized' units, that
  is, per 'year', but returns are observed possibly at a
  rate of \code{opy} per 'year.' default value is 1,
  meaning no deannualization is performed.}

  \item{drag}{the 'drag' term, \eqn{c_0/R}{c0/R}. defaults
  to 0. It is assumed that \code{drag} has been annualized,
  \emph{i.e.} is given in the same units as \code{z.s}.}

  \item{...}{the \code{type} which is passed on to
  \code{F.inference}.}

  \item{df1}{the number of assets in the portfolio.}

  \item{df2}{the number of observations.}
}
\value{
  an estimate of the non-centrality parameter.
}
\description{
  Estimates the non-centrality parameter associated with an
  observed statistic following a (non-central) F,
  \eqn{T^2}, or sropt distribution.
}
\details{
  Let \eqn{F}{F} be an observed statistic distributed as a
  non-central F with \eqn{\nu_1}{df1}, \eqn{\nu_2}{df2}
  degrees of freedom and non-centrality parameter
  \eqn{\delta^2}{delta^2}. Three methods are presented to
  estimate the non-centrality parameter from the statistic:

  \itemize{ \item an unbiased estimator, which,
  unfortunately, may be negative. \item the Maximum
  Likelihood Estimator, which may be zero, but not
  negative. \item the estimator of Kubokawa, Roberts, and
  Shaleh (KRS), which is a shrinkage estimator. }

  Since a Hotelling distribution is equivalent to the
  F-distribution up to scaling, the same estimators can be
  used to estimate the non-centrality parameter of a
  non-central Hotelling T-squared statistic.

  The sropt distribution is equivalent to a Hotelling up to
  a square root and some rescalings.

  The non-centrality parameter of the sropt distribution is
  the square root of that of the Hotelling, \emph{i.e.} has
  units 'per square root time'. As such, the
  \code{'unbiased'} type can be problematic!
}
\examples{
rvs <- rf(1024, 4, 1000, 5)
unbs <- F.inference(rvs, 4, 1000, type="unbiased")
# generate some sropts
true.snrstar <- 1.25
df1 <- 6
df2 <- 2000
opy <- 253
rvs <- rsropt(500, df1, df2, true.snrstar, opy)
est1 <- sropt.inference(rvs,df1,df2,opy,type='unbiased')
est2 <- sropt.inference(rvs,df1,df2,opy,type='KRS')
est3 <- sropt.inference(rvs,df1,df2,opy,type='MLE')
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\references{
  Kubokawa, T., C. P. Robert, and A. K. Saleh. "Estimation
  of noncentrality parameters." Canadian Journal of
  Statistics 21, no. 1 (1993): 45-57.
  \url{http://www.jstor.org/stable/3315657}

  Spruill, M. C. "Computation of the maximum likelihood
  estimate of a noncentrality parameter." Journal of
  multivariate analysis 18, no. 2 (1986): 216-224.
  \url{http://www.sciencedirect.com/science/article/pii/0047259X86900709}
}
\seealso{
  F-distribution functions, \code{\link{df}}
}
\keyword{htest}


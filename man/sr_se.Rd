\name{sr_se}
\alias{sr_se}
\title{Standard error of Sharpe ratio}
\usage{
  sr_se(z,df,opy,type=c("t","Lo","Z","F"))
}
\arguments{
  \item{z}{an observed Sharpe ratio statistic, annualized.}

  \item{df}{the number of observations the statistic is
  based on. This is one more than the number of degrees of
  freedom in the corresponding t-statistic, although the
  effect will be small when \code{df} is large.}

  \item{opy}{the number of observations per 'year'.
  \code{z}, and \code{zeta} are quoted in 'annualized'
  units, that is, per square root 'year', but returns are
  observed possibly at a rate of \code{opy} per 'year.'
  default value is 1, meaning no deannualization is
  performed.}

  \item{type}{the estimator type. one of \code{"t", "Lo",
  "Z", "F"}}
}
\value{
  an estimate of standard error.
}
\description{
  Estimates the standard error of the Sharpe ratio
  statistic.
}
\details{
  2FIX; document There are three methods:

  \itemize{ \item The default, \code{t}, based on Johnson &
  Welch, with a correction for small sample size. \item An
  asymptotically equivalent method, \code{Lo}, based on Lo,
  which is Johnson & Welch's method but without correcting
  for d.f. \item An approximation based on normality,
  \code{Z}. \item An approximation based on an F statistic,
  \code{F}. }
}
\note{
  Eventually this should include corrections for
  autocorrelation, skew, kurtosis.
}
\examples{
opy <- 253
df <- opy * 6
rvs <- rsr(1, df, 1.0, opy)
anse <- sr_se(rvs,df,opy,type="t")
anse2 <- sr_se(rvs,df,opy,type="Z")
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\references{
  Walck, C. "Hand-book on STATISTICAL DISTRIBUTIONS for
  experimentalists." 1996.
  \url{http://www.stat.rice.edu/~dobelman/textfiles/DistributionsHandbook.pdf}

  Johnson, N. L., and Welch, B. L. "Applications of the
  non-central t-distribution." Biometrika 31, no. 3-4
  (1940): 362-389.
  \url{http://dx.doi.org/10.1093/biomet/31.3-4.362}

  Lo, Andrew W. "The statistics of Sharpe ratios."
  Financial Analysts Journal 58, no. 4 (2002): 36-52.
  \url{http://ssrn.com/paper=377260}

  Opdyke, J. D. "Comparing Sharpe Ratios: So Where are the
  p-values?" Journal of Asset Management 8, no. 5 (2006):
  308-336. \url{http://ssrn.com/paper=886728}
}
\seealso{
  sr-distribution functions, \code{\link{dsr}}

  Other sr: \code{\link{confint.sr}}, \code{\link{dsr}},
  \code{\link{plambdap}}, \code{\link{power.sr.test}},
  \code{\link{sr}}, \code{\link{sr_confint}},
  \code{\link{sr.default}}, \code{\link{sr.equality.test}},
  \code{\link{sr.test}}
}
\keyword{htest}


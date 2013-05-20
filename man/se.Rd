\name{se}
\alias{se}
\alias{se.sr}
\title{Standard error computation}
\usage{
  se(z, type)

  \method{se}{sr} (z, type = c("t", "Lo"))
}
\arguments{
  \item{z}{an observed Sharpe ratio statistic, of class
  \code{sr}.}

  \item{type}{estimator type. one of \code{"t", "Lo",
  "exact"}}

  \item{...}{further arguments to be passed to or from
  methods.}
}
\value{
  an estimate of standard error.
}
\description{
  Estimates the standard error of the Sharpe ratio
  statistic.
}
\details{
  For an observed Sharpe ratio, estimate the standard
  error. There are two methods:

  \itemize{ \item The default, \code{t}, based on Johnson &
  Welch, with a correction for small sample size, also
  known as \code{Lo}. \item A method based on the exact
  variance of the non-central t-distribution, \code{exact}.
  } There should be very little difference between these
  except for very small sample sizes.
}
\note{
  Eventually this should include corrections for
  autocorrelation, skew, kurtosis.
}
\examples{
asr <- as.sr(rnorm(128,0.2))
anse <- se(asr,type="t")
anse <- se(asr,type="Lo")
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\references{
  Sharpe, William F. "Mutual fund performance." Journal of
  business (1966): 119-138.
  \url{http://ideas.repec.org/a/ucp/jnlbus/v39y1965p119.html}

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

  Walck, C. "Hand-book on STATISTICAL DISTRIBUTIONS for
  experimentalists." 1996.
  \url{http://www.stat.rice.edu/~dobelman/textfiles/DistributionsHandbook.pdf}
}
\seealso{
  sr-distribution functions, \code{\link{dsr}}

  Other sr: \code{\link{as.sr}},
  \code{\link{as.sr.data.frame}},
  \code{\link{as.sr.default}}, \code{\link{as.sr.lm}},
  \code{\link{as.sr.xts}}, \code{\link{confint.sr}},
  \code{\link{confint.sropt}}, \code{\link{dsr}},
  \code{\link{is.sr}}, \code{\link{power.sr_test}},
  \code{\link{print.sr}}, \code{\link{print.sropt}},
  \code{\link{reannualize}}, \code{\link{reannualize.sr}},
  \code{\link{reannualize.sropt}}, \code{\link{sr}},
  \code{\link{sr_equality_test}}, \code{\link{sr_test}}
}
\keyword{htest}


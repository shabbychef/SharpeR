\name{sr.equality.test}
\alias{sr.equality.test}
\title{Paired test for equality of Sharpe ratio}
\usage{
  sr.equality.test(X,contrasts=NULL,type=c("chisq","F","t"),
    alternative=c("two.sided","less","greater"))
}
\arguments{
  \item{X}{an \eqn{n \times p}{n x p} matrix of paired
  observations.}

  \item{contrasts}{an \eqn{k \times p}{k x p} matrix of the
  contrasts}

  \item{type}{which approximation to use. \code{"chisq"} is
  preferred when the returns are non-normal, but the
  approximation is asymptotic.  the \code{"t"} test is only
  supported when \eqn{k = 1}{k = 1}.}

  \item{alternative}{a character string specifying the
  alternative hypothesis, must be one of \code{"two.sided"}
  (default), \code{"greater"} or \code{"less"}. You can
  specify just the initial letter.  This is only relevant
  for the \code{"t"} test.  \code{"greater"} corresponds to
  \eqn{H_a: E s > 0}{Ha: E s > 0}.}
}
\value{
  Object of class \code{htest}, a list of the test
  statistic, the size of \code{X}, and the \code{method}
  noted.
}
\description{
  Performs a hypothesis test of equality of Sharpe ratios
  of p assets given paired observations.
}
\details{
  Given \eqn{n} \emph{i.i.d.} observations of the excess
  returns of \eqn{p} strategies, we test \deqn{H_0:
  \frac{\mu_i}{\sigma_i} = \frac{\mu_j}{\sigma_j}, 1 \le i
  < j \le p}{H0: sr1 = sr2 = ...} using the method of
  Wright, et. al.

  More generally, a matrix of constrasts, \eqn{E}{E} can be
  given, and we can test \deqn{H_0: E s = 0,}{H0: E s = 0,}
  where \eqn{s}{s} is the vector of Sharpe ratios of the
  \eqn{p} strategies.

  When \eqn{E}{E} consists of a single row (a single
  contrast), as is the case when the default contrasts are
  used and only two strategies are compared, then an
  approximate t-test can be performed against the
  alternative hypothesis \eqn{H_a: E s > 0}{Ha: E s > 0}

  Both chi-squared and F- approximations are supported; the
  former is described by Wright. \emph{et. al.}, the latter
  by Leung and Wong.
}
\examples{
rv <- sr.equality.test(matrix(rnorm(500*5),500,5))
# test for uniformity
pvs <- replicate(500,{ x <- sr.equality.test(matrix(rnorm(400*5),400,5),type="chisq")
                       x$p.value })
plot(ecdf(pvs))
abline(0,1,col='red')
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\references{
  Wright, J. A., Yam, S. C. P., and Yung, S. P. "A note on
  the test for the equality of multiple Sharpe ratios and
  its application on the evaluation of iShares." J. Risk.
  to appear.
  \url{http://www.sta.cuhk.edu.hk/scpy/Preprints/John\%20Wright/A\%20test\%20for\%20the\%20equality\%20of\%20multiple\%20Sharpe\%20ratios.pdf}

  Leung, P.-L., and Wong, W.-K. "On testing the equality of
  multiple Sharpe ratios, with application on the
  evaluation of iShares." J. Risk 10, no. 3 (2008): 15--30.
  \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=907270}

  Memmel, C. "Performance hypothesis testing with the
  Sharpe ratio." Finance Letters 1 (2003): 21--23.
}
\seealso{
  \code{\link{sr.test}}

  Other sr: \code{\link{dsr}}, \code{\link{full.sr}},
  \code{\link{plambdap}}, \code{\link{power.sr.test}},
  \code{\link{sr.confint}}, \code{\link{sr.se}},
  \code{\link{sr.test}}
}
\keyword{htest}


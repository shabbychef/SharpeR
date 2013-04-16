\name{sr_test}
\alias{sr_test}
\title{test for Sharpe ratio}
\usage{
  sr_test(x,y=NULL,alternative=c("two.sided","less","greater"),
    snr=0,opy=1,paired=FALSE,conf.level=0.95)
}
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.}

  \item{y}{an optional (non-empty) numeric vector of data
  values.}

  \item{alternative}{a character string specifying the
  alternative hypothesis, must be one of \code{"two.sided"}
  (default), \code{"greater"} or \code{"less"}.  You can
  specify just the initial letter.}

  \item{snr}{a number indicating the null hypothesis offset
  value, the \eqn{S} value.}

  \item{opy}{the number of observations per 'year'.
  \code{snr} is quoted in 'annualized' units, that is, per
  square root 'year', but returns are observed possibly at
  a rate of \code{opy} per 'year.' default value is 1,
  meaning no deannualization is performed.}

  \item{paired}{a logical indicating whether you want a
  paired test.}

  \item{conf.level}{confidence level of the interval. (not
  used yet)}
}
\value{
  A list with class \code{"htest"} containing the following
  components: \item{statistic}{the value of the t- or
  Z-statistic.} \item{parameter}{the degrees of freedom for
  the statistic.} \item{p.value}{the p-value for the test.}
  \item{conf.int}{a confidence interval appropriate to the
  specified alternative hypothesis.} \item{estimate}{the
  estimated Sharpe or difference in Sharpes depending on
  whether it was a one-sample test or a two-sample test.
  Annualized} \item{null.value}{the specified hypothesized
  value of the Sharpe or difference of Sharpes depending on
  whether it was a one-sample test or a two-sample test.}
  \item{alternative}{a character string describing the
  alternative hypothesis.} \item{method}{a character string
  indicating what type of test was performed.}
  \item{data.name}{a character string giving the name(s) of
  the data.}
}
\description{
  Performs one and two sample tests of Sharpe ratio on
  vectors of data.
}
\details{
  Given \eqn{n}{n} observations \eqn{x_i}{xi} from a normal
  random variable, with mean \eqn{\mu}{mu} and standard
  deviation \eqn{\sigma}{sigma}, tests \deqn{H_0:
  \frac{\mu}{\sigma} = S}{H0: mu/sigma = S} against two or
  one sided alternatives.

  Can also perform two sample tests of Sharpe ratio. For
  paired observations \eqn{x_i}{xi} and \eqn{y_i}{yi},
  tests \deqn{H_0: \frac{\mu_x}{\sigma_x} =
  \frac{\mu_u}{\sigma_y}}{H0: mu_x sigma_y = mu_y sigma_x}
  against two or one sided alternative, via
  \code{\link{sr_equality_test}}.

  For unpaired (and independent) observations, tests
  \deqn{H_0: \frac{\mu_x}{\sigma_x} -
  \frac{\mu_u}{\sigma_y} = S}{H0: mu_x / sigma_x - mu_y /
  sigma_y = S} against two or one-sided alternatives via a
  normal approximation (which is probably not very good for
  small sample sizes). At some point, the procedure of
  Ghosh (1975) or variant thereof should be employed.
}
\examples{
# should reject null
x <- sr_test(rnorm(1000,mean=0.5,sd=0.1),snr=2,opy=1,alternative="greater")
x <- sr_test(rnorm(1000,mean=0.5,sd=0.1),snr=2,opy=1,alternative="two.sided")
# should not reject null
x <- sr_test(rnorm(1000,mean=0.5,sd=0.1),snr=2,opy=1,alternative="less")

# test for uniformity
pvs <- replicate(1000,{ x <- sr_test(rnorm(1000),opy=253,alternative="two.sided")
                        x$p.value })
plot(ecdf(pvs))
abline(0,1,col='red')
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\references{
  Ghosh, B. K. "On the Distribution of the Difference of
  Two t-Variables." Journal of the American Statistical
  Association 70, no 350 (1975): 463--467.
  \url{http://www.jstor.org/stable/2285841}
}
\seealso{
  \code{\link{sr_equality_test}}, \code{\link{t.test}}.

  Other sr: \code{\link{confint.sr}}, \code{\link{dsr}},
  \code{\link{plambdap}}, \code{\link{power.sr.test}},
  \code{\link{se}}, \code{\link{se.default}},
  \code{\link{se.sr}}, \code{\link{sr}},
  \code{\link{sr.default}}, \code{\link{sr_equality_test}},
  \code{\link{sr.lm}}, \code{\link{sr.xts}}
}
\keyword{htest}


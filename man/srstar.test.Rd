\name{srstar.test}
\alias{srstar.test}
\title{test for maximal Sharpe ratio}
\usage{
  srstar.test(X,alternative=c("greater","two.sided","less"),
    snrstar=0,opy=1,conf.level=0.95)
}
\arguments{
  \item{X}{a (non-empty) numeric matrix of data values,
  each row independent,}

  \item{alternative}{a character string specifying the
  alternative hypothesis, must be one of
  \code{"two.sided"}, \code{"greater"} (default) or
  \code{"less"}.  You can specify just the initial letter.}

  \item{snrstar}{a number indicating the null hypothesis
  value.}

  \item{opy}{the number of observations per 'year'.
  \code{snrstar} is quoted in 'annualized' units, that is,
  per square root 'year', but returns are observed possibly
  at a rate of \code{opy} per 'year.' default value is 1,
  meaning no deannualization is performed.  This is only
  used if \code{snrstar} is nonzero.}

  \item{conf.level}{confidence level of the interval. (not
  used yet)}
}
\value{
  A list with class \code{"htest"} containing the following
  components: \item{statistic}{the value of the
  \eqn{T^2}-statistic.} \item{parameter}{a list of the
  degrees of freedom for the statistic.} \item{p.value}{the
  p-value for the test.} \item{conf.int}{a confidence
  interval appropriate to the specified alternative
  hypothesis. NYI.} \item{estimate}{the estimated maximal
  Sharpe, annualized} \item{null.value}{the specified
  hypothesized value of the maximal Sharpe.}
  \item{alternative}{a character string describing the
  alternative hypothesis.} \item{method}{a character string
  indicating what type of test was performed.}
  \item{data.name}{a character string giving the name(s) of
  the data.}
}
\description{
  Performs one sample tests of Sharpe ratio of the
  Markowitz portfolio.
}
\details{
  Suppose \eqn{x_i}{xi} are \eqn{n}{n} independent draws of
  a \eqn{q}{q}-variate normal random variable with mean
  \eqn{\mu}{mu} and covariance matrix \eqn{\Sigma}{Sigma}.
  This code tests the hypothesis \deqn{H_0:
  \mu^{\top}\Sigma^{-1}\mu = \delta_0^2}{H0: mu' Sigma^-1
  mu = delta_0^2}

  The default alternative hypothesis is the one-sided
  \deqn{H_0: \mu^{\top}\Sigma^{-1}\mu > \delta_0^2}{H0: mu'
  Sigma^-1 mu > delta_0^2} but this can be set otherwise.

  Note there is no 'drag' term here since this represents a
  linear offset of the population parameter.
}
\examples{
# test for uniformity
pvs <- replicate(1000,{ x <- srstar.test(matrix(rnorm(1000*4),ncol=4),alternative="two.sided")
                        x$p.value })
plot(ecdf(pvs))
abline(0,1,col='red')
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  \code{\link{sr.test}}, \code{\link{t.test}}.

  Other srstar: \code{\link{dsrstar}},
  \code{\link{full.srstar}}, \code{\link{pcosrstar}},
  \code{\link{srstar.confint}}
}
\keyword{htest}


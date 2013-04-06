\name{power.sr.test}
\alias{power.sr.test}
\title{Power calculations for Sharpe ratio tests}
\usage{
  power.sr.test(n=NULL,snr=NULL,sig.level=0.05,power=NULL,
    alternative=c("one.sided","two.sided"),opy=NULL)
}
\arguments{
  \item{n}{Number of observations}

  \item{snr}{the 'signal-to-noise' parameter, defined as
  the population mean divided by the population standard
  deviation, 'annualized'.}

  \item{sig.level}{Significance level (Type I error
  probability).}

  \item{power}{Power of test (1 minus Type II error
  probability).}

  \item{alternative}{One- or two-sided test.}

  \item{opy}{the number of observations per 'year'.
  \code{x}, \code{q}, and \code{snr} are quoted in
  'annualized' units, that is, per square root 'year', but
  returns are observed possibly at a rate of \code{opy} per
  'year.' default value is 1, meaning no deannualization is
  performed.}
}
\value{
  Object of class \code{power.htest}, a list of the
  arguments (including the computed one) augmented with
  \code{method}, \code{note} and \code{n.yr} elements, the
  latter is the number of years under the given
  annualization (\code{opy}), \code{NA} if none given.
}
\description{
  Compute power of test, or determine parameters to obtain
  target power.
}
\details{
  Suppose you perform a single-sample test for significance
  of the Sharpe ratio based on the corresponding
  single-sample t-test. Given any three of: the effect size
  (the population SNR), the number of observations, and the
  type I and type II rates, this computes the fourth.

  This is a thin wrapper on \code{\link{power.t.test}}.

  Exactly one of the parameters \code{n}, \code{snr},
  \code{power}, and \code{sig.level} must be passed as
  NULL, and that parameter is determined from the others.
  Notice that \code{sig.level} has non-NULL default, so
  NULL must be explicitly passed if you want to compute it.

  2FIX: change snr to zeta?
}
\examples{
anex <- power.sr.test(253,1,0.05,NULL,opy=253)
anex <- power.sr.test(n=253,snr=NULL,sig.level=0.05,power=0.5,opy=253)
anex <- power.sr.test(n=NULL,snr=0.6,sig.level=0.05,power=0.5,opy=253)
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  \code{\link{power.t.test}}, \code{\link{sr.test}}

  Other sr: \code{\link{confint.sr}}, \code{\link{dsr}},
  \code{\link{plambdap}}, \code{\link{sr}},
  \code{\link{sr_confint}}, \code{\link{sr.default}},
  \code{\link{sr.equality.test}}, \code{\link{sr_se}},
  \code{\link{sr.test}}
}
\keyword{htest}


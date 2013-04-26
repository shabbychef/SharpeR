\name{power.sr.test}
\alias{power.sr.test}
\title{Power calculations for Sharpe ratio tests}
\usage{
  power.sr.test(n=NULL,zeta=NULL,sig.level=0.05,power=NULL,
    alternative=c("one.sided","two.sided"),opy=NULL)
}
\arguments{
  \item{n}{Number of observations}

  \item{zeta}{the 'signal-to-noise' parameter, defined as
  the population mean divided by the population standard
  deviation, 'annualized'.}

  \item{sig.level}{Significance level (Type I error
  probability).}

  \item{power}{Power of test (1 minus Type II error
  probability).}

  \item{alternative}{One- or two-sided test.}

  \item{opy}{the number of observations per 'year'.
  \code{x}, \code{q}, and \code{zeta} are quoted in
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
  (the population SNR, \eqn{\zeta}{zeta}), the number of
  observations, and the type I and type II rates, this
  function computes the fourth.

  This is a thin wrapper on \code{\link{power.t.test}}.

  Exactly one of the parameters \code{n}, \code{zeta},
  \code{power}, and \code{sig.level} must be passed as
  NULL, and that parameter is determined from the others.
  Notice that \code{sig.level} has non-NULL default, so
  NULL must be explicitly passed if you want to compute it.
}
\examples{
anex <- power.sr.test(253,1,0.05,NULL,opy=253)
anex <- power.sr.test(n=253,zeta=NULL,sig.level=0.05,power=0.5,opy=253)
anex <- power.sr.test(n=NULL,zeta=0.6,sig.level=0.05,power=0.5,opy=253)
# Lehr's Rule
zetas <- seq(0.1,2.5,length.out=201)
ssizes <- sapply(zetas,function(zed) {
  x <- power.sr.test(n=NULL,zeta=zed,sig.level=0.05,power=0.8,opy=253)
  x$n / 253})
plot(8 / (zetas*zetas),ssizes)
# e = n z^2 mnemonic approximate rule for 0.05 type I, 50\% power
ssizes <- sapply(zetas,function(zed) {
  x <- power.sr.test(n=NULL,zeta=zed,sig.level=0.05,power=0.5,opy=253)
  x$n / 253 })
print(summary(ssizes * zetas * zetas - exp(1)))
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\references{
  Sharpe, William F. "Mutual fund performance." Journal of
  business (1966): 119-138.
  \url{http://ideas.repec.org/a/ucp/jnlbus/v39y1965p119.html}

  Lehr, R. "Sixteen S-squared over D-squared: A relation
  for crude sample size estimates." Statist. Med., 11, no 8
  (1992): 1099–1102. doi: 10.1002/sim.4780110811

  Johnson, N. L., and Welch, B. L. "Applications of the
  non-central t-distribution." Biometrika 31, no. 3-4
  (1940): 362-389.
  \url{http://dx.doi.org/10.1093/biomet/31.3-4.362}
}
\seealso{
  \code{\link{power.t.test}}, \code{\link{sr_test}}

  Other sr: \code{\link{as.sr}},
  \code{\link{as.sr.default}}, \code{\link{as.sr.lm}},
  \code{\link{as.sr.xts}}, \code{\link{confint.sr}},
  \code{\link{dsr}}, \code{\link{is.sr}},
  \code{\link{plambdap}}, \code{\link{reannualize}},
  \code{\link{se}}, \code{\link{se.default}},
  \code{\link{se.sr}}, \code{\link{sr}},
  \code{\link{sr_equality_test}}, \code{\link{sr_test}},
  \code{\link{sr_test.default}}, \code{\link{sr_test.sr}}
}
\keyword{htest}


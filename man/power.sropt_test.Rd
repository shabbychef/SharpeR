\name{power.sropt_test}
\alias{power.sropt_test}
\title{Power calculations for optimal Sharpe ratio tests}
\usage{
  power.sropt_test(df1=NULL,df2=NULL,zeta.s=NULL,
    sig.level=0.05,power=NULL,ope=1)
}
\arguments{
  \item{zeta.s}{the 'signal-to-noise' parameter, defined as
  ...}

  \item{sig.level}{Significance level (Type I error
  probability).}

  \item{power}{Power of test (1 minus Type II error
  probability).}

  \item{ope}{the number of observations per 'epoch'. For
  convenience of interpretation, The Sharpe ratio is
  typically quoted in 'annualized' units for some epoch,
  that is, 'per square root epoch', though returns are
  observed at a frequency of \code{ope} per epoch.  The
  default value is 1, meaning the code will not attempt to
  guess, what the observation frequency is, and no
  annualization adjustments will be made.}

  \item{df1}{the number of assets in the portfolio.}

  \item{df2}{the number of observations.}
}
\value{
  Object of class \code{power.htest}, a list of the
  arguments (including the computed one) augmented with
  \code{method}, \code{note} and \code{n.epoch} elements,
  the latter is the number of epochs under the given
  annualization (\code{ope}), \code{NA} if none given.
}
\description{
  Compute power of test, or determine parameters to obtain
  target power.
}
\details{
  Suppose you perform a single-sample test for significance
  of the optimal Sharpe ratio based on the corresponding
  single-sample T^2-test. Given any four of: the effect
  size (the population optimal SNR, \eqn{\zeta_*}{zeta*}),
  the number of assets, the number of observations, and the
  type I and type II rates, this function computes the
  fifth.

  Exactly one of the parameters \code{df1}, \code{df2},
  \code{zeta.s}, \code{power}, and \code{sig.level} must be
  passed as NULL, and that parameter is determined from the
  others.  Notice that \code{sig.level} has non-NULL
  default, so NULL must be explicitly passed if you want to
  compute it.
}
\examples{
anex <- power.sropt_test(8,4*253,1,0.05,NULL,ope=253)
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  \code{\link{reannualize}}

  \code{\link{power.t.test}}, \code{\link{sropt_test}}

  Other sropt: \code{\link{as.sropt}},
  \code{\link{as.sropt.default}},
  \code{\link{as.sropt.xts}},
  \code{\link{confint.del_sropt}},
  \code{\link{confint.sr}}, \code{\link{confint.sropt}},
  \code{\link{dsropt}}, \code{\link{is.sropt}},
  \code{\link{reannualize}}, \code{\link{reannualize.sr}},
  \code{\link{reannualize.sropt}}, \code{\link{sropt}},
  \code{\link{sropt_test}}
}
\keyword{htest}


\name{reannualize}
\alias{reannualize}
\title{Change the annualization of a Sharpe ratio.}
\usage{
  reannualize(x,ope,epoch)
}
\arguments{
  \item{x}{an object of class \code{sr}.}

  \item{ope}{the new observations per epoch. If none given,
  it is not updated.}

  \item{epoch}{a string representation of the epoch. If
  none given, it is not updated.}
}
\value{
  an object of class \code{sr} with the annualization or
  epoch updated.
}
\description{
  Changes the annualization factor of a Sharpe ratio
  statistic.
}
\examples{
# compute a 'daily' Sharpe
mysr <- as.sr(rnorm(253*8),ope=1,epoch="day")
# turn into annual
mysr2 <- reannualize(mysr,ope=253,epoch="yr")
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  sr

  Other sr: \code{\link{as.sr}},
  \code{\link{as.sr.default}}, \code{\link{as.sr.lm}},
  \code{\link{as.sr.xts}}, \code{\link{confint.sr}},
  \code{\link{confint.sropt}}, \code{\link{dsr}},
  \code{\link{is.sr}}, \code{\link{power.sr_test}},
  \code{\link{se}}, \code{\link{se.sr}}, \code{\link{sr}},
  \code{\link{sr_equality_test}}, \code{\link{sr_test}},
  \code{\link{sr_test.default}}, \code{\link{sr_test.sr}}
}


\name{reannualize}
\alias{reannualize}
\title{Change the annualization of a Sharpe ratio.}
\usage{
  reannualize(x,opy,epoch)
}
\arguments{
  \item{x}{an object of class \code{sr}.}

  \item{opy}{the new observations per year (or epoch). If
  none given, it is not updated.}

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
mysr <- sr(rnorm(253*8),opy=1)
# turn into annual
mysr2 <- reannualize(mysr,opy=253,epoch="yr")
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  sr

  Other sr: \code{\link{confint.sr}}, \code{\link{dsr}},
  \code{\link{is.sr}}, \code{\link{plambdap}},
  \code{\link{power.sr.test}}, \code{\link{se}},
  \code{\link{se.default}}, \code{\link{se.sr}},
  \code{\link{sr}}, \code{\link{sr.default}},
  \code{\link{sr_equality_test}}, \code{\link{sr.lm}},
  \code{\link{sr_test}}, \code{\link{sr_test.default}},
  \code{\link{sr_test.sr}}, \code{\link{sr.xts}}
}


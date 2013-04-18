\name{is.sr}
\alias{is.sr}
\title{Is this in the "sr" class?}
\usage{
  is.sr(x)
}
\arguments{
  \item{x}{an object of some kind.}
}
\value{
  a boolean.
}
\description{
  Checks if an object is in the class \code{'sr'}
}
\details{
  To satisfy the minimum requirements of an S3 class.
}
\examples{
rvs <- sr(rnorm(253*8),opy=253)
is.sr(rvs)
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  sr

  Other sr: \code{\link{confint.sr}}, \code{\link{dsr}},
  \code{\link{plambdap}}, \code{\link{power.sr.test}},
  \code{\link{reannualize}}, \code{\link{se}},
  \code{\link{se.default}}, \code{\link{se.sr}},
  \code{\link{sr}}, \code{\link{sr.default}},
  \code{\link{sr_equality_test}}, \code{\link{sr.lm}},
  \code{\link{sr_test}}, \code{\link{sr_test.default}},
  \code{\link{sr_test.sr}}, \code{\link{sr.xts}}
}


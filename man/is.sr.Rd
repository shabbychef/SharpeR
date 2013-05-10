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
rvs <- as.sr(rnorm(253*8),ope=253)
is.sr(rvs)
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  sr

  Other sr: \code{\link{as.sr}},
  \code{\link{as.sr.data.frame}},
  \code{\link{as.sr.default}}, \code{\link{as.sr.lm}},
  \code{\link{as.sr.xts}}, \code{\link{confint.sr}},
  \code{\link{confint.sropt}}, \code{\link{dsr}},
  \code{\link{power.sr_test}}, \code{\link{print.sr}},
  \code{\link{print.sropt}}, \code{\link{reannualize}},
  \code{\link{reannualize.sr}},
  \code{\link{reannualize.sropt}}, \code{\link{se}},
  \code{\link{se.sr}}, \code{\link{sr}},
  \code{\link{sr_equality_test}}, \code{\link{sr_test}}
}


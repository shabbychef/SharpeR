\name{is.sropt}
\alias{is.sropt}
\title{Is this in the "sropt" class?}
\usage{
  is.sropt(x)
}
\arguments{
  \item{x}{an object of some kind.}
}
\value{
  a boolean.
}
\description{
  Checks if an object is in the class \code{'sropt'}
}
\details{
  To satisfy the minimum requirements of an S3 class.
}
\examples{
nfac <- 5
nyr <- 10
ope <- 253
# simulations with no covariance structure.
# under the null:
set.seed(as.integer(charToRaw("be determinstic")))
Returns <- matrix(rnorm(ope*nyr*nfac,mean=0,sd=0.0125),ncol=nfac)
asro <- as.sropt(Returns,drag=0,ope=ope)
is.sropt(asro)
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  sropt

  Other sropt: \code{\link{as.sropt}},
  \code{\link{as.sropt.default}},
  \code{\link{as.sropt.xts}},
  \code{\link{confint.del_sropt}},
  \code{\link{confint.sr}}, \code{\link{confint.sropt}},
  \code{\link{dsropt}}, \code{\link{pco_sropt}},
  \code{\link{power.sropt_test}},
  \code{\link{reannualize}}, \code{\link{reannualize.sr}},
  \code{\link{reannualize.sropt}}, \code{\link{sropt}},
  \code{\link{sropt_test}}
}


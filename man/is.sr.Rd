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
}


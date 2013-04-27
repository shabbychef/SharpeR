\name{confint}
\alias{confint}
\title{Confidence Intervals for Model Parameters}
\usage{
  confint(object,level=0.95,level.lo=(1-level)/2,level.hi=1-level.lo,...)
}
\arguments{
  \item{object}{an object of class \code{sr} or
  \code{sropt}.}

  \item{level}{the confidence level required.}

  \item{level.lo}{the lower confidence level required.}

  \item{level.hi}{the upper confidence level required.}

  \item{...}{additional argument(s) for methods.}
}
\value{
  A matrix (or vector) with columns giving lower and upper
  confidence limits for the parameter. These will be
  labelled as level.lo and level.hi in \%, \emph{e.g.}
  \code{"2.5 \%"}
}
\description{
  Computes confidence intervals for the (optimal) signal to
  noise ratio in a fitted model. Works on objects of class
  \code{sr} and \code{sropt}.
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  \code{\link{confint}}, \code{\link{se}}
}
\keyword{htest}


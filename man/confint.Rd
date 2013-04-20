\name{confint.sr}
\alias{confint.sr}
\title{Confidence Interval on Signal-Noise Ratio}
\usage{
  confint(object,parm,level=0.95,...)
}
\arguments{
  \item{object}{an observed Sharpe ratio statistic, of
  class \code{sr}.}

  \item{parm}{ignored here}

  \item{level}{the confidence level required.}

  \item{...}{the following parameters are relevant:
  \itemize{ \item \code{type} is oe of
  \code{c("exact","t","Z","F")} \item \code{level.lo}, and
  \code{level.hi} allow one to compute non-symmetric CI. }}
}
\value{
  A matrix (or vector) with columns giving lower and upper
  confidence limits for the SNR. These will be labelled as
  level.lo and level.hi in \%, \emph{e.g.} \code{"2.5 \%"}
}
\description{
  Computes approximate confidence intervals on the
  Signal-Noise ratio given the Sharpe ratio.
}
\details{
  Constructs confidence intervals on the Signal-Noise ratio
  given observed Sharpe ratio statistic. The available
  methods are:

  \itemize{ \item The default, \code{exact}, which is only
  exact when returns are normal, based on inverting the
  non-central t distribution. \item A method based on the
  standard error of a non-central t distribution. \item A
  method based on a normal approximation. \item A method
  based on an F statistic. }
}
\examples{
# using "sr" class:
opy <- 253
df <- opy * 6
xv <- rnorm(df, 1 / sqrt(opy))
mysr <- as.sr(xv,opy=opy)
confint(mysr,level=0.90)
# using "lm" class
yv <- xv + rnorm(length(xv))
amod <- lm(yv ~ xv)
mysr <- as.sr(amod,opy=opy)
confint(mysr,level.lo=0.05,level.hi=1.0)
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\references{
  Sharpe, William F. "Mutual fund performance." Journal of
  business (1966): 119-138.
  \url{http://ideas.repec.org/a/ucp/jnlbus/v39y1965p119.html}
}
\seealso{
  \code{\link{confint}}, \code{\link{se}},
  \code{\link{qlambdap}}

  Other sr: \code{\link{as.sr}},
  \code{\link{as.sr.default}}, \code{\link{as.sr.lm}},
  \code{\link{as.sr.xts}}, \code{\link{dsr}},
  \code{\link{is.sr}}, \code{\link{plambdap}},
  \code{\link{power.sr.test}}, \code{\link{reannualize}},
  \code{\link{se}}, \code{\link{se.default}},
  \code{\link{se.sr}}, \code{\link{sr}},
  \code{\link{sr_equality_test}}, \code{\link{sr_test}},
  \code{\link{sr_test.default}}, \code{\link{sr_test.sr}}
}
\keyword{htest}


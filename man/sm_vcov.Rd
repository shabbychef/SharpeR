\name{sm_vcov}
\alias{sm_vcov}
\title{Compute variance covariance of 'Unified' Second Moment}
\usage{
  sm_vcov(X,vcov.func=vcov)
}
\arguments{
  \item{X}{an \eqn{n \times p}{n x p} matrix of observed
  returns.}

  \item{vcov.func}{a function which takes an object of
  class \code{lm}, and computes a variance-covariance
  matrix.}
}
\value{
  a list containing the following components: \item{mu}{a
  \eqn{q = p(p+3)/2} vector of the mean, then the vech'd
  second moment of the sample data} \item{Ohat}{the \eqn{q
  \times q}{q x q} estimated variance covariance matrix.}
  \item{n}{the number of rows in \code{X}.} \item{p}{the
  number of assets.}
}
\description{
  Computes the variance covariance matrix of sample mean
  and second moment.
}
\details{
  Given \eqn{p}-vector \eqn{x}, the 'unified' sample is the
  \eqn{p(p+3)/2} vector of \eqn{x} stacked on top of
  \eqn{\mbox{vech}(x x^{\top})}{vech(x x')}. Given \eqn{n}
  contemporaneous observations of \eqn{p}-vectors, stacked
  as rows in the \eqn{n \times p}{n x p} matrix \eqn{X},
  this function computes the mean and the
  variance-covariance matrix of the 'unified' sample.

  One may use the default method for computing covariance,
  via the \code{\link{vcov}} function, or via a 'fancy'
  estimator, like \code{sandwich:vcovHAC},
  \code{sandwich:vcovHC}, \emph{etc.}
}
\examples{
X <- matrix(rnorm(1000*3),ncol=3)
Sigmas <- sm_vcov(X)
# make it fat tailed:
X <- matrix(rt(1000*3,df=5),ncol=3)
Sigmas <- sm_vcov(X)
\dontrun{
if (require(sandwich)) {
 Sigmas <- sm_vcov(X,vcov.func=vcovHC)
}
}
# add some autocorrelation to X
Xf <- filter(X,c(0.2),"recursive")
colnames(Xf) <- colnames(X)
Sigmas <- sm_vcov(Xf)
\dontrun{
if (require(sandwich)) {
	Sigmas <- sm_vcov(Xf,vcov.func=vcovHAC)
}
}
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  \code{\link{ism_vcov}}, \code{\link{sr_vcov}}
}
\keyword{univar}


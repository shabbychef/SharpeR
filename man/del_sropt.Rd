\name{del_sropt}
\alias{del_sropt}
\title{Create an 'del_sropt' object.}
\usage{
  del_sropt(z.s,z.sub,df1,df2,del_df,drag=0,ope=1,epoch="yr")
}
\arguments{
  \item{z.s}{an optimum Sharpe ratio statistic, on some set
  of assets.}

  \item{z.sub}{an optimum Sharpe ratio statistic, on a
  linear subspace of the assets.  If larger than \code{z.s}
  an error is thrown.}

  \item{del_df}{the difference in rank of the full set and
  the linear subspace. Equivalently the 'number of degrees
  of freedom lost' by restricting attention to the
  subspace.}

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

  \item{drag}{the 'drag' term, \eqn{c_0/R}{c0/R}. defaults
  to 0. It is assumed that \code{drag} has been annualized,
  \emph{i.e.} has been multiplied by
  \eqn{\sqrt{ope}}{sqrt(ope)}. This is in contrast to the
  \code{c0} term given to \code{\link{sr}}.}

  \item{epoch}{the string representation of the 'epoch',
  defaulting to 'yr'.}
}
\value{
  a list cast to class \code{del_sropt}.
}
\description{
  Spawns an object of class \code{del_sropt}.
}
\details{
  The \code{del_sropt} class contains information about the
  difference between two rescaled T^2-statistics, useful
  for spanning tests, and inference on hedged portfolios.
  The following are list attributes of the object:
  \itemize{ \item \code{sropt} The (optimal) Sharpe ratio
  statistic of the 'full' set of assets. \item
  \code{sropt_sub} The (optimal) Sharpe ratio statistic on
  some subset, or linear subspace, of the assets. \item
  \code{df1} The number of assets. \item \code{df2} The
  number of observations. \item \code{del_df} The number of
  degrees of freedom lost when constrained to the subspace,
  or the difference in number of assets when a subset.
  \item \code{drag} The drag term, which is the 'risk free
  rate' divided by the maximum risk. \item \code{ope} The
  'observations per epoch'. \item \code{epoch} The string
  name of the 'epoch'. }

  For the most part, this constructor should \emph{not} be
  called directly, rather \code{\link{as.del_sropt}} should
  be called instead to compute the needed statistics.
}
\note{
  2FIX: allow rownames?
}
\examples{
# roll your own.
ope <- 253

set.seed(as.integer(charToRaw("be determinstic")))
X <- matrix(rnorm(1000*10),nrow=1000)
Sigma <- cov(X)
mu <- colMeans(X)
w <- solve(Sigma,mu)
z <- t(mu) \%*\% w
wsub <- solve(Sigma[1:5,1:5],mu[1:5])
zsub <- t(mu[1:5]) \%*\% wsub
del_df <- length(w) - length(wsub)

roll.own <- sropt(z.s=z,z.sub=zsub,df1=10,df2=1000,del_df=del_df,ope=ope)
print(roll.own)

del_sropt(z.s,z.sub,df1,df2,del_df,drag=0,ope=1,epoch="yr")
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  \code{\link{reannualize}}

  \code{\link{as.del_sropt}}

  Other sropt: \code{\link{as.sropt}},
  \code{\link{as.sropt}}, \code{\link{as.sropt.default}},
  \code{\link{as.sropt.default}},
  \code{\link{as.sropt.xts}}, \code{\link{as.sropt.xts}},
  \code{\link{confint.sr}}, \code{\link{confint.sropt}},
  \code{\link{dsropt}}, \code{\link{is.sropt}},
  \code{\link{is.sropt}}, \code{\link{power.sropt_test}},
  \code{\link{reannualize}}, \code{\link{reannualize.sr}},
  \code{\link{reannualize.sropt}}, \code{\link{sropt}},
  \code{\link{sropt_test}}
}
\keyword{univar}


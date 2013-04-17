\name{reannualize}
\alias{reannualize}
\title{Change the annualization of a Sharpe ratio.}
\usage{
  reannualize(x,opy,epoch="yr")
}
\arguments{
  \item{x}{an object of class \code{sr}.}

  \item{opy}{the new observations per year (or epoch).}

  \item{epoch}{a string representation of the epoch.}
}
\value{
  an object of class \code{sr} with the annualization
  parameter updated.
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
}


\name{reannualize}
\alias{reannualize}
\title{Change the annualization of a Sharpe ratio.}
\usage{
  reannualize(x,opy)
}
\arguments{
  \item{x}{an object of class \code{sr}}

  \item{opy}{the new observations per year}
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
mysr <- sr(rnorm(253*8),opy=253)
# turn back to 'daily' Sharpe
mysr2 <- reannualize(mysr,opy=1)
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  sr
}


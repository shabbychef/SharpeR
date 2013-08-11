\name{reannualize}
\alias{reannualize}
\alias{reannualize.sr}
\alias{reannualize.sropt}
\title{Change the annualization of a Sharpe ratio.}
\usage{
  reannualize(object, new.ope = NULL, new.epoch = NULL)

  \method{reannualize}{sr} (object, new.ope = NULL,
    new.epoch = NULL)

  \method{reannualize}{sropt} (object, new.ope = NULL,
    new.epoch = NULL)
}
\arguments{
  \item{object}{an object of class \code{sr} or
  \code{sropt}.}

  \item{new.ope}{the new observations per epoch. If none
  given, it is not updated.}

  \item{new.epoch}{a string representation of the epoch. If
  none given, it is not updated.}
}
\value{
  the input object with the annualization and/or epoch
  updated.
}
\description{
  Changes the annualization factor of a Sharpe ratio
  statistic, or the rate at which observations are made.
}
\examples{
# compute a 'daily' Sharpe
mysr <- as.sr(rnorm(253*8),ope=1,epoch="day")
# turn into annual
mysr2 <- reannualize(mysr,new.ope=253,new.epoch="yr")

# for sropt
ope <- 253
zeta.s <- 1.0
df1 <- 10
df2 <- 6 * ope
rvs <- rsropt(1,df1,df2,zeta.s,ope,drag=0)
roll.own <- sropt(z.s=rvs,df1,df2,drag=0,ope=ope,epoch="yr")
# make 'monthly'
roll.monthly <- reannualize(roll.own,new.ope=21,new.epoch="mo.")
# make 'daily'
roll.daily <- reannualize(roll.own,new.ope=1,new.epoch="day")
}
\author{
  Steven E. Pav \email{shabbychef@gmail.com}
}
\seealso{
  sr

  sropt

  Other sr: \code{\link{as.sr}},
  \code{\link{as.sr.data.frame}},
  \code{\link{as.sr.default}}, \code{\link{as.sr.lm}},
  \code{\link{as.sr.matrix}},
  \code{\link{as.sr.timeSeries}}, \code{\link{as.sr.xts}},
  \code{\link{confint.sr}}, \code{\link{confint.sropt}},
  \code{\link{dsr}}, \code{\link{is.sr}},
  \code{\link{power.sr_test}}, \code{\link{print.sr}},
  \code{\link{print.sropt}}, \code{\link{se}},
  \code{\link{se.sr}}, \code{\link{sr}},
  \code{\link{sr_equality_test}}, \code{\link{sr_test}}

  Other sropt: \code{\link{as.sropt}},
  \code{\link{as.sropt.default}},
  \code{\link{as.sropt.xts}}, \code{\link{confint.sr}},
  \code{\link{confint.sropt}}, \code{\link{dsropt}},
  \code{\link{is.sropt}}, \code{\link{power.sropt_test}},
  \code{\link{sropt}}, \code{\link{sropt_test}}
}


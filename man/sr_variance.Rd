% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sr_bias.r
\name{sr_variance}
\alias{sr_variance}
\title{sr_variance .}
\usage{
sr_variance(snr, n, cumulants)
}
\arguments{
\item{snr}{the population Signal Noise ratio. Often one will use
the population estimate instead.}

\item{n}{the sample size that the Shapre ratio is observed on.}

\item{cumulants}{a vector of the third through fourth, or the third
through seventh population cumulants of the random variable.
More terms are needed for the higher accuracy approximation.}
}
\value{
the variance of the sample statistic.
}
\description{
Computes the variance of the sample Sharpe ratio.
}
\details{
The sample Sharpe ratio has variance of the form
\deqn{V = \frac{1}{n}\left(1 + \frac{\zeta^2}{2}\right)
+\frac{1}{n^2}\left(\frac{19\zeta^2}{8} + 2\right)
-\gamma_1\zeta\left(\frac{1}{n} + \frac{5}{2n^2}\right)
+\gamma_2\zeta^2\left(\frac{1}{4n} + \frac{3}{8n^2}\right)
+\frac{5\gamma_3\zeta}{4n^2} 
+\gamma_1^2\left(\frac{7}{4n^2} - \frac{3\zeta^2}{2n^2}\right)
+\frac{39\gamma_2^2\zeta^2}{32n^2} 
-\frac{15\gamma_1\gamma_2\zeta}{4n^2} 
+o\left(n^{-2}\right),}
where \eqn{\zeta} is the population Signal Noise ratio, 
\eqn{n} is the sample size, \eqn{\gamma_1} is the population skewness,
and \eqn{\gamma_2} is the population excess kurtosis, and
\eqn{\gamma_3} through \eqn{\gamma_5} are the fifth through
seventh cumulants of the error term.
This form of the variance appears as Equation (4) in Bao.

See \sQuote{The Sharpe Ratio: Statistics and Applications},
section 3.2.3.
}
\examples{
# variance under normality:
sr_variance(1, 100, rep(0,5))
}
\references{
Bao, Yong. "Estimation Risk-Adjusted Sharpe Ratio and Fund Performance Ranking 
Under a General Return Distribution." Journal of Financial Econometrics 7, 
no. 2 (2009): 152-173. \doi{10.1093/jjfinec/nbn022}

Pav, S. E. "The Sharpe Ratio: Statistics and Applications." CRC Press, 2021.
}
\seealso{
\code{\link{sr_bias}}.
}
\author{
Steven E. Pav \email{shabbychef@gmail.com}
}

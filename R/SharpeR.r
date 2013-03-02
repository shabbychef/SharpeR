# Copyright 2012 Steven E. Pav. All Rights Reserved.
# Author: Steven E. Pav

# This file is part of SharpeR.
#
# SharpeR is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# SharpeR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with SharpeR.  If not, see <http://www.gnu.org/licenses/>.

# env var:
# nb: 
# see also:
# todo:
# changelog: 
#
# Created: 2012.05.19
# Copyright: Steven E. Pav, 2012-2013
# Author: Steven E. Pav
# Comments: Steven E. Pav
# SVN: $Id: blankheader.txt 25454 2012-02-14 23:35:25Z steven $

#' Inference on Sharpe ratio and Markowitz portfolio.
#' 
#'
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @references
#'
#' Sharpe, William F. "Mutual fund performance." Journal of business (1966): 119-138.
#' \url{http://ideas.repec.org/a/ucp/jnlbus/v39y1965p119.html}
#' 
#' Lo, Andrew W. "The statistics of Sharpe ratios." Financial Analysts Journal 58, no. 4 
#' (2002): 36-52. \url{http://ssrn.com/paper=377260}
#'
#' Opdyke, J. D. "Comparing Sharpe Ratios: So Where are the p-values?" Journal of Asset
#' Management 8, no. 5 (2006): 308-336. \url{http://ssrn.com/paper=886728}
#'
#' Johnson, N. L., and Welch, B. L. "Applications of the non-central t-distribution."
#' Biometrika 31, no. 3-4 (1940): 362-389. \url{http://dx.doi.org/10.1093/biomet/31.3-4.362}
#'
#' Kan, Raymond and Smith, Daniel R. "The Distribution of the Sample Minimum-Variance Frontier."
#' Journal of Management Science 54, no. 7 (2008): 1364--1380.
#' \url{http://mansci.journal.informs.org/cgi/doi/10.1287/mnsc.1070.0852}
#'
#' Kan, Raymond and Zhou, GuoFu. "Tests of Mean-Variance Spanning."
#' Annals of Economics and Finance 13, no. 1 (2012)
#' \url{http://www.aeconf.net/Articles/May2012/aef130105.pdf}
#'
#' Britten-Jones, Mark. "The Sampling Error in Estimates of Mean-Variance 
#' Efficient Portfolio Weights." The Journal of Finance 54, no. 2 (1999):
#' 655--671. \url{http://www.jstor.org/stable/2697722}
#'
#' Silvapulle, Mervyn. J. "A Hotelling's T2-type statistic for testing against 
#' one-sided hypotheses." Journal of Multivariate Analysis 55, no. 2 (1995):
#' 312--319. \url{http://dx.doi.org/10.1006/jmva.1995.1081}
#'
#' Bodnar, Taras and Okhrin, Yarema. "On the Product of Inverse Wishart
#' and Normal Distributions with Applications to Discriminant Analysis 
#' and Portfolio Theory." Scandinavian Journal of Statistics 38, no. 2 (2011):
#' 311--331. \url{http://dx.doi.org/10.1111/j.1467-9469.2011.00729.x}
#'
#' @name SharpeR
#' @docType package
#' @title ...
#' @keywords package
#' @note The following are still in the works:
#' \enumerate{
#' \item Corrections for standard error based on skew, kurtosis and
#' autocorrelation.
#' \item Tests on Sharpe under positivity constraint. (\emph{c.f.} Silvapulle)
#' \item Portfolio spanning tests.
#' \item Tests on portfolio weights.
#' \item Tests of hedge restrictions.
#' }
#'
NULL
